# tests/testthat/test-single_turn.R
library(testthat)
library(httptest2) # Or library(httptest) if using that
library(withr)
library(jsonlite)

# Assume your package name is 'yourPackageName'
# library(yourPackageName) # Load functions if testing installed package
# Or use devtools::load_all() interactively before running tests


# --- Test Group 1: Input Validation ---
# (No changes needed in this group)

test_that("Error on missing or invalid 'user' prompt", {
  expect_error(single_turn(user = NULL), regexp = "'user' message must be")
  expect_error(single_turn(user = 123), regexp = "'user' message must be")
  expect_error(single_turn(user = ""), regexp = "'user' message must be")
  expect_error(single_turn(user = " "), regexp = "'user' message must be")
  expect_error(single_turn(user = c("a", "b")), regexp = "'user' message must be")
})

test_that("Error on invalid 'system' prompt", {
  # NULL is allowed, character is allowed
  expect_error(single_turn(user = "test", system = 123), regexp = "'system' message must be")
  expect_error(single_turn(user = "test", system = c("a", "b")), regexp = "'system' message must be")
  # Check it runs with valid system prompts (implicitly tested later)
})

test_that("Error on invalid 'org'", {
  expect_error(single_turn(user = "test", org = "invalid"), regexp = "Invalid 'org' specified")
  expect_error(single_turn(user = "test", org = 123), regexp = "Invalid 'org' specified")
  expect_error(single_turn(user = "test", org = c("google", "openai")), regexp = "Invalid 'org' specified") # match.arg fails
})

test_that("Error on invalid 'model'", {
  # NULL is allowed
  expect_error(single_turn(user = "test", model = 123), regexp = "'model' must be NULL or a non-empty")
  expect_error(single_turn(user = "test", model = ""), regexp = "'model' must be NULL or a non-empty")
  expect_error(single_turn(user = "test", model = " "), regexp = "'model' must be NULL or a non-empty")
  expect_error(single_turn(user = "test", model = c("a", "b")), regexp = "'model' must be NULL or a non-empty")
})

test_that("Error on invalid 'temperature'", {
  expect_error(single_turn(user = "test", temperature = -0.1), regexp = "'temperature' must be.*non-negative")
  expect_error(single_turn(user = "test", temperature = "high"), regexp = "'temperature' must be.*non-negative")
  expect_error(single_turn(user = "test", temperature = c(0.1, 0.2)), regexp = "'temperature' must be.*non-negative")
})

test_that("Error on invalid 'max_tokens'", {
  expect_error(single_turn(user = "test", max_tokens = 0), regexp = "'max_tokens' must be.*positive integer")
  expect_error(single_turn(user = "test", max_tokens = -100), regexp = "'max_tokens' must be.*positive integer")
  expect_error(single_turn(user = "test", max_tokens = 100.5), regexp = "'max_tokens' must be.*positive integer")
  expect_error(single_turn(user = "test", max_tokens = "many"), regexp = "'max_tokens' must be.*positive integer")
  expect_error(single_turn(user = "test", max_tokens = c(100, 200)), regexp = "'max_tokens' must be.*positive integer")
})

test_that("Error on invalid 'timeout'", {
  expect_error(single_turn(user = "test", timeout = 0), regexp = "'timeout' must be.*positive number")
  expect_error(single_turn(user = "test", timeout = -10), regexp = "'timeout' must be.*positive number")
  expect_error(single_turn(user = "test", timeout = "long"), regexp = "'timeout' must be.*positive number")
  expect_error(single_turn(user = "test", timeout = c(60, 120)), regexp = "'timeout' must be.*positive number")
})

test_that("Error on invalid 'output_format'", {
  expect_error(single_turn(user = "test", output_format = "invalid"),
               regexp = "Invalid 'output_format' specified")
  expect_error(single_turn(user = "test", output_format = 1),
               regexp = "'output_format' must be a single character string")
  expect_error(single_turn(user = "test", output_format = c("text", "jsonl")),
               regexp = "'output_format' must be a single character string") # Now fails length check
})

test_that("Error on invalid 'jsonl_file' when needed", {
  expect_error(
    single_turn(user = "test", output_format = "jsonl"),
    regexp = "'jsonl_file' must be provided.*when output_format is 'jsonl'"
  )
  expect_error(
    single_turn(user = "test", output_format = "jsonl", jsonl_file = NULL),
    regexp = "'jsonl_file' must be provided.*when output_format is 'jsonl'"
  )
  expect_error(
    single_turn(user = "test", output_format = "jsonl", jsonl_file = ""),
    regexp = "'jsonl_file' must be provided.*when output_format is 'jsonl'"
  )
  expect_error(
    single_turn(user = "test", output_format = "jsonl", jsonl_file = 123),
    regexp = "'jsonl_file' must be provided.*when output_format is 'jsonl'"
  )
})

# --- Test Group 2: API Key Handling ---
# (No changes needed in this group)

# Use withr::local_envvar for temporary environment changes
test_that("Error if API key env var is not set", {
  local_envvar(GOOGLE_API_KEY = NA) # Unset it
  expect_error(
    single_turn(user = "test", org = "google"),
    regexp = "API key environment variable 'GOOGLE_API_KEY' not set"
  )

  local_envvar(ANTHROPIC_API_KEY = NA)
  expect_error(
    single_turn(user = "test", org = "anthropic"),
    regexp = "API key environment variable 'ANTHROPIC_API_KEY' not set"
  )

  local_envvar(OPENAI_API_KEY = NA)
  expect_error(
    single_turn(user = "test", org = "openai"),
    regexp = "API key environment variable 'OPENAI_API_KEY' not set"
  )
})

test_that("Error if API key env var is empty", {
  local_envvar(GOOGLE_API_KEY = "")
  expect_error(
    single_turn(user = "test", org = "google"),
    regexp = "API key environment variable 'GOOGLE_API_KEY' not set or empty"
  )
  local_envvar(ANTHROPIC_API_KEY = "")
  expect_error(
    single_turn(user = "test", org = "anthropic"),
    regexp = "API key environment variable 'ANTHROPIC_API_KEY' not set or empty"
  )
  local_envvar(OPENAI_API_KEY = "")
  expect_error(
    single_turn(user = "test", org = "openai"),
    regexp = "API key environment variable 'OPENAI_API_KEY' not set or empty"
  )
})

# --- Test Group 3: API Calls and Response Handling (using httptest2) ---

# IMPORTANT: These tests require mock JSON response files.
# Create minimal valid JSON responses from each API (including the text content)
# and place them where httptest2 can find them (e.g., tests/testthat/api.openai.com/...).
# Mock file content examples:
# google_success.json: { "candidates": [ { "content": { "parts": [ { "text": "Mock Google Response" } ] } } ] }
# anthropic_success.json: { "content": [ { "type": "text", "text": "Mock Anthropic Response" } ] }
# openai_success.json: { "choices": [ { "message": { "content": "Mock OpenAI Response" } } ] }

FAKE_KEYS <- list(
  GOOGLE_API_KEY = "fake-google-key",
  ANTHROPIC_API_KEY = "fake-anthropic-key",
  OPENAI_API_KEY = "fake-openai-key"
)

# Default models used in tests if not specified
DEFAULT_GOOGLE_MODEL <- "gemini-1.5-flash-latest" # Match function default
DEFAULT_ANTHROPIC_MODEL <- "claude-3-haiku-20240307"
DEFAULT_OPENAI_MODEL <- "gpt-4o-mini"

test_that("Google: successful text response", {
  skip_if_not_installed("httptest2")
  skip("Requires mock: POST to generativelanguage.googleapis.com/.../gemini-1.5-flash-latest:generateContent returning google_success.json")

  with_mock_api({
    withr::with_envvar(FAKE_KEYS, {
      expect_message(
        response <- single_turn(user = "hello google", org = "google", output_format = "text"),
        regexp = paste("Using default model.*", DEFAULT_GOOGLE_MODEL, sep="")
      )
      expect_equal(response, "Mock Google Response")
    })
  }) # End with_mock_api
})

test_that("Anthropic: successful text response with specified model", {
  skip_if_not_installed("httptest2")
  skip("Requires mock: POST to api.anthropic.com/v1/messages returning anthropic_success.json")

  with_mock_api({
    withr::with_envvar(FAKE_KEYS, {
      # No message expected if model is provided
      expect_no_message(
        response <- single_turn(
          user = "hello anthropic",
          org = "anthropic",
          model = "claude-3-sonnet-20240229", # Specify model
          output_format = "text"
        )
      )
      expect_equal(response, "Mock Anthropic Response")
    })
  })
})

test_that("OpenAI: successful text response with system prompt", {
  skip_if_not_installed("httptest2")
  skip("Requires mock: POST to api.openai.com/v1/chat/completions returning openai_success.json")

  with_mock_api({
    withr::with_envvar(FAKE_KEYS, {
      expect_message(
        response <- single_turn(
          user = "hello openai",
          system = "be brief",
          org = "openai",
          output_format = "text"
        ),
        regexp = paste("Using default model.*", DEFAULT_OPENAI_MODEL, sep="")
      )
      expect_equal(response, "Mock OpenAI Response")
    })
  })
})

test_that("Successful JSONL output creates file and returns text invisibly", {
  skip_if_not_installed("httptest2")
  skip("Requires mock: POST to generativelanguage.googleapis.com/... returning google_success.json")

  temp_jsonl_file <- local_tempfile(fileext = ".jsonl")

  with_mock_api({
    withr::with_envvar(FAKE_KEYS, {
      # Capture the result to check invisible value
      result <- NULL
      expect_message(
        # Use capture.output to suppress printing of invisible value in test log
        # Use expect_visible/invisible if testthat version supports well
        utils::capture.output(
          result <- single_turn(
            user = "write json test",
            org = "google",
            output_format = "jsonl",
            jsonl_file = temp_jsonl_file
          ), type="message" # only capture messages
        ),
        regexp = "Using default model"
      )

      # Check INVISIBLE return value (should be the extracted text)
      expect_equal(result, "Mock Google Response")
      # You might add expect_invisible(result) here if needed/supported

      # Check file content
      expect_true(file.exists(temp_jsonl_file))
      json_lines <- readLines(temp_jsonl_file)
      expect_length(json_lines, 1)

      # Parse the JSON line and check its content against the mock response
      parsed_content <- jsonlite::fromJSON(json_lines[1])
      expect_equal(parsed_content$candidates[[1]]$content$parts[[1]]$text, "Mock Google Response")

    }) # End envvar
  }) # End mock_api
})

test_that("JSONL output appends and returns text invisibly", {
  skip_if_not_installed("httptest2")
  skip("Requires mock: POST to api.anthropic.com/v1/messages returning anthropic_success.json")

  temp_jsonl_file <- local_tempfile(fileext = ".jsonl")

  # Initial content
  initial_content <- '{"initial": "data"}'
  writeLines(initial_content, temp_jsonl_file)

  with_mock_api({
    withr::with_envvar(FAKE_KEYS, {
      result_append <- NULL
      expect_no_message( # model specified
        utils::capture.output(
          result_append <- single_turn(
            user = "append test",
            org = "anthropic",
            model = DEFAULT_ANTHROPIC_MODEL, # Specify model
            output_format = "jsonl",
            jsonl_file = temp_jsonl_file
          ), type="message"
        )
      )
      # Check INVISIBLE return value
      expect_equal(result_append, "Mock Anthropic Response")

      # Check file content
      all_lines <- readLines(temp_jsonl_file)
      expect_length(all_lines, 2)
      expect_equal(all_lines[1], initial_content)

      # Check appended line
      parsed_append <- jsonlite::fromJSON(all_lines[2])
      expect_equal(parsed_append$content[[1]]$text, "Mock Anthropic Response") # From mock file

    }) # End envvar
  }) # End mock_api
})


# --- Test Group 4: Error Handling during API Call/Response ---

test_that("Handles HTTP 4xx error from API", {
  skip_if_not_installed("httptest2")
  skip("Requires mock: POST returning 401 status and error body")

  # Mock URL for Google default model
  mock_google_url <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    DEFAULT_GOOGLE_MODEL,
    ":generateContent?key=", FAKE_KEYS$GOOGLE_API_KEY
  )

  # Minimal example of how to mock a specific response status/body with httptest2
  # Actual implementation might vary based on setup (e.g., using .mockPaths)
  # Register a mock response for the specific request
  register_mock_response(
    url = mock_google_url,
    method = "POST",
    response = response(
      status_code = 401,
      headers = list(`Content-Type` = "application/json; charset=UTF-8"),
      body = charToRaw('{ "error": { "code": 401, "message": "API key invalid mock", "status": "UNAUTHENTICATED" } }')
    )
  )

  with_mock_api({
    withr::with_envvar(FAKE_KEYS, {
      expect_error(
        single_turn(user = "test 401", org = "google"),
        # Check for parts of the error generated by req_error's body function
        regexp = "API request failed with status 401.*Provider: google.*API key invalid mock"
      )
    })
  })
  # Clean up registered mocks if necessary (depends on httptest2 usage pattern)
  # clear_mock_responses()
})

test_that("Handles HTTP 5xx error from API", {
  skip_if_not_installed("httptest2")
  skip("Requires mock: POST returning 500 status")

  # Mock URL for OpenAI default model
  mock_openai_url <- "https://api.openai.com/v1/chat/completions"

  register_mock_response(
    url = mock_openai_url,
    method = "POST",
    response = response(
      status_code = 500,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw('{ "error": { "message": "Server error mock", "type": "server_error" } }')
    )
  )

  with_mock_api({
    withr::with_envvar(FAKE_KEYS, {
      expect_error(
        single_turn(user = "test 500", org = "openai"),
        regexp = "API request failed with status 500.*Provider: openai.*Server error mock"
      )
    })
  })
  # clear_mock_responses()
})

test_that("Handles text extraction failure (malformed JSON) for format='text'", {
  skip_if_not_installed("httptest2")
  skip("Requires mock: POST returning 200 OK but with JSON missing the text field")

  # Mock a response for OpenAI that's valid JSON but missing 'content'
  mock_openai_malformed_json <- '{
    "id": "chatcmpl-mockid", "object": "chat.completion", "created": 1700000000,
    "model": "gpt-4o-mini",
    "choices": [ { "index": 0, "message": { "role": "assistant" }, "finish_reason": "stop" } ],
    "usage": { "prompt_tokens": 9, "completion_tokens": 0, "total_tokens": 9 }
  }'

  register_mock_response(
    url = "https://api.openai.com/v1/chat/completions",
    method = "POST",
    response = response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(mock_openai_malformed_json)
    )
  )

  with_mock_api({
    withr::with_envvar(FAKE_KEYS, {
      # Expect a WARNING because extraction fails
      expect_warning(
        result <- single_turn(user = "test malformed", org = "openai", output_format = "text"),
        regexp = "Could not extract text for response.*returning NA"
      )
      # Expect the function to return NA_character_ now, not error out
      expect_equal(result, NA_character_)
    })
  })
  # clear_mock_responses()
})

test_that("Handles text extraction failure (malformed JSON) for format='jsonl'", {
  skip_if_not_installed("httptest2")
  skip("Requires mock: POST returning 200 OK but with JSON missing the text field")

  temp_jsonl_file <- local_tempfile(fileext = ".jsonl")

  # Use the same malformed JSON as the previous test
  mock_openai_malformed_json <- '{
    "id": "chatcmpl-mockid", "object": "chat.completion", "created": 1700000000,
    "model": "gpt-4o-mini",
    "choices": [ { "index": 0, "message": { "role": "assistant" }, "finish_reason": "stop" } ],
    "usage": { "prompt_tokens": 9, "completion_tokens": 0, "total_tokens": 9 }
  }'

  register_mock_response(
    url = "https://api.openai.com/v1/chat/completions",
    method = "POST",
    response = response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(mock_openai_malformed_json)
    )
  )

  with_mock_api({
    withr::with_envvar(FAKE_KEYS, {
      result <- NULL
      # Expect a WARNING because extraction fails
      expect_warning(
        utils::capture.output(
          result <- single_turn(
            user = "test malformed jsonl",
            org = "openai",
            output_format = "jsonl",
            jsonl_file = temp_jsonl_file
          ), type="message"
        ),
        regexp = "Could not extract text for response.*returning NA"
      )
      # Check that the function completed and returned NA invisibly
      expect_equal(result, NA_character_)

      # CRITICAL: Check that the malformed JSON WAS written to the file
      expect_true(file.exists(temp_jsonl_file))
      json_lines <- readLines(temp_jsonl_file)
      expect_length(json_lines, 1)
      # Quick check if the written line looks like the input JSON
      expect_true(startsWith(json_lines[1], '{"id":"chatcmpl-mockid"'))
    })
  })
  # clear_mock_responses()
})


test_that("Handles error during JSON parsing (invalid JSON response body)", {
  skip_if_not_installed("httptest2")
  skip("Requires mock: POST returning 200 OK but with non-JSON body")

  # Mock a response that's not JSON
  mock_invalid_body <- "<html><body>Server Error</body></html>"

  register_mock_response(
    url = "https://api.anthropic.com/v1/messages", # Test with Anthropic this time
    method = "POST",
    response = response(
      status_code = 200, # API call succeeded, but body is wrong
      headers = list(`Content-Type` = "text/html"), # Header might indicate non-JSON
      body = charToRaw(mock_invalid_body)
    )
  )

  with_mock_api({
    withr::with_envvar(FAKE_KEYS, {
      # Test for text output format
      expect_error(
        single_turn(user="test invalid json", org="anthropic", output_format="text"),
        regexp = "Error processing API response:.*lexical error: invalid char|unexpected character.*Server Error"
      )

      # Test for jsonl output format - should also error here
      temp_jsonl_file <- local_tempfile(fileext = ".jsonl")
      expect_error(
        single_turn(user="test invalid json", org="anthropic", output_format="jsonl", jsonl_file=temp_jsonl_file),
        regexp = "Error processing API response:.*lexical error: invalid char|unexpected character.*Server Error"
      )
      # Ensure file was NOT written to
      expect_false(file.exists(temp_jsonl_file)) # Or check if size is 0 if file might be created then error occurs
    })
  })
  # clear_mock_responses()
})

# --- Test Group 5: Edge Cases ---

# test_that("Handles empty string system prompt correctly", {
#   skip_if_not_installed("httptest2")
#   skip("Requires mock: POST for request *without* system prompt content")
#
#   # This test verifies "" doesn't cause validation errors and the function still runs.
#   # A more advanced test would capture the request body to ensure the 'system' field
#   # is handled correctly (e.g., omitted for OpenAI/Anthropic if empty).
#   # Assume openai_success.json is the correct mock for a request without a system prompt.
#
#   register_mock_response( # Assume this serves openai_success.json
#     url = "https://api.openai.com/v1/chat/completions",
#     method = "POST",
#     response = response( status_code = 200, ... body = openai_success_json ... )
#   )
#
#   with_mock_api({
#     withr::with_envvar(FAKE_KEYS, {
#       expect_no_error( # Check it doesn't fail validation
#         res <- single_turn(user="test empty system", system="", org="openai", output_format="text")
#       )
#       # Check it still returns the expected mock response
#       expect_equal(res, "Mock OpenAI Response")
#     })
#   })
#   # clear_mock_responses()
# })

# Note: Testing the directory warning for jsonl_file is omitted as it's low priority
# and potentially tricky to set up reliably across different environments.
