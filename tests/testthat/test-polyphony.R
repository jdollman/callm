# tests/testthat/test-polyphony.R

library(testthat)
# Assuming your package name is 'callm' - adjust if different
# library(callm) # Usually done via devtools::load_all() or DESCRIPTION Depends/Imports

# --- Mock single_turn ---
# This mock allows us to test polyphony without real API calls
# It captures arguments and returns predefined results or errors

# Store captured calls here for inspection
mock_calls <- list()

# Define the mock function
mock_single_turn <- function(user, system = NULL, org, model, max_tokens = 1024L, timeout = 60, temperature = NULL, output_format = "text", ...) {
  # Capture the arguments for inspection later
  call_args <- list(
    user = user, system = system, org = org, model = model,
    max_tokens = max_tokens, timeout = timeout, temperature = temperature,
    output_format = output_format, dots = list(...)
  )
  # Use <<- to modify mock_calls in the parent env (test env)
  mock_calls[[length(mock_calls) + 1]] <<- call_args

  # Predefined behavior based on model name (for testing errors)
  if (model == "fail-model") {
    stop("Simulated API error for fail-model")
  }
  if (model == "another-fail") {
    stop("Another simulated failure")
  }

  # Return a predictable success string
  return(paste("Success from mock:", org, model, "temp:", ifelse(is.null(temperature), "default", temperature)))
}


# --- Test Suite ---

test_that("polyphony runs successfully with valid inputs", {
  # Use local_mocked_bindings for testthat 3e+ style mocking
  testthat::local_mocked_bindings(
    single_turn = mock_single_turn,
    .package = "callm" # Specify the package where single_turn should be mocked
  )
  # Reset mock calls before test
  mock_calls <<- list()

  perspectives <- list(
    list(id = "p1", org = "org1", model = "modelA", temperature = 0.5),
    list(id = "p2", org = "org2", model = "modelB") # Uses defaults
  )

  # Run without verbose messages in tests unless specifically testing messages
  results <- callm::polyphony(
    user = "test prompt",
    perspectives = perspectives,
    verbose = FALSE
  )

  expect_type(results, "list")
  expect_length(results, 2)
  expect_named(results, c("p1", "p2"))
  expect_equal(results$p1, "Success from mock: org1 modelA temp: 0.5")
  expect_equal(results$p2, "Success from mock: org2 modelB temp: default")

  # Check arguments passed to mock_single_turn
  expect_length(mock_calls, 2)
  # Check call 1 (p1)
  expect_equal(mock_calls[[1]]$user, "test prompt")
  expect_equal(mock_calls[[1]]$org, "org1")
  expect_equal(mock_calls[[1]]$model, "modelA")
  expect_equal(mock_calls[[1]]$temperature, 0.5)
  expect_equal(mock_calls[[1]]$max_tokens, 1024L) # Default from polyphony
  expect_equal(mock_calls[[1]]$output_format, "text") # Forced by polyphony
  # Check call 2 (p2)
  expect_equal(mock_calls[[2]]$user, "test prompt")
  expect_equal(mock_calls[[2]]$org, "org2")
  expect_equal(mock_calls[[2]]$model, "modelB")
  expect_null(mock_calls[[2]]$temperature) # Not provided
  expect_equal(mock_calls[[2]]$max_tokens, 1024L) # Default from polyphony
  expect_equal(mock_calls[[2]]$output_format, "text") # Forced by polyphony

})

test_that("polyphony respects perspective-specific max_tokens and timeout", {
  testthat::local_mocked_bindings(single_turn = mock_single_turn, .package = "callm")
  mock_calls <<- list()

  perspectives <- list(
    list(id = "p1", org = "org1", model = "modelA", max_tokens = 500, timeout = 30)
  )

  results <- callm::polyphony(
    user = "test prompt",
    perspectives = perspectives,
    max_tokens = 2000L, # polyphony default (overridden)
    timeout = 90,      # polyphony default (overridden)
    verbose = FALSE
  )

  expect_length(mock_calls, 1)
  expect_equal(mock_calls[[1]]$max_tokens, 500L) # Perspective value used
  expect_equal(mock_calls[[1]]$timeout, 30)      # Perspective value used
})


test_that("polyphony handles errors with error_strategy = 'stop'", {
  testthat::local_mocked_bindings(single_turn = mock_single_turn, .package = "callm")
  mock_calls <<- list()

  perspectives <- list(
    list(id = "p_ok", org = "org1", model = "modelA"),
    list(id = "p_fail", org = "org2", model = "fail-model") # This one fails
  )

  expect_error(
    callm::polyphony(
      user = "test prompt",
      perspectives = perspectives,
      error_strategy = "stop",
      verbose = FALSE
    ),
    regexp = "One or more perspectives failed: 'p_fail'" # Check error message
  )

  # Ensure it called single_turn for both before stopping (or just the first two)
  expect_length(mock_calls, 2)

})

test_that("polyphony handles errors with error_strategy = 'return_partial'", {
  testthat::local_mocked_bindings(single_turn = mock_single_turn, .package = "callm")
  mock_calls <<- list()

  perspectives <- list(
    list(id = "p_ok", org = "org1", model = "modelA"),
    list(id = "p_fail", org = "org2", model = "fail-model"), # Fails
    list(id = "p_ok2", org = "org3", model = "modelC")
  )

  results <- callm::polyphony(
    user = "test prompt",
    perspectives = perspectives,
    error_strategy = "return_partial",
    verbose = FALSE
  )

  expect_type(results, "list")
  expect_length(results, 3)
  expect_named(results, c("p_ok", "p_fail", "p_ok2"))

  # Check results types
  expect_type(results$p_ok, "character")
  expect_s3_class(results$p_fail, "error")
  expect_type(results$p_ok2, "character")

  # Ensure it called single_turn for all perspectives
  expect_length(mock_calls, 3)
})

test_that("polyphony validates inputs correctly", {
  # Need perspectives for some checks
  perspectives_valid <- list(list(id="p1", org="o", model="m"))

  # Invalid user
  expect_error(callm::polyphony(user = NULL, perspectives = perspectives_valid), regexp="`user` must be a non-empty character string")
  expect_error(callm::polyphony(user = 123, perspectives = perspectives_valid), regexp="`user` must be a non-empty character string")
  expect_error(callm::polyphony(user = "", perspectives = perspectives_valid), regexp="`user` must be a non-empty character string")

  # Invalid perspectives
  expect_error(callm::polyphony(user = "u", perspectives = NULL), regexp="`perspectives` must be a non-empty list")
  expect_error(callm::polyphony(user = "u", perspectives = list()), regexp="`perspectives` must be a non-empty list")
  expect_error(callm::polyphony(user = "u", perspectives = list("a", "b")), regexp="Each element .* must be a list")
  expect_error(callm::polyphony(user = "u", perspectives = list(list(org="o", model="m"))), regexp="must have a non-empty character string 'id'")
  expect_error(callm::polyphony(user = "u", perspectives = list(list(id="p", model="m"))), regexp="must have a non-empty character string 'org'")
  expect_error(callm::polyphony(user = "u", perspectives = list(list(id="p", org="o"))), regexp="must have a non-empty character string 'model'")
  expect_error(callm::polyphony(user = "u", perspectives = list(list(id="p", org="o", model="m"), list(id="p", org="o2", model="m2"))), regexp="Perspective 'id' values must be unique")

  # Invalid other args
  expect_error(
      callm::polyphony(user = "u", perspectives = perspectives_valid, system = 123),
      regexp = "system.*single character string" # Loose match
  )
  expect_error(callm::polyphony(user = "u", perspectives = perspectives_valid, max_tokens = -1), regexp="`max_tokens` must be a single positive integer")
  expect_error(callm::polyphony(user = "u", perspectives = perspectives_valid, timeout = "fast"), regexp="`timeout` must be a single positive number")
  expect_error(callm::polyphony(user = "u", perspectives = perspectives_valid, verbose = "yes"), regexp="`verbose` must be TRUE or FALSE")
  expect_error(callm::polyphony(user = "u", perspectives = perspectives_valid, error_strategy = "ignore"), regexp="'arg' should be one of") # Partial match error
})


test_that("polyphony verbose output works", {
  testthat::local_mocked_bindings(single_turn = mock_single_turn, .package = "callm")
  mock_calls <<- list()

  perspectives <- list(
    list(id = "p1", org = "org1", model = "modelA"),
    list(id = "p2", org = "org2", model = "modelB")
  )

  # Expect specific messages when verbose = TRUE
  expect_message(
    callm::polyphony(user = "u", perspectives = perspectives, verbose = TRUE),
    regexp = "Processing 2 perspectives sequentially", # Initial message
    fixed = TRUE
  )
  expect_message(
    callm::polyphony(user = "u", perspectives = perspectives, verbose = TRUE),
    regexp = "---> Processing perspective 'p1'", # Message for p1
    fixed = TRUE
  )
  expect_message(
    callm::polyphony(user = "u", perspectives = perspectives, verbose = TRUE),
    regexp = "---> Processing perspective 'p2'", # Message for p2
    fixed = TRUE
  )
  expect_message(
    callm::polyphony(user = "u", perspectives = perspectives, verbose = TRUE),
    regexp = "Finished processing all perspectives", # Final message
    fixed = TRUE
  )


  # Expect *no* processing messages when verbose = FALSE
  # Note: `expect_no_message` isn't a standard testthat function.
  # We test the absence by checking that `expect_message` *fails* to find the pattern.
  # A cleaner way is to capture all output, but this is simpler.
  expect_error( # This expression should NOT produce the message, so expect_message errors
    expect_message(
      callm::polyphony(user = "u", perspectives = perspectives, verbose = FALSE),
      regexp = "---> Processing perspective" # Generic pattern for processing message
    )
  )
  expect_error( # Should not produce the initial message either
    expect_message(
      callm::polyphony(user = "u", perspectives = perspectives, verbose = FALSE),
      regexp = "Processing \\d+ perspectives"
    )
  )

})

test_that("polyphony works with a single perspective", {
  testthat::local_mocked_bindings(single_turn = mock_single_turn, .package = "callm")
  mock_calls <<- list()
  perspectives <- list(
    list(id = "solo", org = "org_s", model = "model_s")
  )
  results <- callm::polyphony(user="test", perspectives = perspectives, verbose=FALSE)

  expect_length(results, 1)
  expect_named(results, "solo")
  expect_equal(results$solo, "Success from mock: org_s model_s temp: default")
  expect_length(mock_calls, 1)
})
