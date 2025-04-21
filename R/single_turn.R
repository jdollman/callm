#' Call Large Language Models (Single Turn)
#'
#' Sends a single user message (with optional system message) to a specified
#' LLM provider and model, handling authentication and API differences.
#' Returns either the extracted text response or appends the full JSON
#' response to a JSONL file.
#'
#' @param user Character string. The user's message/prompt. Required.
#' @param system Character string. An optional system message/instruction. Default is NULL.
#' @param org Character vector. The LLM provider. Defaults to "google".
#'   Handles partial matching (e.g., "goog", "anthro", "open").
#'   Allowed values: "google", "anthropic", "openai".
#' @param model Character string. The specific model ID to use. If NULL (default),
#'   a provider-specific default is chosen (e.g., "gemini-2.0-flash",
#'   "claude-3-haiku-20240307", "gpt-4o-mini").
#' @param temperature Numeric. Sampling temperature (>= 0). Lower values are more
#'   deterministic. Default is 0.0. Note: Different providers may have different
#'   effective upper bounds (e.g., Google <= 1.0, OpenAI/Anthropic <= 2.0).
#'   Validation only checks for >= 0.
#' @param max_tokens Integer. Maximum number of tokens to generate in the response.
#'   Default is 1024L. Required by some providers (Anthropic).
#' @param timeout Numeric. Request timeout in seconds. Default is 60.
#' @param output_format Character vector. How to return the result.
#'   Allowed values: "text" (default), "jsonl". Handles partial matching.
#' @param jsonl_file Character string. The path to the output file if
#'   `output_format` is "jsonl". Required in that case, otherwise ignored.
#'   The full JSON response will be appended as a single line.
#'
#' @return If `output_format` is "text", returns the extracted text content
#'   as a character string (or NA if extraction fails).
#'   If `output_format` is "jsonl", appends the full JSON response to the
#'   specified file and returns `invisible(extracted_text)` (where
#'   `extracted_text` is the attempted text extraction, possibly NA).
#'   Stops execution with an error message on failure (e.g., missing API key,
#'   API error, validation failure, JSON parsing failure). Extraction failures
#'   when `output_format` is "jsonl" produce a warning but allow the function
#'   to complete the file write.
#'
#' @importFrom httr2 request req_method req_headers req_body_json req_timeout
#' @importFrom httr2 req_error req_perform resp_body_string resp_status
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' # Ensure API keys are set as environment variables:
#' # Sys.setenv(GOOGLE_API_KEY = "YOUR_GOOGLE_KEY")
#' # Sys.setenv(ANTHROPIC_API_KEY = "YOUR_ANTHROPIC_KEY")
#' # Sys.setenv(OPENAI_API_KEY = "YOUR_OPENAI_KEY")
#'
#' # --- Text Output Examples ---
#'
#' # Google (default org)
#' response_text_google <- single_turn(user = "Explain the concept of recursion simply.")
#' print(response_text_google)
#'
#' # Anthropic
#' response_text_anthropic <- single_turn(
#'   user = "Write a short poem about R programming.",
#'   org = "anthropic",
#'   model = "claude-3-sonnet-20240229" # Use Sonnet instead of default Haiku
#' )
#' print(response_text_anthropic)
#'
#' # OpenAI with system message
#' response_text_openai <- single_turn(
#'   user = "Why is the sky blue?",
#'   system = "Explain like I'm five years old.",
#'   org = "openai",
#'   model = "gpt-4o",
#'   temperature = 0.7
#' )
#' print(response_text_openai)
#'
#' # --- JSONL Output Example ---
#'
#' tmp_file <- tempfile(fileext = ".jsonl")
#' print(paste("Writing JSONL output to:", tmp_file))
#'
#' # The return value is now the text (invisibly)
#' invisible_text_google <- single_turn(
#'   user = "What is the capital of France?",
#'   org = "google",
#'   output_format = "jsonl",
#'   jsonl_file = tmp_file
#' )
#' # Can capture it if needed:
#' print(paste("Invisible text from Google call:", invisible_text_google))
#'
#' invisible_text_openai <- single_turn(
#'   user = "What are the main benefits of using version control?",
#'   system = "You are a helpful software development assistant.",
#'   org = "openai",
#'   output_format = "jsonl",
#'   jsonl_file = tmp_file
#' )
#' print(paste("Invisible text from OpenAI call:", invisible_text_openai))
#'
#' # Read the results from the file
#' results <- readLines(tmp_file)
#' cat("Contents of JSONL file:\n")
#' cat(results, sep = "\n")
#'
#' # Clean up the temporary file
#' unlink(tmp_file)
#' }
single_turn <- function(
    user,
    system = NULL,
    org = c("google", "anthropic", "openai"),
    model = NULL,
    temperature = 0.0,
    max_tokens = 1024L,
    timeout = 60,
    output_format = c("text"),
    jsonl_file = NULL
) {

  # --- 1. Parameter Validation ---
  org <- tryCatch(
    match.arg(tolower(org), c("google", "anthropic", "openai")),
    error = function(e) stop("Invalid 'org' specified. Choose from 'google', 'anthropic', 'openai'.")
  )

  if (!is.character(output_format) || length(output_format) != 1) {
    stop("'output_format' must be a single character string ('text' or 'jsonl').")
  }

  output_format <- tryCatch(
    match.arg(tolower(output_format), c("text", "jsonl")),
    error = function(e) stop("Invalid 'output_format' specified. Choose from 'text', 'jsonl'.")
  )

  if (is.null(user) || !is.character(user) || length(user) != 1 || !nzchar(trimws(user))) {
    stop("'user' message must be a non-empty character string.")
  }
  if (!is.null(system) && (!is.character(system) || length(system) != 1)) {
    # Allow empty string "" for system prompt if needed by API, just check type
    stop("'system' message must be NULL or a character string.")
  }
  if (!is.null(model) && (!is.character(model) || length(model) != 1 || !nzchar(trimws(model)))) {
    stop("'model' must be NULL or a non-empty character string.")
  }
  if (!is.numeric(temperature) || length(temperature) != 1 || temperature < 0) {
    stop("'temperature' must be a single non-negative number.")
  }
  if (!is.numeric(max_tokens) || length(max_tokens) != 1 || max_tokens <= 0 || (max_tokens %% 1 != 0)) {
    stop("'max_tokens' must be a single positive integer.")
  }
  max_tokens <- as.integer(max_tokens) # Ensure integer type

  if (!is.numeric(timeout) || length(timeout) != 1 || timeout <= 0) {
    stop("'timeout' must be a single positive number.")
  }

  if (output_format == "jsonl") {
    if (is.null(jsonl_file) || !is.character(jsonl_file) || length(jsonl_file) != 1 || !nzchar(trimws(jsonl_file))) {
      stop("'jsonl_file' must be provided as a non-empty character string when output_format is 'jsonl'.")
    }
    if (!dir.exists(dirname(jsonl_file))) {
      # Warning is okay, maybe user wants to create dir later. Error might be too strict.
      warning(paste("Directory for jsonl_file may not exist:", dirname(jsonl_file)))
    }
  }

  # --- 2. Set Default Model ---
  if (is.null(model)) {
    model <- switch(org,
                    "google"    = "gemini-2.0-flash",
                    "anthropic" = "claude-3-haiku-20240307",
                    "openai"    = "gpt-4o-mini"
    )
    message(paste("Using default model for", org, ":", model))
  }

  # --- 3. Get API Key ---
  api_key_env_var <- switch(org,
                            "google"    = "GOOGLE_API_KEY",
                            "anthropic" = "ANTHROPIC_API_KEY",
                            "openai"    = "OPENAI_API_KEY"
  )
  api_key <- Sys.getenv(api_key_env_var)
  if (api_key == "") {
    stop(paste0("API key environment variable '", api_key_env_var, "' not set or empty for organization '", org, "'."))
  }

  # --- 4 & 5. Initialize and Build Provider-Specific Components ---
  api_url <- NULL
  headers_list <- list("Content-Type" = "application/json") # Base header
  body_list <- list()

  # --- SWITCH to build URL, Headers, Body ---
  switch(org,
         "google" = {
           api_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                             model, ":generateContent?key=", api_key)

           # Body construction
           contents_part <- list(list(parts = list(list(text = user))))
           generation_config_part <- list(
             temperature = temperature,
             maxOutputTokens = max_tokens
           )

           if (!is.null(system) && nzchar(trimws(system))) {
             body_list <- list(
               system_instruction = list(parts = list(list(text = system))),
               contents = contents_part,
               generationConfig = generation_config_part
             )
           } else {
             body_list <- list(
               contents = contents_part,
               generationConfig = generation_config_part
             )
           }
           # No specific headers beyond Content-Type needed here
         },

         "anthropic" = {
           api_url <- "https://api.anthropic.com/v1/messages"
           # Add specific headers
           headers_list$`x-api-key` <- api_key
           headers_list$`anthropic-version` <- "2023-06-01" # Keep version static for now

           # Body construction
           messages_part <- list(list(role = "user", content = user))
           body_list <- list(
             model = model,
             messages = messages_part,
             max_tokens = max_tokens,
             temperature = temperature
           )
           # Add optional system prompt
           if (!is.null(system) && nzchar(trimws(system))) {
             body_list$system <- system
           }
         },

         "openai" = {
           api_url <- "https://api.openai.com/v1/chat/completions"
           # Add specific header
           headers_list$Authorization <- paste("Bearer", api_key)

           # Body construction - messages list
           messages_list <- list()
           if (!is.null(system) && nzchar(trimws(system))) {
             messages_list[[length(messages_list) + 1]] <- list(role = "system", content = system)
           }
           messages_list[[length(messages_list) + 1]] <- list(role = "user", content = user)

           body_list <- list(
             model = model,
             messages = messages_list,
             temperature = temperature,
             max_tokens = max_tokens
           )
         }
  ) # End switch

  # --- 6. Build httr2 Request ---
  req <- request(api_url) %>%
    req_method("POST") %>%
    req_headers(!!!headers_list) %>% # Use !!! to splice the named list
    req_body_json(body_list, auto_unbox = TRUE) %>%
    req_timeout(seconds = timeout) %>%
    req_error(is_error = function(resp) resp_status(resp) >= 400,
              body = function(resp) {
                # Provide informative error message on HTTP failure
                paste("API request failed with status", resp_status(resp),
                      "\nProvider:", org,
                      "\nModel:", model,
                      "\nResponse Body:", resp_body_string(resp))
              })

  # --- 7. Perform API Call ---
  resp <- tryCatch({
    req_perform(req)
  }, error = function(e) {
    # Catch errors during the request itself (network issues, etc.)
    stop(paste("Error during API request:", conditionMessage(e)))
  })

  # --- 8. Process Response ---
  tryCatch({
    # Always get raw body for potential JSONL output or error reporting
    raw_body_str <- resp_body_string(resp)

    # Parse JSON once for either output format
    # Using simplifyDataFrame=FALSE and simplifyVector=FALSE is robust
    parsed_body <- jsonlite::fromJSON(raw_body_str, simplifyVector = FALSE, simplifyDataFrame = FALSE)

    # --- Initialize return text value ---
    # ALWAYS try to extract text, even if logging JSONL
    extracted_text_for_return <- NA_character_ # Default to NA

    # --- Attempt Text Extraction (with specific error handling) ---
    tryCatch({
      # Direct Access Logic based on provider
      if (org == "google") {
        extracted_text_for_return <- parsed_body[["candidates"]][[1]][["content"]][["parts"]][[1]][["text"]]
      } else if (org == "anthropic") {
        extracted_text_for_return <- parsed_body[["content"]][[1]][["text"]]
      } else if (org == "openai") {
        extracted_text_for_return <- parsed_body[["choices"]][[1]][["message"]][["content"]]
      } else {
        # This should theoretically not be reached due to earlier validation
        warning("Internal error: Unknown organization in extraction logic.")
        extracted_text_for_return <- NA_character_
      }

      # --- Validation of the extracted result ---
      if (is.null(extracted_text_for_return)) {
        stop("Extracted text is NULL after direct access. Check API response structure.")
      } else if (!is.character(extracted_text_for_return) || length(extracted_text_for_return) != 1) {
        if(is.atomic(extracted_text_for_return) && length(extracted_text_for_return) == 1) {
          warning("Extracted content was atomic but not character, coercing.")
          extracted_text_for_return <- as.character(extracted_text_for_return)
        } else {
          stop(paste("Extracted content is not a single string or coercible value. Class:",
                     class(extracted_text_for_return)))
        }
      }
      # If we got here, text extraction and validation succeeded (within this tryCatch)

    }, error = function(e) {
      # Catch errors specifically from the [[...]] access chain or validation
      # If text extraction fails, log it but DON'T STOP if JSONL is the target
      warning(paste("Could not extract text for response, returning NA. Error:", conditionMessage(e)))
      # Ensure the outer scope variable is NA (using <<- because we are inside error function)
      extracted_text_for_return <<- NA_character_
      # Do not re-throw the error here if we want JSONL to proceed
    }) # End nested tryCatch for text extraction

    # --- Handle JSONL Output ---
    if (output_format == "jsonl") {
      # Convert the parsed R list back to compact JSON for logging
      json_line <- jsonlite::toJSON(parsed_body, auto_unbox = TRUE, pretty = FALSE)
      # Append the JSON line
      tryCatch({
        write(json_line, file = jsonl_file, append = TRUE)
      }, error = function(e_write) {
         # Handle potential file writing errors
         stop(paste("Error writing to jsonl_file '", jsonl_file, "': ", conditionMessage(e_write)))
      })
      # *** CHANGE: Return the extracted text (or NA) INVISIBLY ***
      return(invisible(extracted_text_for_return))
    }

    # --- Handle Text Output ---
    if (output_format == "text") {
       # Return the text directly (already extracted/validated or set to NA above)
       # If extraction failed previously, a warning was issued and NA will be returned.
       return(extracted_text_for_return)
    }

  }, error = function(e) {
    # Catch errors during general response processing (e.g., initial JSON parsing)
    # If text extraction failed above (for text output), it would have stopped there.
    # If JSON parsing itself fails, it will be caught here.
    stop(paste("Error processing API response:", conditionMessage(e),
               "\nRaw Response Body (first 500 chars):", substr(raw_body_str, 1, 500), "..."))
  }) # End main tryCatch for response processing

  # Should not be reached if output_format logic is correct, but added for safety
  stop("Internal error: Function did not return expected value.")

} # End function definition
