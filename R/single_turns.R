#' Call Large Language Models (Multiple Single Turns)
#'
#' Sequentially sends multiple independent user messages (each potentially with a
#' system message) to a specified LLM provider and model. Handles logging of
#' raw responses and offers experimental batch processing via the OpenAI Batch API.
#'
#' @details
#' This function iterates through the provided `user_msgs` prompts. For each prompt,
#' it determines the corresponding system message based on the `system_msgs` argument
#' and calls the underlying `single_turn` function.
#'
#' **System Prompt Handling:**
#' * If `system_msgs` is `NULL` (default), no system message is used for any prompt.
#' * If `system_msgs` is a single character string, that string is used as the system
#'   message for *all* user prompts.
#' * If `system_msgs` is a character vector, it *must* be the same length as `user_msgs`.
#'   `system_msgs[i]` will be used with `user_msgs[i]`.
#'
#' **Output and Logging:**
#' By default (`log_jsonl = TRUE`), the full JSON response from the API for each
#' successful call is appended to a JSONL file. If `jsonl_file` is not provided,
#' a filename is automatically generated based on the timestamp. The path to the
#' JSONL file is printed to the console when logging is active. The primary return
#' value (when `batch = FALSE`) is a character vector containing the extracted text
#' responses, with `NA_character_` indicating failures for specific prompts.
#'
#' **Batch Processing (Experimental - OpenAI Only):**
#' If `batch = TRUE` and `org = "openai"`, the function will:
#' 1. Prepare a JSONL file suitable for the OpenAI Batch API.
#' 2. Upload this file to OpenAI.
#' 3. Create a batch processing job.
#' 4. Print messages indicating the uploaded file ID and the created batch job ID.
#' 5. **Return the batch job ID** as a character string.
#' Note: The actual results of the batch job are *not* retrieved by this function;
#' you will need to check the job status and download the results separately
#' using the batch ID (functionality potentially added to this package later).
#' Batch processing for other providers is not yet implemented.
#'
#' @param user_msgs Character vector. A vector of user messages/prompts. Required.
#' @param system_msgs Character string or vector, or NULL. System message(s).
#'   See Details for behavior. Default is `NULL`.
#' @param org Character vector. The LLM provider. Defaults to "google".
#'   Handles partial matching. Allowed values: "google", "anthropic", "openai".
#' @param model Character string. The specific model ID. Defaults to `NULL`,
#'   triggering provider-specific defaults (see `single_turn`).
#' @param temperature Numeric. Sampling temperature (>= 0). Default is 0.0.
#' @param max_tokens Integer. Maximum tokens per response. Default is 1024L.
#' @param timeout Numeric. Request timeout *per call* in sequential mode, or
#'   for the batch *creation* call in batch mode. Default is 60.
#' @param log_jsonl Logical. Should the full JSON response for each successful
#'   call be appended to a JSONL file? Default is `TRUE`.
#' @param jsonl_file Character string or `NULL`. Path to the JSONL file for logging.
#'   If `NULL` (default) and `log_jsonl` is `TRUE`, a filename is generated
#'   automatically (e.g., "llm_calls_YYYYMMDD_HHMMSS.jsonl"). Ignored if
#'   `log_jsonl` is `FALSE` or if `batch = TRUE`.
#' @param batch Logical. Use batch processing (currently OpenAI only)? Default `FALSE`.
#' @param ... Additional arguments to be passed to `single_turn` (in sequential mode)
#'   or potentially used in batch preparation (currently unused).
#'
#' @return
#' * If `batch = FALSE`: A character vector of the same length as `user_msgs`,
#'   containing the extracted text responses. `NA_character_` indicates an error
#'   occurred for that specific prompt during the API call or processing.
#' * If `batch = TRUE` and `org = "openai"`: The OpenAI batch job ID (character string).
#' * If `batch = TRUE` and `org != "openai"`: Stops with an error message.
#'
#' @importFrom httr2 request req_method req_headers req_body_json req_timeout
#' @importFrom httr2 req_error req_perform resp_body_string resp_status resp_body_json
#' @importFrom jsonlite toJSON write_json stream_out
#' @importFrom tools file_path_sans_ext
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done cli_alert_info cli_alert_danger cli_alert_success
#' @export
#'
#' @examples
#' \dontrun{
#' # Ensure API keys are set
#' # Sys.setenv(GOOGLE_API_KEY = "YOUR_GOOGLE_KEY")
#' # Sys.setenv(OPENAI_API_KEY = "YOUR_OPENAI_KEY")
#'
#' prompts <- c("What is R?", "Explain dplyr::mutate", "Why use version control?")
#' system_general <- "You are a helpful R programming assistant."
#'
#' # --- Sequential Execution (Default) ---
#'
#' # Using Google with default logging
#' responses_google <- single_turns(user_msgs = prompts, org = "google")
#' print(responses_google)
#'
#' # Using OpenAI with a single system prompt and disabling logging
#' responses_openai <- single_turns(
#'   user_msgs = prompts,
#'   system_msgs = system_general,
#'   org = "openai",
#'   model = "gpt-4o-mini",
#'   log_jsonl = FALSE
#' )
#' print(responses_openai)
#'
#' # Using specific system prompts per user prompt
#' specific_system_msgs <- c("Explain like I'm 5", "Explain for data analyst", NA) # NA -> NULL system
#' responses_mixed_system_msgs <- single_turns(
#'   user_msgs = prompts,
#'   system_msgs = specific_system_msgs,
#'   org = "openai"
#' )
#' print(responses_mixed_system_msgs)
#'
#' # Specify a custom JSONL file location
#' my_log <- tempfile(fileext = ".jsonl")
#' responses_custom_log <- single_turns(
#'   user_msgs = prompts[1:2],
#'   org = "google",
#'   jsonl_file = my_log
#' )
#' print(readLines(my_log))
#' unlink(my_log)
#'
#' # --- Batch Execution (OpenAI Only Example) ---
#'
#' # Note: This only *creates* the batch job. Results must be fetched later.
#' prompts_for_batch <- paste("Translate to French:", c("Hello", "Goodbye", "Thank you"))
#'
#' batch_id <- single_turns(
#'   user_msgs = prompts_for_batch,
#'   org = "openai",
#'   model = "gpt-4o-mini", # Ensure model supports batch if needed
#'   batch = TRUE
#' )
#'
#' if (!is.null(batch_id)) {
#'  print(paste("OpenAI Batch job created with ID:", batch_id))
#'  # You would later use batch_id to check status and get results
#' }
#'
#' # Example of trying batch with non-OpenAI provider (will stop)
#' tryCatch({
#'   single_turns(user_msgs = prompts, org = "google", batch = TRUE)
#' }, error = function(e) {
#'   print(paste("Caught expected error:", e$message))
#' })
#'
#' }
single_turns <- function(
    user_msgs,
    system_msgs = NULL,
    org = c("google", "anthropic", "openai"),
    model = NULL,
    temperature = 0.0,
    max_tokens = 1024L,
    timeout = 60,
    log_jsonl = TRUE,
    jsonl_file = NULL,
    batch = FALSE,
    ... # Allow pass-through args
) {

  # --- 1. Argument Validation ---
  if (missing(user_msgs) || !is.character(user_msgs) || length(user_msgs) == 0) {
    stop("'user_msgs' must be a non-empty character vector.")
  }
  n_user_msgs <- length(user_msgs)

  org <- tryCatch(
    match.arg(tolower(org[[1]]), c("google", "anthropic", "openai")), # Use [[1]] to ensure single value if multiple passed
    error = function(e) stop("Invalid 'org' specified. Choose from 'google', 'anthropic', 'openai'.")
  )

  # Validate system_msgs argument
  n_system_msgs <- 0
  if (!is.null(system_msgs)) {
    if (!is.character(system_msgs)) {
      stop("'system_msgs' must be NULL or a character vector/string.")
    }
    n_system_msgs <- length(system_msgs)
    if (n_system_msgs != 1 && n_system_msgs != n_user_msgs) {
      stop("'system_msgs' must be NULL, a single string, or a character vector of the same length as 'user_msgs'.")
    }
  }

  if (!is.logical(log_jsonl) || length(log_jsonl) != 1) {
    stop("'log_jsonl' must be a single logical value (TRUE or FALSE).")
  }
  if (!is.logical(batch) || length(batch) != 1) {
    stop("'batch' must be a single logical value (TRUE or FALSE).")
  }

  # --- 2. Handle Batch Processing ---
  if (batch) {
    if (org != "openai") {
      stop("Batch processing is currently only implemented for 'openai'.")
    } else {
      cli::cli_alert_info("Initiating OpenAI Batch process for chat completions...")
      # --- Get API Key ---
      api_key <- Sys.getenv("OPENAI_API_KEY")
      if (api_key == "") {
        stop("API key environment variable 'OPENAI_API_KEY' not set or empty.")
      }

      # --- Prepare Batch Input Data specific to chat completions ---
      batch_requests <- vector("list", n_users) # Use user_msgs length
      for (i in 1:n_users) { # Use user_msgs index
        # Determine system message for this request
        current_system <- NULL
        if (n_systems == 1) {
          current_system <- system_msgs # Use your arg name
        } else if (n_systems == n_users) {
          if(!is.na(system_msgs[i])) { current_system <- system_msgs[i] }
        }

        # Construct OpenAI message list
        messages_list <- list()
        if (!is.null(current_system) && nzchar(trimws(current_system))) {
          messages_list[[length(messages_list) + 1]] <- list(role = "system", content = current_system)
        }
        messages_list[[length(messages_list) + 1]] <- list(role = "user", content = user_msgs[i]) # Use your arg name

        # Construct body for this request line
        body_list <- list(
          model = model %||% "gpt-4o-mini",
          messages = messages_list,
          temperature = temperature,
          max_tokens = max_tokens
        )

        # Create the entry for the JSONL file list
        batch_requests[[i]] <- list(
          custom_id = paste0("request-", i),
          method = "POST",
          url = "/v1/chat/completions", # Specific URL for chat
          body = body_list
        )
      } # end loop for requests

      # --- Call the internal helper function ---
      target_endpoint <- "/v1/chat/completions"
      batch_id_result <- tryCatch({
        create_openai_batch_job(
          list_of_requests = batch_requests,
          target_endpoint = target_endpoint,
          api_key = api_key,
          timeout = timeout
        )
      }, error = function(e){
        # Catch errors from the helper and stop execution of single_turns
        stop(paste("Failed to create OpenAI batch job:", conditionMessage(e)))
      })

      # Return the batch ID (already printed by helper)
      cli::cli_alert_info("Use check_batch() and workspace_batch() to get results later.")
      return(invisible(batch_id_result))

    } # End OpenAI Batch logic
  } # End if (batch)

  # --- 3. Sequential Processing ---
  cli::cli_alert_info("Starting sequential processing for {n_user_msgs} prompt{?s}.")

  # Setup JSONL logging if enabled
  actual_jsonl_file <- NULL
  if (log_jsonl) {
    if (is.null(jsonl_file)) {
      # Auto-generate filename
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      actual_jsonl_file <- paste0("llm_calls_", timestamp, ".jsonl")
    } else {
      if (!is.character(jsonl_file) || length(jsonl_file) != 1 || !nzchar(trimws(jsonl_file))) {
        stop("'jsonl_file' must be NULL or a non-empty character string.")
      }
      actual_jsonl_file <- jsonl_file
    }
    # Ensure directory exists (or warn)
    log_dir <- dirname(actual_jsonl_file)
    if (!dir.exists(log_dir) && log_dir != ".") {
        warning(paste("Directory for jsonl_file does not exist:", log_dir, "- attempting to create."))
        tryCatch(dir.create(log_dir, recursive = TRUE),
                 warning = function(w) warning(paste("Could not create directory:", conditionMessage(w))),
                 error = function(e) stop(paste("Could not create directory:", conditionMessage(e))))
    }
     # Delete file if it exists? Or append? Let's append, consistent with JSONL.
     # if (file.exists(actual_jsonl_file)) {
     #    warning(paste("JSONL file already exists and will be appended to:", actual_jsonl_file))
     # }
    cli::cli_alert_info("Logging full JSON responses to: {.path {actual_jsonl_file}}")
  }

  # Pre-allocate results vector
  results <- vector("character", n_user_msgs)
  results[] <- NA_character_ # Default to NA

  # Initialize progress bar
  cli::cli_progress_bar(name = "Processing prompts", total = n_user_msgs, clear = FALSE)

  # Loop through user_msgs prompts
  for (i in 1:n_user_msgs) {
    cli::cli_progress_update() # Increment progress bar

    current_user <- user_msgs[i]
    current_system <- NULL # Default for this iteration

    if (n_system_msgs == 1) {
      current_system <- system_msgs # Recycle single system prompt
    } else if (n_system_msgs == n_user_msgs) {
       # Handle potential NA values -> NULL system prompt
       if(!is.na(system_msgs[i])) {
         current_system <- system_msgs[i]
       }
    }

    # Determine arguments for single_turn based on logging
    output_format_arg <- "text"   # Default to text
    jsonl_file_arg <- NULL      # Default to NULL

    if (log_jsonl) {
      output_format_arg <- "jsonl"
      jsonl_file_arg <- actual_jsonl_file
    }

    # Call single_turn with error handling for this specific prompt
    call_result <- tryCatch({
      single_turn(
        user = current_user,
        system = current_system,
        org = org,
        model = model,
        temperature = temperature,
        max_tokens = max_tokens,
        timeout = timeout,
        output_format = output_format_arg,
        # Set to "jsonl"
        jsonl_file = jsonl_file_arg,
        ... # Pass any extra arguments
      )
    }, error = function(e) {
      # Log the error for this specific prompt and return an error marker
      cli::cli_alert_danger("Error processing prompt {i}/{n_user_msgs}: {conditionMessage(e)}")
      return(list(error = TRUE, message = conditionMessage(e))) # Return a list to distinguish from valid NULL/empty text
    })

    # Store result
    if (is.list(call_result) &&
        !is.null(call_result$error) && call_result$error) {
      results[i] <- NA_character_
    } else if (is.character(call_result) &&
               length(call_result) == 1) {
      results[i] <- call_result # Store the invisibly returned text
    } else {
      # Handle unexpected return type (e.g., NULL if text extraction failed in single_turn)
      results[i] <- NA_character_
    }
  }

  cli::cli_progress_done() # Finalize progress bar
  cli::cli_alert_success("Sequential processing finished.")

  # Return the collected results
  return(results)
}

# Helper for default values (similar to %||% from rlang, but base R)
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
