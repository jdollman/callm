#' Fetch, Process, and Load OpenAI Batch Job Results into Workspace
#'
#' Retrieves results from a *completed* OpenAI batch job, processes them
#' according to the original API endpoint called (e.g., chat completions,
#' embeddings), and returns them as an R object.
#'
#' @details
#' This function checks the batch job status. If not "completed", it stops.
#' If completed, it downloads the corresponding output file from OpenAI.
#'
#' The function then parses the output JSONL file. Crucially, it inspects the
#' `endpoint` field recorded in the batch job status (e.g., `/v1/chat/completions`
#' or `/v1/embeddings`) to determine how to process the `body` within each
#' successful response line.
#'
#' * For `/v1/chat/completions`: Extracts the text content.
#' * For `/v1/embeddings`: Extracts the numeric embedding vector.
#' * For other endpoints: Returns the raw parsed body (as a list) and issues a warning.
#'
#' Failed requests within the batch are represented by `NA_character_` (for chat)
#' or `NULL` (for embeddings or other types). The results are ordered according
#' to the `custom_id` (e.g., "request-1", "request-2", ...) used during batch creation.
#'
#' @param batch_id Character string. The ID of the OpenAI batch job (e.g., "batch_abc123"). Required.
#' @param timeout Numeric. Request timeout in seconds, applied to both the status check
#'   and the file download requests. Default is 120 (allowing more time for potential file download).
#'
#' @return The type of object returned depends on the batch job's endpoint:
#'   * `/v1/chat/completions`: A `character` vector of responses (`NA_character_` for failures).
#'   * `/v1/embeddings`: A `list` where each element is a numeric embedding vector (`NULL` for failures).
#'   * Other endpoints: A `list` where each element is the raw parsed body from the
#'      response JSONL line (`NULL` for failures).
#'   Returns `NULL` if the batch job is not yet completed, if critical API calls fail,
#'   or if essential information (like output file ID or endpoint) is missing.
#'
#' @importFrom httr2 request req_headers req_method req_timeout req_retry req_error req_perform resp_body_raw resp_content_type resp_body_string
#' @importFrom jsonlite fromJSON stream_in
#' @importFrom cli cli_alert_info cli_alert_warning cli_alert_danger cli_alert_success cli_text cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom utils str
#' @export
#'
#' @examples
#' \dontrun{
#' # Ensure API key is set
#' # Sys.setenv(OPENAI_API_KEY = "YOUR_OPENAI_KEY")
#'
#' # --- Example for a completed Chat Completion batch ---
#' completed_chat_batch_id <- "batch_chat_xxxxxxxx" # Replace with your actual ID
#' chat_results <- workspace_batch(completed_chat_batch_id)
#' if (!is.null(chat_results)) {
#'   print("Fetched Chat Batch Results:")
#'   print(chat_results)
#' }
#'
#' # --- Example for a completed Embeddings batch ---
#' completed_embed_batch_id <- "batch_embed_yyyyyyyy" # Replace with your actual ID
#' embedding_results <- workspace_batch(completed_embed_batch_id)
#' if (!is.null(embedding_results)) {
#'   print("Fetched Embedding Batch Results:")
#'   # Print dimensions of the first embedding
#'   if(length(embedding_results) > 0 && is.numeric(embedding_results[[1]])) {
#'      print(paste("Dim of first embedding:", length(embedding_results[[1]])))
#'   }
#' }
#' }
workspace_batch <- function(batch_id, timeout = 120) {

  # --- 1. Input Validation ---
  if (missing(batch_id) || !is.character(batch_id) || length(batch_id) != 1 || !nzchar(batch_id)) {
    stop("'batch_id' must be a single, non-empty character string.")
  }
  if (!is.numeric(timeout) || length(timeout) != 1 || timeout <= 0) {
    stop("'timeout' must be a single positive number.")
  }

  # --- 2. Check Batch Status & Get Metadata ---
  cli::cli_alert_info("Checking status for batch ID: {.val {batch_id}}")
  # Assuming check_batch() is available and returns the full status list or NULL on error
  # and prints its own summary/errors.
  batch_status_obj <- check_batch(batch_id, timeout = timeout)

  if (is.null(batch_status_obj)) {
    # Error message already printed by check_batch's handler
    return(NULL)
  }

  # Check status returned by check_batch
  if (batch_status_obj$status != "completed") {
    cli::cli_alert_warning("Batch job status is not 'completed' (status: {.val {batch_status_obj$status}}). Cannot fetch results yet.")
    return(NULL)
  }

  # Get essential info from status object
  output_file_id <- batch_status_obj$output_file_id
  endpoint <- batch_status_obj$endpoint # Crucial for polymorphism
  expected_results_n <- batch_status_obj$request_counts$total

  # Validate essential info
  if (is.null(output_file_id)) {
    cli::cli_alert_danger("Batch status is 'completed' but 'output_file_id' is missing. Cannot fetch results.")
    return(NULL)
  }
  if (is.null(endpoint)) {
    cli::cli_alert_danger("Batch status object is missing the 'endpoint' field. Cannot determine how to process results.")
    return(NULL)
  }
  if (is.null(expected_results_n) || expected_results_n <= 0) {
    cli::cli_alert_warning("Could not determine a valid expected number of results from batch status ('request_counts$total' missing or zero). Cannot reliably process results.")
    return(NULL)
  }

  cli::cli_alert_info("Batch completed. Endpoint: {.val {endpoint}}, Output file ID: {.val {output_file_id}}")

  # --- 3. API Key (Needed again for file download) ---
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") {
    stop("API key environment variable 'OPENAI_API_KEY' not set or empty.")
  }

  # --- 4. Download Output File Content ---
  file_content_url <- paste0("https://api.openai.com/v1/files/", output_file_id, "/content")
  cli::cli_alert_info("Downloading output file content...")

  parsed_results_list <- NULL
  tryCatch({
    resp <- httr2::request(file_content_url) |>
      httr2::req_headers(Authorization = paste("Bearer", api_key)) |>
      httr2::req_method("GET") |>
      httr2::req_timeout(seconds = timeout) |>
      httr2::req_retry(max_tries = 3, is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 500, 503)) |>
      httr2::req_error(is_error = function(resp) httr2::resp_status(resp) >= 400,
                       body = function(resp) paste("OpenAI file content download failed for file ID", output_file_id,
                                                   "with status", httr2::resp_status(resp), ":",
                                                   httr2::resp_body_string(resp))) |>
      httr2::req_perform()

    # Use stream_in with simplifyVector = FALSE to preserve list structures needed for chat
    raw_body <- httr2::resp_body_raw(resp)
    conn <- gzcon(rawConnection(raw_body))
    parsed_results_list <- jsonlite::stream_in(conn, simplifyVector = FALSE, verbose = FALSE) # <-- Use FALSE
    close(conn)

    cli::cli_alert_success("Successfully downloaded and parsed output file.")

  }, error = function(e) {
    cli::cli_alert_danger("Error downloading or parsing batch output file: {conditionMessage(e)}")
    # parsed_results_list remains NULL
  })

  if (is.null(parsed_results_list)) {
    return(NULL) # Return NULL if download/parsing failed
  }

  # --- 5. Process Parsed Results (Polymorphic Extraction & Reordering) ---
  cli::cli_alert_info("Processing results based on endpoint: {.val {endpoint}}")

  # Initialize the final results object based on the endpoint
  failure_value <- NULL
  if (endpoint == "/v1/chat/completions") {
    final_results <- vector("character", expected_results_n)
    failure_value <- NA_character_
    final_results[] <- failure_value
  } else if (endpoint == "/v1/embeddings") {
    final_results <- vector("list", expected_results_n)
    failure_value <- NULL
  } else {
    cli::cli_alert_warning("Processing results for an unknown endpoint: {.val {endpoint}}. Will return raw response bodies.")
    final_results <- vector("list", expected_results_n)
    failure_value <- NULL
  }

  processed_indices <- logical(expected_results_n) # Track filled slots
  cli::cli_progress_bar(name = "Processing results", total = length(parsed_results_list), clear = FALSE)

  # --- Loop through each line from the downloaded JSONL file ---
  for (line_result in parsed_results_list) {
    cli::cli_progress_update() # Increment progress

    # Extract components for this line
    current_custom_id <- line_result$custom_id
    response_obj <- line_result$response
    error_obj <- line_result$error

    # --- Validate custom_id and get index ---
    if (is.null(current_custom_id)) {
      cli::cli_alert_warning("Found result line with missing 'custom_id'. Skipping.")
      next
    }
    index <- suppressWarnings(as.integer(sub("request-", "", current_custom_id)))
    if (is.na(index) || index <= 0 || index > expected_results_n) {
      cli::cli_alert_warning("Found result line with invalid or out-of-bounds 'custom_id': {.val {current_custom_id}}. Skipping.")
      next
    }
    # --- End validation ---

    extracted_value <- failure_value # Default to failure value for this line

    # --- Process successful responses (status code 200/201) ---
    if (!is.null(response_obj) && response_obj$status_code %in% c(200, 201)) {

      # --- >>> Define response_body HERE (inside the success check) <<< ---
      response_body <- response_obj$body
      # --- >>> ---

      # --- Check if response body itself is NULL ---
      if(is.null(response_body)) {
        cli::cli_alert_warning("Response body is NULL for custom_id: {.val {current_custom_id}}, despite 2xx status.")
        extraction_successful <- FALSE # Mark as failed
      } else {
        # --- Polymorphic Extraction Logic ---
        extraction_successful <- FALSE # Reset for this item
        tryCatch({
          if (endpoint == "/v1/chat/completions") {
            # --- Extract Chat Content ---
            if (!is.null(response_body$choices) && is.list(response_body$choices) && length(response_body$choices) > 0) {
              first_choice <- response_body$choices[[1]] # Access first list element
              if(!is.null(first_choice) && is.list(first_choice) && !is.null(first_choice$message) && is.list(first_choice$message) && !is.null(first_choice$message$content)){
                text_content <- first_choice$message$content
                if (is.character(text_content) && length(text_content) == 1) {
                  extracted_value <- text_content
                  extraction_successful <- TRUE
                } else {
                  cli::cli_text("Warning: Extracted chat content for {.val {current_custom_id}} not a single string.")
                  extracted_value <- as.character(text_content)[1] # Attempt coercion
                  extraction_successful <- TRUE
                }
              } else {
                cli::cli_text("Warning: Missing 'message' or 'content' structure within choice for {.val {current_custom_id}}.")
              }
            } else {
              cli::cli_text("Warning: Missing or invalid 'choices' list in response body for {.val {current_custom_id}}.")
            }

          } else if (endpoint == "/v1/embeddings") {

            # --- Extraction logic ---
            if (!is.null(response_body$data) && is.list(response_body$data) && length(response_body$data) > 0) {
              first_item_in_data <- response_body$data[[1]]
              if (!is.null(first_item_in_data) && is.list(first_item_in_data)) {
                embedding_list <- first_item_in_data$embedding # Get the field (which is a list)

                if (!is.null(embedding_list) && is.list(embedding_list)) { # Check it's a list
                  # --- >>> Convert the list to a vector <<< ---
                  embedding_vector <- unlist(embedding_list, use.names = FALSE)

                  # --- Check if the *result* is numeric ---
                  if (is.numeric(embedding_vector)) {
                    extracted_value <- embedding_vector
                    extraction_successful <- TRUE # SUCCESS!
                  } else {
                    cli::cli_text("Warning: Unlisted 'embedding' field for {.val {current_custom_id}} is not numeric.")
                  }
                  # --- End check ---
                } else {
                  cli::cli_text("Warning: 'embedding' field missing or not a list inside data[[1]] for {.val {current_custom_id}}.")
                }
              } else {
                cli::cli_text("Warning: First element of 'data' list (data[[1]]) is NULL or not a list for {.val {current_custom_id}}.")
              }
            } else {
              cli::cli_text("Warning: 'data' field missing, not a list, or empty in response_body for {.val {current_custom_id}}.")
            }
            # --- End Embedding Extraction ---

          } else {
            # --- Handle Unknown Endpoint ---
            cli::cli_alert_warning("Processing results for an unknown endpoint: {.val {endpoint}}. Returning raw response body for {.val {current_custom_id}}.")
            extracted_value <- response_body
            extraction_successful <- TRUE # Consider returning raw body a 'success' for this path
          }
        }, error = function(e){
          # Catch errors specifically during the extraction logic
          cli::cli_alert_warning("Error during data extraction for {.val {current_custom_id}}: {conditionMessage(e)}")
          # extraction_successful remains FALSE
        }) # End tryCatch for extraction

        # If extraction failed despite checks, issue the warning
        if (!extraction_successful) {
          cli::cli_alert_warning("Failed to extract expected data structure for {.val {current_custom_id}} from response body, despite 2xx status.")
          # extracted_value remains failure_value from initialization
        }
      } # End else block for !is.null(response_body)

    } else {
      # --- Handle batch line error (non-200 status or explicit error object) ---
      error_message <- "Unknown error"
      if(!is.null(error_obj) && !is.null(error_obj$message)){
        error_message <- error_obj$message
      } else if (!is.null(response_obj) && !is.null(response_obj$status_code)) {
        error_message <- paste("Response status code:", response_obj$status_code)
      }
      cli::cli_text("Request {.val {current_custom_id}} failed within the batch. Error: {.val {error_message}}")
      extracted_value <- failure_value # Assign failure value
    } # End if/else processing successful vs failed lines

    # --- Place the result/failure value in the correct position ---
    if(processed_indices[index]){
      cli::cli_alert_warning("Duplicate custom_id found processing results: {.val {current_custom_id}}. Overwriting previous entry.")
    }
    # Assign value using appropriate indexing (list vs vector)
    if (is.list(final_results)) {
      final_results[[index]] <- extracted_value
    } else {
      # This path should only be taken for chat completions now
      final_results[index] <- extracted_value
    }
    processed_indices[index] <- TRUE

  } # End for loop through parsed results list

  cli::cli_progress_done()

  # --- Final checks for completeness ---
  if(length(parsed_results_list) != expected_results_n){
    cli::cli_alert_warning("Number of lines processed ({length(parsed_results_list)}) does not match expected number of requests ({expected_results_n}).")
  }
  if(!all(processed_indices)){
    missing_count <- sum(!processed_indices)
    cli::cli_alert_warning("{missing_count} request{?s} seem to be missing from the output file or failed processing based on custom_ids.")
  }

  cli::cli_alert_success("Finished processing batch results.")
  return(final_results) # Return list/vector invisibly handled by check_batch caller if desired
}

# Helper for default values (if not already defined elsewhere)
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) b else a
}
