#' Create an OpenAI Batch Job (Internal Helper)
#'
#' Handles the process of writing requests to a JSONL file, uploading the file
#' to OpenAI, and creating a batch job targeting a specific endpoint.
#' Not intended for direct user calls.
#'
#' @param list_of_requests A list where each element is a sub-list representing
#'   one API request line for the batch file (must include `custom_id`, `method`,
#'   `url`, and `body`).
#' @param target_endpoint Character string. The API endpoint the batch job targets
#'   (e.g., "/v1/chat/completions", "/v1/embeddings"). Passed to the create batch API call.
#' @param api_key Character string. The OpenAI API key.
#' @param timeout Numeric. Timeout for API calls.
#'
#' @return On success, the batch job ID (character string). Stops execution on failure.
#' @noRd
#' @importFrom httr2 request req_headers req_method req_timeout req_retry req_error req_perform resp_body_json req_body_multipart
#' @importFrom jsonlite toJSON
#' @importFrom cli cli_alert_info cli_alert_danger cli_alert_success cli_text
#' @importFrom curl form_file
create_openai_batch_job <- function(list_of_requests, target_endpoint, api_key, timeout) {

  if (api_key == "") {
    stop("Internal error: API key is missing.") # Should be checked before calling
  }
  n_requests <- length(list_of_requests)

  # --- Write requests to a temporary JSONL file (line-by-line) ---
  temp_batch_input_file <- tempfile(fileext = ".jsonl")
  conn <- file(temp_batch_input_file, open = "wt", encoding = "UTF-8")

  tryCatch({
    for (request_item in list_of_requests) {
      json_line <- jsonlite::toJSON(request_item, auto_unbox = TRUE, digits = NA, pretty = FALSE)
      write(json_line, file = conn)
    }
    cli::cli_alert_success("Internal: Successfully wrote {n_requests} request(s) to batch input file.")
  }, error = function(e) {
     stop(paste("Internal error writing batch input file:", conditionMessage(e))) # Stop execution
  }, finally = {
    if (isOpen(conn)) { close(conn) }
  })

  cli::cli_alert_info("Internal: Prepared batch input file: {.path {temp_batch_input_file}}")

  # --- Upload File ---
  upload_url <- "https://api.openai.com/v1/files"
  file_id <- NULL
  tryCatch({
      file_upload_resp <- httr2::request(upload_url) |>
          httr2::req_headers(Authorization = paste("Bearer", api_key)) |>
          httr2::req_method("POST") |>
          httr2::req_body_multipart(
              purpose = "batch",
              file = curl::form_file(temp_batch_input_file, type = "application/jsonl")
          ) |>
          httr2::req_timeout(seconds = timeout) |>
          httr2::req_retry(max_tries = 3) |>
          httr2::req_error(is_error = function(resp) httr2::resp_status(resp) >= 400,
                           body = function(resp) paste("OpenAI file upload failed:", httr2::resp_body_string(resp))) |>
          httr2::req_perform()

      uploaded_file_info <- httr2::resp_body_json(file_upload_resp, simplifyVector = TRUE)
      file_id <- uploaded_file_info$id
      cli::cli_alert_success("Internal: Uploaded batch input file successfully. File ID: {.val {file_id}}")

  }, error = function(e) {
      unlink(temp_batch_input_file, force = TRUE) # Clean up temp file on error
      stop(paste("Internal error uploading batch file to OpenAI:", conditionMessage(e)))
  }, finally = {
       # Ensure cleanup even if upload succeeds but batch creation fails later
       # However, OpenAI might need the file temporarily? Let's remove it *after* batch creation attempt too.
       # For now, clean up only on error here. Clean up on success *after* batch creation.
       # Decision: Clean up *after* successful batch creation or if batch creation fails. Don't cleanup here yet.
       # Let's stick to cleanup on upload error for now, main functions will call this.
       # Revised Decision: Cleanup should happen *after* the batch job is successfully created, or if *any* step fails.
       # Let's move cleanup to the end of this function using on.exit().

  })

  # Use on.exit to ensure temp file cleanup regardless of success/failure below
  on.exit(unlink(temp_batch_input_file, force = TRUE), add = TRUE)


  # --- Create Batch Job ---
  batch_create_url <- "https://api.openai.com/v1/batches"
  batch_body <- list(
      input_file_id = file_id,
      endpoint = target_endpoint, # Use the provided endpoint
      completion_window = "24h"
  )
  batch_id_result <- NULL

   tryCatch({
        batch_create_resp <- httr2::request(batch_create_url) |>
            httr2::req_headers(Authorization = paste("Bearer", api_key), `Content-Type` = "application/json") |>
            httr2::req_method("POST") |>
            httr2::req_body_json(batch_body) |>
            httr2::req_timeout(seconds = timeout) |>
            httr2::req_retry(max_tries = 3) |>
            httr2::req_error(is_error = function(resp) httr2::resp_status(resp) >= 400,
                            body = function(resp) paste("OpenAI batch creation failed:", httr2::resp_body_string(resp))) |>
            httr2::req_perform()

        batch_info <- httr2::resp_body_json(batch_create_resp, simplifyVector = TRUE)
        batch_id_result <- batch_info$id # Assign the ID
        cli::cli_alert_success("Internal: Created OpenAI Batch job successfully. Batch ID: {.val {batch_id_result}}")

  }, error = function(e) {
      # Error during batch creation
      stop(paste("Internal error creating OpenAI batch job:", conditionMessage(e)))
  })

  # Return the batch ID if successful
  if (is.null(batch_id_result)) {
      # Should not happen if tryCatch worked, but as safety
      stop("Internal error: Batch ID was NULL after supposedly successful creation.")
  }

  return(batch_id_result) # Return only the ID
}
