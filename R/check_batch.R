#' Check OpenAI Batch Job Status with User-Friendly Summary
#'
#' Retrieves the status object for a specified OpenAI batch job ID and prints a
#' concise, human-friendly summary of the current status (e.g., completed, failed,
#' in progress with percentage). The full status object is returned invisibly.
#'
#' @param batch_id Character string. The ID of the OpenAI batch job (e.g., "batch_abc123"). Required.
#' @param timeout Numeric. Request timeout in seconds. Default is 60.
#'
#' @return Invisibly returns a list representing the parsed JSON response (the full batch object)
#'   from the OpenAI API. This includes status, file IDs (if generated), timestamps,
#'   request counts, etc. Returns `NULL` visibly and prints an error message if the API
#'   call fails. If the call succeeds, it prints a summary message (using `cli`)
#'   to the console before returning the full object invisibly.
#'
#' @importFrom httr2 request req_headers req_method req_timeout req_retry req_error req_perform resp_body_json resp_body_string resp_status
#' @importFrom cli cli_alert_danger cli_alert_success cli_alert_info cli_alert_warning cli_text
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have a batch ID from single_turns(..., batch = TRUE)
#' # Sys.setenv(OPENAI_API_KEY = "YOUR_OPENAI_KEY")
#' my_batch_id <- "batch_xxxxxxxxxxxx" # Replace with your actual batch ID
#'
#' # Run the check - it will PRINT the summary
#' check_batch(my_batch_id)
#'
#' # If you need the full details programmaticallly, assign the result
#' full_status <- check_batch(my_batch_id)
#' # It still prints the summary, but full_status now holds the list
#' if (!is.null(full_status)) {
#'   print(paste("Programmatic access to status:", full_status$status))
#' }
#' }
check_batch <- function(batch_id, timeout = 60) {

  # --- 1. Input Validation ---
  if (missing(batch_id) || !is.character(batch_id) || length(batch_id) != 1 || !nzchar(batch_id)) {
    stop("'batch_id' must be a single, non-empty character string.")
  }
  if (!is.numeric(timeout) || length(timeout) != 1 || timeout <= 0) {
    stop("'timeout' must be a single positive number.")
  }

  # --- 2. API Key ---
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") {
    stop("API key environment variable 'OPENAI_API_KEY' not set or empty.")
  }

  # --- 3. API Call ---
  batch_url <- paste0("https://api.openai.com/v1/batches/", batch_id)

  batch_status_obj <- tryCatch({
    resp <- httr2::request(batch_url) |>
      httr2::req_headers(Authorization = paste("Bearer", api_key)) |>
      httr2::req_method("GET") |>
      httr2::req_timeout(seconds = timeout) |>
      httr2::req_retry(max_tries = 3, is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 500, 503)) |>
      httr2::req_error(is_error = function(resp) httr2::resp_status(resp) >= 400,
                       body = function(resp) paste("OpenAI batch status check failed for ID", batch_id,
                                                   "with status", httr2::resp_status(resp), ":",
                                                   httr2::resp_body_string(resp))) |>
      httr2::req_perform()

    # Parse the response - use simplifyVector for easier list access below
    httr2::resp_body_json(resp, simplifyVector = TRUE)

  }, error = function(e) {
    # If the API call itself fails, print danger alert and return NULL visibly
    cli::cli_alert_danger("Error retrieving status for batch ID {.val {batch_id}}: {conditionMessage(e)}")
    return(NULL)
  })

  # --- 4. Check if API call failed ---
  if (is.null(batch_status_obj)) {
    return(NULL) # API call failed, message already printed by error handler
  }

  # --- 5. Interpret Status and Print Summary Message ---
  status <- batch_status_obj$status %||% "unknown" # Use NULL coalescing
  req_counts <- batch_status_obj$request_counts # List or NULL
  total <- req_counts$total %||% 0
  completed <- req_counts$completed %||% 0
  failed <- req_counts$failed %||% 0
  done <- completed + failed

  # Use cli to print status-dependent messages
  cli_args <- list(class = "batch_status_summary") # For potential theming

  if (status == "completed") {
    cli::cli_alert_success(
      "Batch job {.val {batch_id}} completed!",
      wrap = TRUE, .envir = environment())

    # Add details about counts and files
    cli::cli_text("{.field Completed requests}: {completed}/{total}")
    if (failed > 0) {
      cli::cli_alert_warning("{.field Failed requests}: {failed}/{total}")
      if (!is.null(batch_status_obj$error_file_id)) {
        cli::cli_text("  (Check {.field error_file_id}: {.val {batch_status_obj$error_file_id}} for details)")
      }
    }
    if (!is.null(batch_status_obj$output_file_id)) {
      cli::cli_text("{.field Output file}: {.val {batch_status_obj$output_file_id}}")
    }

  } else if (status %in% c("failed", "expired", "cancelled")) {
    cli::cli_alert_danger(
      "Batch job {.val {batch_id}} status: {.strong {status}}.",
      wrap = TRUE, .envir = environment())

    # Add failure details
    cli::cli_text("{.field Failed requests}: {failed}/{total}")
    if (!is.null(batch_status_obj$errors) && !is.null(batch_status_obj$errors$message)) {
      cli::cli_text("  Job error message: {.val {batch_status_obj$errors$message}}")
    }
    if (!is.null(batch_status_obj$error_file_id)) {
      cli::cli_text("  (Check {.field error_file_id}: {.val {batch_status_obj$error_file_id}} for per-request details)")
    }


  } else if (status %in% c("validating", "in_progress", "finalizing", "cancelling")) {
    percent <- if (total > 0) round(done / total * 100) else 0
    cli::cli_alert_info(
      "Batch job {.val {batch_id}} status: {.strong {status}}.",
      wrap = TRUE, .envir = environment())
    cli::cli_text("  Progress: {percent}% ({done}/{total} requests processed)")

  } else {
    # Unknown status
    cli::cli_alert_warning(
      "Batch job {.val {batch_id}} has an unexpected status: {.val {status}}.",
      wrap = TRUE, .envir = environment())
  }

  # --- 6. Return the full object invisibly ---
  # The messages above are printed as a side effect
  return(invisible(batch_status_obj))
}

# Helper for default values (if not already defined elsewhere in your package)
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) b else a # Make slightly more robust for zero-length edge cases
}
