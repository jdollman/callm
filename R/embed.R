#' Get Text Embeddings from OpenAI
#'
#' Retrieves text embeddings from OpenAI's models (text-embedding-3-small or large).
#' Supports optional dimension truncation and batch processing via the OpenAI Batch API.
#'
#' @details
#' For non-batch requests (`batch = FALSE`), the function sends the texts to the
#' OpenAI embeddings endpoint. Note that the standard API endpoint itself can handle
#' multiple texts in a single request (up to API limits). If a large number of
#' texts (>50) is provided with `batch = FALSE`, a message suggests using batch
#' mode for potential cost savings.
#'
#' For batch requests (`batch = TRUE`), the function prepares and uploads an input
#' file to OpenAI and initiates a batch job targeting the embeddings endpoint. It
#' returns the batch job ID. Use `check_batch()` and `workspace_batch()` to monitor
#' and retrieve results later. Note that `workspace_batch()` will return a list of
#' numeric vectors for completed embedding batch jobs.
#'
#' Currently, only `org = "openai"` is supported.
#'
#' @param texts Character vector. The text(s) to embed. Required.
#' @param org Character string. The LLM provider. Currently must be "openai".
#'   Argument exists for future expansion.
#' @param size Character string. The embedding model size. Allowed: "small" (default),
#'   "large". Maps to "text-embedding-3-small" and "text-embedding-3-large".
#' @param dimensions Integer or NULL. The desired number of dimensions for the
#'   output embeddings. If NULL (default), the model's full dimensions are used.
#'   If set, must be a positive integer (OpenAI may have specific constraints).
#' @param batch Logical. Use batch processing via the OpenAI Batch API? Default `FALSE`.
#' @param timeout Numeric. Request timeout in seconds. Applies to synchronous API
#'   calls or the batch initiation steps. Default is 60.
#' @param ... Currently unused. For future expansion.
#'
#' @return
#' * If `batch = FALSE`: A `list` where each element is a numeric vector
#'   representing the embedding for the corresponding input text. Returns `NULL`
#'   on API error.
#' * If `batch = TRUE`: The OpenAI batch job ID (character string). Returns `NULL`
#'   if batch initiation fails.
#'
#' @importFrom httr2 request req_headers req_method req_timeout req_retry req_error req_perform resp_body_json req_body_json req_body_multipart
#' @importFrom jsonlite toJSON write_json stream_out
#' @importFrom cli cli_alert_info cli_alert_danger cli_alert_success cli_text
#' @importFrom curl form_file
#' @export
#'
#' @examples
#' \dontrun{
#' # Ensure API key is set
#' # Sys.setenv(OPENAI_API_KEY = "YOUR_OPENAI_KEY")
#'
#' my_texts <- c("The quick brown fox jumps over the lazy dog.",
#'              "R is a language for statistical computing.")
#'
#' # --- Synchronous (Non-Batch) Example ---
#' embeddings_list <- embed(texts = my_texts, size = "small")
#' if (!is.null(embeddings_list)) {
#'   print(paste("Number of embeddings received:", length(embeddings_list)))
#'   print(paste("Dimensions of first embedding:", length(embeddings_list[[1]])))
#' }
#'
#' # Example with dimension truncation
#' embeddings_short <- embed(texts = my_texts, size = "small", dimensions = 256)
#' if (!is.null(embeddings_short)) {
#'    print(paste("Dimensions of truncated embedding:", length(embeddings_short[[1]])))
#' }
#'
#' # Example triggering the long vector warning
#' long_texts <- rep("Test text.", 60)
#' embeddings_long_warn <- embed(texts = long_texts) # Will show message
#'
#' # --- Batch Example ---
#' batch_texts <- c("Embed this first.", "Embed this second.", "And this third.")
#' embedding_batch_id <- embed(texts = batch_texts, batch = TRUE)
#' if (!is.null(embedding_batch_id)) {
#'    print(paste("Embedding batch job created with ID:", embedding_batch_id))
#'    # Use check_batch(embedding_batch_id) and
#'    # workspace_batch(embedding_batch_id) later...
#' }
#' }
embed <- function(
    texts,
    org = "openai",
    size = c("small", "large"),
    dimensions = NULL,
    batch = FALSE,
    timeout = 60,
    ...
) {

  # --- 1. Argument Validation ---
  if (missing(texts) || !is.character(texts) || length(texts) == 0) {
    stop("'texts' must be a non-empty character vector.")
  }
  n_texts <- length(texts)

  # Currently only OpenAI supported for embeddings
  org <- tryCatch(
    # Force match against allowed values (currently only openai)
    match.arg(tolower(org), c("openai")),
    error = function(e)
      stop(
        "Invalid 'org' specified. Currently only 'openai' is supported for embed()."
      )
  )

  # Validate 'size' using match.arg
  size <- tryCatch(
    match.arg(tolower(size), c("small", "large")),
    error = function(e)
      stop("Invalid 'size' specified. Choose from 'small', 'large'.")
  )

  if (!is.null(dimensions)) {
    if (!is.numeric(dimensions) || length(dimensions) != 1 || dimensions <= 0 || (dimensions %% 1 != 0)) {
      stop("'dimensions' must be NULL or a single positive integer.")
    }
    dimensions <- as.integer(dimensions)
  }

  if (!is.logical(batch) || length(batch) != 1) {
    stop("'batch' must be a single logical value (TRUE or FALSE).")
  }
  if (!is.numeric(timeout) || length(timeout) != 1 || timeout <= 0) {
    stop("'timeout' must be a single positive number.")
  }

  # --- 2. Model Selection ---
  model_id <- switch(size,
    "small" = "text-embedding-3-small",
    "large" = "text-embedding-3-large"
  )

  # --- 3. API Key ---
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") {
    stop("API key environment variable 'OPENAI_API_KEY' not set or empty.")
  }

  # --- 4. Handle Batch Processing ---
  if (batch) {
    cli::cli_alert_info("Initiating OpenAI Batch process for embeddings...")

    # --- Prepare Batch Input Data specific to embeddings ---
    batch_requests <- vector("list", n_texts)
    for (i in 1:n_texts) {
      # Body for one embedding request line
      body_list <- list(
        input = texts[i], # Input must be single string for batch file line
        model = model_id # model_id already determined based on 'size'
      )
      if (!is.null(dimensions)) {
        body_list$dimensions <- dimensions
      }

      # Create the entry for the JSONL file list
      batch_requests[[i]] <- list(
        custom_id = paste0("request-", i),
        method = "POST",
        url = "/v1/embeddings", # Specific URL for embeddings
        body = body_list
      )
    }

    # --- Call the internal helper function ---
    target_endpoint <- "/v1/embeddings"
    batch_id_result <- tryCatch({
      create_openai_batch_job(
        list_of_requests = batch_requests,
        target_endpoint = target_endpoint,
        api_key = api_key,
        timeout = timeout
      )
    }, error = function(e){
      # Catch errors from the helper and stop execution of embed
      stop(paste("Failed to create OpenAI batch job:", conditionMessage(e)))
    })


    # Return the batch ID (already printed by helper)
    cli::cli_alert_info("Use check_batch() and workspace_batch() to get results later.")
    return(invisible(batch_id_result))

  } # End if (batch)

  # --- 5. Synchronous (Non-Batch) Processing ---

  # Warning for potentially expensive non-batch calls
  if (n_texts > 50) {
      cli::cli_alert_info(
          "Processing >50 texts sequentially. Consider using `batch = TRUE` for potential cost savings on OpenAI embeddings."
      )
  }

  api_url <- "https://api.openai.com/v1/embeddings"
  headers_list <- list(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
  )

  # Body construction - input can be a vector of strings
  body_list <- list(
      input = texts,
      model = model_id
  )
  if (!is.null(dimensions)) {
      body_list$dimensions <- dimensions
  }

  cli::cli_alert_info("Sending {n_texts} text{?s} to OpenAI embeddings endpoint...")

  embedding_data <- tryCatch({
      req <- httr2::request(api_url) |>
          httr2::req_headers(!!!headers_list) |>
          httr2::req_method("POST") |>
          httr2::req_body_json(body_list, auto_unbox = TRUE) |> # auto_unbox = TRUE is default, but worth keeping
          httr2::req_timeout(seconds = timeout) |>
          httr2::req_retry(max_tries = 3, is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 500, 503)) |>
          httr2::req_error(is_error = function(resp) httr2::resp_status(resp) >= 400,
                          body = function(resp) {
                             paste("OpenAI embeddings API request failed with status", httr2::resp_status(resp),
                                   "\nResponse Body:", httr2::resp_body_string(resp))
                          })

      resp <- httr2::req_perform(req)
      parsed_body <- httr2::resp_body_json(resp, simplifyVector = TRUE) # Simplify makes accessing 'data' easier

      # Extract embeddings - structure is parsed_body$data which is a data frame
      # Each row corresponds to an input text, containing 'embedding' list-column
      if (!is.null(parsed_body$data) && inherits(parsed_body$data, "data.frame") && "embedding" %in% names(parsed_body$data)) {
          # The 'embedding' column is likely a list column where each element is the numeric vector
          if (nrow(parsed_body$data) == n_texts) {
             cli::cli_alert_success("Successfully received {n_texts} embedding{?s}.")
             return(parsed_body$data$embedding) # Return the list of embedding vectors
          } else {
             stop(paste("API returned different number of embeddings than expected. Expected:", n_texts, "Got:", nrow(parsed_body$data)))
          }
      } else {
          stop("Could not find expected 'data' structure with 'embedding' column in API response.")
      }

  }, error = function(e) {
      cli::cli_alert_danger("Error during OpenAI embeddings API call or processing: {conditionMessage(e)}")
      return(NULL) # Return NULL on error
  })

  return(embedding_data)
}

# Helper for default values (if not already defined)
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) b else a
}
