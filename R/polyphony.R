#' Send a Prompt Sequentially to Multiple LLM Perspectives (Text Output)
#'
#' Executes a single user prompt sequentially against multiple Large Language Model
#' (LLM) configurations defined in the `perspectives` list. It leverages the
#' core `single_turn` function for individual API interactions, collecting only
#' the text responses. Provides verbose output on progress.
#'
#' @param user Character string. The user prompt to send to all perspectives. Required.
#' @param perspectives A list where each element is itself a list defining an
#'   LLM configuration to query. Each inner list *must* contain:
#'   \itemize{
#'     \item `id`: A unique character string identifier for this perspective (used for naming the output).
#'     \item `org`: Character string specifying the LLM provider (e.g., "google", "openai", "anthropic"). Passed to `single_turn`.
#'     \item `model`: Character string specifying the model name for the provider. Passed to `single_turn`.
#'   }
#'   Inner lists can *optionally* contain other arguments accepted by
#'   `single_turn`, such as `temperature`, `max_tokens`, `timeout`, etc. These
#'   will override the top-level defaults (`max_tokens`, `timeout`) if provided.
#'   **Note:** Any `output_format` or `jsonl_file` arguments within a perspective
#'   will be ignored, as `polyphony` forces text output.
#' @param system Optional character string. A system prompt to be applied
#'   identically to all perspectives. Defaults to `NULL`.
#' @param max_tokens Integer. The default maximum number of tokens to generate.
#'   This is used if a perspective does not specify its own `max_tokens`.
#'   Defaults to `1024L`.
#' @param timeout Numeric. The default request timeout in seconds. This is used
#'   if a perspective does not specify its own `timeout`. Defaults to `60`.
#' @param verbose Logical. If `TRUE` (default), prints status messages indicating
#'   which perspective is currently being processed.
#' @param error_strategy Character string defining behavior when one or more
#'   perspectives encounter an error during the API call. Must be one of:
#'   \itemize{
#'     \item `"return_partial"` (default): Returns a list containing text results for
#'       successful perspectives and error objects for failed ones.
#'     \item `"stop"`: If any perspective fails, the entire function stops and
#'       throws an error, summarizing which perspectives failed.
#'   }
#'
#' @return A named list. The names are the `id`s from the `perspectives` input list.
#'   The values are:
#'   \itemize{
#'     \item If the call for that perspective was successful: The character string
#'       containing the LLM's text response (obtained via `single_turn(..., output_format = "text")`).
#'     \item If the call failed and `error_strategy = "return_partial"`: The
#'       `error` object captured during the failed `single_turn` call.
#'   }
#'
#' @details
#' This function acts as a multiplexer, sending the same query sequentially to
#' different models/providers and collecting their textual responses. It relies
#' on the underlying `single_turn` function for handling the specifics of each
#' provider's API, authentication, and response parsing, ensuring that
#' `output_format` is always set to `"text"`.
#'
#' Authentication relies on API keys being available as environment variables,
#' as handled by `single_turn` (e.g., `GOOGLE_API_KEY`, `OPENAI_API_KEY`,
#' `ANTHROPIC_API_KEY`).
#'
#' @export
#' @examples
#' \dontrun{
#' # Make sure the single_turn function is available (e.g., via devtools::load_all())
#' # Ensure API keys are set as environment variables:
#' # Sys.setenv(GOOGLE_API_KEY = "YOUR_GOOGLE_KEY")
#' # Sys.setenv(ANTHROPIC_API_KEY = "YOUR_ANTHROPIC_KEY")
#' # Sys.setenv(OPENAI_API_KEY = "YOUR_OPENAI_KEY")
#'
#' # Define perspectives
#' perspectives_list <- list(
#'   list(id = "gpt4o_mini", org = "openai", model = "gpt-4o-mini", temperature = 0.5),
#'   list(id = "claude3h", org = "anthro", model = "claude-3-haiku-20240307"), # Partial org match
#'   list(id = "gemini_flash", org = "google", model = "gemini-1.5-flash-latest", max_tokens = 500)
#' )
#'
#' # --- Sequential Execution with Verbose Output (Default) ---
#' results_seq_verbose <- polyphony(
#'   user = "Explain the concept of 'polyphony' in music.",
#'   perspectives = perspectives_list,
#'   system = "You are a helpful assistant."
#' )
#' print(results_seq_verbose)
#'
#' # --- Sequential Execution without Verbose Output ---
#' results_seq_quiet <- polyphony(
#'   user = "Explain the concept of 'polyphony' in music.",
#'   perspectives = perspectives_list,
#'   system = "You are a helpful assistant.",
#'   verbose = FALSE
#' )
#' print(results_seq_quiet)
#'
#' # --- Example with error handling ---
#' perspectives_with_error <- list(
#'   list(id = "gpt4o_mini_ok", org = "openai", model = "gpt-4o-mini"),
#'   list(id = "invalid_model", org = "openai", model = "non-existent-model-123")
#' )
#'
#' # Stop on error
#' tryCatch({
#'   polyphony(
#'     user = "Hello",
#'     perspectives = perspectives_with_error,
#'     error_strategy = "stop"
#'   )
#' }, error = function(e) {
#'   message("Caught expected error: ", conditionMessage(e))
#' })
#'
#' # Return partial results (verbose output will show processing for both)
#' results_partial <- polyphony(
#'   user = "Hello",
#'   perspectives = perspectives_with_error,
#'   error_strategy = "return_partial"
#' )
#' print(results_partial)
#' # Check which ones failed
#' print(sapply(results_partial, inherits, "error"))
#'
#' }
polyphony <- function(user,
                      perspectives,
                      system = NULL,
                      max_tokens = 1024L,
                      timeout = 60,
                      verbose = TRUE, # Added verbose parameter
                      error_strategy = c("return_partial", "stop")) {

  # --- Input Validation ---
  # (User, system, perspectives structure/content validation remains the same)
  if (!is.character(user) || length(user) != 1 || nchar(user) == 0) {
    stop("`user` must be a non-empty character string.", call. = FALSE)
  }
  if (!is.null(system) && (!is.character(system) || length(system) != 1)) {
    stop("`system` must be NULL or a single character string.", call. = FALSE)
  }
  if (!is.list(perspectives) || length(perspectives) == 0) {
    stop("`perspectives` must be a non-empty list.", call. = FALSE)
  }
  if (!all(sapply(perspectives, is.list))) {
    stop("Each element of `perspectives` must be a list.", call. = FALSE)
  }
  perspective_ids <- character(length(perspectives))
  for (i in seq_along(perspectives)) {
    p <- perspectives[[i]]
    if (!is.list(p)) { stop(sprintf("Element %d of `perspectives` is not a list.", i), call. = FALSE) }
    if (!"id" %in% names(p) || !is.character(p$id) || length(p$id) != 1 || nchar(p$id) == 0) {
      stop(sprintf("Element %d of `perspectives` must have a non-empty character string 'id'.", i), call. = FALSE)
    }
    if (!"org" %in% names(p) || !is.character(p$org) || length(p$org) != 1 || nchar(p$org) == 0) {
      stop(sprintf("Perspective '%s' must have a non-empty character string 'org'.", p$id), call. = FALSE)
    }
    if (!"model" %in% names(p) || !is.character(p$model) || length(p$model) != 1 || nchar(p$model) == 0) {
      stop(sprintf("Perspective '%s' must have a non-empty character string 'model'.", p$id), call. = FALSE)
    }
    perspective_ids[i] <- p$id
  }
  if (any(duplicated(perspective_ids))) {
    stop("Perspective 'id' values must be unique.", call. = FALSE)
  }

  # Validate numeric/integer inputs (max_tokens, timeout)
  if (!is.numeric(max_tokens) || length(max_tokens) != 1 || max_tokens <= 0 || (max_tokens != floor(max_tokens))) {
    stop("`max_tokens` must be a single positive integer.", call. = FALSE)
  }
  max_tokens <- as.integer(max_tokens) # Ensure integer

  if (!is.numeric(timeout) || length(timeout) != 1 || timeout <= 0) {
    stop("`timeout` must be a single positive number.", call. = FALSE)
  }
  # Validate verbose
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }
  error_strategy <- match.arg(error_strategy) # Ensure valid strategy

  # --- Initialize results list ---
  results_list <- vector("list", length(perspectives))
  names(results_list) <- perspective_ids # Name the list upfront

  # --- Execute Calls Sequentially using a loop ---
  if (verbose) message(sprintf("Processing %d perspectives sequentially...", length(perspectives)))

  for (i in seq_along(perspectives)) {
    p <- perspectives[[i]]
    p_id <- perspective_ids[i] # Get the ID

    if (verbose) {
      message(sprintf("---> Processing perspective '%s' (Org: %s, Model: %s)...", p_id, p$org, p$model))
    }

    # Construct arguments for single_turn within the loop
    args <- list()
    args$user <- user
    if (!is.null(system)) args$system <- system
    args$org <- p$org
    args$model <- p$model
    args$max_tokens <- if (!is.null(p$max_tokens)) p$max_tokens else max_tokens
    args$timeout <- if (!is.null(p$timeout)) p$timeout else timeout

    # Add other parameters, excluding handled ones and output formatters
    handled_args <- c("id", "org", "model", "max_tokens", "timeout", "user", "system",
                      "output_format", "jsonl_file")
    other_params <- p[!(names(p) %in% handled_args)]
    args <- c(args, other_params)
    args$output_format <- "text" # Force text output

    # Call single_turn and capture errors robustly
    result <- tryCatch({
      # Use explicit namespacing for robustness, assuming package is 'callm'
      do.call("single_turn", args)
    },
    error = function(e) {
      # Add perspective context to the error message
      e$message <- paste(sprintf("Perspective '%s' failed: ", p_id), conditionMessage(e))
      # Return the modified error object itself
      e
    }
    )

    # Store the result (either text or error object)
    results_list[[p_id]] <- result

    # Optional: Message on completion of this perspective
    # if (verbose) {
    #    if (inherits(result, "error")) {
    #        message(sprintf("<--- Perspective '%s' failed.", p_id))
    #    } else {
    #        message(sprintf("<--- Perspective '%s' completed.", p_id))
    #    }
    # }

  } # End loop through perspectives

  # --- Process Results based on error_strategy ---
  # Check for errors AFTER the loop finishes
  has_errors <- any(sapply(results_list, inherits, "error"))

  if (has_errors && error_strategy == "stop") {
    failed_ids <- names(results_list)[sapply(results_list, inherits, "error")]
    error_summary <- paste(sprintf("'%s'", failed_ids), collapse = ", ")
    stop(sprintf("One or more perspectives failed: %s.\nSet error_strategy = 'return_partial' to view details.",
                 error_summary),
         call. = FALSE)
  }

  if (verbose) message("Finished processing all perspectives.")
  return(results_list)
}
