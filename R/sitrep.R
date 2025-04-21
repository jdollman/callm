#' Get Situation Report on LLM Provider Prerequisites
#'
#' Verifies API key setup via environment variables and checks basic API
#' connectivity for specified LLM providers (Google, OpenAI, Anthropic).
#' Provides informative messages and returns results invisibly.
#'
#' @param orgs A character vector specifying the providers to check.
#'   Supported values are "google", "openai", "anthropic".
#'   Defaults to checking all supported providers.
#' @param verbose Logical. If TRUE (default), prints detailed status messages,
#'   including suggestions, to the console.
#'
#' @return (Invisibly) A named list summarizing the check results for each
#'   requested provider. Each element in the list corresponds to a provider
#'   and contains another list with two logical elements:
#'   \describe{
#'     \item{`key_set`}{TRUE if the expected environment variable for the API key
#'       is set and non-empty, FALSE otherwise.}
#'     \item{`api_ok`}{TRUE if a basic API call succeeded (typically HTTP 2xx status),
#'       FALSE if the API call failed (network error, auth error, timeout, etc.),
#'       or NA if the API key was not set (and thus the check was skipped).}
#'   }
#' @export
#'
#' @importFrom httr2 request req_method req_timeout req_user_agent req_retry
#' @importFrom httr2 req_url_query req_auth_bearer_token req_headers
#' @importFrom httr2 req_body_json req_perform resp_status
#' @importFrom httr2 resp_status_desc resp_body_string
#' @importFrom jsonlite toJSON
#'
#' @examples
#' \dontrun{
#'   # Run checks interactively (messages print, result list doesn't auto-print)
#'   sitrep()
#'
#'   # Capture the results programmatically
#'   check_results <- sitrep(verbose = FALSE) # Quieter run
#'   if (!isTRUE(check_results$openai$key_set)) {
#'     message("OpenAI key not found. See ?sitrep for setup help.")
#'   }
#'   print(check_results) # Explicitly print the captured list
#'
#'   # Check only Anthropic
#'   anthropic_status <- sitrep("anthropic")
#' }
sitrep <- function(
    orgs = c("google", "openai", "anthropic"),
    verbose = TRUE
) {

  # --- Input Validation ---
  supported_orgs <- c("google", "openai", "anthropic")

  # Check orgs type and handle NULL/zero-length case first
  if (!is.character(orgs)) {
    stop("'orgs' must be a character vector.", call. = FALSE)
  }
  if (length(orgs) == 0) {
    warning("No 'orgs' specified to check.", call. = FALSE)
    return(invisible(list()))
  }

  # Now check if all provided orgs are supported
  invalid_orgs <- setdiff(orgs, supported_orgs) # Find elements in orgs NOT in supported_orgs

  if (length(invalid_orgs) > 0) {
    # If any invalid orgs were found, stop execution with a clear message
    stop(sprintf("Invalid 'orgs' specified (%s). Allowed values are: %s.",
                 # List the specific invalid ones found
                 paste0("'", invalid_orgs, "'", collapse=", "),
                 # List the allowed ones
                 paste0("'", supported_orgs, "'", collapse = ", ")),
         call. = FALSE)
  }
  # If we get here, orgs is a non-empty character vector with only valid elements.
  # No need to call match.arg anymore for validation.

  # ** End New Validation Logic **

  # Validate verbose (this part remains the same)
  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    stop("'verbose' must be a single TRUE or FALSE.", call. = FALSE)
  }


  # --- Provider Configuration ---
  # (Provider config remains the same as before)
  provider_config <- list(
    google = list(
      env_var = "GOOGLE_API_KEY",
      url = "https://generativelanguage.googleapis.com/v1beta/models",
      method = "GET",
      auth_type = "query_param",
      auth_param = "key"
    ),
    openai = list(
      env_var = "OPENAI_API_KEY",
      url = "https://api.openai.com/v1/models",
      method = "GET",
      auth_type = "bearer_header"
    ),
    anthropic = list(
      env_var = "ANTHROPIC_API_KEY",
      url = "https://api.anthropic.com/v1/messages",
      method = "POST",
      auth_type = "x_api_key_header",
      body = list(
        model = "claude-3-haiku-20240307",
        max_tokens = 1,
        messages = list(list(role = "user", content = "Hi"))
      ),
      required_headers = list(
        `anthropic-version` = "2023-06-01",
        `content-type` = "application/json"
      )
    )
  )

  # --- Initialization ---
  results <- stats::setNames(vector("list", length(orgs)), orgs)

  # --- Main Loop ---
  for (org in orgs) {
    if (verbose) message(paste0("--- Checking ", org, " ---"))
    config <- provider_config[[org]]
    results[[org]] <- list(key_set = FALSE, api_ok = NA) # Initialize

    # 1. Check Environment Variable
    api_key <- Sys.getenv(config$env_var)
    key_is_set <- nzchar(api_key)
    results[[org]]$key_set <- key_is_set

    # Construct message based on key status
    if (!key_is_set) {
      key_status_msg <- sprintf(
        "[%s] API Key (%s): **NOT SET**\n  Suggestion: Use `usethis::edit_r_environ()` to add/edit '%s=YOUR_KEY_HERE'",
        toupper(org), config$env_var, config$env_var
      )
    } else {
      key_status_msg <- sprintf("[%s] API Key (%s): SET", toupper(org), config$env_var)
    }
    if (verbose) message(key_status_msg)

    # 2. Check API Connectivity (only if key is set)
    if (!key_is_set) {
      if (verbose) message(paste0("[", toupper(org), "] API Connection: SKIPPED (API key not set)"))
      next
    }

    # Define variables for API check scope
    api_ok <- FALSE
    api_status_msg <- "" # Ensure it's defined before tryCatch

    tryCatch({
      # Build Request using httr2
      pkg_name <- tryCatch(utils::packageName(), error = function(e) "callm")
      pkg_version <- tryCatch(as.character(utils::packageVersion(pkg_name)), error = function(e) "0.0.0.9000")
      user_agent_string <- paste0(pkg_name, "/", pkg_version, " (sitrep; ", R.version.string, ")")

      req <- httr2::request(config$url) |>
        httr2::req_method(config$method) |>
        httr2::req_timeout(10) |>
        httr2::req_user_agent(user_agent_string) |>
        httr2::req_retry(max_tries = 1)

      # Add Authentication
      # (Auth logic remains the same)
      if (config$auth_type == "query_param") {
        req <- req |> httr2::req_url_query(!!!stats::setNames(list(api_key), config$auth_param))
      } else if (config$auth_type == "bearer_header") {
        req <- req |> httr2::req_auth_bearer_token(api_key)
      } else if (config$auth_type == "x_api_key_header") {
        req <- req |> httr2::req_headers(`x-api-key` = api_key)
        if (!is.null(config$required_headers)) {
          req <- req |> httr2::req_headers(!!!config$required_headers)
        }
      }

      # Add Body if needed
      if (!is.null(config$body)) {
        req <- req |> httr2::req_body_json(data = config$body)
      }

      # Perform Request (will error on 4xx/5xx by default)
      resp <- httr2::req_perform(req)

      # If code reaches here, req_perform() succeeded (implies 2xx status)
      api_ok <- TRUE
      api_status_msg <- sprintf("[%s] API Connection: OK (Status: %d)",
                                toupper(org), httr2::resp_status(resp))

    }, error = function(e) {
      # Assign api_ok in the parent scope
      api_ok <<- FALSE
      error_reason <- conditionMessage(e) # Default reason

      # Check if the error was due to HTTP status (4xx/5xx)
      if (inherits(e, "httr2_http_error")) {
        resp_from_error <- e$resp
        if (!is.null(resp_from_error)) {
          http_status <- httr2::resp_status(resp_from_error)
          http_status_desc <- httr2::resp_status_desc(resp_from_error)
          http_status_info <- sprintf("HTTP Status %d - %s", http_status, http_status_desc)
          body_text <- tryCatch({
            substr(httr2::resp_body_string(resp_from_error), 1, 100)
          }, error = function(e_body) {"<Failed to read body>"})
          body_text <- gsub("[\r\n]", " ", body_text)
          error_reason <- paste(http_status_info, "Body:", body_text) # More specific reason
        }
      }

      # Assign api_status_msg in the parent scope
      api_status_msg <<- sprintf(
        "[%s] API Connection: FAILED (%s)\n  Suggestion: Verify API key, network, and provider status for '%s'.",
        toupper(org), error_reason, org
      )
    }) # End tryCatch

    # Assign final status
    results[[org]]$api_ok <- api_ok
    if (verbose) message(api_status_msg)

  } # End for loop over orgs

  if (verbose && length(orgs) > 0) message("--- Checks Complete ---")

  # Return the results list invisibly
  return(invisible(results))
}
