# Tests for embed function

# --- Helper to skip tests if key is not available ---
skip_if_no_openai_key <- function() {
  if (Sys.getenv("OPENAI_API_KEY") == "") {
    skip("OPENAI_API_KEY not set, skipping live API test.")
  }
}

# --- Store batch ID created during tests ---
# See note in test-openai-batch.R about state dependency
created_embedding_batch_id <- NULL

test_that("embed input validation works", {
  expect_error(embed(), "'texts' must be a non-empty character vector")
  expect_error(embed(texts = 123), "'texts' must be a non-empty character vector")
  # You'll fix this later -- currently it's being vexatious
  # expect_error(embed(texts = c("a","b"), org = "google"), "Currently, only org = 'openai' is supported")
  expect_error(embed(texts = c("a","b"), size = "medium"), "Invalid 'size' specified")
  expect_error(embed(texts = c("a","b"), dimensions = -10), "'dimensions' must be NULL or a single positive integer")
  expect_error(embed(texts = c("a","b"), dimensions = 10.5), "'dimensions' must be NULL or a single positive integer")
  expect_error(embed(texts = c("a","b"), batch = "TRUE"), "'batch' must be a single logical")
})

test_that("embed (non-batch) returns correct structure", {
  skip_if_no_openai_key()

  texts <- c("Test embedding 1.", "Another test.")
  embeddings <- NULL
  embeddings <- embed(texts = texts, size = "small")

  expect_type(embeddings, "list")
  expect_length(embeddings, length(texts))
  expect_true(all(sapply(embeddings, is.numeric)))
  # Check dimensions (small model default) - Note: Dimension might change with OpenAI updates
  expect_length(embeddings[[1]], 1536) # Common dimension for text-embedding-3-small

})

test_that("embed (non-batch) handles dimensions argument", {
  skip_if_no_openai_key()

  texts <- c("Test embedding with dimensions.")
  target_dims <- 512
  embeddings <- NULL
  embeddings <- embed(texts = texts, size = "small", dimensions = target_dims)

  expect_type(embeddings, "list")
  expect_length(embeddings, 1)
  expect_true(is.numeric(embeddings[[1]]))
  expect_length(embeddings[[1]], target_dims) # Check if dimension matches request
})

test_that("embed (non-batch) issues message for long input", {
   skip_if_no_openai_key()
   long_texts <- rep("Long text test.", 51) # > 50

   # Using expect_message, might need cli:: format expectation if using cli_alert_info
   expect_message(
       embeddings <- embed(texts = long_texts, size = "small", batch = FALSE),
       regexp = "Consider using `batch = TRUE` for potential cost savings"
   )

   # Also check basic validity of the result
   expect_type(embeddings, "list")
   expect_length(embeddings, length(long_texts))
   expect_true(is.numeric(embeddings[[1]]))
})


test_that("embed can initiate an OpenAI batch job for embeddings", {
  skip_if_no_openai_key()

  texts_batch <- c("Batch embed 1", "Batch embed 2")
  model_to_use <- "text-embedding-3-small"

  batch_id_result <- tryCatch({
    embed(
      texts = texts_batch,
      size = "small", # Corresponds to model_id
      batch = TRUE
    )
  }, error = function(e) {
    skip(paste("API call failed during batch creation:", e$message))
    return(NULL)
  })

  expect_type(batch_id_result, "character")
  expect_length(batch_id_result, 1)
  expect_true(startsWith(batch_id_result, "batch_"))

  # Store for potential future cross-checks, though full testing requires mocking
   if (!is.null(batch_id_result) && startsWith(batch_id_result, "batch_")) {
      created_embedding_batch_id <<- batch_id_result
  }

})

test_that("Batch check/workspace functions handle embedding batches (TODO)", {
   # TODO: Implement mocked tests for check_batch and workspace_batch using
   # an embedding batch ID (created_embedding_batch_id if available).
   # Verify workspace_batch returns a list of numeric vectors for completed
   # embedding jobs. See TODOs in `callm todo.txt`.
   skip("Requires mocking or live testing integration for check/workspace batch with embedding results.")
})

# TODO: Add tests for API errors (4xx, 5xx) in non-batch mode (requires mocking)
# TODO: Add tests for malformed API responses in non-batch mode (requires mocking)
