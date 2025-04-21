# Test file for sitrep function

# No context() needed for testthat 3rd edition

test_that("Input validation works", {
  # Check for invalid org names using regexp for partial match
  expect_error(sitrep(orgs = "invalid_provider"),
               regexp = "Invalid 'orgs' specified")
  expect_error(sitrep(orgs = c("google", "invalid")))

  # Check verbose argument
  expect_error(sitrep(verbose = "TRUE"),
               regexp = "'verbose' must be a single TRUE or FALSE.")
  expect_error(sitrep(verbose = c(TRUE, FALSE)),
               regexp = "'verbose' must be a single TRUE or FALSE.")
  expect_error(sitrep(verbose = NA),
               regexp = "'verbose' must be a single TRUE or FALSE.")

  # Check empty orgs vector (should now only warn, not error)
  # Match start of warning message
  expect_warning(empty_res <- sitrep(orgs = character(0)),
                 regexp = "No 'orgs' specified to check")
  # Also check the result from the warning case
  expect_equal(empty_res, list())

})

test_that("Output structure is correct", {
  # Use withr to temporarily ensure keys are NOT set for this test
  withr::with_envvar(
    c(GOOGLE_API_KEY = "", OPENAI_API_KEY = "", ANTHROPIC_API_KEY = ""),
    {
      res_default <- sitrep(verbose = FALSE) # Run quietly
      expect_type(res_default, "list")
      expect_named(res_default, c("google", "openai", "anthropic"), ignore.order = TRUE) # Use ignore.order just in case
      expect_true(all(sapply(res_default, is.list)))

      # Check structure of a sub-list
      expect_named(res_default$google, c("key_set", "api_ok"))
      expect_type(res_default$google$key_set, "logical") # Corrected expectation
      expect_type(res_default$google$api_ok, "logical") # Corrected expectation (value can be NA which is logical)

      # Test with specific orgs
      res_subset <- sitrep(orgs = c("openai", "google"), verbose = FALSE)
      expect_type(res_subset, "list")
      expect_named(res_subset, c("openai", "google"), ignore.order = TRUE)
      expect_true(all(sapply(res_subset, is.list)))
      expect_named(res_subset$openai, c("key_set", "api_ok"))

      # Test return value is invisible by default
      expect_invisible(sitrep(orgs = "google", verbose = FALSE))
      capture_res <- sitrep(orgs = "google", verbose = FALSE)
      expect_type(capture_res, "list")
      expect_named(capture_res, "google")
    }
  )
})

test_that("Handles missing environment variables correctly", {
  # Test specifically when a key is NOT set
  withr::with_envvar(
    c(GOOGLE_API_KEY = "", OPENAI_API_KEY = "dummykey"), # Ensure google is unset
    {
      res <- sitrep(orgs = "google", verbose = FALSE) # Run quietly
      expect_false(res$google$key_set)
      expect_true(is.na(res$google$api_ok)) # api_ok should be NA if key not set

      # Check for the specific helpful message if verbose = TRUE
      expect_message(
        sitrep(orgs = "google", verbose = TRUE),
        regexp = "**NOT SET**",
        fixed = TRUE,
        all = FALSE # Check if message appears at all
      )
      expect_message(
        sitrep(orgs = "google", verbose = TRUE),
        # Match the core suggestion part more robustly
        regexp = "Suggestion: Use `usethis::edit_r_environ\\(\\)`",
        all = FALSE # Check if message appears at all
      )
      expect_message(
        sitrep(orgs = "google", verbose = TRUE),
        regexp = "API Connection: SKIPPED",
        fixed = TRUE,
        all = FALSE # Check if message appears at all
      )

    }
  )
})


test_that("Handles set environment variables (key_set only)", {
  # Test specifically when a key IS set
  # We CANNOT reliably test api_ok == TRUE/FALSE here without mocking APIs
  # or using real (potentially invalid) keys. This test only checks key_set.
  test_key <- "a_fake_but_non_empty_key"
  withr::with_envvar(
    c(OPENAI_API_KEY = test_key),
    {
      # Run quietly as API call will likely fail and be noisy
      res <- sitrep(orgs = "openai", verbose = FALSE)
      expect_true(res$openai$key_set)
      # We cannot assert res$openai$api_ok value here reliably without mocking

      # Check message when verbose = TRUE, expecting key SET, but likely API FAIL
      expect_message(
        sitrep(orgs = "openai", verbose = TRUE),
        regexp = "API Key (OPENAI_API_KEY): SET",
        fixed = TRUE,
        all = FALSE
      )
      # Expect it to *try* the connection, likely failing with a fake key
      expect_message(
        sitrep(orgs = "openai", verbose = TRUE),
        regexp = "API Connection: FAILED", # It should try and likely fail
        all = FALSE
      )
    }
  )
})
