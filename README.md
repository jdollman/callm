
# callm

The goal of `callm` is to provide a robust and user-friendly R interface
for interacting with major large language model (LLM) APIs: OpenAI,
Google’s Gemini, and Anthropic’s Claude. I sincerely apologize to all
open-source advocates.

`callm` simplifies common tasks like single chat turns (`single_turn`
and `single_turns`) and getting text embeddings (`embed`). It handles
API authentication via environment variables (see `sitrep`), and manages
exceptions gracefully. Both `single_turns` and `embed` support efficient
bulk processing using the OpenAI Batch API (`batch = TRUE` argument) and
`callm` exports `check_batch()` to check on your batch’s progress and
`workspace_batch()` and download the finished batch and import it into
your workspace.

## Installation

You can install the development version of `callm` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak") # Run once if you don't have pak
pak::pak("jdollman/callm")
```

## API Key Setup

`callm` requires API keys for the services you want to use. These
**must** be set as environment variables. The package looks for:

- `OPENAI_API_KEY`
- `GOOGLE_API_KEY`
- `ANTHROPIC_API_KEY`

The recommended way to set these securely and persistently is to add
them to your user-level `.Renviron` file. You can open this file for
editing by running:

``` r
usethis::edit_r_environ()
```

Add lines like `OPENAI_API_KEY=sk-...` to the file, save it, and
**restart your R session** for the changes to take effect.

Alternatively, you can set them for the current session using
`Sys.setenv()`, but you’ll need to do this every time you start R:

``` r
# Example for current session only:
Sys.setenv(OPENAI_API_KEY = "YOUR_KEY_HERE")
```

## Basic Examples

``` r
library(callm)
```

You can use `sitrep()` to get a `sit`uation `rep`ort, which here means,
“Why environment variables do I have set?” You’ll need to have *e.g.*
the Anthropic key set and in your environment if you want to “Claude it
up.”

**Single Chat Turn (Gemini):**

``` r
prompt <- "Explain the concept of 'vectorization' in R simply."
response <- single_turn(prompt, org = "google")

print(response)
```

**Multiple Single Chat Turns (Anthropic):**

``` r
# Ensure OPENAI_API_KEY is set as environment variable

prompt_1 <- "Explain why R is superior to Python"
prompt_2 <- "Is Dario Amodei a better person than Sam Altman?"
prompts <- c(prompt_1, prompt_2)
response <- single_turns(user_msgs = prompts, org = "anthropic")

print(response)
```

**Get Text Embeddings (OpenAI):**

``` r
# Ensure OPENAI_API_KEY is set as environment variable

texts_to_embed <- c(
  "R is great for statistics.",
  "The quick brown fox."
)
# Get default small embeddings (1536 dimensions)
embeddings <- embed(texts = texts_to_embed)

# Check dimensions of the first embedding
print(length(embeddings[[1]]))

# Get truncated embeddings
# (and send me an email telling me why you did this)
embeddings_short <- embed(texts = texts_to_embed, dimensions = 128)
print(length(embeddings_short[[1]]))
```

**OpenAI Batch Processing Workflow (Conceptual):**

Batch processing is useful for large numbers of requests to save time
and potentially cost (especially for embeddings).

``` r
# Ensure OPENAI_API_KEY is set

# a) Initiate batch job (e.g., for embeddings)
batch_texts <- paste("Item", 1:100) # Example: 100 texts
batch_id <- embed(texts = batch_texts, batch = TRUE)
print(paste("Batch job started with ID:", batch_id))

# b) Check status later (might need to wait minutes/hours)
# This prints a user-friendly summary
check_batch(batch_id)

# c) When check_batch shows 'completed', retrieve results
# workspace_batch returns results formatted like the non-batch version of 
# `single_turns` or `embed` (e.g., a list of numeric vectors for embeddings)
results_list <- workspace_batch(batch_id)
# print(length(results_list)) # Should be 100
```
