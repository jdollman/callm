# callm 0.0.0.9000

* Initial release providing functions for:
    * Checking environment for API keys (`sitrep`)
    * Single-turn chat completions (`single_turn`) for OpenAI, Google, Anthropic.
    * **Multiple** single-turn chat completions (`single_turns`) for OpenAI, Google, Anthropic.
    * Text embeddings (`embed`) for OpenAI (small/large, dimensions).
    * OpenAI Batch API workflow:
        * Initiation via `single_turns(batch=TRUE)` and `embed(batch=TRUE)`.
        * Status checking (`check_batch`) with user-friendly summaries.
        * Result retrieval (`workspace_batch`) handling different endpoints (chat/embedding).
* Includes basic non-mocked tests and TODOs for future mocking.
