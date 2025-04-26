# tests/testthat/test_build_researcher_agent.R

context("build_researcher_agent()")

library(testthat)

# ── Fake LLMs ────────────────────────────────────────────────────────────

fake_llm_ok    <- function(prompt) "FAKE LLM RESPONSE"
fake_llm_error <- function(prompt) stop("LLM failed")

# ── 1. Even if search fails or is skipped, we still get the LLM output ────

test_that("agent returns LLM response (and NULL search_results) on fake_llm_ok", {
  # ensure we skip any real web search
  Sys.setenv(TAVILY_API_KEY = "")

  agent <- build_researcher_agent(
    llm     = fake_llm_ok,
    verbose = FALSE
  )

  res <- agent("My test query")

  # structure
  expect_type(res, "list")
  expect_named(res, c("query", "prompt", "response", "search_results", "success", "attempts"))

  # core fields
  expect_identical(res$query, "My test query")
  expect_identical(res$response, "FAKE LLM RESPONSE")

  # since search is always skipped, search_results is NULL
  expect_null(res$search_results)

  # success = search_success && llm_success → FALSE && TRUE == FALSE
  expect_false(res$success)

  # your implementation currently retries all n_tries (default = 3) even on success
  expect_equal(res$attempts$search, 3)
  expect_equal(res$attempts$interpretation, 3)

  # prompt at least contains our query
  expect_match(res$prompt, "RESEARCH QUESTION:")
  expect_match(res$prompt, "My test query")
})

# ── 2. LLM errors are caught and surfaced in response ────────────────────

test_that("agent surfaces LLM errors in the response field", {
  Sys.setenv(TAVILY_API_KEY = "")

  agent <- build_researcher_agent(
    llm     = fake_llm_error,
    verbose = FALSE
  )

  # suppress internal retry warnings
  res <- suppressWarnings(agent("Another query"))

  # core fields
  expect_identical(res$query, "Another query")
  expect_null(res$search_results)

  # on LLM error, response is from your catch
  expect_identical(res$response, "LLM interpretation failed: LLM failed")

  # llm_success never becomes TRUE, so success remains FALSE
  expect_false(res$success)

  # LLM was retried all 3 times
  expect_equal(res$attempts$search, 3)
  expect_equal(res$attempts$interpretation, 3)

  # prompt still built around the query
  expect_match(res$prompt, "RESEARCH QUESTION:")
  expect_match(res$prompt, "Another query")
})


