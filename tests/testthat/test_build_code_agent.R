# tests/testthat/test_build_code_agent.R

context("build_code_agent()")


library(testthat)

#skip_if_not_installed("prrr")


# ---------------------------------------------------------------------------
# Helper mock LLMs -----------------------------------------------------------
# ---------------------------------------------------------------------------

fake_llm_success <- function(prompt) {
  # Always return the same string so the test is deterministic
  "FAKE LLM RESPONSE"
}

fake_llm_error_then_success <- local({
  # First call throws, second call succeeds
  counter <- 0L
  function(prompt) {
    counter <<- counter + 1L
    if (counter == 1L) stop("Simulated failure")
    "RECOVERED RESPONSE"
  }
})

fake_llm_always_error <- function(prompt) {
  stop("Persistent failure")
}

# ---------------------------------------------------------------------------
# Tests ----------------------------------------------------------------------
# ---------------------------------------------------------------------------

test_that("build_code_agent returns the expected structure on success", {
  ui <- "Write a function that doubles every element of a numeric vector."
  result <- build_code_agent(
    llm        = fake_llm_success,
    user_input = ui
  )

  expect_type(result, "list")
  expect_named(result, c("input", "llm_response", "system_prompt"))
  expect_identical(result$input, ui)
  expect_identical(result$llm_response, "FAKE LLM RESPONSE")
  expect_true(is.character(result$system_prompt))
})

test_that("build_code_agent handles a transient LLM error gracefully", {
  ui <- "Sort a data.frame by multiple columns."
  result <- build_code_agent(
    llm        = fake_llm_error_then_success,
    user_input = ui,
    n_tries    = 2,   # allow one retry
    backoff    = 0    # no sleep during tests
  )

  expect_identical(result$llm_response, "RECOVERED RESPONSE")
})

test_that("build_code_agent surfaces persistent LLM failures", {
  ui <- "Plot a histogram with ggplot2."
  result <- build_code_agent(
    llm        = fake_llm_always_error,
    user_input = ui,
    n_tries    = 1,   # a single attempt will fail
    backoff    = 0
  )

  expect_match(result$llm_response, "LLM call failed", fixed = TRUE)
})
