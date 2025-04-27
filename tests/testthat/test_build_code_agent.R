context("build_code_agent()")

library(testthat)

# ---------------------------------------------------------------------------
# Helper mock LLMs -----------------------------------------------------------
# ---------------------------------------------------------------------------

fake_llm_success <- function(prompt) {
  return("FAKE LLM RESPONSE")
}

fake_llm_error_then_success <- local({
  counter <- 0L
  function(prompt) {
    counter <<- counter + 1L
    if (counter == 1L) stop("Simulated failure")
    return("RECOVERED RESPONSE")
  }
})

fake_llm_always_error <- function(prompt) {
  stop("Persistent failure")
}

# ---------------------------------------------------------------------------
# Tests ----------------------------------------------------------------------
# ---------------------------------------------------------------------------

test_that("build_code_agent returns expected structure", {
  ui <- "Write a function that doubles every element of a numeric vector."
  result <- build_code_agent(
    llm = fake_llm_success,
    user_input = ui,
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_named(result, c("input", "llm_response", "system_prompt", "success", "attempts"))
  expect_identical(result$input, ui)
  expect_true(is.character(result$llm_response))
  expect_true(is.character(result$system_prompt))
})

test_that("build_code_agent handles LLM errors gracefully", {
  ui <- "Sort a data.frame by multiple columns."
  result <- build_code_agent(
    llm = fake_llm_always_error,
    user_input = ui,
    max_tries = 3,
    backoff = 0,
    verbose = FALSE
  )

  expect_match(result$llm_response, "LLM call failed")
  expect_false(result$success)
})
