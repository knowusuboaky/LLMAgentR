# tests/testthat/test_build_doc_summarizer_agent.R

context("build_doc_summarizer_agent()")

library(testthat)
library(glue)

skip_if_not_installed("pdftools")
skip_if_not_installed("rvest")
skip_if_not_installed("tesseract")
skip_if_not_installed("officer")
skip_if_not_installed("xml2")

# ── Fake LLMs ────────────────────────────────────────────────────────────
fake_llm_ok <- function(prompt) "FAKE SUMMARY"
fake_llm_error <- function(prompt) stop("LLM is down")

summarizer_ok <- build_doc_summarizer_agent(fake_llm_ok, verbose = FALSE)

# ── 1. .txt file is read and summarised ──────────────────────────────────
test_that("summariser works for a .txt file", {
  tmp <- tempfile(fileext = ".txt")
  writeLines(c("Line A", "Line B"), tmp)

  res <- summarizer_ok(tmp)
  expect_identical(res$summary, "FAKE SUMMARY")
  expect_true(res$success)

  unlink(tmp)
})

# ── 2. PDF read failure propagates an error ──────────────────────────────
test_that("error from pdftools bubbles up", {
  skip_if_not_installed("pdftools")

  bad_pdf <- tempfile(fileext = ".pdf")
  file.create(bad_pdf)

  # Suppress expected warning from error case
  suppressWarnings({
    with_mocked_bindings(
      pdf_text = function(path, ...) stop("Corrupt PDF"),
      .package = "pdftools",
      {
        res <- summarizer_ok(bad_pdf)
        expect_false(res$success)
        expect_false(grepl("Corrupt PDF", res$error) || grepl("Failed to load document", res$error))
      }
    )
  })

  unlink(bad_pdf)
})

# ── 3. LLM failure is handled gracefully ─────────────────────────────────
test_that("LLM errors are caught and reported", {
  summarizer_err <- build_doc_summarizer_agent(
    fake_llm_error,
    verbose = FALSE
  )

  # Suppress expected warning from error case
  suppressWarnings({
    res <- summarizer_err("test")
  })

  expect_false(res$success)
  expect_true(!is.null(res$error))
  expect_true(nchar(res$error) > 0)
})
