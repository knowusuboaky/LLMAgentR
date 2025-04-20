# tests/testthat/test_build_doc_summarizer_agent.R

context("build_doc_summarizer_agent()")

library(testthat)
library(glue)
library(pdftools)

# ── Fake LLMs ────────────────────────────────────────────────────────────
fake_llm_ok    <- function(prompt) "FAKE SUMMARY"
fake_llm_error <- function(prompt) stop("LLM is down")

summarizer_ok <- build_doc_summarizer_agent(fake_llm_ok)

# ── 1. Plain character input summarised ──────────────────────────────────
test_that("summariser works for raw text input", {
  res <- summarizer_ok("Some text to summarise.")
  expect_identical(res, "FAKE SUMMARY")
})

# ── 2. .txt file is read and summarised ----------------------------------
test_that("summariser works for a .txt file", {
  tmp <- tempfile(fileext = ".txt")
  writeLines(c("Line A", "Line B"), tmp)

  res <- summarizer_ok(tmp)
  expect_identical(res, "FAKE SUMMARY")

  unlink(tmp)
})

# ── 3. PDF path is handled when pdftools is available --------------------
test_that("summariser works for a PDF via mocked pdftools::pdf_text", {
  skip_if_not_installed("pdftools")      # ensures test skipped on CRAN without pdftools

  pdf_file <- tempfile(fileext = ".pdf")
  file.create(pdf_file)                  # empty file is fine; we mock the reader

  with_mocked_bindings(
    pdf_text = function(path, ...) c("Page 1 text", "Page 2 text"),
    .package = "pdftools",
    {
      res <- summarizer_ok(pdf_file)
      expect_identical(res, "FAKE SUMMARY")
    }
  )

  unlink(pdf_file)
})

# ── 4. PDF read failure propagates an error ------------------------------
test_that("error from pdftools bubbles up", {
  skip_if_not_installed("pdftools")

  bad_pdf <- tempfile(fileext = ".pdf")
  file.create(bad_pdf)

  with_mocked_bindings(
    pdf_text = function(path, ...) stop("Corrupt PDF"),
    .package = "pdftools",
    {
      expect_error(
        summarizer_ok(bad_pdf),
        "Error loading document"
      )
    }
  )

  unlink(bad_pdf)
})
