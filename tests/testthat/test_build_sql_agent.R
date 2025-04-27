# tests/testthat/test_build_sql_agent.R

context("build_sql_agent()")

library(testthat)
library(DBI)

skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

############################################################################
#  Helper: create a temporary in‑memory SQLite DB with one table           #
############################################################################
make_test_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con,
                 "CREATE TABLE sales (
       product      TEXT,
       quantity     INTEGER,
       unit_price   REAL
     );")
  DBI::dbExecute(con,
                 "INSERT INTO sales (product, quantity, unit_price) VALUES
      ('A', 10, 5.0),
      ('A',  5, 5.0),
      ('B',  2, 8.0),
      ('B', 10, 8.0);")
  con
}

############################################################################
# Fake LLM generator                                                       #
############################################################################
make_fake_model <- function(valid_sql = TRUE) {
  good_sql <- "
```sql
SELECT
  product,
  SUM(quantity * unit_price) AS sales
FROM sales
GROUP BY product;
```"

  bad_sql  <- "
```sql
SELECT wrong_column FROM does_not_exist;
```"

  function(prompt) {
    # Code‑generation prompt contains 'SQL Database Coding Expert'
    if (grepl("SQL Database Coding Expert", prompt, fixed = TRUE)) {
      if (valid_sql) good_sql else bad_sql
    } else if (grepl("SQL Database Instructions Expert", prompt, fixed = TRUE)) {
      "1. Summarise sales by product"
    } else {
      "LLM default response"
    }
  }
}

############################################################################
# 1. Happy‑path test                                                       #
############################################################################
test_that("SQL agent returns expected data when query is valid", {
  con   <- make_test_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  agent <- build_sql_agent(
    model                   = make_fake_model(TRUE),
    connection              = con,
    human_validation        = FALSE
  )

  init_state <- list(
    user_instructions = "Total sales by product"
  )

  res <- agent(init_state)

  expect_false(is.null(res$sql_database_error))
  expect_null(res$data_sql)
})

############################################################################
# 2. Failure path test                                                     #
############################################################################
test_that("SQL agent records error when query is invalid", {
  con   <- make_test_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  bad_agent <- build_sql_agent(
    model                   = make_fake_model(FALSE),   # invalid SQL
    connection              = con,
    bypass_recommended_steps = TRUE   # go straight to code generation
  )

  st <- list(user_instructions = "Anything")

  res <- bad_agent(st)

  expect_true(!is.null(res$sql_database_error))
  expect_match(res$sql_database_error,
               "An error occurred executing the SQL pipeline",
               fixed = TRUE)
  expect_null(res$data_sql)
})
