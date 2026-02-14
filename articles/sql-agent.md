# SQL Query Agent

## What This Agent Does

[`build_sql_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_sql_agent.md)
generates SQL from user instructions, executes it, retries on errors,
and can explain the final query.

## Workflow Diagram

![](../sql-agent-workflow.png)

## Generate Mermaid PNGs

``` r
library(LLMAgentR)

my_llm_wrapper <- function(prompt, verbose = FALSE) "LLM response placeholder"

workflow <- build_sql_agent(
  model = my_llm_wrapper,
  connection = NULL,
  output = "both",
  direction = "LR"
)

save_mermaid_png(
  x = workflow,
  file = "pkgdown/assets/sql-agent-workflow.png"
)
```

## Step 1: Prepare a Database

``` r
library(DBI)
library(RSQLite)

conn <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(conn, "sales", data.frame(
  region = c("North", "North", "South"),
  amount = c(120, 80, 90)
))
```

## Step 2: Build the Agent

``` r
library(LLMAgentR)

my_llm_wrapper <- function(prompt, verbose = FALSE) "LLM response placeholder"

sql_agent <- build_sql_agent(
  model = my_llm_wrapper,
  connection = conn,
  human_validation = FALSE,
  bypass_recommended_steps = FALSE,
  bypass_explain_code = FALSE,
  verbose = FALSE
)
```

## Step 3: Run the Workflow

``` r
initial_state <- list(
  user_instructions = "Return total sales by region.",
  max_retries = 3,
  retry_count = 0
)

final_state <- sql_agent(initial_state)
str(final_state)
```

## Notes for Beginners

- Always validate generated SQL for production use.
- Keep `human_validation = TRUE` for high-stakes databases.
- Close the connection when done: `dbDisconnect(conn)`.
