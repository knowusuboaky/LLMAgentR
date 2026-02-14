# Build a SQL Agent Graph

This function constructs a full SQL database agent using a graph-based
workflow. It supports step recommendation, SQL code generation, error
handling, optional human review, and automatic explanation of the final
code.

## Arguments

- model:

  A function that accepts prompts and returns LLM responses.

- connection:

  A DBI connection object to the target SQL database.

- n_samples:

  Number of candidate SQL plans to consider (used in prompt).

- human_validation:

  Whether to include a human review node.

- bypass_recommended_steps:

  If TRUE, skip the step recommendation node.

- bypass_explain_code:

  If TRUE, skip the final explanation step.

- verbose:

  Logical indicating whether to print progress messages (default: TRUE).

## Value

A compiled SQL agent function that runs via a state machine (graph
execution).

## Examples

``` r
if (FALSE) { # \dontrun{
# 1) Connect to the database
conn <- DBI::dbConnect(RSQLite::SQLite(), "tests/testthat/test-data/northwind.db")

# 2) Create the SQL agent
sql_agent <- build_sql_agent(
  model                    = my_llm_wrapper,
  connection               = conn,
  human_validation         = FALSE,
  bypass_recommended_steps = FALSE,
  bypass_explain_code      = FALSE,
  verbose                  = FALSE
)

# 3) Define the initial state
initial_state <- list(
  user_instructions = "Identify the Regions (or Territories) with the highest
  CustomerCount and TotalSales.
  Return a table with columns: Region, CustomerCount, and TotalSales.
Hint: (UnitPrice × Quantity).",
  max_retries       = 3,
  retry_count       = 0
)

# 4) Run the agent
final_state <- sql_agent(initial_state)
} # }
```
