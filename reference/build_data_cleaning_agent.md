# Build a Data Cleaning Agent

Constructs a multi-step agent workflow to recommend, generate, fix,
execute, and explain robust R code for data cleaning tasks using LLMs
and user-defined data.

## Arguments

- model:

  A function that accepts a prompt and returns a text response (e.g.,
  OpenAI, Claude).

- data_raw:

  A raw data.frame (or list convertible to data.frame) to be cleaned.

- human_validation:

  Logical; whether to include a manual review step.

- bypass_recommended_steps:

  Logical; whether to skip LLM-based cleaning step suggestions.

- bypass_explain_code:

  Logical; whether to skip explanation of the generated code.

- verbose:

  Logical; whether to print progress messages (default: TRUE)

## Value

A compiled graph-based cleaning agent function that accepts and mutates
a state list.

## Examples

``` r
if (FALSE) { # \dontrun{
# 1) Load the data
data <- read.csv("tests/testthat/test-data/churn_data.csv")

# 2) Create the agent
data_cleaner_agent <- build_data_cleaning_agent(
  model = my_llm_wrapper,
  human_validation = FALSE,
  bypass_recommended_steps = FALSE,
  bypass_explain_code = FALSE,
  verbose = FALSE
)

# 3) Define the initial state
initial_state <- list(
  data_raw = data,
  user_instructions = "Don't remove outliers when cleaning the data.",
  max_retries = 3,
  retry_count = 0
)

# 4) Run the agent
final_state <- data_cleaner_agent(initial_state)
} # }
```
