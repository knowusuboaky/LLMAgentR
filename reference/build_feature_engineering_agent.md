# Build a Feature Engineering Agent

Constructs a graph-based feature engineering agent that guides the
process of: recommending, generating, executing, fixing, and explaining
feature engineering code.

## Arguments

- model:

  A function that accepts a prompt and returns an LLM-generated
  response.

- human_validation:

  Logical; include a manual review node before code execution.

- bypass_recommended_steps:

  Logical; skip the LLM-based recommendation phase.

- bypass_explain_code:

  Logical; skip final explanation step.

- verbose:

  Logical; whether to print progress messages (default: TRUE)

## Value

A callable agent function that executes feature engineering via a state
graph.

## Examples

``` r
if (FALSE) { # \dontrun{
# 1) Load the data
data <- read.csv("tests/testthat/test-data/churn_data.csv")

# 2) Create the feature engineering agent
feature_engineering_agent <- build_feature_engineering_agent(
  model = my_llm_wrapper,
  human_validation = FALSE,
  bypass_recommended_steps = FALSE,
  bypass_explain_code = FALSE,
  verbose = TRUE
)

# 3) Define the initial state
initial_state <- list(
  data_raw = data,
  target_variable = "Churn",
  user_instructions = "Inspect the data. Make any new features and transformations
  that you think will be useful for predicting the target variable.",
  max_retries = 3,
  retry_count = 0
)

# 4) Run the agent
final_state <- feature_engineering_agent(initial_state)
} # }
```
