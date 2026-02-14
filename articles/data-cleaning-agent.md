# Data Cleaning Agent

## What This Agent Does

[`build_data_cleaning_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_data_cleaning_agent.md)
recommends and applies cleaning steps, runs code, and can explain what
was done.

## Workflow Diagram

![](../data-cleaning-agent-workflow.png)

## Step 1: Build the Agent

``` r
library(LLMAgentR)

my_llm_wrapper <- function(prompt, verbose = FALSE) "LLM response placeholder"

data_cleaner <- build_data_cleaning_agent(
  model = my_llm_wrapper,
  human_validation = FALSE,
  bypass_recommended_steps = FALSE,
  bypass_explain_code = FALSE,
  verbose = FALSE
)
```

## Step 2: Run with State

``` r
initial_state <- list(
  data_raw = mtcars,
  user_instructions = "Clean missing values and standardize numeric columns.",
  max_retries = 3,
  retry_count = 0
)

final_state <- data_cleaner(initial_state)
str(final_state)
```

## Notes for Beginners

- The agent expects a state list, not just a data frame.
- Keep `human_validation = TRUE` if you want manual checkpoints.
- Inspect both cleaned data and explanation fields in output state.
