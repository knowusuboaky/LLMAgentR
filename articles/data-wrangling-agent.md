# Data Wrangling Agent

## What This Agent Does

[`build_data_wrangling_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_data_wrangling_agent.md)
helps with joins, reshaping, transformations, and repeatable wrangling
functions.

## Step 1: Build the Agent

``` r
library(LLMAgentR)

my_llm_wrapper <- function(prompt, verbose = FALSE) "LLM response placeholder"

wrangler <- build_data_wrangling_agent(
  model = my_llm_wrapper,
  human_validation = FALSE,
  bypass_recommended_steps = FALSE,
  bypass_explain_code = FALSE,
  verbose = FALSE
)
```

## Step 2: Run with a State List

``` r
initial_state <- list(
  data_raw = mtcars,
  user_instructions = "Group by cyl and return average mpg and hp.",
  max_retries = 3,
  retry_count = 0
)

final_state <- wrangler(initial_state)
str(final_state)
```

## Notes for Beginners

- This agent is for structure changes, not only cleaning.
- Use clear instructions: group, filter, join, pivot, summarize.
- Review generated function code before production use.
