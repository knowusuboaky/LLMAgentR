# Feature Engineering Agent

## What This Agent Does

[`build_feature_engineering_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_feature_engineering_agent.md)
creates/derives features to improve model performance and explains the
transformation logic.

## Workflow Diagram

![](../feature-engineering-agent-workflow.png)

## Generate Mermaid PNGs

``` r
library(LLMAgentR)

my_llm_wrapper <- function(prompt, verbose = FALSE) "LLM response placeholder"

workflow <- build_feature_engineering_agent(
  model = my_llm_wrapper,
  output = "both",
  direction = "LR"
)

save_mermaid_png(
  x = workflow,
  file = "pkgdown/assets/feature-engineering-agent-workflow.png"
)
```

## Step 1: Build the Agent

``` r
library(LLMAgentR)

my_llm_wrapper <- function(prompt, verbose = FALSE) "LLM response placeholder"

feature_agent <- build_feature_engineering_agent(
  model = my_llm_wrapper,
  human_validation = FALSE,
  bypass_recommended_steps = FALSE,
  bypass_explain_code = FALSE,
  verbose = FALSE
)
```

## Step 2: Run with State

``` r
data <- transform(mtcars, target = ifelse(mpg > median(mpg), "high", "low"))

initial_state <- list(
  data_raw = data,
  target_variable = "target",
  user_instructions = "Create useful derived numeric and categorical features.",
  max_retries = 3,
  retry_count = 0
)

final_state <- feature_agent(initial_state)
str(final_state)
```

## Notes for Beginners

- Always set `target_variable` for supervised tasks.
- Review engineered features for leakage before training models.
- Keep human validation on for high-stakes use cases.
