# Forecasting Agent

## What This Agent Does

[`build_forecasting_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_forecasting_agent.md)
creates a forecasting workflow with model recommendation, code
generation, retries, execution, and explanation.

## Step 1: Prepare Example Data

``` r
set.seed(42)
my_data <- data.frame(
  id = "series_1",
  date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 120),
  value = 100 + cumsum(rnorm(120, 0.1, 1))
)
```

## Step 2: Build and Run the Agent

``` r
library(LLMAgentR)

my_llm_wrapper <- function(prompt, verbose = FALSE) "LLM response placeholder"

forecast_agent <- build_forecasting_agent(
  model = my_llm_wrapper,
  bypass_recommended_steps = FALSE,
  bypass_explain_code = FALSE,
  mode = "dark",
  line_width = 3,
  verbose = FALSE
)

initial_state <- list(
  user_instructions = "Forecast value for the next 30 days grouped by id.",
  data_raw = my_data
)

final_state <- forecast_agent(initial_state)
str(final_state)
```

## Notes for Beginners

- Ensure your data has a time column and target value column.
- Start with one series before scaling to many IDs.
- Use the explanation output to review model choices.
