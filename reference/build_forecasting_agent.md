# Build a Time Series Forecasting Agent

Constructs a state graph-based forecasting agent that: recommends
forecasting steps, extracts parameters, generates code, executes the
forecast using \`modeltime\`, fixes errors if needed, and explains the
result. It leverages multiple models including Prophet, XGBoost, Random
Forest, SVM, and Prophet Boost, and combines them in an ensemble.

## Arguments

- model:

  A function that takes a prompt and returns an LLM-generated result.

- bypass_recommended_steps:

  Logical; skip initial step recommendation.

- bypass_explain_code:

  Logical; skip the final explanation step.

- mode:

  Visualization mode for forecast plots. One of \`"light"\` or
  \`"dark"\`.

- line_width:

  Line width used in plotly forecast visualization.

- verbose:

  Logical; whether to print progress messages.

## Value

A callable agent function that mutates the given \`state\` list.

## Examples

``` r
if (FALSE) { # \dontrun{
# 2) Prepare the dataset
my_data <- walmart_sales_weekly

# 3) Create the forecasting agent
forecasting_agent <- build_forecasting_agent(
  model = my_llm_wrapper,
  bypass_recommended_steps = FALSE,
  bypass_explain_code = FALSE,
  mode = "dark", # dark or light
  line_width = 3,
  verbose = FALSE
)

# 4) Define the initial state
initial_state <- list(
  user_instructions = "Forecast sales for the next 30 days, using `id` as the grouping variable,
  a forecasting horizon of 30, and a confidence level of 90%.",
  data_raw = my_data
)

# 5) Run the agent
final_state <- forecasting_agent(initial_state)
} # }
```
