# Weather Agent

## What This Agent Does

[`build_weather_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_weather_agent.md)
fetches weather data from OpenWeatherMap and asks an LLM to produce a
user-friendly report.

## Workflow Diagram

![](../weather-agent-workflow.png)

## Step 1: Configure API Key

``` r
Sys.setenv(OPENWEATHERMAP_API_KEY = "your-key")
```

## Step 2: Call the Agent

``` r
library(LLMAgentR)

my_llm_wrapper <- function(prompt, verbose = FALSE) "LLM response placeholder"

result <- build_weather_agent(
  llm = my_llm_wrapper,
  location_query = "Accra, Ghana",
  weather_api_key = NULL,
  units = "metric",
  n_tries = 3,
  backoff = 2,
  endpoint_url = NULL,
  verbose = FALSE
)

str(result)
```

## Notes for Beginners

- If `weather_api_key = NULL`, the function reads env var key.
- `units = "metric"` gives Celsius; `"imperial"` gives Fahrenheit.
- Inspect `weather_raw` when troubleshooting.
