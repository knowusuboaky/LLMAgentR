# Build a Weather Agent

Constructs an LLM-powered weather assistant that fetches data from
OpenWeatherMap and generates user-friendly reports. Handles location
parsing, API calls, caching, and LLM-based summarization.

## Arguments

- llm:

  A function that accepts a character prompt and returns an LLM
  response.

- location_query:

  Free-text location query (e.g., "weather in Toronto").

- system_prompt:

  Optional LLM system prompt for weather reporting.

- weather_api_key:

  OpenWeatherMap API key (defaults to OPENWEATHERMAP_API_KEY env var).

- units:

  Unit system ("metric" or "imperial").

- n_tries:

  Number of retry attempts for API/LLM calls (default: 3).

- backoff:

  Base seconds to wait between retries (default: 2).

- endpoint_url:

  OpenWeatherMap endpoint URL.

- verbose:

  Logical controlling progress messages (default: TRUE).

## Value

A list containing:

- success - Logical indicating if operation succeeded

- location - Cleaned location string

- weather_raw - Raw API response

- weather_formatted - Formatted weather string

- llm_response - Generated weather report

- timestamp - Time of response

- cache_hit - Logical indicating cache usage

- attempts - Number of tries made

## Examples

``` r
if (FALSE) { # \dontrun{
# Get weather information
weather_agent <- build_weather_agent(
  llm = my_llm_wrapper,
  location_query = "Tokyo, Japan",
  system_prompt = NULL,
  weather_api_key = NULL,
  units = "metric", # metric or imperial
  n_tries = 3,
  backoff = 2,
  endpoint_url = NULL,
  verbose = FALSE
)
} # }
```
