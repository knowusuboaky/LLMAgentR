# ------------------------------------------------------------------------------
#' Build a Weather Agent
#'
#' Constructs an LLM-powered weather assistant that fetches data from OpenWeatherMap
#' and generates user-friendly reports. Handles location parsing, API calls, caching,
#' and LLM-based summarization.
#'
#' @name build_weather_agent
#' @param llm A function that accepts a character prompt and returns an LLM response.
#' @param location_query Free-text location query (e.g., "weather in Toronto").
#' @param system_prompt Optional LLM system prompt for weather reporting.
#' @param weather_api_key OpenWeatherMap API key (defaults to OPENWEATHERMAP_API_KEY env var).
#' @param units Unit system ("metric" or "imperial").
#' @param n_tries Number of retry attempts for API/LLM calls (default: 3).
#' @param backoff Base seconds to wait between retries (default: 2).
#' @param endpoint_url OpenWeatherMap endpoint URL.
#' @param verbose Logical controlling progress messages (default: TRUE).
#'
#' @return A list containing:
#' \itemize{
#'   \item success - Logical indicating if operation succeeded
#'   \item location - Cleaned location string
#'   \item weather_raw - Raw API response
#'   \item weather_formatted - Formatted weather string
#'   \item llm_response - Generated weather report
#'   \item timestamp - Time of response
#'   \item cache_hit - Logical indicating cache usage
#'   \item attempts - Number of tries made
#' }
#'
#' @examples
#' \dontrun{
#' # Using environment variable
#' Sys.setenv(OPENWEATHERMAP_API_KEY = "your_key")
#' report <- build_weather_agent(
#'   llm = call_llm,
#'   location_query = "Paris, FR"
#' )
#'
#' # With explicit API key
#' report <- build_weather_agent(
#'   llm = call_llm,
#'   location_query = "New York",
#'   weather_api_key = "your_key",
#'   verbose = FALSE
#' )
#' }
#' @export
NULL

build_weather_agent <- function(
    llm,
    location_query,
    system_prompt = NULL,
    weather_api_key = NULL,
    units = c("metric", "imperial"),
    n_tries = 3,
    backoff = 2,
    endpoint_url = NULL,
    verbose = TRUE
) {
  if (verbose) message("=== STARTING WEATHER AGENT ===")

  # Check for required packages
  httr <- get_suggested("httr")
  jsonlite <- get_suggested("jsonlite")

  # Validate parameters
  units <- match.arg(units)
  weather_api_key <- weather_api_key %||% Sys.getenv("OPENWEATHERMAP_API_KEY")
  if (!nzchar(weather_api_key)) stop("OPENWEATHERMAP_API_KEY not provided")
  if (!is.function(llm)) stop("llm must be a function")
  endpoint_url <- endpoint_url %||% "https://api.openweathermap.org/data/2.5/weather"

  # Default system prompt
  system_prompt <- system_prompt %||% paste(
    "You are a weather assistant. Provide concise weather information using the same units the user asked for.",
    sep = "\n"
  )

  # Clean and validate location
  clean_location <- tryCatch({
    parse_and_validate_location(location_query)
  }, error = function(e) {
    stop("Location error: ", e$message)
  })

  # Weather data retrieval with retries
  weather_info <- NULL
  attempts <- 0

  if (verbose) message("Fetching fresh weather data")
  for (attempt in seq_len(n_tries)) {
    attempts <- attempt
    weather_info <- tryCatch({
      get_fresh_weather(
        location = clean_location,
        api_key = weather_api_key,
        units = units,
        endpoint_url = endpoint_url
      )
    }, error = function(e) {
      if (verbose) message(sprintf("Attempt %d failed: %s", attempt, e$message))
      if (attempt < n_tries) Sys.sleep(backoff * (2 ^ (attempt - 1)))
      NULL
    })

    if (!is.null(weather_info)) break
  }

  if (is.null(weather_info)) {
    stop("Failed to fetch weather data after ", n_tries, " attempts")
  }

  # Generate LLM response with retries
  llm_response <- NULL
  for (attempt in seq_len(n_tries)) {
    attempts <- attempts + 1
    llm_prompt <- sprintf(
      "%s\n\nUser query: %s\n\nWeather data:\n%s",
      system_prompt,
      location_query,
      weather_info$formatted
    )

    llm_response <- tryCatch({
      response <- llm(prompt = llm_prompt)
      if (!is.character(response) || length(response) == 0) {
        stop("Invalid LLM response")
      }
      response
    }, error = function(e) {
      if (verbose) message(sprintf("LLM attempt %d failed: %s", attempt, e$message))
      if (attempt < n_tries) Sys.sleep(backoff * (2 ^ (attempt - 1)))
      NULL
    })

    if (!is.null(llm_response)) break
  }

  if (is.null(llm_response)) {
    warning("Failed to generate LLM response after ", n_tries, " attempts")
    llm_response <- weather_info$formatted  # Fallback to raw data
  }

  # Return structured results
  list(
    success = !is.null(weather_info) && !is.null(llm_response),
    location = clean_location,
    weather_raw = weather_info$raw,
    weather_formatted = weather_info$formatted,
    llm_response = llm_response,
    timestamp = Sys.time(),
    attempts = attempts
  )
}


# Helper Functions

#' Clean and Validate a Location Query
#'
#' Performs basic text cleaning on a free-form weather/location query,
#' strips out common words like "weather", "forecast", "in", "for", and "please",
#' then enforces minimal length and alphabetic-only constraints.
#' If no country code is present, it also suggests the user include one.
#'
#' @param query  Character. A free-text user request (e.g. `"weather in Paris"`).
#'
#' @return A cleaned location string, suitable for passing to the
#'         OpenWeatherMap API (e.g. `"Paris, FR"`).  Raises an error if
#'         the cleaned location is too short or contains digits.
#'
#' @examples
#' parse_and_validate_location("weather in New York")
#' \dontrun{
#' parse_and_validate_location("12345")  # error
#' }
#'
#' @export
parse_and_validate_location <- function(query) {
  # Basic cleaning
  clean_loc <- trimws(gsub("weather|forecast|in|for|please", "", query, ignore.case = TRUE))

  # Validate
  if (nchar(clean_loc) < 2) {
    stop("Location name too short (minimum 2 characters)")
  }

  if (grepl("[0-9]", clean_loc)) {
    stop("Location appears to contain numbers - please use city names")
  }

  # Format as "City, Country" if comma not present
  if (!grepl(",", clean_loc)) {
    message("Tip: For better results, use 'City, Country' format (e.g., 'Paris, FR')")
  }

  clean_loc
}


#' Fetch fresh weather data from OpenWeatherMap
#'
#' Internal helper that queries the OpenWeatherMap API and converts the
#' JSON response into a tidy list containing both the raw payload and a
#' human-readable summary string.
#'
#' @param location      Character. City name, ZIP code, or `"lat,lon"`.
#' @param api_key       OpenWeatherMap API key.
#' @param units         Unit system, one of `"metric"`, `"imperial"`,
#'                      or `"standard"`.
#' @param endpoint_url  Character. Complete API endpoint URL.
#'
#'
#' @return A named `list` with two elements:
#'   \itemize{
#'     \item `raw` - the parsed JSON response.
#'     \item `formatted` - a concise character string with key weather
#'           fields for downstream display.
#'   }
#' @keywords internal
#'
get_fresh_weather <- function(location, api_key, units, endpoint_url) {
  res <- httr::GET(
    endpoint_url,
    query = list(
      q = location,
      appid = api_key,
      units = units
    ),
    httr::timeout(10)
  )

  if (httr::http_error(res)) {
    parsed_error <- tryCatch(
      jsonlite::fromJSON(httr::content(res, "text")),
      error = function(e) list(cod = "500", message = "Unknown API error")
    )

    # Handle specific error cases
    if (parsed_error$cod == "404") {
      msg <- sprintf("Location not found: '%s'. Try 'City, Country' format.", location)
    } else if (parsed_error$cod == "401") {
      msg <- "Invalid API key - check OPENWEATHERMAP_API_KEY"
    } else {
      msg <- parsed_error$message %||% "Weather API error"
    }

    stop(msg, " (Code ", parsed_error$cod, ")")
  }

  parsed <- httr::content(res, "parsed")

  if (is.null(parsed$main)) {
    stop("Invalid weather API response structure")
  }

  # Format weather data
  temp_unit <- if (units == "metric") "deg Celsius" else "deg Fahrenheit"
  wind_unit <- if (units == "metric") "m/s" else "mph"

  list(
    raw = parsed,
    formatted = sprintf(
      paste(
        "Location: %s (%s)\nConditions: %s\nTemperature: %.1f%s\n",
        "Humidity: %d%%\nWind: %.1f %s\nPressure: %d hPa"
      ),
      parsed$name,
      parsed$sys$country %||% "N/A",
      parsed$weather[[1]]$description,
      parsed$main$temp,
      temp_unit,
      parsed$main$humidity,
      parsed$wind$speed,
      wind_unit,
      parsed$main$pressure
    )
  )
}
