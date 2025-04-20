# Weather Agent: Builds an LLM-powered assistant to summarize weather queries

#' Build a Weather Agent using OpenWeatherMap + LLM
#'
#' This function fetches weather data using the OpenWeatherMap API and formats
#' the results through an LLM (like OpenAI or Claude) to generate a concise
#' user-facing weather report.
#'
#' @name build_weather_agent
#' @param llm A function that accepts a character prompt and returns an LLM response.
#' @param location_query Free-text user query (e.g., "weather in Toronto").
#' @param system_prompt Optional LLM system prompt.
#' @param weather_api_key API key for OpenWeatherMap. Defaults to \code{Sys.getenv("OPENWEATHERMAP_API_KEY")}.
#' @param units Unit system, either "metric" (deg Celsius) or "imperial" (deg Fahrenheit).
#' @param n_tries Number of retry attempts if weather or LLM calls fail.
#' @param backoff Seconds to wait between retries.
#' @param cache_ttl Time-to-live (in minutes) for cached weather data.
#' @param endpoint_url OpenWeatherMap endpoint URL. Defaults to \code{https://api.openweathermap.org/data/2.5/weather}.
#'
#' @return A list including success flag, location, weather data, LLM response, and cache status.
#' @examples
#' \dontrun{
#' Sys.setenv(OPENWEATHERMAP_API_KEY = "your_api_key")
#' response <- build_weather_agent(
#'   llm = call_llm,
#'   location_query = "Paris, FR"
#' )
#' cat(response$llm_response)
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
    cache_ttl = 30,
    endpoint_url = NULL
) {
  cat("=== STARTING WEATHER AGENT ===\n")

  # Use default endpoint if not provided
  if (is.null(endpoint_url)) {
    endpoint_url <- "https://api.openweathermap.org/data/2.5/weather"
  }

  # Required packages
  httr <- get_suggested("httr")
  jsonlite <- get_suggested("jsonlite")

  # Package-level cache environment
  .weather_cache <- new.env(hash = TRUE)

  # Validate inputs
  units <- match.arg(units)
  weather_api_key <- weather_api_key %||% Sys.getenv("OPENWEATHERMAP_API_KEY")
  if (!nzchar(weather_api_key)) stop("OPENWEATHERMAP_API_KEY not set")
  if (!is.function(llm)) stop("llm must be a function")

  # Default system prompt
  system_prompt <- system_prompt %||%
    "You are a weather assistant. Provide concise weather information using the same units the user asked for."

  # Get clean location
  clean_location <- tryCatch({
    location <- trimws(gsub("weather|forecast|in|for", "", location_query, ignore.case = TRUE))
    if (nchar(location) < 2) stop("Location too short")
    location
  }, error = function(e) stop("Location error: ", e$message))

  # Get weather data - modified cache handling
  cache_key <- paste0(tolower(clean_location), "_", units)

  if (exists(cache_key, envir = .weather_cache)) {
    cached <- get(cache_key, envir = .weather_cache)
    if (difftime(Sys.time(), cached$timestamp, units = "mins") < cache_ttl) {
      cat("Using valid cached data\n")
      weather_info <- cached$data
    } else {
      cat("Cache expired\n")
      weather_info <- NULL
    }
  } else {
    weather_info <- NULL
  }


  if (is.null(weather_info)) {
    cat("Fetching fresh weather data...\n")
    weather_info <- tryCatch({
      query_params <- list(
        q = clean_location,
        units = units,
        appid = weather_api_key
      )

      res <- httr::GET(endpoint_url, query = query_params, httr::timeout(10))
      if (httr::http_error(res)) {
        stop("Weather API error: ", httr::content(res, "text"))
      }

      parsed <- httr::content(res, "parsed")
      if (is.null(parsed$main)) stop("Invalid weather data structure")

      temp_unit <- if (units == "metric") "deg Celsius" else "deg Fahrenheit"
      wind_unit <- if (units == "metric") "m/s" else "mph"

      list(
        raw = parsed,
        formatted = sprintf(
          "Location: %s, %s\nConditions: %s\nTemperature: %.1f%s\nHumidity: %d%%\nWind: %.1f %s\nPressure: %d hPa",
          parsed$name,
          parsed$sys$country %||% "",
          parsed$weather[[1]]$description,
          parsed$main$temp,
          temp_unit,
          parsed$main$humidity,
          parsed$wind$speed,
          wind_unit,
          parsed$main$pressure
        )
      )
    }, error = function(e) {
      stop("Weather data error: ", e$message)
    })

    # Update cache
    .weather_cache[[cache_key]] <- list(
      data = weather_info,
      timestamp = Sys.time()
    )
  }

  # Create LLM prompt and get response
  llm_prompt <- sprintf(
    "%s\n\nUser asked: %s\n\nWeather data:\n%s\n\nProvide a concise weather report:",
    system_prompt,
    location_query,
    weather_info$formatted
  )

  llm_response <- tryCatch({
    response <- llm(prompt = llm_prompt)
    if (is.null(response) || !is.character(response) || length(response) == 0) {
      stop("Invalid response from llm")
    }
    response
  }, error = function(e) {
    warning("LLM failed: ", e$message)
    "Could not generate weather report. Please try again later."
  })

  # Return complete results
  list(
    success = !startsWith(llm_response, "Could not generate"),
    location = clean_location,
    weather_raw = weather_info$raw,
    weather_formatted = weather_info$formatted,
    llm_response = llm_response,
    timestamp = Sys.time(),
    cache_hit = exists(cache_key, envir = .weather_cache) &&
      difftime(Sys.time(), get(cache_key, envir = .weather_cache)$timestamp, units = "mins") < cache_ttl
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

#' Try an operation with exponential back-off
#'
#' Repeatedly calls a *zero-argument* function up to `n_tries` times,
#' pausing `backoff * 2^(i - 1)` seconds after each failure
#' (i.e., 2, 4,8 .seconds when `backoff = 2`).
#' If the function succeeds, its return value is passed through;
#' otherwise the last captured error is re-thrown.
#'
#' @param fn       A function of **no arguments** to execute.
#' @param n_tries  Integer. Maximum number of attempts.  Default`3`.
#' @param backoff  Numeric. Base delay in seconds for the exponential
#'                 back-off sequence.  Default`2`.
#'
#' @return Whatever `fn()` returns on the first successful run.
#'         If all attempts fail the function calls `stop()`.
#'
#' @examples
#' \dontrun{
#' # Retry a flaky network call up to five times
#' try_with_retry(function() readLines("https://httpbin.org/status/500"),
#'                n_tries = 5, backoff = 1)
#' }
#' @export
#'
try_with_retry <- function(fn, n_tries = 3, backoff = 2) {
  for (i in seq_len(n_tries)) {
    result <- tryCatch(fn(), error = function(e) e)
    if (!inherits(result, "error")) return(result)
    if (i < n_tries) {
      Sys.sleep(backoff * (2 ^ (i - 1))) # Exponential backoff
    }
  }
  stop("All ", n_tries, " attempts failed. Last error: ", conditionMessage(result))
}

#' Retrieve (and cache) OpenWeather data
#'
#' @param location  Character. City name, ZIP code, or "lat,lon".
#' @param api_key   Your OpenWeather API key.
#' @param units     Units, one of "metric", "imperial", or "standard".
#' @param ttl  Cache time-to-live in **seconds**.  Default 3600 (= 1 h).
#' @return A `list` with elements `raw_json` and `meta`.
#' @export
#'
get_cached_weather <- function(location, api_key, units, ttl) {
  cache_key <- paste0(tolower(location), "_", units)

  if (exists(cache_key, envir = .weather_cache)) {
    cached <- get(cache_key, envir = .weather_cache)
    if (difftime(Sys.time(), cached$timestamp, units = "mins") < ttl) {
      return(list(success = TRUE, data = cached$data))
    }
  }

  fresh_data <- tryCatch({
    get_fresh_weather(location, api_key, units)
  }, error = function(e) {
    stop(e$message)
  })

  .weather_cache[[cache_key]] <- list(
    data = fresh_data,
    timestamp = Sys.time()
  )

  list(success = TRUE, data = fresh_data)
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
