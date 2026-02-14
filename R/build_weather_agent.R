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
#' # Get weather information
#' weather_agent <- build_weather_agent(
#'   llm = my_llm_wrapper,
#'   location_query = "Tokyo, Japan",
#'   system_prompt = NULL,
#'   weather_api_key = NULL,
#'   units = "metric", # metric or imperial
#'   n_tries = 3,
#'   backoff = 2,
#'   endpoint_url = NULL,
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
    verbose = TRUE,
    output = c("agent", "mermaid", "both"),
    direction = c("TD", "LR"),
    subgraphs = NULL,
    style = TRUE
) {
  output <- match.arg(output)
  direction <- match.arg(direction)

  if (!identical(output, "agent")) {
    `%||%` <- function(a, b) if (!is.null(a)) a else b

    node_functions <- list(
      validate_api_and_inputs = function(state) {
        key <- state$weather_api_key %||% weather_api_key %||% Sys.getenv("OPENWEATHERMAP_API_KEY")
        endpoint <- state$endpoint_url %||% endpoint_url %||% "https://api.openweathermap.org/data/2.5/weather"
        list(
          weather_api_key = key,
          endpoint_url = endpoint,
          units = state$units %||% match.arg(units),
          location_query = state$location_query %||% location_query,
          fetch_attempt = 0L,
          llm_attempt = 0L
        )
      },
      parse_location = function(state) {
        parsed <- tryCatch(parse_and_validate_location(state$location_query), error = function(e) e)
        if (inherits(parsed, "error")) {
          return(list(location_error = parsed$message))
        }
        list(clean_location = parsed, location_error = NULL)
      },
      fetch_weather_api = function(state) {
        attempt <- as.integer(state$fetch_attempt %||% 0L) + 1L
        if (!is.null(state$location_error)) {
          return(list(fetch_attempt = attempt, weather_error = state$location_error))
        }
        weather_info <- tryCatch(
          get_fresh_weather(
            location = state$clean_location,
            api_key = state$weather_api_key,
            units = state$units,
            endpoint_url = state$endpoint_url
          ),
          error = function(e) e
        )
        if (inherits(weather_info, "error")) {
          list(fetch_attempt = attempt, weather_error = weather_info$message, weather_info = NULL)
        } else {
          list(fetch_attempt = attempt, weather_error = NULL, weather_info = weather_info)
        }
      },
      check_weather_fetch = function(state) {
        route <- if (!is.null(state$weather_info)) {
          "success"
        } else if (as.integer(state$fetch_attempt %||% 0L) < as.integer(state$n_tries %||% n_tries)) {
          "retry"
        } else {
          "failed"
        }
        list(route = route)
      },
      fetch_backoff = function(state) {
        attempt <- as.integer(state$fetch_attempt %||% 1L)
        wait_sec <- as.numeric(state$backoff %||% backoff) * (2 ^ max(0, attempt - 1L))
        Sys.sleep(wait_sec)
        list()
      },
      build_weather_prompt = function(state) {
        prompt_used <- system_prompt %||% "You are a weather assistant."
        llm_prompt <- sprintf(
          "%s\n\nUser query: %s\n\nWeather data:\n%s",
          prompt_used,
          state$location_query,
          state$weather_info$formatted
        )
        list(llm_prompt = llm_prompt)
      },
      call_llm = function(state) {
        attempt <- as.integer(state$llm_attempt %||% 0L) + 1L
        llm_response <- tryCatch(llm(prompt = state$llm_prompt), error = function(e) e)
        if (inherits(llm_response, "error")) {
          list(llm_attempt = attempt, llm_response = NULL, llm_error = llm_response$message)
        } else {
          list(llm_attempt = attempt, llm_response = llm_response, llm_error = NULL)
        }
      },
      check_llm_response = function(state) {
        route <- if (!is.null(state$llm_response) && nzchar(trimws(state$llm_response))) {
          "success"
        } else if (as.integer(state$llm_attempt %||% 0L) < as.integer(state$n_tries %||% n_tries)) {
          "retry"
        } else {
          "failed"
        }
        list(route = route)
      },
      llm_backoff = function(state) {
        attempt <- as.integer(state$llm_attempt %||% 1L)
        wait_sec <- as.numeric(state$backoff %||% backoff) * (2 ^ max(0, attempt - 1L))
        Sys.sleep(wait_sec)
        list()
      },
      fallback_to_formatted_weather = function(state) {
        list(llm_response = state$weather_info$formatted %||% state$weather_error %||% "Weather unavailable")
      },
      return_weather_payload = function(state) {
        list(
          success = !is.null(state$weather_info),
          location = state$clean_location %||% "",
          weather_raw = state$weather_info$raw %||% NULL,
          weather_formatted = state$weather_info$formatted %||% NULL,
          llm_response = state$llm_response %||% state$weather_info$formatted %||% NULL,
          timestamp = Sys.time(),
          attempts = as.integer(state$fetch_attempt %||% 0L) + as.integer(state$llm_attempt %||% 0L)
        )
      },
      return_error = function(state) {
        stop(state$weather_error %||% state$location_error %||% "Weather request failed")
      }
    )

    edges <- list(
      c("validate_api_and_inputs", "parse_location"),
      c("parse_location", "fetch_weather_api"),
      c("fetch_weather_api", "check_weather_fetch"),
      c("fetch_backoff", "fetch_weather_api"),
      c("build_weather_prompt", "call_llm"),
      c("call_llm", "check_llm_response"),
      c("llm_backoff", "call_llm"),
      c("fallback_to_formatted_weather", "return_weather_payload"),
      c("return_weather_payload", "__end__"),
      c("return_error", "__end__")
    )
    conditional_edges <- list(
      list(
        from = "check_weather_fetch",
        condition = function(state) state$route %||% "failed",
        mapping = list(
          success = "build_weather_prompt",
          retry = "fetch_backoff",
          failed = "return_error"
        )
      ),
      list(
        from = "check_llm_response",
        condition = function(state) state$route %||% "failed",
        mapping = list(
          success = "return_weather_payload",
          retry = "llm_backoff",
          failed = "fallback_to_formatted_weather"
        )
      )
    )

    compiled <- build_custom_agent(
      node_functions = node_functions,
      entry_point = "validate_api_and_inputs",
      edges = edges,
      conditional_edges = conditional_edges,
      output = "both",
      direction = direction,
      subgraphs = subgraphs,
      style = style
    )

    if (identical(output, "mermaid")) {
      return(compiled$mermaid)
    }
    return(compiled)
  }

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
