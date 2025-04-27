# ------------------------------------------------------------------------------
#' Build an Interpreter Agent
#'
#' Constructs an agent that uses LLM to interpret various outputs (plots, tables,
#' text results) and provides structured explanations suitable for both technical
#' and non-technical audiences.
#'
#' @name build_interpreter_agent
#' @param llm A function that accepts a character prompt and returns an LLM response.
#' @param interpreter_prompt Optional custom prompt template (default provides
#'        structured interpretation framework).
#' @param code_output The output to interpret (chart summary, table, text results etc.).
#' @param max_tries Maximum number of attempts for LLM calls (default: 3).
#' @param backoff Seconds to wait between retries (default: 2).
#' @param verbose Logical controlling progress messages (default: TRUE).
#'
#' @return A list containing:
#' \itemize{
#'   \item prompt - The full prompt sent to LLM
#'   \item interpretation - The generated interpretation
#'   \item success - Logical indicating if interpretation succeeded
#'   \item attempts - Number of tries made
#' }
#'
#' @examples
#' \dontrun{
#' # Example table or code output
#' output_text <- "
#' | Region  | Sales | Profit |
#' |---------|-------|--------|
#' | North   |  2000 |   300  |
#' | South   |  1500 |   250  |
#' | East    |  1800 |   400  |
#' | West    |  2200 |   100  |
#' "
#'
#' # Build interpreter agent
#' interpreter_agent <- build_interpreter_agent(
#'   llm = my_llm_wrapper,
#'   code_output = output_text,
#'   max_tries = 3,
#'   backoff = 2,
#'   verbose = FALSE
#' )
#' }
#' @export
NULL

build_interpreter_agent <- function(
    llm,
    interpreter_prompt = NULL,
    code_output,
    max_tries = 3,
    backoff = 2,
    verbose = TRUE
) {
  if (verbose) message("=== STARTING INTERPRETATION AGENT ===")

  # Check for suggested packages
  glue <- get_suggested("glue")

  # Default interpretation prompt template
  if (is.null(interpreter_prompt)) {
    interpreter_prompt <- "
You are a versatile data interpreter. Your role is to provide clear, insightful, and precise interpretations for various types of outputs, including visual plots, tables, descriptions, and results (regardless of whether they come from R or another source).

Below is an output:

{code_output}


Please provide:

**1. Interpretation**:
- If it's a plot or graph (e.g., line chart, scatter plot, bar chart), explain trends, correlations, outliers, and insights.
- If it's a data table or summary statistics, explain key findings, patterns, outliers, and important relationships.
- If it's another kind of result, explain what it means and its real-world implications.

**2. Key Takeaways**:
- Summarize the most important insights in a concise, accessible way.

Make sure your interpretation is easy to follow for both technical and non-technical audiences."
  }

  # Interpolate code_output into the prompt
  final_prompt <- gsub("\\{code_output\\}", code_output, interpreter_prompt)

  # -- Retry LLM interpretation ------------------------------------------
  response <- NULL
  error_message <- NULL
  success <- FALSE

  for (attempt in seq_len(max_tries)) {
    if (verbose) message(sprintf("Attempt %d/%d", attempt, max_tries))

    attempt_result <- tryCatch({
      llm_response <- llm(prompt = final_prompt)  # --- FIX --- use final_prompt

      if (is.null(llm_response) || nchar(trimws(llm_response)) == 0) {
        stop("Empty response received from LLM.")
      }
      llm_response
    }, error = function(e) {
      if (verbose) warning(sprintf("Attempt %d failed: %s", attempt, e$message))
      error_message <<- e$message  # store error separately
      if (attempt < max_tries) Sys.sleep(backoff)
      NULL
    })

    if (!is.null(attempt_result)) {
      response <- attempt_result
      success <- TRUE
      break  # --- FIX --- stop if successful
    }
  }

  if (verbose && !success) message("Interpretation agent failed after ", max_tries, " attempts.")

  # Return structured response
  list(
    prompt         = final_prompt,
    interpretation = if (success) response else paste("Interpretation failed:", error_message),
    success        = success,
    attempts       = attempt
  )
}

