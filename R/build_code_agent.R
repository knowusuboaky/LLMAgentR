# ------------------------------------------------------------------------------
#' Build an R Code Generation Agent
#'
#' Constructs an LLM-based agent for generating, debugging, explaining, or
#' optimizing R code using structured prompts. The agent handles retries and
#' provides comprehensive code assistance.
#'
#' @name build_code_agent
#' @param llm A function that accepts a character prompt and returns an LLM response.
#' @param system_prompt Optional system-level instructions for the agent's behavior.
#' @param user_input The user's task/query (e.g., "Write function to filter NAs").
#' @param max_tries Maximum number of attempts for LLM calls (default: 3).
#' @param backoff Seconds to wait between retries (default: 2).
#' @param verbose Logical controlling progress messages (default: TRUE).
#'
#' @return A list containing:
#' \itemize{
#'   \item input - The user's original query
#'   \item llm_response - The processed LLM response
#'   \item system_prompt - The system instructions used
#'   \item success - Logical indicating if call succeeded
#'   \item attempts - Number of tries made
#' }
#'
#' @examples
#' \dontrun{
#' agent <- build_code_agent(
#'   llm = call_llm,
#'   user_input = "Write function to remove NA rows from dataframe",
#'   verbose = FALSE
#' )
#' }
#' @export
NULL

build_code_agent <- function(
    llm,
    system_prompt = NULL,
    user_input,
    max_tries = 3,
    backoff = 2,
    verbose = TRUE
) {
  if (verbose) message("=== STARTING CODER AGENT ===")

  # Check for suggested packages
  glue     <- get_suggested("glue")
  tidyr    <- get_suggested("tidyr")
  stringr  <- get_suggested("stringr")

  # Default instructions for R coder
  if (is.null(system_prompt)) {
    system_prompt <- "
You are a highly proficient R coding assistant specialized in generating, debugging, explaining, and optimizing R code.
You are also skilled in data analysis using ggplot2, dplyr, tidyr, and other tidyverse tools.

INSTRUCTIONS:

1. **Understand the Task**:
   - Is it:
     - A **code generation** task (e.g., 'Write a function for z-score normalization'),
     - A **debugging** task (e.g., 'Fix this dplyr pipeline'),
     - An **explanation** (e.g., 'Explain what mutate() is doing here'),
     - A **visualization task** (e.g., 'Plot time series by group'),
     - A **multi-step R analysis** (e.g., 'Load, clean, summarize, and plot data')#

2. **Code Generation & Debugging**:
   - Ensure generated code is valid, well-commented, and includes necessary libraries.
   - Add comments to explain logic and reasoning.
   - Include defensive programming and basic error handling when appropriate.
   - If debugging, explain the root cause clearly and suggest improved code.

3. **Data Analysis & Visualization**:
   - Use ggplot2 with attractive styling: titles, labels, themes, color palettes.
   - Clearly label all axes and legends.
   - Show best practices in dplyr pipelines (e.g., use across(), group_by(), case_when()).

4. **Explain Code**:
   - Use bullet points or markdown-style summaries to explain R logic clearly.

5. **Testing**:
   - Include example inputs/outputs or unit test suggestions if applicable.

RESPONSE FORMAT:
- For code, use triple backticks with ```r
- For explanations, use ```markdown blocks

Always follow best R practices, write clear and robust code, and be helpful."
  }

  # Compose prompt
  full_prompt <- sprintf("%s\n\nUSER TASK:\n%s", system_prompt, user_input)

  # LLM call with retry logic
  response <- NULL
  error_message <- NULL
  success <- FALSE

  for (attempt in seq_len(max_tries)) {
    if (verbose) message(sprintf("Attempt %d/%d", attempt, max_tries))

    attempt_result <- tryCatch({
      llm_response <- llm(prompt = full_prompt)

      if (is.null(llm_response) || nchar(trimws(llm_response)) == 0) {
        stop("Empty response received from LLM.")
      }
      llm_response
    }, error = function(e) {
      if (verbose) warning(sprintf("Attempt %d failed: %s", attempt, e$message))
      error_message <<- e$message  # store error separately
      if (attempt < max_tries) Sys.sleep(backoff)
      NULL  # return NULL explicitly
    })

    if (!is.null(attempt_result)) {
      response <- attempt_result
      success <- TRUE
      break
    }
  }

  # Return structured
  list(
    input = user_input,
    llm_response = if (success) response else paste("LLM call failed:", error_message),
    system_prompt = system_prompt,
    success = success,
    attempts = attempt
  )
}
