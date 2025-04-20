# ------------------------------------------------------------------------------
# R Code Generation Agent using LLM
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#' Build an R Code Generation Agent
#'
#' This function constructs an LLM-based agent for generating, debugging,
#' explaining, or optimizing R code using a structured task-specific prompt.
#'
#'
#' @name build_code_agent
#' @param llm A function that accepts a character prompt and returns an LLM response.
#' @param system_prompt Optional system-level prompt with behavior instructions.
#' @param user_input The user's input task/query (e.g., "Write a function to filter NA values").
#' @param n_tries Number of attempts to retry the LLM call if it fails.
#' @param backoff Seconds to wait between retries.
#'
#' @return A list containing the user input, the system prompt, and the LLM response.
#' @examples
#' \dontrun{
#' agent <- build_code_agent(
#'   llm = call_llm,
#'   user_input = "Write an R function that removes NA rows from a dataframe"
#' )
#' cat(agent$llm_response)
#' }
#' @export
NULL


build_code_agent <- function(
    llm,
    system_prompt = NULL,
    user_input,
    n_tries = 3,
    backoff = 2
) {
  cat("=== STARTING CODER AGENT ===\n")


  # 1. Packages
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

  # Compose the full prompt
  full_prompt <- sprintf("%s\n\nUser Query:\n%s", system_prompt, user_input)

  # Call LLM with retries
  attempt <- 1L
  repeat {
    result <- tryCatch(
      llm(prompt = full_prompt),
      error = function(e) e
    )

    # success#
    if (!inherits(result, "error")) break

    # failure and out of tries#
    if (attempt >= n_tries) {
      result <- paste("LLM call failed:", result$message)
      break
    }

    # failure but we can retry
    attempt <- attempt + 1L
    Sys.sleep(backoff)
  }

  # -------------------------------------------------------------------
  # 4. Return ----------------------------------------------------------
  # -------------------------------------------------------------------
  list(
    input         = user_input,
    llm_response  = result,
    system_prompt = system_prompt
  )
}

