# ------------------------------------------------------------------------------
#' Build an R Code-Generation Agent
#'
#' Constructs an LLM-powered agent for generating, debugging, explaining, or
#' optimizing R code.
#' **Two calling patterns are supported**:
#' \itemize{
#'   \item **Builder pattern** – omit `user_input`. The function returns a
#'         reusable \strong{coder-agent closure}. Call that closure with
#'         different queries whenever you need code help.
#'   \item **One-shot pattern** – provide `user_input`. The function executes
#'         immediately and returns the result once.
#' }
#'
#' The agent automatically retries failed LLM calls (with exponential back-off)
#' and always returns a structured result.
#'
#' @name build_code_agent
#' @param llm          A function that accepts a character `prompt` and returns
#'                     an LLM response (optionally accepts `verbose`).
#' @param system_prompt Optional system-level instructions that override the
#'                      built-in default prompt.
#' @param user_input   The coding task/query (e.g., `"Write function to filter NAs"`).
#'                      **Default `NULL`** – omit to obtain a reusable agent.
#' @param max_tries    Maximum LLM retry attempts (default `3`).
#' @param backoff      Seconds to wait between retries (default `2`).
#' @param verbose      Logical flag to show progress messages (default `TRUE`).
#'
#' @return
#' \itemize{
#'   \item If `user_input` is `NULL`: a \strong{function} (the coder-agent closure).
#'   \item Otherwise: a \strong{list} with the fields
#'     \describe{
#'       \item{input}{The original user query.}
#'       \item{llm_response}{The LLM output (or error message).}
#'       \item{system_prompt}{Prompt actually sent.}
#'       \item{success}{Logical; did the call succeed?}
#'       \item{attempts}{Number of attempts made.}
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' ## ------------------------------------------------------------------
#' ## 1)  Builder pattern – create a reusable coder agent
#' ## ------------------------------------------------------------------
#' coder <- build_code_agent(
#'   llm       = my_llm_wrapper,   # your own wrapper around the LLM API
#'   max_tries = 3,
#'   backoff   = 2,
#'   verbose   = FALSE
#' )
#'
#' # Use the agent multiple times
#' res1 <- coder("Write an R function that z-score–standardises all numeric columns.")
#' res2 <- coder("Explain what `%>%` does in tidyverse pipelines.")
#'
#' ## ------------------------------------------------------------------
#' ## 2)  One-shot pattern – run a single request immediately
#' ## ------------------------------------------------------------------
#' one_shot <- build_code_agent(
#'   llm        = my_llm_wrapper,
#'   user_input = "Create a ggplot2 bar chart of mpg by cyl in mtcars.",
#'   max_tries  = 3,
#'   backoff    = 2,
#'   verbose    = FALSE
#' )
#' }
#' @export
NULL

build_code_agent <- function(
    llm,
    system_prompt = NULL,
    user_input = NULL,
    max_tries = 3,
    backoff = 2,
    verbose = TRUE
) {
  # ------------------------------------------------------------------------
  # Helper that does the actual work for a single task ---------------------
  # ------------------------------------------------------------------------
  run_agent <- function(task) {
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
    full_prompt <- sprintf("%s\n\nUSER TASK:\n%s", system_prompt, task)

    response       <- NULL
    error_message  <- NULL
    success        <- FALSE

    for (attempt in seq_len(max_tries)) {
      if (verbose) message(sprintf("Attempt %d/%d", attempt, max_tries))

      attempt_result <- tryCatch({
        # ---- FIX: only pass verbose if supported --------------------------
        llm_response <- if ("verbose" %in% names(formals(llm))) {
          llm(prompt = full_prompt, verbose = verbose)
        } else {
          llm(prompt = full_prompt)
        }

        if (is.null(llm_response) || nchar(trimws(llm_response)) == 0)
          stop("Empty response received from LLM.", call. = FALSE)

        llm_response
      }, error = function(e) {
        if (verbose) warning(sprintf("Attempt %d failed: %s",
                                     attempt, e$message))
        error_message <<- e$message
        if (attempt < max_tries) Sys.sleep(backoff * (2 ^ (attempt - 1)))
        NULL
      })

      if (!is.null(attempt_result)) {
        response <- attempt_result
        success  <- TRUE
        break
      }
    }

    list(
      input         = task,
      llm_response  = if (success) response
      else paste("LLM call failed:", error_message),
      system_prompt = system_prompt,
      success       = success,
      attempts      = attempt
    )
  }

  if (is.null(user_input)) {
    function(query) run_agent(query)   # builder pattern
  } else {
    run_agent(user_input)              # one-shot pattern
  }
}
