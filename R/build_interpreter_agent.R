# ------------------------------------------------------------------------------
#' Build an Interpreter Agent
#'
#' Constructs an LLM-powered agent that explains plots, tables, text, or other
#' outputs for both technical and non-technical audiences.
#'
#' **Two calling patterns**
#' \itemize{
#'   \item **Builder pattern** – omit \code{code_output}; a reusable
#'         \strong{interpreter-agent closure} is returned.
#'   \item **One-shot pattern** – provide \code{code_output}; the function runs
#'         immediately and returns the interpretation.
#' }
#'
#' @name build_interpreter_agent
#' @param llm               Function that takes \code{prompt} and returns an LLM
#'                          response (may or may not accept \code{verbose}).
#' @param interpreter_prompt Optional template for the prompt (default supplied).
#' @param code_output        The output to interpret (plot caption, table text,
#'                          model summary, etc.). **Default \code{NULL}**.
#' @param max_tries          Max LLM retry attempts (default \code{3}).
#' @param backoff            Seconds between retries (default \code{2}).
#' @param verbose            Logical; print progress (default \code{TRUE}).
#'
#' @return
#' \itemize{
#'   \item If \code{code_output} is \code{NULL}: a \strong{function} (closure).
#'   \item Otherwise: a \strong{list} with
#'     \describe{
#'       \item{prompt}{The full prompt sent to the LLM.}
#'       \item{interpretation}{The LLM’s explanation (or error).}
#'       \item{success}{Logical; did it succeed?}
#'       \item{attempts}{Number of attempts made.}
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' ## 1) Builder pattern --------------------------------------------
#' interp <- build_interpreter_agent(llm = my_llm_wrapper, verbose = FALSE)
#'
#' table_txt <- "
#' | Region | Sales | Profit |
#' | North  | 2000  | 300    |
#' | South  | 1500  | 250    |"
#'
#' res1 <- interp(table_txt)
#' res2 <- interp("R² = 0.87 for the fitted model …")
#'
#' ## 2) One-shot pattern -------------------------------------------
#' build_interpreter_agent(
#'   llm         = my_llm_wrapper,
#'   code_output = table_txt,
#'   verbose     = FALSE
#' )
#' }
#' @export
NULL

build_interpreter_agent <- function(
    llm,
    interpreter_prompt = NULL,
    code_output        = NULL,
    max_tries          = 3,
    backoff            = 2,
    verbose            = TRUE
) {

  # ----------------------------------------------------------------------
  run_agent <- function(output) {
    if (verbose) message("=== STARTING INTERPRETATION AGENT ===")

    glue <- get_suggested("glue")  # optional helper

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

    # Straight substitution, no back-tick wrapping
    final_prompt <- sub("{code_output}", output, interpreter_prompt, fixed = TRUE)

    response <- NULL; err <- NULL; success <- FALSE
    for (attempt in seq_len(max_tries)) {
      if (verbose) message(sprintf("Attempt %d/%d", attempt, max_tries))

      attempt_result <- tryCatch({
        if ("verbose" %in% names(formals(llm))) {
          llm(prompt = final_prompt, verbose = verbose)
        } else {
          llm(prompt = final_prompt)
        }
      }, error = function(e) {
        err <<- e$message
        if (attempt < max_tries) Sys.sleep(backoff * (2 ^ (attempt - 1)))
        NULL
      })

      if (!is.null(attempt_result) && nzchar(trimws(attempt_result))) {
        response <- attempt_result
        success  <- TRUE
        break
      }
    }

    list(
      prompt         = final_prompt,
      interpretation = if (success) response
      else paste("Interpretation failed:", err),
      success        = success,
      attempts       = attempt
    )
  }

  # ----------------------------------------------------------------------
  # Return either a closure (builder) or a result (one-shot)
  # ----------------------------------------------------------------------
  if (is.null(code_output)) {
    function(output) run_agent(output)      # builder pattern
  } else {
    run_agent(code_output)                  # one-shot pattern
  }
}
