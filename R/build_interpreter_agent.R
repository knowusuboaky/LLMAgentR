# ------------------------------------------------------------------------------
#' Build an Interpreter Agent
#'
#' This agent uses an LLM to interpret various types of output such as plots,
#' tables, or text-based results. It returns a structured explanation and
#' takeaways, useful for both technical and non-technical audiences.
#'
#' @name build_interpreter_agent
#' @param llm A function that accepts a character prompt and returns an LLM response.
#' @param interpreter_prompt Optional custom prompt template.
#' @param code_output A string representing the output to interpret (e.g., chart summary, table, etc.).
#'
#' @return A list with the full prompt and LLM-generated interpretation.
#' @examples
#' \dontrun{
#' result <- build_interpreter_agent(
#'   llm = call_llm,
#'   code_output = "Explain the results..."
#' )
#' cat(result$interpretation)
#' }
#' @export
NULL


build_interpreter_agent <- function(
    llm,
    interpreter_prompt = NULL,
    code_output
) {
  cat("=== STARTING INTERPRETATION AGENT ===\n")

  # 1. Packages
  glue <- get_suggested("glue")

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

  # Call LLM
  response <- tryCatch({
    llm(prompt = final_prompt)
  }, error = function(e) {
    paste("LLM call failed:", e$message)
  })

  list(
    prompt = final_prompt,
    interpretation = response
  )
}
