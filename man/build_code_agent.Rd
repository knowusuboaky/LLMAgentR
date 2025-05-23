% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_code_agent.R
\name{build_code_agent}
\alias{build_code_agent}
\title{Build an R Code-Generation Agent}
\arguments{
\item{llm}{A function that accepts a character `prompt` and returns
an LLM response (optionally accepts `verbose`).}

\item{system_prompt}{Optional system-level instructions that override the
built-in default prompt.}

\item{user_input}{The coding task/query (e.g., `"Write function to filter NAs"`).
**Default `NULL`** – omit to obtain a reusable agent.}

\item{max_tries}{Maximum LLM retry attempts (default `3`).}

\item{backoff}{Seconds to wait between retries (default `2`).}

\item{verbose}{Logical flag to show progress messages (default `TRUE`).}
}
\value{
\itemize{
  \item If `user_input` is `NULL`: a \strong{function} (the coder-agent closure).
  \item Otherwise: a \strong{list} with the fields
    \describe{
      \item{input}{The original user query.}
      \item{llm_response}{The LLM output (or error message).}
      \item{system_prompt}{Prompt actually sent.}
      \item{success}{Logical; did the call succeed?}
      \item{attempts}{Number of attempts made.}
    }
}
}
\description{
Constructs an LLM-powered agent for generating, debugging, explaining, or
optimizing R code.
**Two calling patterns are supported**:
\itemize{
  \item **Builder pattern** – omit `user_input`. The function returns a
        reusable \strong{coder-agent closure}. Call that closure with
        different queries whenever you need code help.
  \item **One-shot pattern** – provide `user_input`. The function executes
        immediately and returns the result once.
}
}
\details{
The agent automatically retries failed LLM calls (with exponential back-off)
and always returns a structured result.
}
\examples{
\dontrun{
## ------------------------------------------------------------------
## 1)  Builder pattern – create a reusable coder agent
## ------------------------------------------------------------------
coder <- build_code_agent(
  llm       = my_llm_wrapper,   # your own wrapper around the LLM API
  max_tries = 3,
  backoff   = 2,
  verbose   = FALSE
)

# Use the agent multiple times
res1 <- coder("Write an R function that z-score–standardises all numeric columns.")
res2 <- coder("Explain what `\%>\%` does in tidyverse pipelines.")

## ------------------------------------------------------------------
## 2)  One-shot pattern – run a single request immediately
## ------------------------------------------------------------------
one_shot <- build_code_agent(
  llm        = my_llm_wrapper,
  user_input = "Create a ggplot2 bar chart of mpg by cyl in mtcars.",
  max_tries  = 3,
  backoff    = 2,
  verbose    = FALSE
)
}
}
