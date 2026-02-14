# LLMAgentR [![](reference/figures/llmaopenlogo.png)](https://knowusuboaky.github.io/LLMAgentR/)

## Overview

**LLMAgentR** is an R package for building **Language Model Agents**
using a modular **state graph** execution framework. Inspired by
LangGraph and LangChain architectures, it supports iterative workflows
for research, data analysis, and automation.

------------------------------------------------------------------------

## Installation

``` r
install.packages("LLMAgentR")
```

------------------------------------------------------------------------

## Development version

To get the latest features or bug fixes, you can install the development
version of `LLMAgentR` from GitHub:

``` r
# If needed
install.packages("remotes")

remotes::install_github("knowusuboaky/LLMAgentR")
```

See the full [function
reference](https://knowusuboaky.github.io/LLMAgentR/reference) or the
[package website](https://knowusuboaky.github.io/LLMAgentR/) for more
details.

------------------------------------------------------------------------

## Environment Setup

## API Setup

``` r
Sys.setenv(
  OPENAI_API_KEY     = "your-openai-key",
  GROQ_API_KEY       = "your-groq-key",
  ANTHROPIC_API_KEY  = "your-anthropic-key",
  DEEPSEEK_API_KEY   = "your-deepseek-key",
  DASHSCOPE_API_KEY  = "your-dashscope-key",
  GH_MODELS_TOKEN    = "your-github-models-token"
)
```

------------------------------------------------------------------------

## Ã°Å¸â€?Â? LLM Support (Minimal Wrapper)

The `chatLLM` package allows you to interact with large language models
(LLMs) effortlesslyÃ¢â‚¬â€?either through direct calls or via reusable
minimal wrappers.

### Ã°Å¸â€œÂ¦ Load the Package

``` r
library(chatLLM)
```

------------------------------------------------------------------------

### Ã°Å¸Â§Â© Minimal Wrapper Function

Create a lightweight wrapper around `call_llm()` for reuse. It
optionally provides verbose output:

``` r

call_llm(
  prompt     = "Summarize the capital of France.",
  provider   = "groq",
  model      = "llama3-8b",
  temperature = 0.7,
  max_tokens = 200,
  verbose = TRUE
)

my_llm_wrapper <- function(prompt, verbose = FALSE) {
  if (verbose) {
    message("[my_llm_wrapper] Sending prompt to LLM...")
  }

  # Suppress console output but always return the response
  response_text <- if (verbose) {
    call_llm(
      prompt     = prompt,
      provider   = "openai",
      model      = "gpt-4o",
      max_tokens = 3000,
      verbose    = TRUE
    )
  } else {
    suppressMessages(
      suppressWarnings(
        call_llm(
          prompt     = prompt,
          provider   = "openai",
          model      = "gpt-4o",
          max_tokens = 3000,
          verbose    = TRUE
        )
      )
    )
  }

  if (verbose) {
    message("[my_llm_wrapper] Response received.")
  }

  return(response_text)
}
```

------------------------------------------------------------------------

### Ã°Å¸â€?Â? Quick Access Shortcut

Alternatively, preconfigure an LLM call for one-liners:

``` r
my_llm_wrapper <- call_llm(
      provider    = "openai",
      model       = "gpt-4o",
      max_tokens  = 3000,
      verbose = TRUE
)
```

------------------------------------------------------------------------

## Ã°Å¸â€œÂ¦ Related Package: [`chatLLM`](https://cran.r-project.org/package=chatLLM)

The [`chatLLM`](https://github.com/knowusuboaky/chatLLM) package (now
available on CRAN Ã°Å¸Å½â€°) offers a modular interface for interacting
with LLM providers including **OpenAI**, **Groq**, **Anthropic**,
**DeepSeek**, **DashScope**, and **GitHub Models**.

``` r
install.packages("chatLLM")
```

------------------------------------------------------------------------

## Agent Articles

Detailed guides now live in pkgdown **Articles** (one per agent):

- [Code Generation
  Agent](https://knowusuboaky.github.io/LLMAgentR/articles/code-agent.html)
- [SQL Query
  Agent](https://knowusuboaky.github.io/LLMAgentR/articles/sql-agent.html)
- [Research
  Agent](https://knowusuboaky.github.io/LLMAgentR/articles/research-agent.html)
- [Interpreter
  Agent](https://knowusuboaky.github.io/LLMAgentR/articles/interpreter-agent.html)
- [Document Summarizer
  Agent](https://knowusuboaky.github.io/LLMAgentR/articles/document-summarizer-agent.html)
- [Data Cleaning
  Agent](https://knowusuboaky.github.io/LLMAgentR/articles/data-cleaning-agent.html)
- [Forecasting
  Agent](https://knowusuboaky.github.io/LLMAgentR/articles/forecasting-agent.html)
- [Data Wrangling
  Agent](https://knowusuboaky.github.io/LLMAgentR/articles/data-wrangling-agent.html)
- [Weather
  Agent](https://knowusuboaky.github.io/LLMAgentR/articles/weather-agent.html)
- [Feature Engineering
  Agent](https://knowusuboaky.github.io/LLMAgentR/articles/feature-engineering-agent.html)
- [Visualization
  Agent](https://knowusuboaky.github.io/LLMAgentR/articles/visualization-agent.html)

Custom graph workflows:

- [Building Custom
  Agents](https://knowusuboaky.github.io/LLMAgentR/articles/custom-agents.html)
- [Building Multi-Agent
  Teams](https://knowusuboaky.github.io/LLMAgentR/articles/multi-agent-teams.html)

A full index page is also available:

- [Agent
  Index](https://knowusuboaky.github.io/LLMAgentR/articles/agent-index.html)

## License

MIT Ã‚Â© [Kwadwo Daddy Nyame Owusu
Boakye](mailto:kwadwo.owusuboakye@outlook.com)
