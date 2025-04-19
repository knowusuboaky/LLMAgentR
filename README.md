# chatLLM

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![CRAN status](https://www.r-pkg.org/badges/version/chatLLM)](https://cran.r-project.org/package=chatLLM)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/chatLLM?color=orange)](https://cranlogs.r-pkg.org/badges/grand-total/chatLLM)
[![Codecov test coverage](https://codecov.io/gh/knowusuboaky/chatLLM/branch/main/graph/badge.svg)](https://app.codecov.io/gh/knowusuboaky/chatLLM?branch=main)
[![Last Commit](https://img.shields.io/github/last-commit/knowusuboaky/chatLLM.svg)](https://github.com/knowusuboaky/chatLLM/commits/main)
[![Issues](https://img.shields.io/github/issues/knowusuboaky/chatLLM.svg)](https://github.com/knowusuboaky/chatLLM/issues)
<!-- badges: end -->

## Overview

**chatLLM** is an R package that provides a unified and flexible interface for interacting with popular Large Language Model (LLM) providers such as **OpenAI**, **Groq**, and **Anthropic**. It allows you to easily switch between providers, design complex multi-message interactions, and simulate API calls for testing purposes—using a customizable `.post_func` parameter.

Features include:

- 🔁 Seamless provider switching (OpenAI, Groq, Anthropic)
- 🗣 Support for both single-prompt and multi-message interactions
- 🔄 Integrated retry mechanism with configurable attempts and backoff delays
- 🔌 Parameter extensibility for advanced API features (e.g., presence/frequency penalty, top_p, stop sequences)
- 🛠 Easy testing and simulation via a built-in mechanism to override HTTP calls

---

## Installation

```r
# Install from GitHub
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("knowusuboaky/chatLLM")
```

---

## Environment Setup

To use `chatLLM` with LLM providers, please ensure your API keys are set as environment variables so that sensitive credentials are not hardcoded in your scripts.

### Example: Setting Environment Variables in R

```r
# You can add these to your .Renviron file or run them once per session
Sys.setenv(OPENAI_API_KEY    = "your-openai-api-key")
Sys.setenv(GROQ_API_KEY      = "your-groq-api-key")
Sys.setenv(ANTHROPIC_API_KEY = "your-anthropic-api-key")
```

> 💡 **Tip:** To persist these keys across sessions, add them to your `~/.Renviron` file (this file should be excluded from version control).

---

## Usage

### 1. Simple Prompt Call

```r
response <- call_llm(
  prompt = "Who is messi?",
  provider = "openai",
  max_tokens = 50,
  n_tries = 3,
  backoff = 2
)
cat(response)
```

### 2. Multi-Message Conversation

```r
conv <- list(
  list(role = "system", content = "You are a helpful assistant."),
  list(role = "user", content = "Explain recursion in R.")
)
response <- call_llm(
  messages = conv,
  provider = "openai",
  max_tokens = 200,
  presence_penalty = 0.2,
  frequency_penalty = 0.1,
  top_p = 0.95
)
cat(response)
```

### 3. Using a Custom Fake POST Function for Testing

For testing or simulation, you can override the default HTTP POST call using the `.post_func` argument. For example, the code snippet below defines a fake POST function that mimics a real `httr` response object:

```r
# A revised fake POST function that returns a closer-to-real httr::response object
fake_post <- function(url, encode, body, req_headers, ...) {
  # Check for a "recursion" message: if found, return a recursion-related answer:
  if (!is.null(body$messages)) {
    if (any(grepl("recursion", unlist(lapply(body$messages, `[[`, "content")), ignore.case = TRUE))) {
      return(structure(
        list(
          status_code = 200L,
          url         = url,
          headers     = c("Content-Type" = "application/json"),
          all_headers = list(list(
            status  = 200L,
            version = "HTTP/1.1",
            headers = c("Content-Type" = "application/json")
          )),
          content = charToRaw('{"choices": [{"message": {"content": "Fake explanation: In R, recursion is a technique where a function calls itself. It helps with tree traversals, etc."}}]}'),
          date    = Sys.time()
        ),
        class = "response"
      ))
    }
  }
  # Otherwise, return a generic Lionel Messi answer:
  structure(
    list(
      status_code = 200L,
      url         = url,
      headers     = c("Content-Type" = "application/json"),
      all_headers = list(list(
        status  = 200L,
        version = "HTTP/1.1",
        headers = c("Content-Type" = "application/json")
      )),
      content = charToRaw('{"choices": [{"message": {"content": "Fake answer: Lionel Messi is a renowned professional footballer."}}]}'),
      date    = Sys.time()
    ),
    class = "response"
  )
}

# Using fake_post for a simple prompt:
response <- call_llm(
  prompt = "Who is messi?",
  provider = "openai",
  max_tokens = 50,
  n_tries = 3,
  backoff = 2,
  .post_func = fake_post
)
cat(response, "\n\n")

# And for a multi-message conversation:
conv <- list(
  list(role = "system", content = "You are a helpful assistant."),
  list(role = "user", content = "Explain recursion in R.")
)
response <- call_llm(
  messages = conv,
  provider = "openai",
  max_tokens = 200,
  presence_penalty = 0.2,
  frequency_penalty = 0.1,
  top_p = 0.95,
  .post_func = fake_post
)
cat(response)
```

---

## LLM Support

`chatLLM` leverages the `call_llm()` function to interface with various providers. For example, switching to Groq's API is as simple as:

```r
call_llm(
  prompt = "Summarize the capital of France.",
  provider = "groq",
  model = "mixtral-8x7b-32768",
  temperature = 0.7,
  max_tokens = 200
)
```

---

## Contributing

Contributions are welcome! Please review the [CONTRIBUTING](CONTRIBUTING.md) file for guidelines on how to get started.

---

## Issues

If you encounter any issues or have suggestions, please open an issue on [GitHub Issues](https://github.com/knowusuboaky/chatLLM/issues).

---

## License

MIT © [Kwadwo Daddy Nyame Owusu Boakye](mailto:kwadwo.owusuboakye@outlook.com)

---

## Acknowledgements

`chatLLM` draws inspiration from other innovative projects like **RAGFlowChainR** and benefits from the vibrant R community dedicated to open-source development. Enjoy chatting with your LLMs!
```
