# tests/testthat/00_setup.R

# You can set environment variables to mock or hold real API keys:
Sys.setenv(OPENAI_API_KEY         = "FAKE_OPENAI_KEY_FOR_TESTS")
Sys.setenv(GROQ_API_KEY           = "FAKE_GROQ_KEY_FOR_TESTS")
Sys.setenv(ANTHROPIC_API_KEY      = "FAKE_ANTHROPIC_KEY_FOR_TESTS")
Sys.setenv(TAVILY_API_KEY         = "FAKE_TAVILY_KEY_FOR_TESTS")
Sys.setenv(OPENWEATHERMAP_API_KEY = "FAKE_OPENWEATHERMAP_KEY_FOR_TESTS")


# If you have additional setup steps for testthat, place them here.
# This file is automatically sourced before test_*.R files.
