
# LLMAgentR

**Build Powerful, Iterative Language Model Agents in R**

---

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![CRAN status](https://www.r-pkg.org/badges/version/LLMAgentR)](https://cran.r-project.org/package=LLMAgentR)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/LLMAgentR?color=orange)](https://cranlogs.r-pkg.org/badges/grand-total/LLMAgentR)
[![Last Commit](https://img.shields.io/github/last-commit/knowusuboaky/LLMAgentR.svg)](https://github.com/knowusuboaky/LLMAgentR/commits/main)
[![Issues](https://img.shields.io/github/issues/knowusuboaky/LLMAgentR.svg)](https://github.com/knowusuboaky/LLMAgentR/issues)

---

## 🚀 Overview

**LLMAgentR** is an R package for building **Language Model Agents** using a modular **state graph** execution framework. Inspired by the LangGraph and LangChain architecture, it supports iterative workflows for:

- 🧠 Researching
- 📝 Document Summarization
- 💻 Code Generation
- 🔍 SQL Querying
- 🧹 Data Cleaning
- 🧬 Feature Engineering
- 📊 Data Visualization
- ⏳ Time Series Forecasting
- 🔄 Data Wrangling
- ☀️ Weather Reporting
- 🗣️ Code Interpretation

Each agent performs **iterative reasoning**: recommending steps, generating R code, executing, debugging, and explaining results. Includes built-in support for 'tidymodels', 'modeltime', 'plotly', 'ggplot2', 'prophet', and more. Designed for analysts, developers, and teams building intelligent, reproducible AI workflows in R.

---

## 🗺️ StateGraph Workflow Framework

`StateGraph()` provides a flexible framework for creating **state-driven workflows** in R. Inspired by graph-based execution libraries such as LangAgent and LangGraph, it helps you define structured agent execution through nodes, edges, and conditional transitions.

### 🚧 Key Components Provided by `StateGraph()`:

- **`add_node(name, func)`**
  - Adds a new node.
  - `name`: *(string)* Node identifier.
  - `func`: *(function)* Node logic; receives and modifies `state`.

- **`add_edge(from, to)`**
  - Defines a direct transition between two nodes.
  - `from`: *(string)* Source node name.
  - `to`: *(string)* Destination node name.

- **`add_conditional_edges(node_name, condition_fun, mapping_list)`**
  - Adds conditional branching from a node.
  - `node_name`: *(string)* Source node name.
  - `condition_fun`: *(function)* Evaluates state, returning a label.
  - `mapping_list`: *(named list)* Maps condition labels to destination nodes.

- **`set_entry_point(node_name)`**
  - Defines the entry (starting) node of the graph.
  - `node_name`: *(string)* Entry node identifier.

- **`compile(checkpointer = NULL)`**
  - Compiles the graph into an executable workflow function.
  - `checkpointer`: *(optional function)* Callback for logging or tracking state between nodes.

- **`END_NODE_NAME`**
  - Special constant `"__end__"` signaling graph termination.

### 🔧 Complete Example using all components clearly:

```r
# Initialize StateGraph
graph <- StateGraph()

# Add nodes with logic
graph$add_node("start", function(state) {
  state$decision <- sample(c("path_A", "path_B"), 1)
  list(update = state)                              # Update state, proceed conditionally
})

graph$add_node("path_A", function(state) {
  cat("Taking path A.\n")
  list(goto = graph$END_NODE_NAME)                  # Go directly to end
})

graph$add_node("path_B", function(state) {
  cat("Taking path B.\n")
  list(goto = graph$END_NODE_NAME)                  # Go directly to end
})

# Add conditional edges from 'start' node
graph$add_conditional_edges(
  node_name = "start",
  condition_fun = function(state) state$decision,   # Condition based on state$decision
  mapping_list = list(
    path_A = "path_A",
    path_B = "path_B"
  )
)

# Set workflow entry point
graph$set_entry_point("start")

# Optional checkpointer function for state logging
checkpointer <- function(state, current_node) {
  cat(sprintf("Checkpoint: currently at node '%s'\n", current_node))
}

# Compile the graph into an executable agent with optional checkpointing
agent <- graph$compile(checkpointer = checkpointer)

# Initialize empty state
state <- list()

# Run compiled agent
final_state <- agent(state)

# Examine the final state
print(final_state)
```
---

### 🎯 **Explanation of Workflow**:

1. **Starting Node** (`"start"`): randomly sets the state to `"path_A"` or `"path_B"`.
2. **Conditional Edges**: dynamically route to either `"path_A"` or `"path_B"` depending on the state set in `"start"`.
3. **Execution and Checkpointing**: workflow execution includes checkpoints at each node transition (logged to the console).
4. **Termination**: explicitly transitions to special end node `"__end__"` signaling workflow completion.


## 📦 Installation

Install the development version from GitHub:

```r
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("knowusuboaky/LLMAgentR")
```

Soon available on CRAN:

```r
install.packages("LLMAgentR") # Coming soon!
```

---

## 🔐 API Setup

To use LLMs or web-based tools, set these environment variables:

```r
Sys.setenv(OPENAI_API_KEY = "")
Sys.setenv(GROQ_API_KEY = "")
Sys.setenv(ANTHROPIC_API_KEY = "")
Sys.setenv(TAVILY_API_KEY = "")
Sys.setenv(OPENWEATHERMAP_API_KEY = "")
```

> ✅ Store these in your `.Renviron` file to persist across sessions.

### .Renviron Example

Place this in a file named `.Renviron` in your home directory:

```
TAVILY_API_KEY=your-tavily-api-key
OPENAI_API_KEY=your-openai-api-key
GROQ_API_KEY=your-groq-api-key
ANTHROPIC_API_KEY=your-anthropic-api-key
OPENWEATHERMAP_API_KEY=your-openweather-api-key
```

Then restart R for the changes to take effect.

---

## ⚙️ LLM Wrapper

Create a reusable LLM wrapper function to standardize your calls:

```r
library(chatLLM)

my_llm_wrapper <- function(prompt) {
  call_llm(
    prompt = prompt,
    provider = "openai",
    model = "gpt-3.5-turbo",
    max_tokens = 2000
  )
}
```

---

## 💼 Supported Agents

| Agent Type             | Function Name                     |
|------------------------|-----------------------------------|
| Code Generator         | `build_code_agent()`              |
| SQL Agent              | `build_sql_agent()`               |
| Researcher             | `build_researcher_agent()`       |
| Interpreter            | `build_interpreter_agent()`       |
| Document Summarizer    | `build_doc_summarizer()`          |
| Feature Engineering    | `build_feature_engineering_agent()` |
| Forecasting            | `build_forecasting_agent()`       |
| Visualization          | `build_visualization_agent()`     |
| Data Cleaning          | `build_data_cleaning_agent()`     |
| Data Wrangling         | `build_data_wrangling_agent()`    |
| Weather Reporter       | `build_weather_agent()`           |

Each agent follows a consistent stateful lifecycle and modular design for enhanced workflow reproducibility and scalability.

---

## 🧠 LLM Providers

LLMAgentR seamlessly integrates with various providers through `call_llm()` including:

- **OpenAI** ([openai.com](https://openai.com))
- **Groq** ([groq.com](https://groq.com))
- **Anthropic** ([anthropic.com](https://www.anthropic.com))

Extend to custom, local, or fine-tuned models via your own wrapper function.

> **Note:** `call_llm()` functionality is part of [ChatLLM](https://github.com/knowusuboaky/chatLLM). You can also use your own LLM wrapper function directly with agents.

---

## 🚩 Usage

Explore detailed usage of each agent clearly:

### 1. Coding with Code Generator `build_code_agent()`

```r
result <- build_code_agent(
  llm = my_llm_wrapper, # LLM wrapper function 
  user_input = "Write an R function that removes NA rows from a dataframe.",
  system_prompt = NULL, # Optional system-level prompt with behavior instructions.
  n_tries = 3, # Number of attempts to generate code
  backoff = 2 # Backoff time in seconds between attempts
)

cat(result$llm_response) # Output the generated code
```

### 2. SQL Database Queries with `build_sql_agent()`

```r
conn <- DBI::dbConnect(RSQLite::SQLite(), "northwind.db") # Connect to SQLite database

sql_agent <- build_sql_agent(
  model = my_llm_wrapper, # LLM wrapper function
  connection = conn, # Database connection
  human_validation = FALSE, # Enable human validation for SQL queries
  bypass_recommended_steps = FALSE, # Skip recommended steps?
  bypass_explain_code = FALSE   # Skip explanation step?
)

state <- list(user_instructions = "What are the sales for each product?. Hint: (UnitPrice * Quantity) = Sales.")
result <- sql_agent(state)
print(result$data_sql) # Output the SQL query result
```

### 3. Research Queries with `create_researcher_agent()`

```r
researcher <- build_researcher_agent(llm = my_llm_wrapper) 
response <- researcher("Latest Messi news today?")
cat(response$response) # Output the research response
```

### 4. Code Interpretation with `build_interpreter_agent()`

```r
output_text <- "| Region | Sales | Profit |
|--------|-------|--------|
| North  |  2000 |   300  |
| South  |  1500 |   250  |" 

interpreter <- build_interpreter_agent(
  llm = my_llm_wrapper, # LLM wrapper function
  code_output = output_text # Code output to interpret
)
cat(interpreter$interpretation) # Output the interpretation of the code
```

### 5. Document Summarization `build_doc_summarizer()`

```r
summarizer <- build_doc_summarizer(llm = my_llm_wrapper)
summary <- summarizer("report.pdf") # Path to the PDF file
cat(summary) # Output the summary of the document
```

### 6. Data Cleaning with `build_data_cleaning_agent()`

```r
cleaner <- build_data_cleaning_agent(
  model = my_llm_wrapper, # LLM wrapper function
  human_validation = FALSE, # Enable human validation for data cleaning
  bypass_recommended_steps = FALSE, # Skip recommended steps?
  bypass_explain_code = FALSE   # Skip explanation step?
)

state <- list(
  user_instructions = "Don't remove outliers when cleaning the data.",
  data_raw = data # Data to be cleaned
) 
result <- cleaner(state)
print(result$data_cleaned) # Output the cleaned data
```

### 7. Forecasting with `build_forecasting_agent()`

```r
forecaster <- build_forecasting_agent(
  model = my_llm_wrapper, # LLM wrapper function
  mode = "dark", # Mode for the forecasting agent # (dark or light)
  line_width = 2, # Line width for the plot
  bypass_recommended_steps = FALSE, # Skip recommended steps?
  bypass_explain_code = FALSE   # Skip explanation step?
)

state <- list(
  user_instructions = "Forecast next 12 months of sales per store with a confidence level of 90%.",
  data_raw = sales_data # Data to be used for forecasting
)

forecast <- forecaster(state)
forecast$forecasting_result # Output the forecasting result

# Save as standalone HTML file
htmlwidgets::saveWidget(
  widget = result$forecasting_result,
  file = "forecast_plot.html", # Save the plot as an HTML file
  selfcontained = TRUE
)
```

### 8. Data Wrangling with `build_data_wrangling_agent()`

```r
wrangler <- build_data_wrangling_agent(
  model = my_llm_wrapper, # LLM wrapper function
  human_validation = FALSE # Enable human validation for data wrangling
  bypass_recommended_steps = FALSE, # Skip recommended steps?
  bypass_explain_code = FALSE   # Skip explanation step?
)

state <- list(
  data_raw = list(df1, df2, df3), # List of dataframes to be wrangled
  user_instructions = "Merge the data frames on the ID column."
)

result <- wrangler(state)
print(result$data_wrangled) # Output the wrangled data
```

### 9. Weather Reports with `build_weather_agent()`

```r
weather <- build_weather_agent(
  llm = my_llm_wrapper, # LLM wrapper function
  location_query = "Paris, FR", # Location for the weather report
  units = "metric" # Units for the weather report (metric or imperial)
)
cat(weather$llm_response) # Output the weather report
```

### 10. Feature Engineering with `build_feature_engineering_agent()`

```r
fe_agent <- build_feature_engineering_agent(
  model = my_llm_wrapper,             # LLM wrapper function
  human_validation = FALSE,           # Enable human validation for feature engineering
  bypass_recommended_steps = FALSE,   # Skip recommended steps?
  bypass_explain_code = FALSE         # Skip explanation step?
)

state <- list(
  data_raw = iris,                    # Data to be used for feature engineering
  target_variable = "Churn",        # Target variable for prediction
  user_instructions = "Inspect the data. Make any new features and transformations that you think will be useful for predicting the target variable.", 
  max_retries = 3,                    # Maximum retries for LLM calls
  retry_count = 0                     # Retry count for LLM calls
)

result <- fe_agent(state)
print(result$data_engineered) # Output the engineered data
```

### 11. Data Visualization with `build_visualization_agent()`

```r
dv_agent <- build_visualization_agent(
  model = my_llm_wrapper,  # Your LLM wrapper function
  human_validation = FALSE,  # Enable human validation for data visualization
  bypass_recommended_steps = FALSE, # Skip recommended steps?
  bypass_explain_code = FALSE   # Skip explanation step?
)

initial_state <- list(
  data_raw = data,  # Data to be used for data visualization
  target_variable = "Churn",  # Target variable for prediction
  user_instructions = "Make a boxplot of the monthly charges vs Churn. Churn should be on X axis and distribution on Y axis.",  # Your instructions
  max_retries = 3,                    # Maximum retries for LLM calls
  retry_count = 0                     # Retry count for LLM calls
)

result <- dv_agent(state)
result$visualization_result # Output the visualization result

# Save as standalone HTML file
htmlwidgets::saveWidget(
  widget = result$visualization_result,
  file = "monthly_charges_vs_churn.html", # Save the plot as an HTML file
  selfcontained = TRUE
)
```
---

## 💬 Contributing

We warmly welcome contributions:

- Fork & Clone
- Branch (`git checkout -b feature/awesome-agent`)
- Commit & Push (`git commit -am 'Add amazing agent'`, `git push`)
- Submit a PR 🎉

---

## 📄 License

**LLMAgentR** is released under the [MIT License](https://opensource.org/licenses/MIT).

---

## Acknowledgements

`LLMAgentR` draws inspiration from the Python library **[LangAgent](https://github.com/langchain-ai/langgraph)**, which provides powerful state-driven graph-based agent workflows. It also benefits from innovative projects like **[RAGFlowChainR](https://github.com/knowusuboaky/RAGFlowChainR)** and the vibrant R community dedicated to open-source AI development. Enjoy building intelligent agents with LLMAgentR!

---

🎉 **LLMAgentR: Building Smart, Scalable, and Reproducible LLM Workflows in R!**

Created with ❤️ by [Kwadwo Daddy Nyame Owusu Boakye](mailto:kwadwo.owusuboakye@outlook.com)

[🌟 Star this repository! 🌟](https://github.com/knowusuboaky/LLMAgentR)

---
