# Agent Index

## Overview

This index links to one article per agent and also shows the workflow
diagram for each agent.

## Generate Mermaid PNGs

Use each agent’s built-in graph output (`output = "both"`) and save with
[`save_mermaid_png()`](https://knowusuboaky.github.io/LLMAgentR/reference/as_mermaid.md):

``` r
library(LLMAgentR)

dummy_llm <- function(prompt, verbose = FALSE) "LLM response placeholder"

workflows <- list(
  "code-agent" = build_code_agent(
    llm = dummy_llm,
    output = "both",
    direction = "LR"
  ),
  "sql-agent" = build_sql_agent(
    model = dummy_llm,
    connection = NULL,
    output = "both",
    direction = "LR"
  ),
  "research-agent" = build_researcher_agent(
    llm = dummy_llm,
    tavily_search = "your-tavily-key",
    output = "both",
    direction = "LR"
  ),
  "interpreter-agent" = build_interpreter_agent(
    llm = dummy_llm,
    output = "both",
    direction = "LR"
  ),
  "document-summarizer-agent" = build_doc_summarizer_agent(
    llm = dummy_llm,
    output = "both",
    direction = "LR"
  ),
  "data-cleaning-agent" = build_data_cleaning_agent(
    model = dummy_llm,
    output = "both",
    direction = "LR"
  ),
  "forecasting-agent" = build_forecasting_agent(
    model = dummy_llm,
    output = "both",
    direction = "LR"
  ),
  "data-wrangling-agent" = build_data_wrangling_agent(
    model = dummy_llm,
    output = "both",
    direction = "LR"
  ),
  "weather-agent" = build_weather_agent(
    llm = dummy_llm,
    location_query = "Accra, Ghana",
    weather_api_key = "your-openweathermap-key",
    output = "both",
    direction = "LR"
  ),
  "feature-engineering-agent" = build_feature_engineering_agent(
    model = dummy_llm,
    output = "both",
    direction = "LR"
  ),
  "visualization-agent" = build_visualization_agent(
    model = dummy_llm,
    output = "both",
    direction = "LR"
  )
)

dir.create("pkgdown/assets", recursive = TRUE, showWarnings = FALSE)

for (id in names(workflows)) {
  save_mermaid_png(
    x = workflows[[id]],
    file = file.path("pkgdown", "assets", paste0(id, "-workflow.png"))
  )
}
```

## Core Agents

### Code Generation Agent

Article: [Code Generation
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/code-agent.md)

![](../code-agent-workflow.png)

``` r
workflow <- build_code_agent(
  llm = dummy_llm,
  output = "both",
  direction = "LR"
)
save_mermaid_png(workflow, "pkgdown/assets/code-agent-workflow.png")
```

### SQL Query Agent

Article: [SQL Query
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/sql-agent.md)

![](../sql-agent-workflow.png)

``` r
workflow <- build_sql_agent(
  model = dummy_llm,
  connection = NULL,
  output = "both",
  direction = "LR"
)
save_mermaid_png(workflow, "pkgdown/assets/sql-agent-workflow.png")
```

### Research Agent

Article: [Research
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/research-agent.md)

![](../research-agent-workflow.png)

``` r
workflow <- build_researcher_agent(
  llm = dummy_llm,
  tavily_search = "your-tavily-key",
  output = "both",
  direction = "LR"
)
save_mermaid_png(workflow, "pkgdown/assets/research-agent-workflow.png")
```

### Interpreter Agent

Article: [Interpreter
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/interpreter-agent.md)

![](../interpreter-agent-workflow.png)

``` r
workflow <- build_interpreter_agent(
  llm = dummy_llm,
  output = "both",
  direction = "LR"
)
save_mermaid_png(workflow, "pkgdown/assets/interpreter-agent-workflow.png")
```

### Document Summarizer Agent

Article: [Document Summarizer
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/document-summarizer-agent.md)

![](../document-summarizer-agent-workflow.png)

``` r
workflow <- build_doc_summarizer_agent(
  llm = dummy_llm,
  output = "both",
  direction = "LR"
)
save_mermaid_png(workflow, "pkgdown/assets/document-summarizer-agent-workflow.png")
```

### Data Cleaning Agent

Article: [Data Cleaning
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/data-cleaning-agent.md)

![](../data-cleaning-agent-workflow.png)

``` r
workflow <- build_data_cleaning_agent(
  model = dummy_llm,
  output = "both",
  direction = "LR"
)
save_mermaid_png(workflow, "pkgdown/assets/data-cleaning-agent-workflow.png")
```

### Forecasting Agent

Article: [Forecasting
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/forecasting-agent.md)

![](../forecasting-agent-workflow.png)

``` r
workflow <- build_forecasting_agent(
  model = dummy_llm,
  output = "both",
  direction = "LR"
)
save_mermaid_png(workflow, "pkgdown/assets/forecasting-agent-workflow.png")
```

### Data Wrangling Agent

Article: [Data Wrangling
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/data-wrangling-agent.md)

![](../data-wrangling-agent-workflow.png)

``` r
workflow <- build_data_wrangling_agent(
  model = dummy_llm,
  output = "both",
  direction = "LR"
)
save_mermaid_png(workflow, "pkgdown/assets/data-wrangling-agent-workflow.png")
```

### Weather Agent

Article: [Weather
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/weather-agent.md)

![](../weather-agent-workflow.png)

``` r
workflow <- build_weather_agent(
  llm = dummy_llm,
  location_query = "Accra, Ghana",
  weather_api_key = "your-openweathermap-key",
  output = "both",
  direction = "LR"
)
save_mermaid_png(workflow, "pkgdown/assets/weather-agent-workflow.png")
```

### Feature Engineering Agent

Article: [Feature Engineering
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/feature-engineering-agent.md)

![](../feature-engineering-agent-workflow.png)

``` r
workflow <- build_feature_engineering_agent(
  model = dummy_llm,
  output = "both",
  direction = "LR"
)
save_mermaid_png(workflow, "pkgdown/assets/feature-engineering-agent-workflow.png")
```

### Visualization Agent

Article: [Visualization
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/visualization-agent.md)

![](../visualization-agent-workflow.png)

``` r
workflow <- build_visualization_agent(
  model = dummy_llm,
  output = "both",
  direction = "LR"
)
save_mermaid_png(workflow, "pkgdown/assets/visualization-agent-workflow.png")
```

## Custom Graph Workflows

- [Building Custom
  Agents](https://knowusuboaky.github.io/LLMAgentR/articles/custom-agents.md)
- [Building Multi-Agent
  Teams](https://knowusuboaky.github.io/LLMAgentR/articles/multi-agent-teams.md)
