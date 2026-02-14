# Agent Index

## Overview

This index links to one article per agent and also shows the workflow
diagram for each agent.

## Generate Mermaid PNGs

Use the built-in generator script
([`compile_graph()`](https://knowusuboaky.github.io/LLMAgentR/reference/as_mermaid.md) +
[`save_mermaid_png()`](https://knowusuboaky.github.io/LLMAgentR/reference/as_mermaid.md)):

``` r
# From the LLMAgentR project root
source("pkgdown/generate-agent-workflow-pngs.R")

# Generate all workflow PNGs
generate_all_agent_workflow_pngs()

# Or generate only one workflow PNG
generate_agent_workflow_png("code-agent")
```

## Core Agents

### Code Generation Agent

Article: [Code Generation
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/code-agent.md)

![](../code-agent-workflow.png)

``` r
source("pkgdown/generate-agent-workflow-pngs.R")
generate_agent_workflow_png("code-agent")
```

### SQL Query Agent

Article: [SQL Query
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/sql-agent.md)

![](../sql-agent-workflow.png)

``` r
source("pkgdown/generate-agent-workflow-pngs.R")
generate_agent_workflow_png("sql-agent")
```

### Research Agent

Article: [Research
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/research-agent.md)

![](../research-agent-workflow.png)

``` r
source("pkgdown/generate-agent-workflow-pngs.R")
generate_agent_workflow_png("research-agent")
```

### Interpreter Agent

Article: [Interpreter
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/interpreter-agent.md)

![](../interpreter-agent-workflow.png)

``` r
source("pkgdown/generate-agent-workflow-pngs.R")
generate_agent_workflow_png("interpreter-agent")
```

### Document Summarizer Agent

Article: [Document Summarizer
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/document-summarizer-agent.md)

![](../document-summarizer-agent-workflow.png)

``` r
source("pkgdown/generate-agent-workflow-pngs.R")
generate_agent_workflow_png("document-summarizer-agent")
```

### Data Cleaning Agent

Article: [Data Cleaning
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/data-cleaning-agent.md)

![](../data-cleaning-agent-workflow.png)

``` r
source("pkgdown/generate-agent-workflow-pngs.R")
generate_agent_workflow_png("data-cleaning-agent")
```

### Forecasting Agent

Article: [Forecasting
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/forecasting-agent.md)

![](../forecasting-agent-workflow.png)

``` r
source("pkgdown/generate-agent-workflow-pngs.R")
generate_agent_workflow_png("forecasting-agent")
```

### Data Wrangling Agent

Article: [Data Wrangling
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/data-wrangling-agent.md)

![](../data-wrangling-agent-workflow.png)

``` r
source("pkgdown/generate-agent-workflow-pngs.R")
generate_agent_workflow_png("data-wrangling-agent")
```

### Weather Agent

Article: [Weather
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/weather-agent.md)

![](../weather-agent-workflow.png)

``` r
source("pkgdown/generate-agent-workflow-pngs.R")
generate_agent_workflow_png("weather-agent")
```

### Feature Engineering Agent

Article: [Feature Engineering
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/feature-engineering-agent.md)

![](../feature-engineering-agent-workflow.png)

``` r
source("pkgdown/generate-agent-workflow-pngs.R")
generate_agent_workflow_png("feature-engineering-agent")
```

### Visualization Agent

Article: [Visualization
Agent](https://knowusuboaky.github.io/LLMAgentR/articles/visualization-agent.md)

![](../visualization-agent-workflow.png)

``` r
source("pkgdown/generate-agent-workflow-pngs.R")
generate_agent_workflow_png("visualization-agent")
```

## Custom Graph Workflows

- [Building Custom
  Agents](https://knowusuboaky.github.io/LLMAgentR/articles/custom-agents.md)
- [Building Multi-Agent
  Teams](https://knowusuboaky.github.io/LLMAgentR/articles/multi-agent-teams.md)
