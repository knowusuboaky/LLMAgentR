
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LLMAgentR <a href="https://knowusuboaky.github.io/LLMAgentR/"><img src="man/figures/llmaopenlogo.png" align="right" height="120" /></a>

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![CRAN
status](https://www.r-pkg.org/badges/version/LLMAgentR)](https://cran.r-project.org/package=LLMAgentR)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/LLMAgentR?color=orange)](https://cranlogs.r-pkg.org/badges/grand-total/LLMAgentR)
[![Last
Commit](https://img.shields.io/github/last-commit/knowusuboaky/LLMAgentR.svg)](https://github.com/knowusuboaky/LLMAgentR/commits/main)
[![Issues](https://img.shields.io/github/issues/knowusuboaky/LLMAgentR.svg)](https://github.com/knowusuboaky/LLMAgentR/issues)
<!-- badges: end -->

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
reference](https://knowusuboaky.github.io/LLMAgentR/reference/) or the
[package website](https://knowusuboaky.github.io/LLMAgentR/) for more
details.

------------------------------------------------------------------------

## Environment Setup

## API Setup

``` r
Sys.setenv(
  OPENAI_API_KEY = "your-key",
  GROQ_API_KEY = "your-key",
  ANTHROPIC_API_KEY = "your-key",
  TAVILY_API_KEY = "your-key",
  OPENWEATHERMAP_API_KEY = "your-key"
)
```

------------------------------------------------------------------------

## LLM Support (Minimal Wrapper)

Load the package and either call an LLM directly or create a reusable
wrapper:

``` r
# Load the chatLLM package
library(chatLLM)

# Direct call example
call_llm(
  prompt = "Summarize the capital of France.",
  provider = "groq",
  model = "llama3-8b",
  temperature = 0.7,
  max_tokens = 200
)

# Silent Minimal wrapper around call_llm() with verbose option
my_llm_wrapper <- function(prompt, verbose = FALSE) {
  if (verbose) {
    message("[my_llm_wrapper] Sending prompt to LLM...")
  }
  
  # Suppress only the printing, NOT the return value
  response_text <- if (verbose) {
    call_llm(
      prompt      = prompt,
      provider    = "openai",
      model       = "gpt-4o",
      max_tokens  = 3000
    )
  } else {
    suppressMessages(
      suppressWarnings(
        call_llm(
          prompt      = prompt,
          provider    = "openai",
          model       = "gpt-4o",
          max_tokens  = 3000
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

## 📦 Related Package: [`chatLLM`](https://cran.r-project.org/package=chatLLM)

The [`chatLLM`](https://github.com/knowusuboaky/chatLLM) package (now
available on CRAN 🎉) offers a modular interface for interacting with
LLM providers including **OpenAI**, **Groq**, and **Anthropic**.

``` r
install.packages("chatLLM")
```

------------------------------------------------------------------------

## Agent Examples

### 1. Code Generation Agent

``` r
library(LLMAgentR)

coder_agent <- build_code_agent(
  llm = my_llm_wrapper,
  user_input = "Write an R function to standardize numeric columns in a data frame using dplyr.",
  max_tries = 3,
  backoff = 2,
  verbose = FALSE
)
```

``` r
cat(coder_agent$llm_response)
#> ```r
#> library(dplyr)
#> 
#> # Function to standardize numeric columns in a data frame
#> standardize_numeric <- function(data) {
#>   # Select only numeric columns
#>   numeric_cols <- data %>% select_if(is.numeric)
#>   
#>   # Standardize each numeric column
#>   standardized_data <- numeric_cols %>% 
#>     mutate(across(everything(), ~ scale(.)))
#>   
#>   # Return the standardized data frame
#>   return(standardized_data)
#> }
#> 
#> # Example usage:
#> # data <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#> # standardized_data <- standardize_numeric(data)
#> # print(standardized_data)
#> ```
#> 
#> ```markdown
#> Explanation:
#> - The function first selects only the numeric columns from the input data frame using select_if(is.numeric).
#> - Then, it standardizes each numeric column using the scale() function within the mutate(across()) function.
#> - The output is a data frame with standardized numeric columns.
#> - The function can be used by passing a data frame as an argument and storing the result in a new variable.
#> ```
```

### 2. SQL Query Agent

``` r
library(LLMAgentR)
library(DBI)
library(RSQLite)

# 1) Connect to the database
conn <- DBI::dbConnect(RSQLite::SQLite(), "tests/testthat/test-data/northwind.db")

# 2) Create the SQL agent
sql_agent <- build_sql_agent(
  model                    = my_llm_wrapper,
  connection               = conn,
  human_validation         = FALSE,
  bypass_recommended_steps = FALSE,
  bypass_explain_code      = FALSE,
  verbose                  = FALSE
)

# 3) Define the initial state
initial_state <- list(
  user_instructions = "Identify the Regions (or Territories) with the highest CustomerCount and TotalSales. Return a table with columns: Region, CustomerCount, and TotalSales.
Hint: (UnitPrice × Quantity).",
  max_retries       = 3,
  retry_count       = 0
)

# 4) Run the agent
final_state <- sql_agent(initial_state)

# 5) Inspect the final state structure
str(final_state)
```

``` r
cat(final_state$sql_database_function)
#> ```r
#> sql_database_pipeline <- function(connection) {
#>   library(DBI)
#>   query <- "SELECT [c].[Region], 
#>        COUNT(DISTINCT [o].[CustomerID]) AS [CustomerCount], 
#>        SUM([od].[UnitPrice] * [od].[Quantity]) AS [TotalSales]
#> FROM [Customers] [c]
#> JOIN [Orders] [o] ON [c].[CustomerID] = [o].[CustomerID]
#> JOIN [Order Details] [od] ON [o].[OrderID] = [od].[OrderID]
#> GROUP BY [c].[Region]
#> ORDER BY [CustomerCount] DESC, [TotalSales] DESC;"
#>   DBI::dbGetQuery(connection, query)
#> }
#> ```

cat(final_state$sql_query_code)
#> ```sql
#> SELECT [c].[Region], 
#>        COUNT(DISTINCT [o].[CustomerID]) AS [CustomerCount], 
#>        SUM([od].[UnitPrice] * [od].[Quantity]) AS [TotalSales]
#> FROM [Customers] [c]
#> JOIN [Orders] [o] ON [c].[CustomerID] = [o].[CustomerID]
#> JOIN [Order Details] [od] ON [o].[OrderID] = [od].[OrderID]
#> GROUP BY [c].[Region]
#> ORDER BY [CustomerCount] DESC, [TotalSales] DESC;
#>```

View the extracted data
df <- as.data.frame(final_state$data_sql)
#> print(df)
#>            Region CustomerCount TotalSales
#> 1   Western Europe            28  133709032
#> 2    South America            16   79472305
#> 3    North America            16   76826352
#> 4  Southern Europe            10   47563030
#> 5    British Isles             8   38700520
#> 6  Central America             5   24802774
#> 7  Northern Europe             4   19015224
#> 8      Scandinavia             3   13902226
#> 9             <NA>             2    9745371
#> 10  Eastern Europe             1    4738465
#>
Disconnect it when done
#>DBI::dbDisconnect(conn)
#>
View the explanation generated by the agent
cat(final_state$messages[[1]]$content)
#> # SQL Database Agent:
#> 
#> The SQL steps in the provided function can be broken down as follows:
#> 
#> 1. **SELECT Clause**: Specifies the columns:
#>    - `[c].[Region]`: Region of the customers.
#>    - `COUNT(DISTINCT [o].[CustomerID]) AS [CustomerCount]`: Distinct customers.
#>    - `SUM([od].[UnitPrice] * [od].[Quantity]) AS [TotalSales]`: Total sales amount.
#> 
#> 2. **FROM Clause**: Lists involved tables:
#>    - `Customers [c]`, `Orders [o]`, `Order Details [od]`.
#> 
#> 3. **JOIN Clauses**: Joins the tables using customer and order IDs.
#> 
#> 4. **GROUP BY Clause**: Groups by `[c].[Region]`.
#> 
#> 5. **ORDER BY Clause**: Sorts by customer count descending, then total sales descending.
#> 
#> 6. **DBI::dbGetQuery()**: Executes the query and fetches the data.
```

### 3. Research Agent

``` r
library(LLMAgentR)

# Initialize researcher agent
researcher_agent <- build_researcher_agent(
  llm = my_llm_wrapper,
  tavily_search = NULL,
  system_prompt = NULL,
  max_results = 5,
  max_tries = 3,
  backoff = 2,
  verbose = FALSE
)

# Perform research
result <- researcher_agent("Who is Messi?")
```

``` r
cat(result$response)
#> ```markdown
#> **Summary**: Lionel Messi is an Argentine professional footballer widely regarded as one of the greatest players of all time. He plays as a forward for Inter Miami in Major League Soccer and captains the Argentina national team.
#> 
#> **Key Facts**:
#> - Born on June 24, 1987, in Rosario, Argentina.
#> - Has won eight Ballon d'Or awards, more than any other player.
#> - All-time leading goalscorer and most-capped player for Argentina.
#> - Has won 45 team trophies, making him the most decorated player in professional football history.
#> - Known for his exceptional dribbling, playmaking, and goalscoring abilities.
#> 
#> **Context**: Messi's career began at FC Barcelona, where he spent over 20 years and became a global football icon. His playing style and achievements have drawn comparisons to Diego Maradona, and he has been recognized as one of the most influential figures in football history.
#> 
#> **Sources**:
#> - [Lionel Messi - Wikipedia](https://en.wikipedia.org/wiki/Lionel_Messi)
#> - [Career of Lionel Messi - Wikipedia](https://en.wikipedia.org/wiki/Career_of_Lionel_Messi)
#> ```
```

### 4. Interpreter Agent

``` r
library(LLMAgentR)

# Example table or code output
output_text <- "
| Region  | Sales | Profit |
|---------|-------|--------|
| North   |  2000 |   300  |
| South   |  1500 |   250  |
| East    |  1800 |   400  |
| West    |  2200 |   100  |
"

# Build interpreter agent
interpreter_agent <- build_interpreter_agent(
  llm = my_llm_wrapper,
  code_output = output_text,
  max_tries = 3,
  backoff = 2,
  verbose = FALSE
)
```

``` r
cat(interpreter_agent$interpretation)
#> **1. Interpretation**:
#> 
#> The provided output is a data table detailing sales and profits across four different regions: North, South, East, and West.
#> 
#> - **Sales Analysis**: Among the four regions, the West region has the highest sales at 2200 units, followed by the North with 2000 units. The East region has sales of 1800 units, and the South region has the lowest sales at 1500 units. This suggests that the West region is the most successful in terms of sales volume.
#> 
#> - **Profit Analysis**: When examining profit, the East region leads with a profit of 400 units, despite not having the highest sales. The North region follows with a profit of 300 units. The South region has a profit of 250 units, and the West region, despite having the highest sales, has the lowest profit at 100 units. This indicates that the East region is the most profitable and that the West region, although successful in sales, might have higher costs or lower margins affecting its profitability.
#> 
#> - **Relationship between Sales and Profit**: There is no direct correlation between sales volume and profit level in this dataset. For instance, the West region, which has the highest sales, does not have the highest profit, which is held by the East region. This suggests that factors other than sales volume, such as cost structures or pricing strategies, might be affecting profitability across these regions.
#> 
#> **2. Key Takeaways**:
#> 
#> - The West region excels in sales, while the East region is the most profitable, indicating efficiency in converting sales to profit.
#> - Despite high sales, the West region's profitability is low, suggesting potential areas for cost optimization or pricing adjustments.
#> - There is no straightforward relationship between sales and profit, highlighting the need for a balanced approach in managing both sales strategies and cost efficiencies to maximize profitability.
```

### 5. Document Summarizer

``` r
library(LLMAgentR)
library(officer)
library(pdftools)
library(glue)
library(purrr)
library(stringr)
library(dplyr)
library(xml2)
library(rvest)
library(tesseract)

# Build document summarizer agent
summarizer_agent <- build_doc_summarizer_agent(
  llm = my_llm_wrapper,
  summary_template = NULL,
  chunk_size = 4000,
  overlap = 200,
  verbose = FALSE
)

# Summarize document
final_state <- summarizer_agent("https://github.com/knowusuboaky/LLMAgentR/raw/main/tests/testthat/test-data/scrum.docx")
```

``` r
cat(final_state$summary)
#> # Key Findings
#> 
#> - **Scrum Framework**: This worksheet serves as a practical guide to help teams work with the Scrum framework without needing a specific Scrum tool.
#> - **Steps Involved**: The document outlines a step-by-step approach to implementing Scrum, covering roles such as **Product Owner**, **Scrum Master**, and **Developer Team**.
#> - **Course Connection**: This resource is part of the 'Getting Started with Scrum' course aimed at beginners including **Developers**, **Scrum Masters**, and **Product Owners**.
#> - **Practical Tool Selection**: The document emphasizes the use of tools integrated with existing development platforms, such as **Azure DevOps** for teams using Visual Studio.
#> 
#> ## Methodology
#> 
#> - **Structured Steps**:
#>   - Step 1: Select a **Product Owner**
#>   - Step 2: Define **Project Details**
#>   - Step 3: Select a **Scrum Master**
#>   - Step 4: Select the **Developer Team**
#>   - Step 5: Choose the **Tool to Run Sprint**
#>   - Step 6: Prepare the **Sprint Tool**
#>   - Step 7: Create a **Sprint Board**
#>   - Step 8: Develop the **Product Backlog**
#>   - Step 9: Prioritize **Product Backlog Items**
#>   - Step 10: Plan the **First Sprint**
#>   - Step 11: Conduct the **Sprint Planning Meeting**
#>   - Step 12: Estimate the **Sprint Backlog Items**
#>   - Step 13: Implement the **Sprint Backlog Items**
#>   - Step 14: Conduct **Daily Stand-up Meetings**
#>   - Step 15: Hold the **Sprint Review Meeting**
#>   - Step 16: Conduct the **Sprint Retrospective Meeting**
#>   - Step 17: **Repeat the Process** Sprint after Sprint
#> 
#> - **Tool Integration**:
#>   - ```code
#>     Selected Tool: Azure DevOps
#>     Reasons: Product development based on Visual Studio with deep integration with Azure DevOps; use of Azure Repos for source control management; potential for CI/CD extension.
#>     ```
#> 
#> - **Meeting Structures**:
#>   - **Sprint Planning Meeting**: Defined goals and work items
#>   - **Daily Stand-ups**: Address impediments and responsibilities
#>   - **Sprint Review**: Analyze completed tasks and process improvements
#>   - **Retrospective Meetings**: Reflect on successes and areas for improvement
#> 
#> ## Conclusions
#> 
#> - **Comprehensive Guide**: The worksheet is a detailed resource providing a structured approach to implementing Scrum, making it suitable for teams new to the framework.
#> - **Adaptability**: Users are encouraged to adjust the steps and tools to better fit their specific needs and context.
#> - **Effective Start**: By following the steps outlined, teams can effectively start using Scrum to manage their projects, enhancing productivity and collaboration.
#> - **Continuous Improvement**: Emphasizes the importance of iterating and refining the process through regular retrospectives and feedback loops.
```

### 6. Data Cleaning Agent

``` r
library(LLMAgentR)

# 1) Load the data
data <- read.csv("tests/testthat/test-data/churn_data.csv")

# 2) Create the agent
data_cleaner_agent <- build_data_cleaning_agent(
  model = my_llm_wrapper,
  human_validation = FALSE,
  bypass_recommended_steps = FALSE,
  bypass_explain_code = FALSE,
  verbose = FALSE
)

# 3) Define the initial state
initial_state <- list(
  data_raw = data,
  user_instructions = "Don't remove outliers when cleaning the data.",
  max_retries = 3,
  retry_count = 0
)

# 4) Run the agent
final_state <- data_cleaner_agent(initial_state)
```

``` r
# 5) Convert cleaned output to data.frame
cleaned_df <- as.data.frame(final_state$data_cleaned)

# 6) View the cleaned data (first 10 rows)
head(cleaned_df, 10)
#>   customerid gender seniorcitizen partner dependents tenure phoneservice    multiplelines
#> 1  7590-VHVEG Female             0     Yes         No      1           No No phone service
#> 2  5575-GNVDE   Male             0      No         No     34          Yes               No
#> 3  3668-QPYBK   Male             0      No         No      2          Yes               No
#> 4  7795-CFOCW   Male             0      No         No     45           No No phone service
#> 5  9237-HQITU Female             0      No         No      2          Yes               No
#> 6  9305-CDSKC Female             0      No         No      8          Yes              Yes
#> 7  1452-KIOVK   Male             0      No        Yes     22          Yes              Yes
#> 8  6713-OKOMC Female             0      No         No     10           No No phone service
#> 9  7892-POOKP Female             0     Yes         No     28          Yes              Yes
#> 10 6388-TABGU   Male             0      No        Yes     62          Yes               No
#> (…other columns truncated for brevity…)

# 7) View the explanation from the LLM
cat(final_state$messages[[1]]$content)
#> # Data Cleaning Agent:
#> 
#> The `data_cleaner` function performs a series of data cleaning steps:
#> 
#> 1. **Validate Input**: Ensure input is a data.frame or tibble.
#> 2. **Load tidyverse**: Stop if not installed.
#> 3. **Convert to tibble**.
#> 4. **Drop High-NA and Zero-Variance Columns**.
#> 5. **Remove All-NA Rows**.
#> 6. **Impute NAs**: Numeric -> mean, Character -> "Unknown".
#> 7. **Convert 'TotalCharges' to Numeric**.
#> 8. **Rename & Parse**: Lowercase and parse dates.
#> 9. **Deduplicate Rows**.
#> 10. **Remove Remaining NAs**.
#> 11. **Reorder Factor Levels** alphabetically.
#> 
#> Cleaned data is returned as a tibble.

# 8) View the cleaned R function generated by the agent
cat(final_state$data_cleaner_function)
#> ```r
#> data_cleaner <- function(data_raw) {
#>   # 1. Validate input
#>   if (!is.data.frame(data_raw)) stop("`data_raw` must be a data.frame or tibble.")
#> 
#>   # 2. Load tidyverse
#>   if (!requireNamespace("tidyverse", quietly = TRUE)) stop("Package 'tidyverse' is required but not installed.")
#>   suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))
#> 
#>   # 3. Convert to tibble
#>   data_cleaned <- tibble::as_tibble(data_raw)
#> 
#>   # 4. Drop high‐NA & zero‐var columns
#>   data_cleaned <- data_cleaned %>%
#>     dplyr::select(dplyr::where(~ mean(is.na(.)) < 0.4)) %>%
#>     dplyr::select(dplyr::where(~ dplyr::n_distinct(.) > 1))
#> 
#>   # 5. Remove all‐NA rows
#>   data_cleaned <- data_cleaned %>%
#>     dplyr::filter(!dplyr::if_all(dplyr::everything(), ~ is.na(.)))
#> 
#>   # 6. Impute NAs
#>   data_cleaned <- data_cleaned %>%
#>     dplyr::mutate(
#>       dplyr::across(dplyr::where(is.numeric), ~ dplyr::if_else(is.na(.), mean(., na.rm = TRUE), .)),
#>       dplyr::across(dplyr::where(is.character), ~ dplyr::if_else(is.na(.), "Unknown", stringr::str_squish(stringr::str_trim(.))))
#>     )
#> 
#>   # 7. Convert TotalCharges to numeric
#>   if ("TotalCharges" %in% colnames(data_cleaned)) {
#>     data_cleaned <- data_cleaned %>%
#>       dplyr::mutate(TotalCharges = as.numeric(TotalCharges))
#>   }
#> 
#>   # 8. Rename & parse
#>   data_cleaned <- data_cleaned %>%
#>     dplyr::rename_with(~ stringr::str_replace_all(stringr::str_to_lower(.), "[^a-z0-9]+", "_")) %>%
#>     dplyr::mutate(dplyr::across(dplyr::contains("date"), ~ readr::parse_date(., guess_formats(., c("Ymd","mdY","dmY")))))
#> 
#>   # 9. Dedup
#>   data_cleaned <- data_cleaned %>%
#>     dplyr::distinct()
#> 
#>   # 10. Remove rows with any remaining NAs
#>   data_cleaned <- data_cleaned %>%
#>     dplyr::filter(!dplyr::if_any(dplyr::everything(), is.na))
#> 
#>   # 11. Re‐order factor levels
#>   data_cleaned <- data_cleaned %>%
#>     dplyr::mutate(dplyr::across(dplyr::where(is.factor), ~ forcats::fct_relevel(., sort(levels(.)))))
#> 
#>   return(data_cleaned)
#> }
#> ```

# 9) Show the name of the generated function
cat(final_state$data_cleaner_function_name)
#> data_cleaner
```

### 7. Forecasting Agent

``` r
library(LLMAgentR)
library(dplyr)
library(rlang)
library(lubridate)
library(tibble)
library(tidyr)
library(tidymodels)

# 2) Prepare the dataset
my_data <- walmart_sales_weekly

# 3) Create the forecasting agent
forecasting_agent <- build_forecasting_agent(
  model = my_llm_wrapper,
  bypass_recommended_steps = FALSE,
  bypass_explain_code = FALSE,
  mode = "dark", #dark or light
  line_width = 3,
  verbose = FALSE
)

# 4) Define the initial state
initial_state <- list(
  user_instructions = "Forecast sales for the next 30 days, using `id` as the grouping variable, a forecasting horizon of 30, and a confidence level of 90%.",
  data_raw = my_data
)

# 5) Run the agent
final_state <- forecasting_agent(initial_state)
```

------------------------------------------------------------------------

``` r
# 6) View forecast parameters
str(final_state$forecasting_params)
#> List of 5
#>  $ params_value      : chr "Weekly_Sales"
#>  $ params_date       : chr "Date"
#>  $ params_group      : chr "id"
#>  $ params_horizon    : num 30
#>  $ params_conf_level : num 0.9
#>```
#>
#>```r
# 7) View the forecast results (first few rows)
head(final_state$forecasting_data)
#> # A tibble: 6 × 5
#>   id    date        value conf_lo conf_hi
#>   <fct> <date>      <dbl>   <dbl>   <dbl>
#> 1 1_1   2010-02-05 24924.      NA      NA
#> 2 1_1   2010-02-12 46039.      NA      NA
#> 3 1_1   2010-02-19 41596.      NA      NA
#> 4 1_1   2010-02-26 19404.      NA      NA
#> 5 1_1   2010-03-05 21828.      NA      NA
#> 6 1_1   2010-03-12 21043.      NA      NA
#>```
#>
#>```r
# 8) View the forecast summary explanation
cat(final_state$forecasting_report)
#> The provided blueprint outlines a comprehensive approach to forecasting...
#> (blueprint explaining models, recipes, ensemble strategy, grouping, etc.)
#>```
#>
#>```r
# 9) Display the forecast plot
final_state$forecasting_result
#>```
#>
#>```r
# 10) Save forecast plot to HTML
#>htmlwidgets::saveWidget(
#>  widget = final_state$forecasting_result,
#>  file = "forecast_dark_plot.html",
#>  selfcontained = TRUE
#>)
#>```
```

#### Forecast Plot: Light Mode

<!--html_preserve-->
<iframe src="assets/forecast_light_plot.html" style="
  width:100%;
  height:300px;
  /* solid black 2-px border */
  border:2px solid #000;
  /* rounded corners */
  border-radius:4px;
">
</iframe>
<!--/html_preserve-->

#### Forecast Plot: Dark Mode

<!--html_preserve-->
<iframe src="assets/forecast_dark_plot.html" style="
  width:100%;
  height:300px;
  /* solid black 2-px border */
  border:2px solid #000;
  /* rounded corners */
  border-radius:4px;
">
</iframe>
<!--/html_preserve-->

### 8. Data Wrangling Agent

``` r
library(LLMAgentR)

# 1) Simulate multiple data frames with a common ID
df1 <- data.frame(
  ID = c(1, 2, 3, 4),
  Name = c("John", "Jane", "Jim", "Jill"),
  stringsAsFactors = FALSE
)

df2 <- data.frame(
  ID = c(1, 2, 3, 4),
  Age = c(25, 30, 35, 40),
  stringsAsFactors = FALSE
)

df3 <- data.frame(
  ID = c(1, 2, 3, 4),
  Education = c("Bachelors", "Masters", "PhD", "MBA"),
  stringsAsFactors = FALSE
)

# 2) Combine into a list
data <- list(df1, df2, df3)

# 3) Create the agent
data_wrangling_agent <- build_data_wrangling_agent(
  model = my_llm_wrapper,
  human_validation = FALSE,
  bypass_recommended_steps = FALSE,
  bypass_explain_code = FALSE,
  verbose = FALSE
)

# 4) Define the initial state
initial_state <- list(
  data_raw = data,
  user_instructions = "Merge the data frames on the ID column.",
  max_retries = 3,
  retry_count = 0
)

# 5) Run the agent
final_state <- data_wrangling_agent(initial_state)
```

``` r
# 6) View the wrangled data
final_state$data_wrangled
#> A tibble: 4 × 6
#>      ID Name    Age Education Age_Group Name_Length
#>   <dbl> <chr> <dbl> <chr>     <chr>           <int>
#> 1     1 John     25 Bachelors 20-29               4
#> 2     2 Jane     30 Masters   30-39               4
#> 3     3 Jim      35 PhD       30-39               3
#> 4     4 Jill     40 MBA       40+                 4
#> ```
#> 
#> ```r
# 7) Convert to data.frame for printing
wrangler_df <- as.data.frame(final_state$data_wrangled)
print(wrangler_df)
#>   ID Name Age Education Age_Group Name_Length
#> 1  1 John  25 Bachelors     20-29           4
#> 2  2 Jane  30   Masters     30-39           4
#> 3  3  Jim  35       PhD     30-39           3
#> 4  4 Jill  40       MBA       40+           4
#> ```
#> 
#> ```r
#> # 8) View the generated wrangler function
cat(final_state$data_wrangler_function)
#> data_wrangler <- function(data_list) {
#>   # 1. Validate input
#>   if (!(is.data.frame(data_list) || is.list(data_list))) stop("`data_list` must be a data.frame or list of data.frames.")
#> 
#>   # 2. Load required packages
#>   for (pkg in c("dplyr", "tidyr", "purrr", "stringr")) {
#>     if (!requireNamespace(pkg, quietly = TRUE)) stop(sprintf("Package '%s' is required.", pkg))
#>   }
#>   suppressPackageStartupMessages({
#>     library(dplyr); library(tidyr); library(purrr); library(stringr)
#>   })
#> 
#>   # 3. Normalize input to list of tibbles
#>   if (is.data.frame(data_list)) data_list <- list(data_list)
#>   data_list <- purrr::map(data_list, tibble::as_tibble)
#> 
#>   # 4. Merge/join datasets by ID
#>   data_wrangled <- purrr::reduce(data_list, dplyr::left_join, by = "ID")
#> 
#>   # 5. Reorder columns
#>   data_wrangled <- data_wrangled %>% dplyr::select(ID, Name, Age, Education)
#> 
#>   # 6. Ensure correct types
#>   data_wrangled <- data_wrangled %>%
#>     dplyr::mutate(
#>       ID = as.numeric(ID),
#>       Name = as.character(Name),
#>       Age = as.numeric(Age),
#>       Education = as.character(Education)
#>     )
#> 
#>   # 7. Feature engineering
#>   data_wrangled <- data_wrangled %>%
#>     dplyr::mutate(
#>       Age_Group = dplyr::case_when(
#>         Age >= 20 & Age < 30 ~ "20-29",
#>         Age >= 30 & Age < 40 ~ "30-39",
#>         TRUE ~ "40+"
#>       ),
#>       Name_Length = stringr::str_length(Name)
#>     )
#> 
#>   # 8. Check for missing data
#>   if (any(is.na(data_wrangled))) stop("There are missing values in the dataset.")
#> 
#>   # 9. Return final tibble
#>   return(data_wrangled)
#> }
#> ```
#> 
#> ```r
# 9) View the name of the generated function
cat(final_state$data_wrangler_function_name)
#> data_wrangler
#> ```
```

### 9. Weather Agent

``` r
library(LLMAgentR)

# Get weather information
weather_agent <- build_weather_agent(
  llm = my_llm_wrapper,
  location_query = "Tokyo, Japan",
  system_prompt = NULL,
  weather_api_key = NULL,
  units = "metric", #metric or imperial
  n_tries = 3,
  backoff = 2,
  endpoint_url = NULL,
  verbose = FALSE
)
```

``` r
# Print formatted weather response
cat(weather_agent$llm_response)
#> Tokyo, Japan is experiencing scattered clouds with a temperature of 21.1°C. 
#> The humidity is at 49%, and there is a wind speed of 10.8 m/s. 
#> The atmospheric pressure is 1014 hPa.
#> ```
#> 
#> ```r
# Access raw weather data if needed
str(weather_agent$weather_raw)
#> List of 13
#>  $ coord     :List of 2
#>   ..$ lon: num 140
#>   ..$ lat: num 35.7
#>  $ weather   :List of 1
#>   ..$ :List of 4
#>   .. ..$ id         : int 802
#>   .. ..$ main       : chr "Clouds"
#>   .. ..$ description: chr "scattered clouds"
#>   .. ..$ icon       : chr "03d"
#>  $ base      : chr "stations"
#>  $ main      :List of 8
#>   ..$ temp      : num 21.1
#>   ..$ feels_like: num 20.6
#>   ..$ temp_min  : num 19.8
#>   ..$ temp_max  : num 22.6
#>   ..$ pressure  : int 1014
#>   ..$ humidity  : int 49
#>   ..$ sea_level : int 1014
#>   ..$ grnd_level: int 1012
#>  $ visibility: int 10000
#>  $ wind      :List of 2
#>   ..$ speed: num 10.8
#>   ..$ deg  : int 200
#>  $ clouds    :List of 1
#>   ..$ all: int 40
#>  $ dt        : int 1745726159
#>  $ sys       :List of 5
#>   ..$ type   : int 2
#>   ..$ id     : int 268395
#>   ..$ country: chr "JP"
#>   ..$ sunrise: int 1745697236
#>   ..$ sunset : int 1745745817
#>  $ timezone  : int 32400
#>  $ id        : int 1850144
#>  $ name      : chr "Tokyo"
#>  $ cod       : int 200
#> ```
```

### 10. Feature Engineering Agent

``` r
library(LLMAgentR)

# 1) Load the data
data <- read.csv("tests/testthat/test-data/churn_data.csv")

# 2) Create the feature engineering agent
feature_engineering_agent <- build_feature_engineering_agent(
  model = my_llm_wrapper,
  human_validation = FALSE,
  bypass_recommended_steps = FALSE,
  bypass_explain_code = FALSE,
  verbose = TRUE
)

# 3) Define the initial state
initial_state <- list(
  data_raw = data,
  target_variable = "Churn",
  user_instructions = "Inspect the data. Make any new features and transformations that you think will be useful for predicting the target variable. ",
  max_retries = 3,
  retry_count = 0
)

# 4) Run the agent
final_state <- feature_engineering_agent(initial_state)
```

``` r
#>```r
# 5) Convert to data.frame if needed
engineered_df <- as.data.frame(final_state$data_engineered)
#>
# 6) View the result (first 10 rows)
head(engineered_df, 10)
#>    SeniorCitizen tenure MonthlyCharges TotalCharges gender_Male Partner_No Dependents_Yes PhoneService_Yes
#> 1              0      1          29.85        29.85           0          0              0                0
#> 2              0     34          56.95      1889.50           1          1              0                1
#> 3              0      2          53.85       108.15           1          1              0                1
#> 4              0     45          42.30      1840.75           1          1              0                0
#> 5              0      2          70.70       151.65           0          1              0                1
#> 6              0      8          99.65       820.50           0          1              0                1
#> 7              0     22          89.10      1949.40           1          1              1                1
#> 8              0     10          29.75       301.90           0          1              0                0
#> 9              0     28         104.80      3046.05           0          0              0                1
#> 10             0     62          56.15      3487.95           1          1              1                1
#> (…additional columns: MultipleLines, InternetService, OnlineSecurity, etc…)
#>```
#>
#>```r
# 7) View the explanation report from the LLM
cat(final_state$feature_engineering_report)
#> Detailed explanation covering:
#> - ID column removal
#> - Type conversions
#> - Missing value handling
#> - One-hot encoding
#> - Feature interactions (e.g., tenure, MonthlyCharges)
#> - No PCA components included
#> - Final review checks
#>```
#>
#>```r
# 8) View the generated feature engineering function
cat(final_state$feature_engineer_function)
#> clean_names(), safe_select(), safe_dummy_cols(), safe_lump_high_card() helpers
#> feature_engineer() main function that:
#>   - Cleans column names
#>   - Converts categorical columns
#>   - Imputes missing values
#>   - One-hot encodes factors
#>   - Removes ID and constant columns
#>   - Outputs a model-ready dataset
#>```
#>
#>```r
# 9) View the function name
cat(final_state$feature_engineer_function_name)
#> feature_engineer
```

### 11. Visualization Agent

``` r
library(LLMAgentR)

# 1) Load the data
data <- read.csv("tests/testthat/test-data/churn_data.csv")

# 2) Create the visualization agent
visualization_agent <- build_visualization_agent(
  model = my_llm_wrapper,
  human_validation = FALSE,
  bypass_recommended_steps = FALSE,
  bypass_explain_code = FALSE,
  verbose = FALSE
)

# 3) Define the initial state
initial_state <- list(
  data_raw = data,
  target_variable = "Churn",
  user_instructions = "Create a clean and visually appealing box plot to show the distribution of Monthly Charges across Churn categories.
Use distinct colors for each Churn group, add clear axis labels, a legend, and a meaningful title.",
  max_retries = 3,
  retry_count = 0
)

# 4) Run the agent
final_state <- visualization_agent(initial_state)
```

``` r
#> ```r
# 5) View the structure of the final state
str(final_state)
#> List of components including:
#> - visualization_code
#> - visualization_result (the plotly object)
#> - code_explanation
#> - execution_success
#> - timestamp
#> ```
#> 
#> ```r
# 6) View the generated interactive plot (Plotly)
final_state$visualization_result
#> Displays an interactive box plot of MonthlyCharges by Churn
#> ```
#> 
#> ```r
# 7) Save the plot to an HTML file
#> htmlwidgets::saveWidget(
#>   widget = final_state$visualization_result,
#>   file = "monthly_charges_vs_churn.html",
#>   selfcontained = TRUE
#> )
Saves the interactive plot as a standalone HTML file: "monthly_charges_vs_churn.html"
#> ```
```

#### Box Plot

<!--html_preserve-->
<iframe src="assets/monthly_charges_vs_churn.html" style="
  width:100%;
  height:300px;
  /* solid black 2-px border */
  border:2px solid #000;
  /* rounded corners */
  border-radius:4px;
">
</iframe>
<!--/html_preserve-->

## License

MIT © [Kwadwo Daddy Nyame Owusu
Boakye](mailto:kwadwo.owusuboakye@outlook.com)
