###############################################################################
## 1) SIMPLE STATE GRAPH IMPLEMENTATION
###############################################################################

make_node <- function(func, name = NULL) {
  list(func = func, name = name)
}

make_edge <- function(from, to, condition = NULL, label = NULL) {
  list(from = from, to = to, condition = condition, label = label)
}

StateGraph <- function() {
  graph_env <- new.env(parent = emptyenv())
  graph_env$nodes <- list()
  graph_env$edges <- list()
  graph_env$entry_point <- NULL

  graph_env$add_node <- function(name, func) {
    graph_env$nodes[[name]] <- make_node(func, name)
  }

  graph_env$add_edge <- function(from, to) {
    edge <- make_edge(from, to)
    graph_env$edges <- c(graph_env$edges, list(edge))
  }

  graph_env$add_conditional_edges <- function(node_name, condition_fun, mapping_list) {
    for (lbl in names(mapping_list)) {
      e <- make_edge(
        from = node_name,
        to = mapping_list[[lbl]],
        condition = condition_fun,
        label = lbl
      )
      graph_env$edges <- c(graph_env$edges, list(e))
    }
  }

  graph_env$set_entry_point <- function(node_name) {
    graph_env$entry_point <- node_name
  }

  END_NODE_NAME <- "__end__"

  graph_env$compile <- function(checkpointer = NULL) {
    function(state) {
      current_node <- if (!is.null(state$current_node)) {
        state$current_node
      } else {
        graph_env$entry_point
      }

      while (!identical(current_node, END_NODE_NAME)) {
        node_obj <- graph_env$nodes[[current_node]]
        if (is.null(node_obj)) {
          stop(sprintf("Node '%s' not found in graph.", current_node))
        }

        # Execute node
        result <- node_obj$func(state)

        # Merge returned list elements
        if (is.list(result)) {
          for (n in names(result)) {
            state[[n]] <- result[[n]]
          }
        }

        # If there's a Command-like object with goto & update:
        if (!is.null(result$goto)) {
          next_node <- result$goto
          if (is.list(result$update)) {
            for (k in names(result$update)) {
              state[[k]] <- result$update[[k]]
            }
          }

          if (identical(next_node, END_NODE_NAME)) {
            current_node <- END_NODE_NAME
            break
          } else {
            current_node <- next_node
            if (!is.null(checkpointer)) {
              checkpointer(state, current_node)
            }
            next
          }
        }

        # If no direct goto, look for edges
        edges_from_node <- Filter(function(e) e$from == current_node, graph_env$edges)
        if (length(edges_from_node) == 0) {
          current_node <- END_NODE_NAME
          break
        }

        # If exactly 1 edge and no condition
        if (length(edges_from_node) == 1 && is.null(edges_from_node[[1]]$condition)) {
          current_node <- edges_from_node[[1]]$to
          if (identical(current_node, END_NODE_NAME)) break
          if (!is.null(checkpointer)) checkpointer(state, current_node)
          next
        }

        # Otherwise we have conditional edges
        chosen_label <- edges_from_node[[1]]$condition(state)

        edge_matched <- NULL
        for (e in edges_from_node) {
          if (!is.null(e$label) && identical(e$label, chosen_label)) {
            edge_matched <- e
            break
          }
        }
        if (is.null(edge_matched)) {
          stop("No matching edge label found!")
        }

        current_node <- edge_matched$to
        if (identical(current_node, END_NODE_NAME)) break
        if (!is.null(checkpointer)) checkpointer(state, current_node)
      }

      state$current_node <- END_NODE_NAME
      invisible(state)
    }
  }

  list(
    add_node              = graph_env$add_node,
    add_edge              = graph_env$add_edge,
    add_conditional_edges = graph_env$add_conditional_edges,
    set_entry_point       = graph_env$set_entry_point,
    compile               = graph_env$compile,
    END_NODE_NAME         = END_NODE_NAME
  )
}


###############################################################################
## 2) HELPER FUNCTIONS
###############################################################################

interrupt <- function(value) {
  message("\n", value, "\n")
  readline("Enter your response: ")
}

make_command <- function(goto = NULL, update = list()) {
  list(goto = goto, update = update)
}

###############################################################################
## 3) NODE FUNCTIONS
###############################################################################

# A simple summary function for a data frame
get_dataframe_summary <- function(df, n_sample = 30, skip_stats = FALSE) {
  info <- capture.output(str(df))
  summ <- capture.output(summary(df))
  head_txt <- capture.output(head(df, n_sample))
  types <- paste(sapply(df, function(col) paste(class(col), collapse = ", ")), collapse = ", ")

  if (!skip_stats) {
    summary_text <- paste(
      sprintf("Shape: %d rows x %d columns", nrow(df), ncol(df)),
      paste("Column types:", types),
      "Head:",
      paste(head_txt, collapse = "\n"),
      "Summary:",
      paste(summ, collapse = "\n"),
      "Structure:",
      paste(info, collapse = "\n"),
      sep = "\n\n"
    )
  } else {
    summary_text <- paste(
      sprintf("Shape: %d rows x %d columns", nrow(df), ncol(df)),
      paste("Column types:", types),
      "Head:",
      paste(head_txt, collapse = "\n"),
      sep = "\n\n"
    )
  }
  summary_text
}

###############################################################################
## 1) GENERIC GRAPH BUILDER (Equivalent to create_coding_agent_graph in Python)
###############################################################################

create_coding_agent_graph <- function(
    node_functions,
    recommended_steps_node_name,
    create_code_node_name,
    execute_code_node_name,
    fix_code_node_name,
    explain_code_node_name,
    error_key,
    max_retries_key = "max_retries",
    retry_count_key = "retry_count",
    human_validation = FALSE,
    human_review_node_name = "human_review",
    checkpointer = NULL,
    bypass_recommended_steps = FALSE,
    bypass_explain_code = FALSE
) {

  workflow <- StateGraph()

  # Always add create, execute, and fix nodes
  workflow$add_node(create_code_node_name, node_functions[[create_code_node_name]])
  workflow$add_node(execute_code_node_name, node_functions[[execute_code_node_name]])
  workflow$add_node(fix_code_node_name, node_functions[[fix_code_node_name]])

  # Conditionally add the recommended-steps node
  if (!bypass_recommended_steps) {
    workflow$add_node(recommended_steps_node_name, node_functions[[recommended_steps_node_name]])
  }

  # Conditionally add the human review node
  if (human_validation) {
    workflow$add_node(human_review_node_name, node_functions[[human_review_node_name]])
  }

  # Conditionally add the explanation node
  if (!bypass_explain_code) {
    workflow$add_node(explain_code_node_name, node_functions[[explain_code_node_name]])
  }

  # Set the entry point
  entry_point <- if (bypass_recommended_steps) create_code_node_name else recommended_steps_node_name
  workflow$set_entry_point(entry_point)

  if (!bypass_recommended_steps) {
    workflow$add_edge(recommended_steps_node_name, create_code_node_name)
  }

  workflow$add_edge(create_code_node_name, execute_code_node_name)
  workflow$add_edge(fix_code_node_name, execute_code_node_name)

  # Helper to check for error and retry possibility
  error_and_can_retry <- function(s) {
    err <- s[[error_key]]
    retr <- s[[retry_count_key]]
    maxr <- s[[max_retries_key]]
    !is.null(err) && !is.null(retr) && !is.null(maxr) && (retr < maxr)
  }

  if (human_validation) {
    workflow$add_conditional_edges(
      execute_code_node_name,
      function(s) {
        if (error_and_can_retry(s)) "fix_code" else "human_review"
      },
      list(
        human_review = human_review_node_name,
        fix_code = fix_code_node_name
      )
    )
  } else {
    if (!bypass_explain_code) {
      workflow$add_conditional_edges(
        execute_code_node_name,
        function(s) {
          if (error_and_can_retry(s)) "fix_code" else "explain_code"
        },
        list(
          fix_code = fix_code_node_name,
          explain_code = explain_code_node_name
        )
      )
    } else {
      workflow$add_conditional_edges(
        execute_code_node_name,
        function(s) {
          if (error_and_can_retry(s)) "fix_code" else "END"
        },
        list(
          fix_code = fix_code_node_name,
          END = workflow$END_NODE_NAME
        )
      )
    }
  }

  if (!bypass_explain_code) {
    workflow$add_edge(explain_code_node_name, workflow$END_NODE_NAME)
  }

  # Compile the workflow
  if (human_validation && !is.null(checkpointer)) {
    app <- workflow$compile(checkpointer = checkpointer)
  } else {
    app <- workflow$compile()
  }
  app
}

###############################################################################
## 2) NODE FUNCTIONS FOR THE DATA CLEANING AGENT
###############################################################################

node_recommend_cleaning_steps <- function(model) {
  function(state) {
    if (state$verbose) message("---DATA CLEANING AGENT----\n")
    if (state$verbose) message("    * RECOMMEND CLEANING STEPS\n")

    # Retrieve user instructions and any previously recommended steps
    user_instructions      <- if (!is.null(state$user_instructions)) state$user_instructions else ""
    recommended_steps_prev <- if (!is.null(state$recommended_steps))    state$recommended_steps    else ""

    # Handle data inputs
    if (is.data.frame(state$data_raw)) {
      df <- state$data_raw
    } else if (is.list(state$data_raw)) {
      df <- as.data.frame(state$data_raw)
    } else {
      stop("Invalid data format - must be dataframe or list")
    }

    # Convert the raw data into a data frame
    n_rows <- nrow(df)
    n_cols <- ncol(df)

    # Overall dataset counts
    dataset_info <- sprintf(
      "Dataset dimensions:\n* Rows: %d\n* Columns: %d\n\n",
      n_rows, n_cols
    )

    #  Build a super-detailed column-by-column summary
    col_summaries <- lapply(names(df), function(col) {
      vec           <- df[[col]]
      type          <- class(vec)[1]
      n_missing     <- sum(is.na(vec))
      pct_missing   <- round(100 * n_missing / n_rows, 2)
      distinct_vals <- unique(vec)
      n_distinct    <- length(distinct_vals)
      pct_unique    <- round(100 * n_distinct / n_rows, 2)

      base_info <- paste0(
        "* **", col, "** (", type, ")\n",
        "  - Missing: ", n_missing, " (", pct_missing, "%)\n",
        "  - Distinct: ", n_distinct, " (", pct_unique, "%)\n"
      )

      #  Numeric columns
      if (is.numeric(vec)) {
        qs      <- quantile(vec, probs = c(0, .25, .5, .75, 1), na.rm = TRUE)
        mn      <- mean(vec, na.rm = TRUE)
        sdv     <- sd(vec, na.rm = TRUE)
        iqr     <- IQR(vec, na.rm = TRUE)
        med_val <- qs[3]

        # zero/negative
        n_zero   <- sum(vec == 0, na.rm = TRUE)
        pct_zero <- round(100 * n_zero / n_rows, 2)
        n_neg    <- sum(vec < 0, na.rm = TRUE)
        pct_neg  <- round(100 * n_neg / n_rows, 2)

        # extreme outliers (3 X IQR around median)
        out_thresh  <- 3 * iqr
        n_extreme   <- sum((vec < (med_val - out_thresh) | vec > (med_val + out_thresh)), na.rm = TRUE)
        pct_extreme <- round(100 * n_extreme / n_rows, 2)

        # skewness & excess kurtosis
        skewness <- if (sdv > 0) mean((vec - mn)^3, na.rm = TRUE) / sdv^3 else NA
        kurtosis <- if (sdv > 0) mean((vec - mn)^4, na.rm = TRUE) / sdv^4 - 3 else NA

        num_info <- paste0(
          "  - Min/1st Qu./Median/Mean/3rd Qu./Max:\n",
          sprintf("    %.3f / %.3f / %.3f / %.3f / %.3f / %.3f\n",
                  qs[1], qs[2], med_val, mn, qs[4], qs[5]),
          "  - SD: ", round(sdv, 3), "  IQR: ", round(iqr, 3), "\n",
          "  - Zeros: ", n_zero, " (", pct_zero, "%)  Negatives: ", n_neg, " (", pct_neg, "%)\n",
          "  - Extreme (3 X IQR): ", n_extreme, " (", pct_extreme, "%)\n",
          "  - Skewness: ", round(skewness, 3), "  Excess Kurtosis: ", round(kurtosis, 3), "\n"
        )

        return(paste0(base_info, num_info))
      }

      #  Categorical (factor/character)
      if (is.factor(vec) || is.character(vec)) {
        tab_no_na <- table(vec, useNA = "no")
        freqs     <- prop.table(tab_no_na)
        entropy   <- -sum(freqs * log2(freqs), na.rm = TRUE)

        tab_sorted <- sort(tab_no_na, decreasing = TRUE)
        top_n      <- head(tab_sorted, 3)
        top_info   <- paste(
          sprintf("    %s: %d (%.1f%%)", names(top_n), as.integer(top_n), 100 * as.integer(top_n) / n_rows),
          collapse = "\n"
        )

        cat_info <- paste0(
          "  - Top levels:\n", top_info, "\n",
          "  - Entropy: ", round(entropy, 3), " bits\n"
        )
        return(paste0(base_info, cat_info))
      }

      #  Logical
      if (is.logical(vec)) {
        t_ct      <- sum(vec, na.rm = TRUE)
        f_ct      <- sum(!vec, na.rm = TRUE)
        pct_true  <- round(100 * t_ct / n_rows, 2)
        pct_false <- round(100 * f_ct / n_rows, 2)
        log_info  <- paste0(
          "  - TRUE: ", t_ct, " (", pct_true, "%)  FALSE: ", f_ct, " (", pct_false, "%)  NA: ", n_missing, "\n"
        )
        return(paste0(base_info, log_info))
      }

      #  Fallback for other types
      sample_vals <- if (n_distinct > 10) {
        paste(head(distinct_vals, 10), collapse = ", ")
      } else {
        paste(distinct_vals, collapse = ", ")
      }
      fallback_info <- paste0(
        "  - Sample values: ", sample_vals,
        if (n_distinct > 10) ", ..." else "", "\n"
      )
      paste0(base_info, fallback_info)
    })

    all_datasets_summary_str <- paste0(
      dataset_info,
      "These are the columns and their detailed statistics:\n\n",
      paste(col_summaries, collapse = "\n")
    )
    #

    # Prompt remains unchanged
    prompt <- sprintf(
      "You are a Data Cleaning Expert. Given the following information about the data,
recommend a series of numbered steps to take to clean and preprocess it.
The steps should be tailored to the data characteristics and should be helpful
for a data cleaning agent that will be implemented.

General Steps:
Things that should be considered in the data cleaning steps:

* Removing columns if more than 40 percent of the data is missing
* Imputing missing values with the mean of the column if the column is numeric
* Imputing missing values with the mode of the column if the column is categorical
* Converting columns to the correct data type
* Removing duplicate rows
* Removing rows with missing values
* Removing rows with extreme outliers (3X the interquartile range)

Custom Steps:
* Analyze the data to determine if any additional data cleaning steps are needed.
* Recommend steps that are specific to the data provided. Include why these steps are necessary or beneficial.
* If no additional steps are needed, simply state that no additional steps are required.

IMPORTANT:
Make sure to take into account any additional user instructions that may add, remove, or modify some of these steps.
Include comments in your reasoning if something is not done because a user requested or is done because a user requested.

User instructions:
%s

Previously Recommended Steps (if any):
%s

Below are summaries of all datasets provided:
%s

Return the steps as a bullet point list (no code, just the steps).

Avoid these:
1. Do not include steps to save files.",
      user_instructions, recommended_steps_prev, all_datasets_summary_str
    )

    # Invoke the model
    steps <- model(prompt)

    list(
      recommended_steps    = paste0("\n\n# Recommended Data Cleaning Steps:\n", trimws(steps)),
      all_datasets_summary = all_datasets_summary_str
    )
  }
}


node_create_data_cleaner_code <- function(model,
                                          bypass_recommended_steps = FALSE) {
  function(state) {
    # -----------------------------------------------------------------
    # Initial Agent Notification and State Retrieval
    # -----------------------------------------------------------------
    if (bypass_recommended_steps && state$verbose) {
      message("---DATA CLEANING AGENT----\n")
    }
    if (state$verbose) message("    * CREATE DATA CLEANER CODE\n")

    # Retrieve recommended steps and dataset summary from the state.
    user_instructions <- if (!is.null(state$user_instructions)) state$user_instructions else ""
    recommended_steps <- if (!is.null(state$recommended_steps)) state$recommended_steps else ""
    all_datasets_summary <- if (!is.null(state$all_datasets_summary)) state$all_datasets_summary else "No dataset summary available."

    # -----------------------------------------------------------------
    # Construct the Prompt with the New Instructions
    # -----------------------------------------------------------------
    # -----------------------------------------------------------------
    # Prompt: tidyverse-only Data Cleaner (all-in-one, production-ready)
    # -----------------------------------------------------------------
    prompt <- paste0(
      "You are a Data Cleaning Agent specialized in preparing and cleaning data for analysis using tidyverse only in R. ",
      "Your task is to generate a complete and valid R function named `data_cleaner` that takes a dataframe (named `data_raw`) ",
      "as its sole input and returns a cleaned version of that dataframe.\n\n",

      "Please follow these guidelines and answer based on information from Step 1 - Step 3:\n\n",

      "1. **User Instructions for Data Cleaning:**\n",
      user_instructions, "\n\n",

      "2. **Recommended Steps for Data Cleaning:**\n",
      recommended_steps, "\n\n",

      "3. **Dataset Overview:**\n",
      all_datasets_summary, "\n\n",

      "4. **Package Requirements (tidyverse-only):**\n",
      "   - Load **tidyverse** with suppression:\n",
      "     ```r\n",
      "     if (!requireNamespace(\"tidyverse\", quietly = TRUE)) stop(\"Package 'tidyverse' is required but not installed.\")\n",
      "     suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))\n",
      "     ```\n",
      "   - Allowed sub-packages:\n",
      "     - Data import & export: readr, readxl, haven, DBI/dbplyr\n",
      "     - Data wrangling & tidying: dplyr, tidyr\n",
      "     - Visualization: ggplot2\n",
      "     - Functional programming & iteration: purrr\n",
      "     - Strings & factors: stringr, forcats\n",
      "     - Tibbles & I/O helpers: tibble, glue\n\n",

      "5. **Namespace Requirement:**\n",
      "   - Always prefix functions with their namespace, e.g. `dplyr::filter()`, `stringr::str_trim()`, `purrr::map()`, etc.\n\n",

      "6. **Function Requirements (all-in-one cleaning):**\n",
      "   - **Input validation:** if not a data.frame/tibble, stop with an error.\n",
      "   - **Ensure tibble:** use `tibble::as_tibble(data_raw)`.\n",
      "   - **Drop high-NA columns:** `dplyr::select(dplyr::where(~ mean(is.na(.)) < 0.5))`.\n",
      "   - **Drop zero-variance columns:** `dplyr::select(dplyr::where(~ dplyr::n_distinct(.) > 1))`.\n",
      "   - **Remove rows all-NA:** `dplyr::filter(!dplyr::if_all(dplyr::everything(), ~ is.na(.)))`.\n",
      "   - **Impute numeric NAs:** `dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ dplyr::if_else(is.na(.), stats::median(., na.rm = TRUE), .)))`.\n",
      "   - **Impute categorical/character NAs:**\n",
      "       - For factors: `forcats::fct_explicit_na(<col>, na_level = \"Unknown\")`.\n",
      "       - For characters: `dplyr::if_else(is.na(.), \"Unknown\", .)` inside `across(where(is.character), ...)`.\n",
      "   - **Trim & squish text:** `stringr::str_squish(stringr::str_trim(.))` on all character columns.\n",
      "   - **Standardize column names to snake_case:**\n",
      "       `dplyr::rename_with(~ stringr::str_replace_all(stringr::str_to_lower(.), \"[^a-z0-9]+\", \"_\"))`.\n",
      "   - **Parse date columns:** `dplyr::across(dplyr::contains(\"date\"), ~ readr::parse_date(., guess_formats(., c(\"Ymd\",\"mdY\",\"dmY\"))))`.\n",
      "   - **Remove duplicates:** `dplyr::distinct()`.\n",
      "   - **Filter outliers (1.5 * IQR rule):**\n",
      "       `dplyr::filter(dplyr::if_all(dplyr::where(is.numeric), ~ dplyr::between(., stats::quantile(., .25, na.rm = TRUE) - 1.5 * IQR(., na.rm = TRUE), stats::quantile(., .75, na.rm = TRUE) + 1.5 * IQR(., na.rm = TRUE))))`.\n",
      "   - **Winsorize extremes (+3 SD) or (-3 SD):**\n",
      "       `dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ { mu <- stats::mean(., na.rm = TRUE); sd <- stats::sd(., na.rm = TRUE); dplyr::if_else(. < mu - 3*sd, mu - 3*sd, dplyr::if_else(. > mu + 3*sd, mu + 3*sd, .)) }))`.\n",
      "   - **Re-encode factor levels alphabetically:**\n",
      "       `dplyr::mutate(dplyr::across(dplyr::where(is.factor), ~ forcats::fct_relevel(., sort(levels(.)))))`.\n\n",

      "7. **Format:** Wrap the entire function in triple backticks tagged with `r`.\n\n",


      "8. **Additional Flexibility & Quality:**\n",
      "   - Feel free to include any extra transformation steps, optimizations, robust error handling, and ensure the generated code is syntactically correct and error-free for production readiness.\n\n",

      "9. **Expected Output Example:**\n",
      "```r\n",
      "data_cleaner <- function(data_raw) {\n",
      "  # 1. Validate input\n",
      "  if (!is.data.frame(data_raw)) stop(\"`data_raw` must be a data.frame or tibble.\")\n\n",
      "  # 2. Load tidyverse\n",
      "  if (!requireNamespace(\"tidyverse\", quietly = TRUE)) stop(\"Package 'tidyverse' is required but not installed.\")\n",
      "  suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))\n\n",
      "  # 3. Convert to tibble\n",
      "  data_cleaned <- tibble::as_tibble(data_raw)\n\n",
      "  # 4. Drop high-NA & zero-var columns\n",
      "  data_cleaned <- data_cleaned %>%\n",
      "    dplyr::select(dplyr::where(~ mean(is.na(.)) < 0.5)) %>%\n",
      "    dplyr::select(dplyr::where(~ dplyr::n_distinct(.) > 1))\n\n",
      "  # 5. Remove all-NA rows\n",
      "  data_cleaned <- data_cleaned %>%\n",
      "    dplyr::filter(!dplyr::if_all(dplyr::everything(), ~ is.na(.)))\n\n",
      "  # 6. Impute NAs & text cleanup\n",
      "  data_cleaned <- data_cleaned %>%\n",
      "    dplyr::mutate(\n",
      "      dplyr::across(dplyr::where(is.numeric), ~ dplyr::if_else(is.na(.), stats::median(., na.rm = TRUE), .)),\n",
      "      dplyr::across(dplyr::where(is.character), ~ dplyr::if_else(is.na(.), \"Unknown\", stringr::str_squish(stringr::str_trim(.))))\n",
      "    )\n\n",
      "  # 7. Rename & parse\n",
      "  data_cleaned <- data_cleaned %>%\n",
      "    dplyr::rename_with(~ stringr::str_replace_all(stringr::str_to_lower(.), \"[^a-z0-9]+\", \"_\")) %>%\n",
      "    dplyr::mutate(dplyr::across(dplyr::contains(\"date\"), ~ readr::parse_date(., guess_formats(., c(\"Ymd\",\"mdY\",\"dmY\")))))\n\n",
      "  # 8. Dedup, outlier & winsorize\n",
      "  data_cleaned <- data_cleaned %>%\n",
      "    dplyr::distinct() %>%\n",
      "    dplyr::filter(dplyr::if_all(dplyr::where(is.numeric), ~ dplyr::between(., stats::quantile(., .25, na.rm = TRUE) - 1.5*IQR(., na.rm = TRUE), stats::quantile(., .75, na.rm = TRUE) + 1.5*IQR(., na.rm = TRUE)))) %>%\n",
      "    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ { mu <- stats::mean(., na.rm = TRUE); sd <- stats::sd(., na.rm = TRUE); dplyr::if_else(. < mu - 3*sd, mu - 3*sd, dplyr::if_else(. > mu + 3*sd, mu + 3*sd, .)) }))\n\n",
      "  # 9. Re-order factor levels\n",
      "  data_cleaned <- data_cleaned %>%\n",
      "    dplyr::mutate(dplyr::across(dplyr::where(is.factor), ~ forcats::fct_relevel(., sort(levels(.)))))\n\n",
      "  return(data_cleaned)\n",
      "}\n",
      "```", "\n\n",

      "Your output should consist solely of the R function code wrapped in triple backticks as shown in the example.\n",
      "Please generate the function accordingly."
    )

    # -----------------------------------------------------------------
    # Generate the Code Using the Provided Model
    # -----------------------------------------------------------------
    code_raw <- model(prompt)

    # -----------------------------------------------------------------
    # Extract the R Code Enclosed in Triple Backticks Tagged with 'r'
    # -----------------------------------------------------------------
    regex_pattern <- "```r(.*?)```"
    match_result <- regexpr(regex_pattern, code_raw, perl = TRUE)
    if (match_result[1] != -1) {
      # Extract and clean the code captured between the backticks.
      captured <- regmatches(code_raw, match_result)
      code_extracted <- gsub("```r|```", "", captured)
      code_extracted <- trimws(code_extracted)
    } else {
      # Fallback to raw code if the expected pattern isn't found.
      code_extracted <- trimws(code_raw)
    }

    # Remove any stray triple backticks.
    code_extracted <- gsub("```+", "", code_extracted)

    # -----------------------------------------------------------------
    # Return the Generated Code as a List
    # -----------------------------------------------------------------
    list(
      data_cleaner_function = code_extracted,
      data_cleaner_function_path = NULL,
      data_cleaner_function_name = "data_cleaner"
    )
  }
}



node_func_human_review <- function(
    prompt_text,
    yes_goto,
    no_goto,
    user_instructions_key = "user_instructions",
    recommended_steps_key = "recommended_steps") {
  function(state) {
    if (state$verbose) message("    * HUMAN REVIEW\n")
    steps <- if (!is.null(state[[recommended_steps_key]])) state[[recommended_steps_key]] else ""
    prompt_filled <- sprintf(prompt_text, steps)
    user_input <- interrupt(prompt_filled)
    if (tolower(trimws(user_input)) == "yes") {
      return(list(goto = yes_goto, update = list()))
    } else {
      modifications <- paste("Modifications:", user_input, sep = "\n")
      old_val <- state[[user_instructions_key]]
      if (is.null(old_val)) old_val <- ""
      new_val <- paste(old_val, modifications, sep = "\n")
      return(list(goto = no_goto, update = list(user_instructions = new_val)))
    }
  }
}


node_execute_data_cleaner_code <- function(state) {
  if (state$verbose) message("    * EXECUTING DATA CLEANER CODE\n")


  # 1. Package Management
  required_packages <- c("dplyr", "tidyr", "stringr", "lubridate", "magrittr")
  invisible(lapply(required_packages, get_suggested))


  # 2. Code Extraction -------------------------------------------------------
  extract_r_code_block <- function(text) {
    if (is.null(text)) {
      warning("No code provided to extract")
      return(NULL)
    }

    # Pattern 1: Triple backtick code blocks
    pattern_r <- "(?s)```(?:r)?\\s*(.*?)\\s*```"
    matches_r <- regmatches(text, regexec(pattern_r, text, perl = TRUE))
    if (length(matches_r) > 0 && length(matches_r[[1]]) >= 2 && nzchar(matches_r[[1]][2])) {
      return(trimws(matches_r[[1]][2]))
    }

    # Pattern 2: Direct function definition
    pattern_func <- "(?s)(data_cleaner\\s*<-\\s*function\\s*\\([^\\)]*\\)\\s*\\{.*\\})"
    matches_func <- regmatches(text, regexec(pattern_func, text, perl = TRUE))
    if (length(matches_func) > 0 && length(matches_func[[1]]) >= 2 && nzchar(matches_func[[1]][2])) {
      return(trimws(matches_func[[1]][2]))
    }

    warning("Could not extract code block from text. Using raw text as code.")
    return(trimws(text))
  }

  # 3. Input Validation ------------------------------------------------------
  if (is.null(state$data_cleaner_function)) {
    stop("State is missing data_cleaner_function")
  }

  if (is.null(state$data_raw)) {
    stop("State is missing raw data")
  }

  code_snippet <- extract_r_code_block(state$data_cleaner_function)

  if (is.null(code_snippet) || nchar(code_snippet) == 0) {
    stop("No R code could be extracted from the data cleaner function")
  }

  if (!grepl("data_cleaner\\s*<-\\s*function", code_snippet)) {
    stop("No valid 'data_cleaner' function detected in the extracted code.")
  }

  # 4. Execution Environment -------------------------------------------------
  local_env <- new.env(parent = baseenv())

  # Load all functions from required packages
  suppressPackageStartupMessages({
    # Core tidyverse functions
    local_env$`%>%` <- magrittr::`%>%`

    # dplyr functions
    dplyr_exports <- getNamespaceExports("dplyr")
    for (f in dplyr_exports) {
      local_env[[f]] <- get(f, envir = getNamespace("dplyr"))
    }

    # tidyr functions
    tidyr_exports <- getNamespaceExports("tidyr")
    for (f in tidyr_exports) {
      local_env[[f]] <- get(f, envir = getNamespace("tidyr"))
    }

    # stringr functions
    stringr_exports <- getNamespaceExports("stringr")
    for (f in stringr_exports) {
      local_env[[f]] <- get(f, envir = getNamespace("stringr"))
    }

    # lubridate functions
    lubridate_exports <- getNamespaceExports("lubridate")
    for (f in lubridate_exports) {
      local_env[[f]] <- get(f, envir = getNamespace("lubridate"))
    }
  })

  # Add essential base R functions
  base_funs <- c("c", "list", "data.frame", "as.data.frame", "names",
                 "colnames", "rownames", "grep", "grepl", "sub", "gsub")
  for (f in base_funs) {
    local_env[[f]] <- get(f, envir = baseenv())
  }

  # 5. Execution ------------------------------------------------------------
  agent_error <- NULL
  result <- NULL

  tryCatch({
    # Parse and evaluate the code
    parsed_code <- parse(text = code_snippet)
    eval(parsed_code, envir = local_env)

    # Verify the function exists
    if (!exists("data_cleaner", envir = local_env) ||
        !is.function(local_env$data_cleaner)) {
      stop("'data_cleaner' function not found or invalid")
    }

    # Prepare input data
    df <- if (!is.data.frame(state$data_raw)) {
      tryCatch(
        as.data.frame(state$data_raw),
        error = function(e) stop("Failed to convert input data to data.frame")
      )
    } else {
      state$data_raw
    }

    # Execute the cleaner function
    res <- local_env$data_cleaner(df)

    # Standardize output format
    result <- if (is.data.frame(res)) {
      as.list(res)
    } else if (is.list(res)) {
      res
    } else {
      list(cleaned_data = res)
    }


  }, error = function(e) {
    agent_error <<- paste("Data cleaning failed:", e$message)
    if (state$verbose) message("ERROR:", agent_error, "\n")
    if (state$verbose) message("Failed code snippet:\n", code_snippet, "\n")
  })

  # 6. Return Results -------------------------------------------------------
  list(
    data_cleaned = result,
    data_cleaner_error = agent_error,
    execution_success = is.null(agent_error),
    timestamp = Sys.time()
  )
}

node_fix_data_cleaner_code <- function(model) {
  function(state) {
    if (state$verbose) message("    * FIX DATA CLEANER CODE\n")
    if (state$verbose) message("      retry_count:", state$retry_count, "\n")

    code_snippet <- state$data_cleaner_function
    error_message <- state$data_cleaner_error
    function_name <- "data_cleaner"

    data_cleaner_prompt <- sprintf(
      "You are a Data Cleaning Agent. Your job is to create a %s() function that can be run on the data provided. The function is currently broken and needs to be fixed.\n\nMake sure to only return the function definition for %s().\n\nReturn R code in ```r``` format with a single function definition, %s(data_raw), that includes all necessary logic inside the function.\n\nThis is the broken code:\n%s\n\nLast Known Error:\n%s",
      function_name, function_name, function_name, code_snippet, error_message
    )

    response <- model(data_cleaner_prompt)

    regex_pattern <- "```r(.*#)```"
    match_result <- regexpr(regex_pattern, response, perl = TRUE)

    if (match_result[1] != -1) {
      extracted <- regmatches(response, match_result)
      new_code <- gsub("```r|```", "", extracted)
      new_code <- trimws(new_code)
    } else {
      warning("Could not extract valid R code from LLM response. Returning original snippet.")
      new_code <- code_snippet
    }

    new_retry_val <- state$retry_count + 1
    list(
      data_cleaner_function = new_code,
      data_cleaner_error = NULL,
      retry_count = new_retry_val
    )
  }
}

node_explain_data_cleaner_code <- function(
    model,
    success_prefix = "# Data Cleaning Agent:\n\n",
    error_message = "The Data Cleaning Agent encountered an error during data cleaning. Data could not be explained.") {
  function(state) {
    if (state$verbose) message("    * EXPLAIN DATA CLEANER CODE\n")
    agent_error <- state$data_cleaner_error
    if (!is.null(agent_error)) {
      msg <- list(list(content = error_message, role = "ERROR"))
      return(list(messages = msg))
    }
    code_snippet <- state$data_cleaner_function
    prompt <- sprintf("Explain the data cleaning steps performed by the following function succinctly:\n\n%s", code_snippet)
    explanation <- model(prompt)
    message_content <- paste0(success_prefix, explanation)
    msg <- list(list(content = message_content, role = "DataCleaningAgent"))
    list(messages = msg)
  }
}

###############################################################################
## 3) MAIN DATA CLEANING AGENT BUILDER FUNCTION
###############################################################################
# ------------------------------------------------------------------------------
#' Build a Data Cleaning Agent
#'
#' Constructs a multi-step agent workflow to recommend, generate, fix, execute,
#' and explain robust R code for data cleaning tasks using LLMs and user-defined data.
#'
#' @name build_data_cleaning_agent
#' @param model A function that accepts a prompt and returns a text response (e.g., OpenAI, Claude).
#' @param data_raw A raw data.frame (or list convertible to data.frame) to be cleaned.
#' @param human_validation Logical; whether to include a manual review step.
#' @param bypass_recommended_steps Logical; whether to skip LLM-based cleaning step suggestions.
#' @param bypass_explain_code Logical; whether to skip explanation of the generated code.
#' @param verbose Logical; whether to print progress messages (default: TRUE)
#'
#' @return A compiled graph-based cleaning agent function that accepts and mutates a state list.
#' @examples
#' \dontrun{
#' # 1) Load the data
#' data <- read.csv("tests/testthat/test-data/churn_data.csv")
#'
#' # 2) Create the agent
#' data_cleaner_agent <- build_data_cleaning_agent(
#'   model = my_llm_wrapper,
#'   human_validation = FALSE,
#'   bypass_recommended_steps = FALSE,
#'   bypass_explain_code = FALSE,
#'   verbose = FALSE
#' )
#'
#' # 3) Define the initial state
#' initial_state <- list(
#'   data_raw = data,
#'   user_instructions = "Don't remove outliers when cleaning the data.",
#'   max_retries = 3,
#'   retry_count = 0
#' )
#'
#' # 4) Run the agent
#' final_state <- data_cleaner_agent(initial_state)
#' }
#' @export
NULL

build_data_cleaning_agent <- function(model,
                                      data_raw,
                                      human_validation = FALSE,
                                      bypass_recommended_steps = FALSE,
                                      bypass_explain_code = FALSE,
                                      verbose = TRUE) {

  # Define node functions list
  node_functions <- list(
    recommend_cleaning_steps = node_recommend_cleaning_steps(model),
    human_review = node_func_human_review(
      prompt_text = "Are the following data cleaning instructions correct# (Answer 'yes' or provide modifications)\n%s",
      yes_goto = if (!bypass_explain_code) "explain_data_cleaner_code" else "__end__",
      no_goto = "recommend_cleaning_steps"
    ),
    create_data_cleaner_code = node_create_data_cleaner_code(model, bypass_recommended_steps = bypass_recommended_steps),
    execute_data_cleaner_code = node_execute_data_cleaner_code,
    fix_data_cleaner_code = node_fix_data_cleaner_code(model),
    explain_data_cleaner_code = node_explain_data_cleaner_code(model)
  )


  app <- create_coding_agent_graph(
    node_functions = node_functions,
    recommended_steps_node_name = "recommend_cleaning_steps",
    create_code_node_name = "create_data_cleaner_code",
    execute_code_node_name = "execute_data_cleaner_code",
    fix_code_node_name = "fix_data_cleaner_code",
    explain_code_node_name = "explain_data_cleaner_code",
    error_key = "data_cleaner_error",
    max_retries_key = "max_retries",
    retry_count_key = "retry_count",
    human_validation = human_validation,
    human_review_node_name = "human_review",
    checkpointer = NULL,
    bypass_recommended_steps = bypass_recommended_steps,
    bypass_explain_code = bypass_explain_code
  )

  function(state) {
    state$verbose <- if (!is.null(state$verbose)) state$verbose else verbose
    app(state)
  }
}
