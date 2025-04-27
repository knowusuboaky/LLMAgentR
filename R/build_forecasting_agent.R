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
    function(state, verbose = FALSE) {
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

        # Execute node with verbose parameter if the function accepts it
        if ("verbose" %in% names(formals(node_obj$func))) {
          result <- node_obj$func(state, verbose = verbose)
        } else {
          result <- node_obj$func(state)
        }

        # Merge returned list elements
        if (is.list(result)) {
          for (n in names(result)) {
            state[[n]] <- result[[n]]
          }
        }

        # Handle Command-like objects with goto & update
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

        # Look for edges if no direct goto
        edges_from_node <- Filter(function(e) e$from == current_node, graph_env$edges)
        if (length(edges_from_node) == 0) {
          current_node <- END_NODE_NAME
          break
        }

        # Handle single unconditional edge
        if (length(edges_from_node) == 1 && is.null(edges_from_node[[1]]$condition)) {
          current_node <- edges_from_node[[1]]$to
          if (identical(current_node, END_NODE_NAME)) break
          if (!is.null(checkpointer)) checkpointer(state, current_node)
          next
        }

        # Handle conditional edges
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
## PLOTTING FUNCTION
###############################################################################

plot_forecast <- function(data, mode = "light", line_width = 2) {

  # Check for required packages (plotly, ggplot2)
  get_suggested("plotly")
  get_suggested("ggplot2")

  # Convert input to data frame if needed
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  # Check for 'id_col' in the data frame
  id_col <- if ("item_id" %in% names(data)) "item_id" else "id"
  if (!id_col %in% names(data)) {
    data[[id_col]] <- seq_len(nrow(data))
  }

  # Define columns
  date_col <- "date"
  value_col <- "value"
  conf_lo_col <- "conf_lo"
  conf_hi_col <- "conf_hi"

  # Define colors
  if (mode == "dark") {
    background_color <- "black"
    text_color <- "white"
    fill_color <- "rgba(255, 165, 0, 0.3)"
    line_color <- "orange"
    actual_color <- "gray"
  } else {
    background_color <- "white"
    text_color <- "black"
    fill_color <- "rgba(173, 216, 230, 0.7)"
    line_color <- "blue"
    actual_color <- "black"
  }

  # Create base plot
  p <- plotly::plot_ly()

  unique_ids <- unique(data[[id_col]])

  for (i in seq_along(unique_ids)) {
    item_id <- unique_ids[i]
    group <- data[data[[id_col]] == item_id, ]

    # Infer actual vs forecast based on NA in confidence intervals
    actual_data <- group[is.na(group[[conf_lo_col]]) & is.na(group[[conf_hi_col]]), ]
    forecast_data <- group[!is.na(group[[conf_lo_col]]) | !is.na(group[[conf_hi_col]]), ]

    # Add actual line
    p <- p %>% plotly::add_trace(
      data = actual_data,
      x = ~get(date_col),
      y = ~get(value_col),
      type = 'scatter',
      mode = 'lines',
      name = paste('Actual -', item_id),
      line = list(color = actual_color, width = line_width),
      visible = i == 1
    )

    # Add forecast line
    p <- p %>% plotly::add_trace(
      data = forecast_data,
      x = ~get(date_col),
      y = ~get(value_col),
      type = 'scatter',
      mode = 'lines',
      name = paste('Forecast -', item_id),
      line = list(color = line_color, width = line_width),
      visible = i == 1
    )

    # Add upper CI
    p <- p %>% plotly::add_trace(
      data = forecast_data,
      x = ~get(date_col),
      y = ~get(conf_hi_col),
      type = 'scatter',
      mode = 'lines',
      name = paste('Conf_High -', item_id),
      line = list(width = 0),
      showlegend = FALSE,
      visible = i == 1
    )

    # Add lower CI
    p <- p %>% plotly::add_trace(
      data = forecast_data,
      x = ~get(date_col),
      y = ~get(conf_lo_col),
      type = 'scatter',
      mode = 'lines',
      name = paste('Conf_Low -', item_id),
      fill = 'tonexty',
      fillcolor = fill_color,
      line = list(width = 0),
      showlegend = FALSE,
      visible = i == 1
    )
  }

  # Dropdown (4 traces per ID)
  dropdown_buttons <- lapply(seq_along(unique_ids), function(i) {
    vis <- rep(FALSE, length(unique_ids) * 4)
    vis[((i - 1) * 4 + 1):((i - 1) * 4 + 4)] <- TRUE
    list(
      label = as.character(unique_ids[i]),
      method = "update",
      args = list(
        list(visible = vis),
        list(title = paste("Forecast -", unique_ids[i]))
      )
    )
  })

  # Layout
  p %>% plotly::layout(
    title = paste("Forecast -", unique_ids[1]),
    xaxis = list(title = date_col),
    yaxis = list(title = value_col),
    plot_bgcolor = background_color,
    paper_bgcolor = background_color,
    font = list(color = text_color),
    updatemenus = list(
      list(
        active = 0,
        buttons = dropdown_buttons,
        x = 0,
        xanchor = "left",
        y = 1.15,
        yanchor = "top"
      )
    ),
    showlegend = FALSE
  )
}

###############################################################################
## FORECASTING FUNCTION
###############################################################################

forecast_ts <- function(
    data,
    value,
    date,
    group      = NULL,  # ID column (e.g., product_id, store_id)
    horizon    = NULL,  # Determined automatically if not specified
    conf_level   = 0.95,  # Confidence level for prediction intervals
    ...
) {

  # 1. Ensure required packages (Suggests:) are available
  check_forecasting_dependencies()

  # -- 3. Column symbols -------------------------------------------------------
  value_sym <- rlang::sym(value)
  date_sym  <- rlang::sym(date)
  group_sym <- if (!is.null(group)) rlang::sym(group) else NULL

  # -- 4. Horizon detection ----------------------------------------------------
  if (is.null(horizon)) {
    dates <- tryCatch(
      if (is.character(data[[date]]))
        lubridate::parse_date_time(data[[date]],
                                   orders = c("ymd", "dmy", "mdy", "Ymd"))
      else data[[date]],
      error = function(e) NULL
    )

    if (!is.null(dates) && length(dates) > 1) {
      time_diff <- as.numeric(difftime(dates[2], dates[1], units = "days"))
      horizon <- dplyr::case_when(
        time_diff <= 1                    ~ 30,   # daily
        time_diff >= 6  & time_diff <= 8  ~ 13,   # weekly
        time_diff >= 28 & time_diff <= 31 ~ 12,   # monthly
        time_diff >= 89 & time_diff <= 92 ~ 8,    # quarterly
        time_diff >= 360 & time_diff <= 370 ~ 5,  # yearly
        nrow(data) < 100                 ~ ceiling(nrow(data) * 0.20),
        TRUE                              ~ ceiling(nrow(data) * 0.10)
      )
    } else horizon <- 12
  }

  # -- 5. Standardised naming & aggregation -------------------------------------

  if (!is.null(group)) {
    # Warn if group column is numeric
    if (is.numeric(data[[group]])) {
      warning("Group column is numeric. Converting to character for proper grouping.")
    }

    data_renamed <- data %>%
      rename(
        value_col = !!value_sym,
        date_col  = !!date_sym,
        group_col = !!group_sym
      ) %>%
      mutate(group_col = as.character(group_col))
  } else {
    data_renamed <- data %>%
      rename(
        value_col = !!value_sym,
        date_col  = !!date_sym
      ) %>%
      mutate(group_col = "ALL_GROUPS")
  }

  # -- 5b. Aggregate to ensure one row per group-date ---------------------------
  data_renamed <- data_renamed %>%
    group_by(group_col, date_col) %>%
    summarise(value_col = sum(value_col, na.rm = TRUE), .groups = "drop")

  # -- 6. Future frame & split -------------------------------------------------
  full_data_tbl <- data_renamed %>%
    select(group_col, date_col, value_col) %>%
    group_by(group_col) %>%
    future_frame(.date_var = date_col, .length_out = horizon, .bind_data = TRUE) %>%
    ungroup() %>%
    mutate(id = forcats::fct_drop(group_col))

  data_prepared_tbl <- full_data_tbl %>% filter(!is.na(value_col))
  future_tbl        <- full_data_tbl %>% filter(is.na(value_col))

  splits <- data_prepared_tbl %>%
    timetk::time_series_split(
      date_var   = date_col,
      assess     = min(horizon, floor(nrow(data_prepared_tbl) * 0.20)),
      cumulative = TRUE
    )

  # -- 7. Recipes --------------------------------------------------------------
  recipe_spec_1 <- recipes::recipe(value_col ~ ., training(splits)) %>%
    recipes::step_rm(matches("^$")) %>%  # Remove unnamed columns
    timetk::step_timeseries_signature(date_col) %>%
    recipes::step_rm(matches("(.iso$)|(.xts$)|(day)|(hour)|(minute)|(second)|(am.pm)")) %>%
    recipes::step_zv(all_predictors()) %>%  # This removes zero-variance predictors
    recipes::step_normalize(all_numeric_predictors(), -all_outcomes()) %>%
    recipes::step_mutate(date_col_week = factor(date_col_week, ordered = TRUE)) %>%
    recipes::step_dummy(all_nominal(), one_hot = TRUE)

  recipe_spec_2 <- recipe_spec_1 %>% update_role(date_col, new_role = "ID")

  # -- 8. Models & workflows ---------------------------------------------------
  model_list <- list()

  ## 8.1 Prophet
  wflw_fit_prophet <- workflow() %>%
    workflows::add_model(
      prophet_reg(
        seasonality_daily  = FALSE,
        seasonality_weekly = ifelse(horizon >= 7, TRUE, FALSE),
        seasonality_yearly = TRUE
      ) %>% set_engine("prophet") %>% set_mode("regression")
    ) %>%
    workflows::add_recipe(recipe_spec_1) %>%
    fit(training(splits))
  model_list <- c(model_list, list(wflw_fit_prophet))

  ## 8.2 XGBoost
  wflw_fit_xgboost <- workflow() %>%
    workflows::add_model(
      boost_tree() %>% set_engine("xgboost") %>% set_mode("regression")
    ) %>%
    workflows::add_recipe(recipe_spec_2) %>%
    fit(training(splits))
  model_list <- c(model_list, list(wflw_fit_xgboost))

  ## 8.3 Random Forest
  wflw_fit_rf <- workflow() %>%
    workflows::add_model(
      rand_forest() %>% set_engine("ranger") %>% set_mode("regression")
    ) %>%
    workflows::add_recipe(recipe_spec_2) %>%
    fit(training(splits))
  model_list <- c(model_list, list(wflw_fit_rf))

  ## 8.4 SVM
  wflw_fit_svm <- workflow() %>%
    workflows::add_model(
      svm_rbf() %>% set_engine("kernlab") %>% set_mode("regression")
    ) %>%
    workflows::add_recipe(recipe_spec_2) %>%
    fit(training(splits))
  model_list <- c(model_list, list(wflw_fit_svm))

  ## 8.5 Prophet Boost
  wflw_fit_prophet_boost <- workflow() %>%
    workflows::add_model(
      prophet_boost(
        seasonality_daily  = FALSE,
        seasonality_weekly = ifelse(horizon >= 7, TRUE, FALSE),
        seasonality_yearly = TRUE
      ) %>% set_engine("prophet_xgboost") %>% set_mode("regression")
    ) %>%
    workflows::add_recipe(recipe_spec_1) %>%
    fit(training(splits))
  model_list <- c(model_list, list(wflw_fit_prophet_boost))

  # -- 9. Ensemble (mean) ------------------------------------------------------
  submodels_tbl        <- do.call(modeltime_table, model_list)
  ensemble_fit_mean    <- submodels_tbl %>% ensemble_average(type = "mean")
  ensemble_tbl         <- modeltime_table(ensemble_fit_mean)
  ensemble_calibrated  <- ensemble_tbl %>% modeltime_calibrate(testing(splits))
  ensemble_refit       <- ensemble_calibrated %>% modeltime_refit(data_prepared_tbl)

  # -- 10. Forecast ------------------------------------------------------------
  forecast_tbl <- ensemble_refit %>%
    modeltime::modeltime_forecast(
      new_data    = future_tbl,
      actual_data = data_prepared_tbl,
      keep_data   = TRUE,
      conf_level  = conf_level
    ) %>%
    select(id, .index, .value, .conf_lo, .conf_hi) %>%
    rename(
      date    = .index,
      value   = .value,
      conf_lo = .conf_lo,
      conf_hi = .conf_hi
    )

  return(forecast_tbl)
}

###############################################################################
## PACKAGES FUNCTION
###############################################################################
#' Check Forecasting Dependencies
#'
#' This function ensures that all suggested packages and specific functions used in forecasting
#' workflows are available at runtime. It assigns each function to a local variable using
#' `fun <- get_suggested(pkg, fun)` style for runtime access.
#'
#' @return Invisibly TRUE if all packages/functions are available or skipped (base).
#' @export
check_forecasting_dependencies <- function() {
  # Base / utils
  capture.output <- get_suggested("utils", "capture.output")
  head <- get_suggested("base", "head")
  id <- get_suggested("base", "id")  # fallback placeholder
  na.omit <- get_suggested("stats", "na.omit")

  # dplyr
  mutate <- get_suggested("dplyr", "mutate")
  filter <- get_suggested("dplyr", "filter")
  group_by <- get_suggested("dplyr", "group_by")
  ungroup <- get_suggested("dplyr", "ungroup")
  select <- get_suggested("dplyr", "select")
  rename <- get_suggested("dplyr", "rename")
  summarise <- get_suggested("dplyr", "summarise")
  bind_rows <- get_suggested("dplyr", "bind_rows")

  # tidyr, purrr, magrittr, tibble, forcats
  matches <- get_suggested("tidyr", "matches")
  map <- get_suggested("purrr", "map")
  `%>%` <- get_suggested("magrittr", "%>%")
  tibble <- get_suggested("tibble", "tibble")
  fct_drop <- get_suggested("forcats", "fct_drop")

  # lubridate
  parse_date_time <- get_suggested("lubridate", "parse_date_time")
  year <- get_suggested("lubridate", "year")
  month <- get_suggested("lubridate", "month")
  day <- get_suggested("lubridate", "day")

  # timetk
  future_frame <- get_suggested("timetk", "future_frame")

  # rlang
  sym <- get_suggested("rlang", "sym")

  # jsonlite
  fromJSON <- get_suggested("jsonlite", "fromJSON")
  toJSON <- get_suggested("jsonlite", "toJSON")

  # recipes
  recipe <- get_suggested("recipes", "recipe")
  step_rm <- get_suggested("recipes", "step_rm")
  step_timeseries_signature <- get_suggested("recipes", "step_timeseries_signature")
  step_zv <- get_suggested("recipes", "step_zv")
  step_normalize <- get_suggested("recipes", "step_normalize")
  step_mutate <- get_suggested("recipes", "step_mutate")
  step_dummy <- get_suggested("recipes", "step_dummy")
  update_role <- get_suggested("recipes", "update_role")
  all_predictors <- get_suggested("recipes", "all_predictors")
  all_outcomes <- get_suggested("recipes", "all_outcomes")
  all_numeric_predictors <- get_suggested("recipes", "all_numeric_predictors")
  all_nominal <- get_suggested("recipes", "all_nominal")

  # parsnip
  boost_tree <- get_suggested("parsnip", "boost_tree")
  rand_forest <- get_suggested("parsnip", "rand_forest")
  svm_rbf <- get_suggested("parsnip", "svm_rbf")
  set_engine <- get_suggested("parsnip", "set_engine")
  set_mode <- get_suggested("parsnip", "set_mode")
  prophet_reg <- get_suggested("parsnip", "prophet_reg")
  prophet_boost <- get_suggested("parsnip", "prophet_boost")

  # workflows
  workflow <- get_suggested("workflows", "workflow")
  add_model <- get_suggested("workflows", "add_model")
  add_recipe <- get_suggested("workflows", "add_recipe")

  # rsample
  training <- get_suggested("rsample", "training")
  testing <- get_suggested("rsample", "testing")
  time_series_split <- get_suggested("rsample", "time_series_split")

  # yardstick
  rmse <- get_suggested("yardstick", "rmse")

  # modeltime
  modeltime_table <- get_suggested("modeltime", "modeltime_table")
  modeltime_calibrate <- get_suggested("modeltime", "modeltime_calibrate")
  modeltime_refit <- get_suggested("modeltime", "modeltime_refit")
  modeltime_forecast <- get_suggested("modeltime", "modeltime_forecast")

  # modeltime.ensemble
  ensemble_average <- get_suggested("modeltime.ensemble", "ensemble_average")

  # plotly
  plot_ly <- get_suggested("plotly", "plot_ly")
  add_trace <- get_suggested("plotly", "add_trace")
  layout <- get_suggested("plotly", "layout")

  # Model engines
  xgb_train <- get_suggested("xgboost", "xgb.train")
  ranger <- get_suggested("ranger", "ranger")
  ksvm <- get_suggested("kernlab", "ksvm")

  invisible(TRUE)
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
## NODE FUNCTIONS FOR TIME SERIES FORECASTING
###############################################################################

node_recommend_forecasting_steps <- function(model, verbose = FALSE) {
  function(state) {

    # -- 1. Helper Functions
    `%||%` <- function(a, b) {
      # -- 1. Input Validation
      if (missing(a) || missing(b)) {
        stop("Both arguments must be provided to the %||% operator")
      }

      # -- 2. Check for NULL --
      if (is.null(a)) return(b)

      # -- 3. Handle Zero-Length Vectors
      if (length(a) == 0) return(b)

      # -- 4. Check for Empty Strings (with whitespace)
      if (is.character(a)) {
        if (all(trimws(a) == "")) return(b)
      }

      # -- 5. Handle Special Cases ------
      if (is.na(a) && !is.nan(a)) return(b)  # NA (but not NaN)
      if (identical(a, logical(0))) return(b)
      if (identical(a, numeric(0))) return(b)
      if (identical(a, integer(0))) return(b)
      if (identical(a, character(0))) return(b)

      # -- 6. Handle Data Frames and Matrices ----------
      if (is.data.frame(a) || is.matrix(a)) {
        if (nrow(a) == 0 || ncol(a) == 0) return(b)
      }

      # -- 7. Handle Lists ----
      if (is.list(a)) {
        if (length(a) == 0) return(b)
        if (all(sapply(a, is.null))) return(b)
      }

      # -- 8. Default Case ----
      return(a)
    }
    # -- 2. Console Output -
    if (verbose) message("--- TIME SERIES FORECASTING AGENT ----")
    if (verbose) message("    * RECOMMEND FORECASTING STEPS\n")

    # -- 4. Data Preparation ---------
    if (is.data.frame(state$data_raw)) {
      df <- state$data_raw
    } else if (is.list(state$data_raw)) {
      df <- as.data.frame(state$data_raw)
    } else {
      stop("state$data_raw must be a data.frame or list convertible to data.frame")
    }

    # -- 5. Input Collection ---------
    user_instructions <- state$user_instructions %||% ""
    previous_steps <- state$recommended_steps %||% ""
    all_datasets_summary <- get_dataframe_summary(df, skip_stats = TRUE)

    # -- 6. Prompt Construction ------
    prompt <- sprintf(
      "You are the **Chief Forecasting Supervisor** overseeing the initial scoping of a time-series forecasting project. Your job is to define a **clear, rigorous forecasting blueprint**, referencing two pre-implemented utility functions:

  * `forecast_ts()` - accepts (data, value, date, group, horizon, conf_level)
    and returns a forecast table with columns: id, date, value, conf_lo, conf_hi

  * `plot_forecast()` - takes the forecast table and produces an interactive Plotly visualization.

Write a structured, professional-grade **FORECASTING BLUEPRINT** based on the user's intent, any prior recommendations, and the dataset characteristics provided.

#######################################################################
# USER REQUEST
%s

# PRIOR RECOMMENDATIONS (if available)
%s

# DATA OVERVIEW
%s

#######################################################################
# DELIVERABLE - FORECASTING BLUEPRINT

1. DATA PROFILE
   * Date Column      : <col_name> (<format>)
   * Target Variable  : <col_name> (<numeric type>)
   * Grouping Column  : <list_> (e.g. id, store_id, product_id)
   * Frequency        : <daily | weekly | monthly | other>

2. FORECASTING SPECIFICATIONS
   * Horizon          : <N periods> + rationale
   * Confidence Level : <e.g., 95%% unless overridden>

3. MODELING STRATEGY
   All five models below will be trained on the same training window.
   Their forecasts will be **averaged equally** to form the ensemble:

   | # | Model Family           | Engine / Function        |
   |---|------------------------|--------------------------|
   | 1 | Prophet w/ Regressors  | `prophet_reg()`          |
   | 2 | Gradient Boosting      | `boost_tree()`           |
   | 3 | Random Forest          | `rand_forest()` (ranger) |
   | 4 | Support Vector Machine | `svm_rbf()` (kernlab)    |
   | 5 | Prophet + Boost        | `prophet_boost()`        |

   * Recipes Used:
     - `recipe_spec_1` for Prophet-based models
     - `recipe_spec_2` for tree/kernel models
   * Ensemble Rule: Equal-weighted average across models
   * Rationale   : Increases robustness via model diversity

4. FUNCTION ARGUMENT MAP
   | `forecast_ts()` Argument | Value                     |
   |--------------------------||
   | data                     | <data frame name>         |
   | value                    | <Target variable column>  |
   | date                     | <Date column>             |
   | group                    | <Group column>            |
   | horizon                  | <Int / NULL>              |
   | conf_level               | <Decimal between 0.80-0.99>

   * Output schema verified: id, date, value, conf_lo, conf_hi

** IMPORTANT: The `group` variable will appear in **_90%% of user inputs_**.
Look for it carefully. If not explicitly provided, default to `'ALL GROUPS'`.**

#######################################################################
# STRICT CONSTRAINTS
1. No code or implementation syntax
2. No filesystem paths or data I/O
3. Time-series focus only
4. Avoid vague language - be specific and actionable"
      ,
      user_instructions,
      previous_steps,
      all_datasets_summary
    )

    # -- 7. Model Query
    steps <- model(prompt)

    # -- 8. Diagnostic Logging
    if (!is.null(state$log)) {
      state$log[[length(state$log) + 1]] <- list(
        step = "recommend_steps",
        prompt = prompt,
        response = steps,
        timestamp = Sys.time()
      )
    }

    # -- 9. Return Updated State -----
    list(
      recommended_steps = paste0("\nRecommended Forecasting Steps:\n", trimws(steps)),
      all_datasets_summary = all_datasets_summary,
      previous_recommendations = if (nchar(previous_steps) > 0) {
        c(state$previous_recommendations %||% list(), list(previous_steps))
      } else {
        state$previous_recommendations %||% list()
      }
    )
  }
}

node_create_forecasting_code <- function(model, mode, line_width, bypass_recommended_steps = FALSE) {
  function(state) {

    # Define safe_as.integer function
    safe_as.integer <- function(x) {
      tryCatch({
        x <- as.character(x)
        if (grepl("^\\d+$", x)) as.integer(x) else NA_integer_
      }, warning = function(w) NA_integer_, error = function(e) NA_integer_)
    }

    # Null-coalescing utility
    `%||%` <- function(a, b) if (!is.null(a) && !identical(a, "")) a else b

    # Console banner
    if (bypass_recommended_steps) message("---TIME SERIES FORECASTING AGENT----")
    message("    * CREATE FORECASTING CODE")

    # 1 # Gather data summary & instructions -
    if (bypass_recommended_steps) {
      df  <- if (is.data.frame(state$data_raw)) state$data_raw else as.data.frame(state$data_raw)
      all_datasets_summary      <- get_dataframe_summary(df, skip_stats = FALSE)
      chart_generator_instructions <- state$user_instructions
    } else {
      all_datasets_summary      <- state$all_datasets_summary
      chart_generator_instructions <- state$recommended_steps
    }

    # 2 # Compose elite prompt
    prompt <- sprintf(
      "You are an elite time-series forecasting assistant supporting high-stakes enterprise projects.

Your task: **extract five forecasting parameters** from the user's input and data summary. Return only a **valid JSON object** - no commentary, no markdown, no extra formatting.

#######################################################################
# REQUIRED PARAMETERS
1. `params_value`       - Numeric column to forecast
2. `params_date`        - Date/time column
3. `params_group`       - Grouping column (or NULL for ungrouped)
4. `params_horizon`     - Integer future periods (#1)
5. `params_conf_level`  - Confidence level between 0.80 and 0.99 (default 0.95)

# EXTRACTION RULES
* Use exact column names from the data summary. **Never invent new names.**
* Prioritize numeric columns matching 'sales', 'revenue', 'value', 'demand', 'units', 'traffic'.
* For grouping: if the user says **'by / per / for each' + <column>**, assign that as `params_group`.
* Horizon rules (in order of precedence):
  # Explicit span: \"next 18 months\" # detect unit & convert
  # Calendar phrases: \"through 2027\", \"Q4\", etc.
  # Defaults: monthly=12, weekly=13, daily=30
  # Fallback: #20%% of N# if N < 100; #10%% of N# otherwise
* Confidence: if absent or invalid, default to **0.95**

* Return **all five keys**, even if some are `null`.

# EXAMPLE OUTPUT
{
  \"params_value\":       \"sales\",
  \"params_date\":        \"date\",
  \"params_group\":       \"product_id\",  # Example: id, store_id, region
  \"params_horizon\":     12,
  \"params_conf_level\":  0.95
}

# GOOD USER EXAMPLES (partial list)
* 'Forecast the next 24 months of sales.'
* 'Show revenue forecast **per region** for FY-2026.'
* 'Give me a weekly forecast of visits by channel for Q2.'
* 'Estimate monthly churn through 2027.'
* 'Predict ride-hailing trips per driver next Friday.'

#######################################################################
# DATA SUMMARY
%s

#######################################################################
# USER INSTRUCTIONS
%s"
      , all_datasets_summary, chart_generator_instructions)

    #-- 3 # Invoke model -----
    raw_response <- model(prompt)

    # Optional: append to diagnostic log
    if (!is.null(state$log)) state$log[[length(state$log)+1]] <-
      list(step = "extract_params", prompt = prompt, response = raw_response)

    #-- 4 # Robust JSON parse
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package 'jsonlite' is required for JSON parsing.")
    }
    # Improved JSON parsing
    parsed <- tryCatch({
      # First clean the response
      cleaned_response <- gsub("```json|```", "", raw_response)
      jsonlite::fromJSON(cleaned_response)
    }, error = function(e) {
      warning("Failed to parse JSON response: ", e$message)
      NULL
    })

    #-- 5. Parameter Validation
    if (is.null(parsed)) {
      stop("Could not parse valid forecasting parameters from model response")
    }

    if (is.null(parsed$params_value) || is.null(parsed$params_date)) {
      warning("Missing required parameters: params_value and params_date must be specified")
      return(NULL)
    }

    params <- list(
      params_value = as.character(parsed$params_value),
      params_date = as.character(parsed$params_date),
      params_group = if (!is.null(parsed$params_group)) as.character(parsed$params_group) else NULL,
      params_horizon = if (!is.null(parsed$params_horizon)) {
        horizon <- safe_as.integer(parsed$params_horizon)
        if (is.na(horizon) || horizon <= 0) NULL else horizon
      } else NULL,
      params_conf_level = if (!is.null(parsed$params_conf_level)) {
        conf <- suppressWarnings(as.numeric(parsed$params_conf_level))
        if (is.na(conf) || conf < 0.8 || conf > 0.99) 0.95 else round(conf/0.05)*0.05
      } else 0.95
    )

    # 7 # Assemble result
    params <- list(
      params_value     = parsed$params_value,
      params_date      = parsed$params_date,
      params_group     = parsed$params_group       %||% NULL,
      params_horizon   = horizon,
      params_conf_level = parsed$params_conf_level %||% 0.95
    )

    # 8 # Plot parameters

    # After getting params, we need to return them in the state
    list(
      plot_mode = mode,
      plot_line_width = line_width,
      forecasting_params = params,
      retry_count = 0  # Initialize retry counter
    )
  }
}

node_execute_forecasting_code <- function(state, verbose = FALSE) {
  if (verbose) message("    * EXECUTING FORECASTING CODE")

  # Define required packages
  required_packages <- c(
    "dplyr", "rlang", "lubridate", "tibble", "tidyr", "tidymodels",
    "modeltime", "modeltime.ensemble", "timetk", "forcats", "recipes",
    "parsnip", "workflows", "rsample", "prophet", "ranger", "kernlab",
    "xgboost", "jsonlite"
  )

  # Check for missing packages using requireNamespace
  missing_pkgs <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    if (verbose) message("Missing packages: ", paste(missing_pkgs, collapse = ", "))
    return(list(forecasting_error = paste("Missing required packages:", paste(missing_pkgs, collapse = ", "))))
  }

  # Load packages silently
  suppressPackageStartupMessages({
    invisible(lapply(required_packages, require, character.only = TRUE))
  })

  # 2. Data Preparation
  if (is.null(state$data_raw)) {
    return(list(forecasting_error = "No data available in state$data_raw"))
  }

  df <- if (is.data.frame(state$data_raw)) {
    state$data_raw
  } else if (is.list(state$data_raw)) {
    as.data.frame(state$data_raw)
  } else {
    return(list(forecasting_error = "Unsupported data format in state$data_raw"))
  }

  # 3. Parameter Validation
  params <- state$forecasting_params
  if (is.null(params)) {
    return(list(forecasting_error = "Missing forecasting parameters in state$forecasting_params"))
  }

  validate_parameters <- function(params, df) {
    # Core parameter checks
    if (!params$params_value %in% names(df)) {
      stop("Target column '", params$params_value, "' not found in data")
    }
    if (!params$params_date %in% names(df)) {
      stop("Date column '", params$params_date, "' not found in data")
    }
    if (!is.null(params$params_group) && !params$params_group %in% names(df)) {
      stop("Group column '", params$params_group, "' not found in data")
    }

    # Type checks
    if (!inherits(df[[params$params_date]], c("Date", "POSIXt"))) {
      stop("Date column '", params$params_date, "' must be Date/Datetime type")
    }
    if (!is.numeric(df[[params$params_value]])) {
      stop("Target column '", params$params_value, "' must be numeric")
    }

    # Horizon validation
    if (!is.null(params$params_horizon)) {
      if (!is.integer(params$params_horizon) || params$params_horizon < 1) {
        stop("Horizon must be positive integer, got ", params$params_horizon)
      }
    }
  }

  tryCatch(
    validate_parameters(params, df),
    error = function(e) {
      return(list(forecasting_error = paste("Parameter validation failed:", e$message)))
    }
  )

  # 4. Execute Forecast
  forecast_args <- list(
    data      = df,
    value     = params$params_value,
    date      = params$params_date,
    group     = params$params_group,
    horizon   = params$params_horizon,
    conf_level= params$params_conf_level
  )
  forecast_args <- forecast_args[!sapply(forecast_args, is.null)]

  result <- suppressWarnings(
    tryCatch({
      fcst <- do.call(forecast_ts, forecast_args)

      # Validate output structure
      required_cols <- c("id", "date", "value", "conf_lo", "conf_hi")
      if (!all(required_cols %in% names(fcst))) {
        stop("Invalid forecast output - missing required columns: ",
             paste(setdiff(required_cols, names(fcst)), collapse = ", "))
      }

      # Generate plot if available
      mode <- state$plot_mode
      line_width <- state$plot_line_width

      plot_obj <- tryCatch(
        plot_forecast(
          data = fcst,
          mode = mode,
          line_width = line_width
        ),
        error = function(e) {
          warning("Plot generation failed: ", e$message)
          NULL
        }
      )

      list(forecast_data = fcst, forecast_plot = plot_obj)
    }, error = function(e) {
      return(list(forecasting_error = paste("Forecast execution failed:", e$message)))
    })
  )

  # 5. Return Updated State
  if (!is.null(result$forecasting_error)) {
    return(list(forecasting_error = result$forecasting_error))
  }

  list(
    forecasting_data = result$forecast_data,
    forecasting_result = result$forecast_plot,
    execution_success = TRUE,
    timestamp = Sys.time()
  )
}

node_fix_forecasting_code <- function(model) {
  function(state) {
    message("    * FIX FORECASTING PARAMETERS")
    message("      retry_count:", state$retry_count)

    # Get context
    error_message <- state$forecasting_error
    prev_params <- state$forecasting_params
    data_summary <- state$all_datasets_summary
    user_instructions <- state$user_instructions

    prompt <- sprintf(
      "You are a forecasting quality assurance specialist. Analyze the failed forecast attempt and suggest corrected parameters.

      # CRITICAL REQUIREMENTS
      1. PRESERVE USER INTENT from original instructions
      2. Only modify parameters that caused the error
      3. Maintain consistency with the data structure

      # CONTEXT
      USER REQUEST: '%s'

      ERROR MESSAGE: '%s'

      DATA STRUCTURE:
      %s

      PREVIOUS PARAMETERS:
      %s

      # FIXING RULES
      - Keep value/date columns UNLESS they caused the error
      - Only remove grouping if absolutely necessary
      - Adjust horizon algorithmically:
        * If date error: set to 1 temporarily
        * If memory error: reduce by 50%%
        * Else: keep original
      - Confidence: Only adjust if outside 0.8-0.99 range

      # OUTPUT FORMAT
      Return ONLY this JSON structure with your fixes:
      {
        \"params_value\": \"<exact_column_name>\",
        \"params_date\": \"<exact_date_column>\",
        \"params_group\": \"<column_name_or_null>\",
        \"params_horizon\": <integer_based_on_rules>,
        \"params_conf_level\": <0.80_to_0.99>,
        \"fix_reason\": \"<brief_explanation>\"
      }",

      user_instructions,
      error_message,
      data_summary,
      jsonlite::toJSON(prev_params, auto_unbox = TRUE)
    )
    # Get LLM response
    raw_response <- model(prompt)

    # Parse and validate
    parsed <- tryCatch(jsonlite::fromJSON(raw_response), error = function(e) NULL)

    # Fallback to original params if parse fails
    if(is.null(parsed)) {
      warning("Failed to parse fixed parameters. Using original values.")
      parsed <- prev_params
    }

    # Ensure required fields exist
    if(is.null(parsed$params_value)) parsed$params_value <- prev_params$params_value
    if(is.null(parsed$params_date)) parsed$params_date <- prev_params$params_date

    # Update retry counter
    new_retry_val <- state$retry_count + 1

    list(
      forecasting_params = parsed,
      forecasting_error = NULL,
      retry_count = new_retry_val
    )
  }
}

node_explain_forecasting_code <- function(model) {
  function(state) {
    summary <- if (!is.null(state$forecasting_error)) {
      paste("Error occurred:", state$forecasting_error)
    } else {
      "Forecasting created successfully"
    }

    prompt <- sprintf(
      "Explain these Forecasting transformations:\nSteps: %s\n\nResult:\n%s",
      state$recommended_steps, summary
    )

    explanation <- model(prompt)

    list(
      forecasting_report = explanation,
      forecasting_summary = summary
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
    message(" * HUMAN REVIEW")
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

###############################################################################
## TIME SERIES FORECASTING AGENT IMPLEMENTATION
###############################################################################
#' Build a Time Series Forecasting Agent
#'
#' Constructs a state graph-based forecasting agent that:
#' recommends forecasting steps, extracts parameters, generates code,
#' executes the forecast using `modeltime`, fixes errors if needed,
#' and explains the result. It leverages multiple models including
#' Prophet, XGBoost, Random Forest, SVM, and Prophet Boost, and
#' combines them in an ensemble.
#'
#' @name build_forecasting_agent
#' @param model A function that takes a prompt and returns an LLM-generated result.
#' @param bypass_recommended_steps Logical; skip initial step recommendation.
#' @param bypass_explain_code Logical; skip the final explanation step.
#' @param mode Visualization mode for forecast plots. One of `"light"` or `"dark"`.
#' @param line_width Line width used in plotly forecast visualization.
#' @param verbose Logical; whether to print progress messages.
#'
#' @return A callable agent function that mutates the given `state` list.
#'
#' @examples
#' \dontrun{
#' state <- list(
#'   data_raw = tsibble_data,
#'   user_instructions = "Forecast next 12 months of sales per store"
#' )
#' agent <- build_forecasting_agent(model = call_llm)
#' agent(state)
#' print(state$forecasting_result)
#' }
#'
#' @export
NULL

build_forecasting_agent <- function(
    model,
    bypass_recommended_steps = FALSE,
    bypass_explain_code = FALSE,
    mode = "light",
    line_width = 3,
    verbose = FALSE) {

  # no human_validation needed
  human_validation = FALSE

  # Define node functions list
  node_functions <- list(
    recommend_forecasting_steps = node_recommend_forecasting_steps(model, verbose),
    human_review = node_func_human_review(
      prompt_text = "Are the following forecasting instructions correct# (Answer 'yes' or provide modifications)\n%s",
      yes_goto = if (!bypass_explain_code) "explain_forecasting_code" else "__end__",
      no_goto = "recommend_forecasting_steps",
      user_instructions_key = "user_instructions",
      recommended_steps_key = "recommended_steps"
    ),
    create_forecasting_code = node_create_forecasting_code(
      model = model,
      mode = mode,
      line_width = line_width,
      bypass_recommended_steps = bypass_recommended_steps
    ),
    execute_forecasting_code = function(state) node_execute_forecasting_code(state, verbose),
    fix_forecasting_code = node_fix_forecasting_code(model),
    explain_forecasting_code = node_explain_forecasting_code(model)
  )

  # Create the agent graph
  app <- create_coding_agent_graph(
    node_functions = node_functions,
    recommended_steps_node_name = "recommend_forecasting_steps",
    create_code_node_name = "create_forecasting_code",
    execute_code_node_name = "execute_forecasting_code",
    fix_code_node_name = "fix_forecasting_code",
    explain_code_node_name = "explain_forecasting_code",
    error_key = "forecasting_error",
    max_retries_key = "max_retries",
    retry_count_key = "retry_count",
    human_validation = human_validation,
    human_review_node_name = "human_review",
    checkpointer = NULL,
    bypass_recommended_steps = bypass_recommended_steps,
    bypass_explain_code = bypass_explain_code
  )

  # Return a function that can be invoked with state
  function(state) {
    if (is.null(state$retry_count)) state$retry_count <- 0
    if (is.null(state$max_retries)) state$max_retries <- 3

    app(state)
  }
}
