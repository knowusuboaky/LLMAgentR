source(file.path("R", "zz_custom_agent.R"))

resolve_mmdc_bin <- function(mmdc = NULL) {
  if (!is.null(mmdc) && nzchar(mmdc) && file.exists(mmdc)) {
    return(mmdc)
  }

  candidate_mmdc <- c(
    Sys.which("mmdc"),
    "C:/Users/kwadw.DESKTOP-T9BSTPE/AppData/Roaming/npm/mmdc.cmd"
  )
  candidate_mmdc <- candidate_mmdc[nzchar(candidate_mmdc)]
  candidate_mmdc <- candidate_mmdc[file.exists(candidate_mmdc)]

  if (!length(candidate_mmdc)) {
    stop("Mermaid CLI not found. Install with `npm i -g @mermaid-js/mermaid-cli`.")
  }

  candidate_mmdc[[1]]
}

make_nodes <- function(node_names) {
  setNames(
    lapply(node_names, function(nm) {
      force(nm)
      function(state) list()
    }),
    node_names
  )
}

route_label <- function(state) {
  "done"
}

get_agent_workflow_specs <- function() {
  list(
    list(
      id = "code-agent",
      entry_point = "receive_task",
      nodes = c(
        "receive_task",
        "assemble_system_prompt",
        "call_llm",
        "check_response",
        "backoff_retry",
        "return_success",
        "return_failure"
      ),
      edges = list(
        c("receive_task", "assemble_system_prompt"),
        c("assemble_system_prompt", "call_llm"),
        c("call_llm", "check_response"),
        c("backoff_retry", "call_llm"),
        c("return_success", "__end__"),
        c("return_failure", "__end__")
      ),
      conditional_edges = list(
        list(
          from = "check_response",
          condition = route_label,
          mapping = list(
            success = "return_success",
            retry = "backoff_retry",
            failed = "return_failure"
          )
        )
      ),
      subgraphs = list(
        "Input Setup" = c("receive_task", "assemble_system_prompt"),
        "Retry Loop" = c("call_llm", "check_response", "backoff_retry"),
        "Output" = c("return_success", "return_failure")
      )
    ),
    list(
      id = "interpreter-agent",
      entry_point = "receive_output",
      nodes = c(
        "receive_output",
        "build_interpreter_prompt",
        "call_llm",
        "check_response",
        "backoff_retry",
        "return_interpretation",
        "return_failure"
      ),
      edges = list(
        c("receive_output", "build_interpreter_prompt"),
        c("build_interpreter_prompt", "call_llm"),
        c("call_llm", "check_response"),
        c("backoff_retry", "call_llm"),
        c("return_interpretation", "__end__"),
        c("return_failure", "__end__")
      ),
      conditional_edges = list(
        list(
          from = "check_response",
          condition = route_label,
          mapping = list(
            success = "return_interpretation",
            retry = "backoff_retry",
            failed = "return_failure"
          )
        )
      ),
      subgraphs = list(
        "Prompting" = c("receive_output", "build_interpreter_prompt"),
        "Retry Loop" = c("call_llm", "check_response", "backoff_retry"),
        "Output" = c("return_interpretation", "return_failure")
      )
    ),
    list(
      id = "research-agent",
      entry_point = "resolve_tavily_key",
      nodes = c(
        "resolve_tavily_key",
        "search_web",
        "check_search_results",
        "search_backoff",
        "build_prompt_from_results",
        "build_prompt_without_results",
        "call_llm",
        "check_llm_response",
        "llm_backoff",
        "return_research_output"
      ),
      edges = list(
        c("resolve_tavily_key", "search_web"),
        c("search_web", "check_search_results"),
        c("search_backoff", "search_web"),
        c("build_prompt_from_results", "call_llm"),
        c("build_prompt_without_results", "call_llm"),
        c("call_llm", "check_llm_response"),
        c("llm_backoff", "call_llm"),
        c("return_research_output", "__end__")
      ),
      conditional_edges = list(
        list(
          from = "check_search_results",
          condition = route_label,
          mapping = list(
            found = "build_prompt_from_results",
            retry = "search_backoff",
            none = "build_prompt_without_results"
          )
        ),
        list(
          from = "check_llm_response",
          condition = route_label,
          mapping = list(
            success = "return_research_output",
            retry = "llm_backoff",
            failed = "return_research_output"
          )
        )
      ),
      subgraphs = list(
        "Search Loop" = c("search_web", "check_search_results", "search_backoff"),
        "Prompt Build" = c("build_prompt_from_results", "build_prompt_without_results"),
        "LLM Loop" = c("call_llm", "check_llm_response", "llm_backoff")
      )
    ),
    list(
      id = "weather-agent",
      entry_point = "validate_api_and_inputs",
      nodes = c(
        "validate_api_and_inputs",
        "parse_location",
        "fetch_weather_api",
        "check_weather_fetch",
        "fetch_backoff",
        "build_weather_prompt",
        "call_llm",
        "check_llm_response",
        "llm_backoff",
        "fallback_to_formatted_weather",
        "return_weather_payload",
        "return_error"
      ),
      edges = list(
        c("validate_api_and_inputs", "parse_location"),
        c("parse_location", "fetch_weather_api"),
        c("fetch_weather_api", "check_weather_fetch"),
        c("fetch_backoff", "fetch_weather_api"),
        c("build_weather_prompt", "call_llm"),
        c("call_llm", "check_llm_response"),
        c("llm_backoff", "call_llm"),
        c("fallback_to_formatted_weather", "return_weather_payload"),
        c("return_weather_payload", "__end__"),
        c("return_error", "__end__")
      ),
      conditional_edges = list(
        list(
          from = "check_weather_fetch",
          condition = route_label,
          mapping = list(
            success = "build_weather_prompt",
            retry = "fetch_backoff",
            failed = "return_error"
          )
        ),
        list(
          from = "check_llm_response",
          condition = route_label,
          mapping = list(
            success = "return_weather_payload",
            retry = "llm_backoff",
            failed = "fallback_to_formatted_weather"
          )
        )
      ),
      subgraphs = list(
        "Input Validation" = c("validate_api_and_inputs", "parse_location"),
        "Weather Fetch Loop" = c("fetch_weather_api", "check_weather_fetch", "fetch_backoff"),
        "LLM Loop" = c("build_weather_prompt", "call_llm", "check_llm_response", "llm_backoff"),
        "Fallbacks" = c("fallback_to_formatted_weather", "return_error")
      )
    ),
    list(
      id = "document-summarizer-agent",
      entry_point = "normalize_input",
      nodes = c(
        "normalize_input",
        "load_files_or_text",
        "clean_text",
        "check_content",
        "split_into_chunks",
        "summarize_chunks",
        "combine_partial_summaries",
        "return_summary",
        "return_empty_summary",
        "return_error"
      ),
      edges = list(
        c("normalize_input", "load_files_or_text"),
        c("load_files_or_text", "clean_text"),
        c("clean_text", "check_content"),
        c("split_into_chunks", "summarize_chunks"),
        c("summarize_chunks", "combine_partial_summaries"),
        c("combine_partial_summaries", "return_summary"),
        c("return_summary", "__end__"),
        c("return_empty_summary", "__end__"),
        c("return_error", "__end__")
      ),
      conditional_edges = list(
        list(
          from = "check_content",
          condition = route_label,
          mapping = list(
            ready = "split_into_chunks",
            empty = "return_empty_summary",
            failed = "return_error"
          )
        )
      ),
      subgraphs = list(
        "Input Handling" = c("normalize_input", "load_files_or_text", "clean_text"),
        "Summarization" = c("split_into_chunks", "summarize_chunks", "combine_partial_summaries"),
        "Output" = c("return_summary", "return_empty_summary", "return_error")
      )
    ),
    list(
      id = "sql-agent",
      entry_point = "recommend_sql_steps",
      nodes = c(
        "recommend_sql_steps",
        "create_sql_query_code",
        "execute_sql_database_code",
        "fix_sql_database_code",
        "explain_sql_database_code"
      ),
      edges = list(
        c("recommend_sql_steps", "create_sql_query_code"),
        c("create_sql_query_code", "execute_sql_database_code"),
        c("fix_sql_database_code", "execute_sql_database_code"),
        c("explain_sql_database_code", "__end__")
      ),
      conditional_edges = list(
        list(
          from = "execute_sql_database_code",
          condition = route_label,
          mapping = list(
            retry = "fix_sql_database_code",
            done = "explain_sql_database_code"
          )
        )
      ),
      subgraphs = list(
        "Planning" = c("recommend_sql_steps", "create_sql_query_code"),
        "Execution Loop" = c("execute_sql_database_code", "fix_sql_database_code"),
        "Reporting" = c("explain_sql_database_code")
      )
    ),
    list(
      id = "data-cleaning-agent",
      entry_point = "recommend_cleaning_steps",
      nodes = c(
        "recommend_cleaning_steps",
        "create_data_cleaner_code",
        "execute_data_cleaner_code",
        "fix_data_cleaner_code",
        "explain_data_cleaner_code"
      ),
      edges = list(
        c("recommend_cleaning_steps", "create_data_cleaner_code"),
        c("create_data_cleaner_code", "execute_data_cleaner_code"),
        c("fix_data_cleaner_code", "execute_data_cleaner_code"),
        c("explain_data_cleaner_code", "__end__")
      ),
      conditional_edges = list(
        list(
          from = "execute_data_cleaner_code",
          condition = route_label,
          mapping = list(
            retry = "fix_data_cleaner_code",
            done = "explain_data_cleaner_code"
          )
        )
      ),
      subgraphs = list(
        "Planning" = c("recommend_cleaning_steps", "create_data_cleaner_code"),
        "Execution Loop" = c("execute_data_cleaner_code", "fix_data_cleaner_code"),
        "Reporting" = c("explain_data_cleaner_code")
      )
    ),
    list(
      id = "data-wrangling-agent",
      entry_point = "recommend_wrangling_steps",
      nodes = c(
        "recommend_wrangling_steps",
        "create_data_wrangler_code",
        "execute_data_wrangler_code",
        "fix_data_wrangler_code",
        "explain_data_wrangler_code"
      ),
      edges = list(
        c("recommend_wrangling_steps", "create_data_wrangler_code"),
        c("create_data_wrangler_code", "execute_data_wrangler_code"),
        c("fix_data_wrangler_code", "execute_data_wrangler_code"),
        c("explain_data_wrangler_code", "__end__")
      ),
      conditional_edges = list(
        list(
          from = "execute_data_wrangler_code",
          condition = route_label,
          mapping = list(
            retry = "fix_data_wrangler_code",
            done = "explain_data_wrangler_code"
          )
        )
      ),
      subgraphs = list(
        "Planning" = c("recommend_wrangling_steps", "create_data_wrangler_code"),
        "Execution Loop" = c("execute_data_wrangler_code", "fix_data_wrangler_code"),
        "Reporting" = c("explain_data_wrangler_code")
      )
    ),
    list(
      id = "forecasting-agent",
      entry_point = "recommend_forecasting_steps",
      nodes = c(
        "recommend_forecasting_steps",
        "create_forecasting_code",
        "execute_forecasting_code",
        "fix_forecasting_code",
        "explain_forecasting_code"
      ),
      edges = list(
        c("recommend_forecasting_steps", "create_forecasting_code"),
        c("create_forecasting_code", "execute_forecasting_code"),
        c("fix_forecasting_code", "execute_forecasting_code"),
        c("explain_forecasting_code", "__end__")
      ),
      conditional_edges = list(
        list(
          from = "execute_forecasting_code",
          condition = route_label,
          mapping = list(
            retry = "fix_forecasting_code",
            done = "explain_forecasting_code"
          )
        )
      ),
      subgraphs = list(
        "Planning" = c("recommend_forecasting_steps", "create_forecasting_code"),
        "Execution Loop" = c("execute_forecasting_code", "fix_forecasting_code"),
        "Reporting" = c("explain_forecasting_code")
      )
    ),
    list(
      id = "feature-engineering-agent",
      entry_point = "recommend_feature_engineering_steps",
      nodes = c(
        "recommend_feature_engineering_steps",
        "create_feature_engineering_code",
        "execute_feature_engineering_code",
        "fix_feature_engineering_code",
        "explain_feature_engineering_code"
      ),
      edges = list(
        c("recommend_feature_engineering_steps", "create_feature_engineering_code"),
        c("create_feature_engineering_code", "execute_feature_engineering_code"),
        c("fix_feature_engineering_code", "execute_feature_engineering_code"),
        c("explain_feature_engineering_code", "__end__")
      ),
      conditional_edges = list(
        list(
          from = "execute_feature_engineering_code",
          condition = route_label,
          mapping = list(
            retry = "fix_feature_engineering_code",
            done = "explain_feature_engineering_code"
          )
        )
      ),
      subgraphs = list(
        "Planning" = c("recommend_feature_engineering_steps", "create_feature_engineering_code"),
        "Execution Loop" = c("execute_feature_engineering_code", "fix_feature_engineering_code"),
        "Reporting" = c("explain_feature_engineering_code")
      )
    ),
    list(
      id = "visualization-agent",
      entry_point = "recommend_visualization_steps",
      nodes = c(
        "recommend_visualization_steps",
        "create_visualization_code",
        "execute_visualization_code",
        "fix_visualization_code",
        "explain_visualization_code"
      ),
      edges = list(
        c("recommend_visualization_steps", "create_visualization_code"),
        c("create_visualization_code", "execute_visualization_code"),
        c("fix_visualization_code", "execute_visualization_code"),
        c("explain_visualization_code", "__end__")
      ),
      conditional_edges = list(
        list(
          from = "execute_visualization_code",
          condition = route_label,
          mapping = list(
            retry = "fix_visualization_code",
            done = "explain_visualization_code"
          )
        )
      ),
      subgraphs = list(
        "Planning" = c("recommend_visualization_steps", "create_visualization_code"),
        "Execution Loop" = c("execute_visualization_code", "fix_visualization_code"),
        "Reporting" = c("explain_visualization_code")
      )
    )
  )
}

get_agent_ids <- function() {
  vapply(get_agent_workflow_specs(), `[[`, character(1), "id")
}

get_agent_workflow_spec <- function(agent_id) {
  specs <- get_agent_workflow_specs()
  ids <- vapply(specs, `[[`, character(1), "id")
  idx <- match(agent_id, ids)
  if (is.na(idx)) {
    stop(
      "Unknown agent_id: ", agent_id, ". Valid ids: ",
      paste(ids, collapse = ", ")
    )
  }
  specs[[idx]]
}

generate_agent_workflow_png <- function(
    agent_id,
    out_dir = file.path("pkgdown", "assets"),
    mmdc = NULL,
    direction = "LR",
    theme = "neutral",
    background = "white",
    width = 2200,
    quiet = TRUE) {

  spec <- get_agent_workflow_spec(agent_id)
  mmdc_bin <- resolve_mmdc_bin(mmdc)

  compiled <- compile_graph(
    node_functions = make_nodes(spec$nodes),
    entry_point = spec$entry_point,
    edges = spec$edges,
    conditional_edges = spec$conditional_edges,
    subgraphs = spec$subgraphs,
    direction = direction
  )

  out_file <- file.path(out_dir, paste0(spec$id, "-workflow.png"))
  save_mermaid_png(
    x = compiled,
    file = out_file,
    mmdc = mmdc_bin,
    theme = theme,
    background = background,
    width = width,
    quiet = quiet
  )

  message("saved: ", out_file)
  invisible(out_file)
}

generate_all_agent_workflow_pngs <- function(
    out_dir = file.path("pkgdown", "assets"),
    agent_ids = get_agent_ids(),
    mmdc = NULL,
    direction = "LR",
    theme = "neutral",
    background = "white",
    width = 2200,
    quiet = TRUE) {

  out <- lapply(agent_ids, function(agent_id) {
    generate_agent_workflow_png(
      agent_id = agent_id,
      out_dir = out_dir,
      mmdc = mmdc,
      direction = direction,
      theme = theme,
      background = background,
      width = width,
      quiet = quiet
    )
  })
  invisible(unlist(out))
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (!length(args)) {
    generate_all_agent_workflow_pngs()
  } else {
    generate_all_agent_workflow_pngs(agent_ids = args)
  }
}

