#' @importFrom dplyr %>% rename mutate group_by summarise select ungroup matches bind_rows
#' @importFrom recipes all_predictors all_numeric_predictors all_outcomes all_nominal
#' @importFrom parsnip boost_tree rand_forest svm_rbf set_engine set_mode fit
#' @importFrom workflows workflow add_model add_recipe
#' @importFrom rsample training testing
#' @importFrom modeltime prophet_reg prophet_boost modeltime_table modeltime_calibrate modeltime_refit modeltime_forecast
#' @importFrom modeltime.ensemble ensemble_average
#' @importFrom utils str installed.packages
#' @importFrom timetk future_frame time_series_split step_timeseries_signature
#' @importFrom recipes recipe step_rm step_zv step_normalize step_mutate step_dummy update_role
#' @importFrom utils str head capture.output install.packages
#' @importFrom purrr map map_chr
#' @importFrom stats filter IQR quantile sd
#' @importFrom DBI dbListTables dbListFields dbGetQuery
#' @importFrom RSQLite SQLite
#' @importFrom utils download.file
NULL


