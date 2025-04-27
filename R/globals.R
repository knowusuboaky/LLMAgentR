#' @importFrom utils globalVariables
NULL

utils::globalVariables(
  c(
    # dplyr/tidyverse vars
    "group_col", "date_col", "value_col", ".value", "date_col_week",

    # modeltime outputs
    ".index", ".conf_lo",

    # plot variables
    "item_id", ".conf_hi", "id",

    # message
    "verbose"
  )
)
