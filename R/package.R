################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

#' @name package_tidyrules
#' @title `tidyrules`
#' @description `tidyrules` package provides a framework to work with decision
#'   rules. Rules can be extracted from supported models using [tidy], augmented
#'   using validation data by [augment][augment.rulelist], manipulated using
#'   standard dataframe operations, (modified) rulelists can be used to
#'   [predict][predict.rulelist] on unseen (test) data. Utilities include:
#'   Create a rulelist manually ([as_rulelist][as_rulelist.data.frame]), Export
#'   a rulelist to SQL ([to_sql_case]) and so on. The package offers two
#'   classes; [rulelist] and [ruleset] based on dataframe.
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist]
#' @importFrom magrittr %>%
#' @importFrom rlang %||%
#' @importFrom data.table :=
#' @importFrom utils data
#' @importFrom stats IQR
#' @importFrom stats weighted.mean
#' @importFrom tidytable select
#' @importFrom tidytable arrange
#' @importFrom tidytable mutate
#' @importFrom tidytable summarise
#' @importFrom tidytable distinct
#' @importFrom tidytable all_of
#' @importFrom tidytable n
#' @importFrom tidytable left_join
#' @importFrom tidytable right_join
#' @importFrom tidytable inner_join
#' @importFrom tidytable inner_join
#' @importFrom tidytable nest
#' @importFrom tidytable unnest
#' @importFrom tidytable row_number
#' @importFrom tidytable drop_na
#' @importFrom tidytable relocate
#' @importFrom tidytable bind_rows
#' @importFrom tidytable pull
#' @importFrom tidytable slice
#' @importFrom tidytable pivot_wider
#' @importFrom tidytable pivot_longer
#' @importFrom tidytable bind_cols
#' @importFrom tidytable across
#' @importFrom graphics abline
#' @importFrom graphics axis
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom stats runif
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom utils capture.output
#'
"_PACKAGE"

list.rules.party = getFromNamespace(".list.rules.party", "partykit")

utils::globalVariables(c(".",
                         "LHS",
                         "RHS",
                         "committee",
                         "desc",
                         "dev",
                         "lift",
                         "n",
                         "predict_class",
                         "rule_nbr",
                         "rule_number",
                         "support",
                         "trial_number",
                         "yval",
                         "confidence",
                         "rn__",
                         "row_nbr",
                         "pref__",
                         "data",
                         "weight",
                         "response",
                         "terminal_node_id",
                         "sum_weight",
                         "prevalence",
                         "winning_response",
                         "average",
                         "RMSE",
                         "weight__",
                         "prevalence_0",
                         ".data",
                         "rn_df",
                         "trial_nbr",
                         "error",
                         "data__",
                         "rn_df__",
                         "hit",
                         "priority",
                         "value"
                         )
                       )

