################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

#' @name print.ruleset
#' @title Print method for ruleset class
#' @description Prints 'keys' and ruleset as a tidytable
#' @param x A ruleset object
#' @param ... Passed to `tidytable::print`
#' @return Input (invisibly)
#' @export
print.ruleset = function(x, ...){

  rlang::inform(paste0("# A ruleset/tidytable with keys: ",
                       paste(attr(x, "keys"), collapse = ", ")
                       )
                )

  class(x) = setdiff(class(x), "ruleset")
  print(x, ...)
  class(x) = c("ruleset", class(x))

  return(invisible(x))
}

#' @name print.rulelist
#' @title Print method for rulelist class
#' @description Prints 'keys' and rulelist as a tidytable
#' @param x A rulelist object
#' @param ... Passed to `tidytable::print`
#' @return Input (invisibly)
#' @export
print.rulelist = function(x, ...){

  rlang::inform(paste0("# A rulelist/tidytable with keys: ",
                       paste(attr(x, "keys"), collapse = ", ")
                       )
                )

  class(x) = setdiff(class(x), "rulelist")
  print(x, ...)
  class(x) = c("rulelist", class(x))

  return(invisible(x))
}

#' @name predict_core
#' @title Core predict for ruleset/list/tidy set of rules
#' @description Core logic of predict method is written in a generic sense. This
#'   function will not be exposed at user level.
#' @param rules_df dataframe with at least two columns: `rule_nbr`, `LHS`.
#'   Should have 'keys' columns such that `rule_nbr` along with 'keys' columns
#'   form a unique combo per row
#' @param new_data Data to predict on
#' @return dataframe with these columns: `row_nbr` (integer), 'keys' columns,
#'   `rule_nbr` (list of integers)
#' @details If a row number is not covered under any rule, then it does not
#'   appear as a row in the output.
#' @examples
#' \dontrun{
#' library("magrittr")
#'
#' # ruleset case
#' rpart::rpart(Species ~ .,data = iris) %>%
#'   tidy() %>%
#'   dplyr::select(rule_nbr, LHS) %>%
#'   predict_core(iris)
#'
#' # rulelist case
#' C50::C5.0(species ~.,
#'           data = palmerpenguins::penguins,
#'           trials = 5,
#'           rules = TRUE
#'           ) %>%
#'   tidy() %>%
#'   dplyr::select(rule_nbr, trial_nbr, LHS) %>%
#'   predict_core(palmerpenguins::penguins)
#' }
predict_core = function(rules_df, new_data){

  # rules_df should have key column (if any), rule_nbr and LHS

  # keys and 'rule_nbr' should form unique combinations
  n_unique_combos = nrow(tidytable::distinct(rules_df, -LHS))

  if (nrow(rules_df) != n_unique_combos) {
    rlang::abort("'rule_nbr' and other keys should be unique combination")
  }

  keys = setdiff(colnames(rules_df), c("LHS", "rule_nbr"))

  new_data_with_rn = tidytable::mutate(new_data, rn__ = tidytable::row_number())

  # function to get row numbers for a given rule and dataset combo
  # return: integer vector or NULL
  get_rows_per_rule = function(rule_string, dataset){

    rule_expr = parse(text = rule_string)

    row_numbers =
      tidytable::filter(dataset, eval(rule_expr)) %>%
      tidytable::pull(rn__)

    return(row_numbers)
  }

  # function to get row numbers from a DF(chunk) of rules
  # chunk should have two columns: rule_nbr, LHS
  # returns a DF with two columns: rule_nbr, row_nbr
  get_rows_df = function(chunk){

    chunk %>%
      tidytable::mutate(row_nbr =
                          purrr::map(LHS,
                                     ~ get_rows_per_rule(.x, new_data_with_rn)
                                     )
                        ) %>%
      tidytable::select(rule_nbr, row_nbr)
  }

  res =
    rules_df %>%
    magrittr::set_class(setdiff(class(rules_df), c("rulelist", "ruleset"))) %>%
    tidytable::nest(.by = keys) %>%
    tidytable::mutate(data = purrr::map(data, get_rows_df)) %>%
    tidytable::unnest(data) %>%
    tidytable::unnest(row_nbr) %>%
    tidytable::nest(.by = -rule_nbr) %>%
    tidytable::mutate(rule_nbr = purrr::map(data, tidytable::pull)) %>%
    tidytable::select(c("row_nbr", keys, "rule_nbr"))

  return(res)
}

#' @name predict.rulelist
#' @title `predict` method for rulelist class
#' @description Returns the `rule_nbr` applicable for a `row_nbr` in new_data
#' @param object rulelist object
#' @param new_data dataframe to predict
#' @param raw (flag, default: FALSE) Whether raw prediction are to be provided
#' @param ... unused
#' @return A dataframe indicating `rule_nbr` applicable for a `row_nbr` in
#'   new_data
#' @details
#'
#' If a `row_nbr` is covered more than one `rule_nbr` per 'keys', then
#' `rule_nbr` appearing in the earlier (as in row order) takes precedence.
#'
#' When raw is `FALSE`(default), output is a tidytable/dataframe with three or
#' more columns: `row_number` (int), columns corresponding to 'keys', `rule_nbr`
#' (int). If a row number is not covered by any rule, then there is one row with
#' all other columns other than `row_nbr` has a missing value.
#'
#' When raw is `TRUE`(default), output is a tidytable/dataframe with three or
#' more columns: `row_number` (int), columns corresponding to 'keys', `rule_nbr`
#' (list of intergers). If a row number is not covered by any rule, then there
#' is no row corresponding the `row_nbr`.
#'
#' @examples
#' model_c5 = C50::C5.0(species ~.,
#'                      data = palmerpenguins::penguins,
#'                      trials = 5,
#'                      rules = TRUE
#'                      )
#' tidy_c5 = tidy(model_c5)
#' tidy_c5
#'
#' output_1 = predict(tidy_c5, palmerpenguins::penguins)
#' output_1 # different rules per 'keys' (`trial_nbr` here)
#'
#' output_2 = predict(tidy_c5, palmerpenguins::penguins, raw = TRUE)
#' output_2 # `rule_nbr` is a list-column of integer vectors
#'
#' @export
predict.rulelist = function(object, new_data, raw = FALSE, ...){

  class(object) = setdiff(class(object), c("rulelist", "ruleset"))
  keys = attr(object, "keys")

  if (raw){
    res =
      object %>%
        tidytable::select(c("rule_nbr", keys, "LHS")) %>%
        predict_core(new_data)

    return(res)
  }

  # idea
  # 1. Get pref order for rules within a key.
  # 2. merge with unnested raw predict.
  # 3. Per key, choose a row_nbr served by the most prefered rule
  # 4. Add empty rows for uncovered row numbers

  if (!is.null(keys)) {
    res =
      # 1. Get pref order for rules within a key.
      object %>%
      tidytable::select(c("rule_nbr", keys)) %>%
      tidytable::group_by(tidytable::all_of(keys)) %>%
      tidytable::mutate(pref__ = tidytable::row_number()) %>%
      tidytable::ungroup() %>%

      # 2. merge with unnested raw predict.
      tidytable::inner_join(
        object %>%
          tidytable::select(c("rule_nbr", keys, "LHS")) %>%
          predict_core(new_data) %>%
          tidytable::unnest(rule_nbr),

        by = c(keys, "rule_nbr")
      ) %>%

      # 3. Per key, choose a row_nbr served by the most prefered rule
      tidytable::slice_min(n = 1,
                           order_by = pref__,
                           by = c("row_nbr", keys)
                           ) %>%
      tidytable::select(c("row_nbr", keys, "rule_nbr")) %>%

      # 4. Add empty rows for uncovered row numbers
      tidytable::right_join(
        object %>%
        tidytable::distinct(keys) %>%
        tidytable::cross_join(tidytable::tidytable(row_nbr = 1:nrow(new_data))),
        by = c("row_nbr", keys)
        ) %>%
      tidytable::arrange(row_nbr)

  } else {

    res =
      # 1. Get pref order for rules within a key.
      object %>%
      tidytable::select("rule_nbr") %>%
      tidytable::mutate(pref__ = tidytable::row_number()) %>%

      # 2. merge with unnested raw predict.
      tidytable::inner_join(
        object %>%
          tidytable::select(c("rule_nbr", "LHS")) %>%
          predict_core(new_data) %>%
          tidytable::unnest(rule_nbr),

        by = "rule_nbr"
      ) %>%

      # 3. Per key, choose a row_nbr served by the most preferred rule
      tidytable::slice_min(n = 1,
                           order_by = pref__,
                           by = "row_nbr"
                           ) %>%
      tidytable::select(c("row_nbr", "rule_nbr")) %>%

      # 4. Add empty rows for uncovered row numbers
      tidytable::right_join(
        tidytable::tidytable(row_nbr = 1:nrow(new_data)),
        by = "row_nbr"
        ) %>%
      tidytable::arrange(row_nbr)
  }

  return(res)
}

#' @name predict.ruleset
#' @title `predict` method for ruleset class
#' @description Returns the `rule_nbr` applicable for a `row_nbr` in new_data
#' @param object ruleset object
#' @param new_data dataframe to predict
#' @param raw (flag, default: FALSE) Whether raw prediction are to be provided
#' @param ... unused
#' @return A dataframe indicating `rule_nbr` applicable for a `row_nbr` in
#'   new_data
#' @details
#'
#' A `row_nbr` is covered more than one `rule_nbr` per 'keys', results in error.
#'
#' When raw is `FALSE`(default), output is a tidytable/dataframe with three or
#' more columns: `row_number` (int), columns corresponding to`keys`, `rule_nbr`
#' (int). If a row number is not covered by any rule, then there is one row with
#' all other columns other than `row_nbr` has a missing value.
#'
#' When raw is `TRUE`(default), output is a tidytable/dataframe with three or
#' more columns: `row_number` (int), columns corresponding to`keys`, `rule_nbr`
#' (list of integers). If a row number is not covered by any rule, then there is
#' no row corresponding the `row_nbr`.
#'
#' @examples
#' model_rpart = rpart::rpart(species ~ .,
#'                            data = palmerpenguins::penguins
#'                            )
#' tidy_rpart = tidy(model_rpart)
#' tidy_rpart
#'
#' output_1 = predict(tidy_rpart, palmerpenguins::penguins)
#' output_1
#'
#' output_2 = predict(tidy_rpart, palmerpenguins::penguins, raw = TRUE)
#' output_2 # `rule_nbr` is a list-column of integer vectors
#'
#' @export
predict.ruleset = function(object, new_data, raw = FALSE, ...){

  class(object) = setdiff(class(object), c("rulelist", "ruleset"))
  keys = attr(object, "keys")

  res =
    object %>%
    tidytable::select(c("rule_nbr", keys, "LHS")) %>%
    predict_core(new_data)

  if (raw) return(res)

  if (any(purrr::map_int(res$rule_nbr, length)) > 1) {
    rlang::inform("Some rows are covered by more than one rule.")
    rlang::inform("Run predict with `raw = TRUE` to see those cases.")
    rlang::abort("A row should be not covered by more than one rule.")
  }

  res =
    res %>%
    tidytable::mutate(rule_nbr = purrr::list_simplify(rule_nbr, strict = TRUE))

  # Add empty rows for uncovered row numbers
  if (!is.null(keys)) {
    res =
      res %>%
      tidytable::right_join(
        object %>%
        tidytable::distinct(keys) %>%
        tidytable::cross_join(tidytable::tidytable(row_nbr = 1:nrow(new_data))),
        by = c("row_nbr", keys)
        ) %>%
      tidytable::arrange(row_nbr)

  } else {
    res =
      res %>%
      tidytable::right_join(
        tidytable::tidytable(row_nbr = 1:nrow(new_data)),
        by = "row_nbr"
      ) %>%
      tidytable::arrange(row_nbr)
  }
  return(res)
}
