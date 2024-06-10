################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

################################################################################
#### rulelist documentation
################################################################################

#' @name rulelist
#' @title Rulelist
#' @description
#' ## Structure
#'
#'   A `rulelist` is ordered list of rules stored as a dataframe. Each row,
#'   specifies a rule (LHS), expected outcome (RHS) and some other details.
#'
#'   It has these mandatory columns:
#'
#' - `rule_nbr`: (integer vector) Rule number
#' - `LHS`: (character vector) A rule is a string that can be parsed using [base::parse()]
#' - `RHS`: (character vector or a literal)
#'
#' ## Example
#'
#' ```
#' | rule_nbr|LHS                                                                  |RHS       | support| confidence|     lift|
#' |--------:|:--------------------------------------------------------------------|:---------|-------:|----------:|--------:|
#' |        1|( island %in% c('Biscoe') ) & ( flipper_length_mm > 203 )            |Gentoo    |     122|  1.0000000| 2.774193|
#' |        2|( island %in% c('Biscoe') ) & ( flipper_length_mm <= 203 )           |Adelie    |      46|  0.9565217| 2.164760|
#' |        3|( island %in% c('Dream', 'Torgersen') ) & ( bill_length_mm > 44.1 )  |Chinstrap |      65|  0.9538462| 4.825339|
#' |        4|( island %in% c('Dream', 'Torgersen') ) & ( bill_length_mm <= 44.1 ) |Adelie    |     111|  0.9459459| 2.140825|
#' ```
#'
#' ## Create a rulelist
#'
#'   A `rulelist` can be created using [tidy()] on some supported model fits
#'   (run: `utils::methods(tidy)`). It can also be created manually from a
#'   existing dataframe using [as_rulelist][as_rulelist.data.frame].
#'
#' ## Keys and attributes
#'
#'   Columns identified as 'keys' along with `rule_nbr` form a unique
#'   combination
#' -- a group of rules. For example, rule-based C5 model with multiple trials
#'   creates rules per each `trial_nbr`. `predict` method understands 'keys',
#'   thereby provides/predicts a rule number (for each row in new data / test
#'   data) within the same `trial_nbr`.
#'
#'   A rulelist has these mandatory attributes:
#' - `estimation_type`: One among `regression`, `classification`
#'
#'   A rulelist has these optional attributes:
#' - `keys`: (character vector)Names of the column that forms a key.
#' - `model_type`: (string) Name of the model
#'
#' ## Methods for rulelist
#'
#'   1. [Predict][predict.rulelist]: Given a dataframe (possibly without a
#'   dependent variable column aka 'test data'), predicts the first rule (as
#'   ordered in the rulelist) per 'keys' that is applicable for each row. When
#'   `multiple = TRUE`, returns all rules applicable for a row (per key).
#'
#'   2. [Augment][augment.rulelist]: Given a dataframe (with dependent variable
#'   column, aka validation data), creates summary statistics per rule and
#'   returns a rulelist with a new dataframe-column.
#'
#'   ## Manipulating a rulelist
#'
#'   Rulelists are essentially dataframes. Hence, any dataframe operations which
#'   preferably preserve attributes will output a rulelist. [as_rulelist] and
#'   [as.data.frame] will help in moving back and forth between rulelist and
#'   dataframe worlds.
#'
#'   ## Utilities for a rulelist
#'
#'   1. [as_rulelist][as_rulelist.data.frame]: Create a `rulelist` from a
#'   dataframe with some mandatory columns. 2. [set_keys]: Set or Unset 'keys'
#'   of a `rulelist`. 3. [to_sql_case]: Outputs a SQL case statement for a
#'   `rulelist`. 4. [convert_rule_flavor]: Converts `R`-parsable rule strings to
#'   python/SQL parsable rule strings.
#'
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist]
identity # just a placeholder for 'rulelist' documentation, not exported


################################################################################
#### print
################################################################################

#' @name print.rulelist
#' @title Print method for [rulelist] class
#' @description Prints [rulelist] attributes and first few rows.
#' @param x A [rulelist] object
#' @param ... Passed to `tidytable::print`
#' @return input [rulelist] (invisibly)
#' @seealso [rulelist], [tidy], [augment][augment.rulelist], [predict][predict.rulelist]
#' @family Core Rulelist Utility
#' @export
print.rulelist = function(x, ...){
  keys = attr(x, "keys")

  cli::cli_rule(left = "Rulelist")

  if (is.null(keys)) {
    cli::cli_alert_info("{.emph keys}: {.strong NULL}")
  } else {
    cli::cli_alert_info("{.emph keys}: {.val {keys}}")
    n_combo = nrow(distinct(select(x, all_of(keys))))
    cli::cli_alert_info("{.emph Number of distinct keys}: {.val {n_combo}}")
  }

  cli::cli_alert_info("{.emph Number of rules}: {.val {nrow(x)}}")

  model_type = attr(x, 'model_type')
  if (is.null(model_type)){
    cli::cli_alert_info("{.emph Model type}: {.strong NULL}")
  } else {
    cli::cli_alert_info("{.emph Model type}: {.val {model_type}}")
  }

  estimation_type = attr(x, 'estimation_type')
  if (is.null(estimation_type)){
    cli::cli_alert_info("{.emph estimation type}: {.strong NULL}")
  } else {
    cli::cli_alert_info("{.emph estimation type}: {.val {estimation_type}}")
  }
  cli::cli_text("")

  class(x) = setdiff(class(x), "rulelist")
  print(x, ...)
  class(x) = c("rulelist", class(x))

  return(invisible(x))
}

################################################################################
#### predict
################################################################################

#' @keywords internal
#' @name predict_all_nokeys_rulelist
#' @title as the name says
#' @description as the name says
#' @param rulelist rulelist
#' @param new_data new_data
#' @return dataframe
# Not to be exported
predict_all_nokeys_rulelist = function(rulelist, new_data){

  # new_data is expected to inherit "data.table"
  new_data2 = rlang::duplicate(new_data)
  new_data2[["row_nbr"]] = 1:nrow(new_data2)

  out = vector("list", nrow(rulelist))
  for (rn in 1:nrow(rulelist)) {

    mask = eval(parse(text = rulelist$LHS[rn]), new_data2)
    mask = ifelse(is.na(mask), FALSE, mask)
    out[[rn]] = new_data2$row_nbr[mask]
  }

  res =
    tidytable::tidytable(rule_nbr = 1:nrow(rulelist),
                         row_nbr = out
                         ) %>%
    unnest(row_nbr, keep_empty = TRUE) %>%
    tidytable::full_join(tidytable::tidytable(row_nbr = 1:nrow(new_data)))

  return(res)
}

#' @keywords internal
#' @name predict_all_rulelist
#' @title with or without keys
#' @description uses predict_all_nokeys_rulelist
#' @param rulelist rulelist
#' @param new_data new_data
#' @return dataframe
# Not to be exported
predict_all_rulelist = function(rulelist, new_data){

  new_data = data.table::as.data.table(new_data)
  keys = attr(rulelist, "keys", exact = TRUE)

  if (is.null(keys)) {

    res =
      predict_all_nokeys_rulelist(rulelist, new_data) %>%
      arrange(row_nbr) %>%
      select(row_nbr, rule_nbr) %>%
      nest(.by = row_nbr, .key = "rule_nbr") %>%
      mutate(rule_nbr = purrr::map(rule_nbr, ~ .x[[1]]))

  } else {

    res =
      rulelist %>%
      as.data.frame() %>%
      nest(data = tidytable::everything(), .by = keys) %>%
      mutate(rn_df = purrr::map(data, ~ predict_all_nokeys_rulelist(.x, new_data))) %>%
      select(-data) %>%
      unnest(rn_df) %>%
      drop_na(row_nbr) %>%
      select(all_of(c("row_nbr", keys, "rule_nbr"))) %>%
      arrange(!!!rlang::syms(c("row_nbr", keys, "rule_nbr"))) %>%
      nest(.by = c("row_nbr", keys), .key = "rule_nbr") %>%
      mutate(rule_nbr = purrr::map(rule_nbr, ~ .x[[1]]))
  }

  return(res)
}

#' @keywords internal
#' @name predict_nokeys_rulelist
#' @title as the name says
#' @description as the name says
#' @param rulelist rulelist
#' @param new_data new_data
#' @return dataframe
# not to be exported
predict_nokeys_rulelist = function(rulelist, new_data){

  # new_data is expected to inherit "data.table"
  new_data2 = rlang::duplicate(new_data)
  new_data2[["row_nbr"]] = 1:nrow(new_data2)

  out = vector("list", nrow(rulelist))
  for (rn in 1:nrow(rulelist)) {

    mask = eval(parse(text = rulelist$LHS[rn]), new_data2)
    mask = ifelse(is.na(mask), FALSE, mask)
    out[[rn]] = new_data2$row_nbr[mask]

    if (sum(mask > 0)) {
      new_data2 = new_data2[!mask]
    }

    if (nrow(new_data2) == 0) {
      break
    }
  }

  res =
    tidytable::tidytable(rule_nbr = 1:nrow(rulelist),
                         row_nbr = out
                         ) %>%
    unnest(row_nbr, keep_empty = TRUE) %>%
    tidytable::full_join(tidytable::tidytable(row_nbr = 1:nrow(new_data)))

  return(res)
}

#' @keywords internal
#' @name predict_rulelist
#' @title with or without keys
#' @description uses predict_nokeys_rulelist
#' @param rulelist rulelist
#' @param new_data new_data
#' @return dataframe
# Not to be exported
predict_rulelist = function(rulelist, new_data){

  new_data = data.table::as.data.table(new_data)
  keys = attr(rulelist, "keys", exact = TRUE)

  if (is.null(keys)) {

    res =
      predict_nokeys_rulelist(rulelist, new_data) %>%
      arrange(row_nbr) %>%
      select(row_nbr, rule_nbr)

  } else {

    res =
      rulelist %>%
      as.data.frame() %>%
      nest(data = tidytable::everything(), .by = keys) %>%
      mutate(rn_df = purrr::map(data, ~ predict_nokeys_rulelist(.x, new_data))) %>%
      select(-data) %>%
      unnest(rn_df) %>%
      drop_na(row_nbr) %>%
      select(all_of(c("row_nbr", keys, "rule_nbr"))) %>%
      arrange(!!!rlang::syms(c("row_nbr", keys, "rule_nbr")))
  }

  return(res)
}

#' @name predict.rulelist
#' @title `predict` method for a [rulelist]
#' @description Predicts `rule_nbr` applicable (as per the order in rulelist)
#'   for a `row_nbr` (per key) in new_data
#' @param object A [rulelist]
#' @param new_data (dataframe)
#' @param multiple (flag, default: FALSE) Whether to output all rule numbers
#'   applicable for a row. If FALSE, the first satisfying rule is provided.
#' @param ... unused
#' @return dataframe. See **Details**.
#' @details If a `row_nbr` is covered more than one `rule_nbr` per 'keys', then
#' `rule_nbr` appearing earlier (as in row order of the [rulelist]) takes
#' precedence.
#'   ## Output Format
#' - When multiple is `FALSE`(default), output is a dataframe with three
#' or more columns: `row_number` (int), columns corresponding to 'keys',
#' `rule_nbr` (int).
#'
#' - When multiple is `TRUE`(default), output is a tidytable/dataframe with three
#' or more columns: `row_number` (int), columns corresponding to 'keys',
#' `rule_nbr` (list column of integers).
#'
#' - If a row number and 'keys' combination is not covered by any rule, then `rule_nbr` column has missing value.
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
#' output_2 = predict(tidy_c5, palmerpenguins::penguins, multiple = TRUE)
#' output_2 # `rule_nbr` is a list-column of integer vectors
#' @seealso [rulelist], [tidy], [augment][augment.rulelist], [predict][predict.rulelist]
#' @importFrom stats predict
#' @family Core Rulelist Utility
#' @export
predict.rulelist = function(object, new_data, multiple = FALSE, ...){

  checkmate::assert_data_frame(new_data)
  checkmate::assert_flag(multiple)

  if (multiple) {
    res = predict_all_rulelist(object, new_data)
  } else {
    res = predict_rulelist(object, new_data)
  }

  return(res)
}

################################################################################
#### coerce from dataframe
################################################################################

#' @name as_rulelist
#' @title as_rulelist generic from [tidyrules][package_tidyrules] package
#' @description as_rulelist generic
#' @param x object to be coerced to a [rulelist]
#' @param ... for methods to use
#' @return A [rulelist]
#' @seealso [rulelist], [tidy], [augment][augment.rulelist], [predict][predict.rulelist]
#' @family Core Rulelist Utility
#' @export
as_rulelist = function(x, ...){
  UseMethod("as_rulelist", x)
}

#' @name as_rulelist.data.frame
#' @title as_rulelist method for a data.frame
#' @description Convert a set of rules in a dataframe to a [rulelist]
#' @param x dataframe to be coerced to a [rulelist]
#' @param keys (character vector, default: NULL) column names which form the key
#' @param model_type (string, default: NULL) Name of the model which generated
#'   the rules
#' @param estimation_type (string) One among: 'regression',
#'   'classification'
#' @param ... currently unused
#' @return [rulelist] object
#' @details Input dataframe should contain these columns: `rule_nbr`, `LHS`,
#' `RHS`. Providing other inputs helps augment better.
#' @examples
#' rules_df = tidytable::tidytable(rule_nbr = 1:2,
#'                                 LHS      = c("var_1 > 50", "var_2 < 30"),
#'                                 RHS      = c(2, 1)
#'                                 )
#' as_rulelist(rules_df, estimation_type = "regression")
#' @seealso [rulelist], [tidy], [augment][augment.rulelist], [predict][predict.rulelist]
#' @family Core Rulelist Utility
#' @export
as_rulelist.data.frame = function(x,
                                  keys = NULL,
                                  model_type = NULL,
                                  estimation_type,
                                  ...
                                  ){
  checkmate::assert_character(keys,
                              min.len = 1,
                              any.missing = FALSE,
                              unique = TRUE,
                              null.ok = TRUE
                              )

  # #### checks
  # 1. basic cols exist.
  # 2. keys are different from basic cols.
  # 3. key columns exist.
  # 4. key along with 'rule_nbr' form unique rows without missing values.
  # 5. rule_nbr (integerish), LHS(character), RHS(any vector) should not have
  #    missing values.
  # 6. 'estimation_type' should be one among: classification, regression

  # check on basic columns and 'key' columns
  basic_cols = c("rule_nbr", "LHS", "RHS")
  if (is.null(keys)) {
    checkmate::assert_subset(basic_cols, colnames(x))
    # create key combo
    key_combo_df = distinct(x, rule_nbr)

  } else {

    # keys should be different from basic cols
    if (length(intersect(keys, basic_cols)) > 0) {
      rlang::abort("keys should not one among: 'rule_nbr', 'LHS', 'RHS'")
    }
    # expected columns exist exist
    checkmate::assert_subset(c(basic_cols, keys), colnames(x))
    # create key combo
    key_combo_df = distinct(select(x, all_of(c("rule_nbr", keys))))
  }

  checkmate::assert_true(anyDuplicated(key_combo_df) == 0)
  checkmate::assert_false(anyNA(key_combo_df))

  checkmate::assert_integerish(x$rule_nbr, any.missing = FALSE)
  checkmate::assert_character(x$LHS, any.missing = FALSE)
  checkmate::assert_vector(x$RHS, any.missing = FALSE)

  checkmate::assert_string(model_type, null.ok = TRUE)
  checkmate::assert_string(estimation_type)
  checkmate::assert_subset(estimation_type, c("classification", "regression"))

  # set class and attributes
  res = rlang::duplicate(x)

  class(res) = c("rulelist", class(res))
  if (!is.null(model_type)) {
    attr(res, "model_type") = model_type
  }

  attr(res, "estimation_type") = estimation_type

  return(res)
}

################################################################################
#### set_keys
################################################################################

#' @name set_keys
#' @title Set keys for a [rulelist]
#' @description 'keys' are a set of column(s) whose unique combination
#'   identifies a group of rules in a [rulelist]. Methods like
#'   [predict.rulelist], [augment.rulelist] produce output per key combination.
#' @param x A [rulelist]
#' @param keys (character vector or NULL)
#' @return A [rulelist] object
#' @details A new [rulelist] is returned with attr `keys` is modified. The input
#'   [rulelist] object is unaltered.
#' @examples
#' model_c5 = C50::C5.0(Attrition ~., data = modeldata::attrition, rules = TRUE)
#' tidy_c5 = tidy(model_c5)
#' tidy_c5 # keys are: "trial_nbr"
#'
#' new_tidy_c5 = set_keys(tidy_c5, NULL) # remove all keys
#' new_tidy_c5
#' new_2_tidy_c5 = set_keys(new_tidy_c5, "trial_nbr") # set "trial_nbr" as key
#' new_2_tidy_c5
#'
#' # Note that `tidy_c5` and `new_tidy_c5` are not altered.
#' tidy_c5
#' new_tidy_c5
#' @seealso [rulelist], [tidy], [augment][augment.rulelist], [predict][predict.rulelist]
#' @family Core Rulelist Utility
#' @export
set_keys = function(x, keys){

  checkmate::assert_character(keys, null.ok = TRUE)
  if (!is.null(keys)){
    checkmate::assert_subset(keys, colnames(x))
    checkmate::assert_false(any(c("LHS", "RHS", "row_nbr") %in% keys))
  }
  res = rlang::duplicate(x)
  attr(res, "keys") = keys
  return(res)
}
