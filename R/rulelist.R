#*******************************************************************************
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
#*******************************************************************************

#### rulelist documentation ----

#' @name rulelist
#' @title Rulelist
#' @description ## Structure
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
#'   ## Create a rulelist
#'
#'   A `rulelist` can be created using [tidy()] on some supported model fits
#'   (run: `utils::methods(tidy)`). It can also be created manually from a
#'   existing dataframe using [as_rulelist][as_rulelist.data.frame].
#'
#'   ## Keys and attributes
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
#'   ## Set Validation data
#'
#'   This helps a few methods like [augment], [calculate], [prune], [reorder]
#'   require few additional attributes which can be set using
#'   [set_validation_data].
#'
#'   ## Methods for rulelist
#'
#'   1. [Predict][predict.rulelist]: Given a dataframe (possibly without a
#'   dependent variable column aka 'test data'), predicts the first rule (as
#'   ordered in the rulelist) per 'keys' that is applicable for each row. When
#'   `multiple = TRUE`, returns all rules applicable for a row (per key).
#'
#'   2. [Augment][augment.rulelist]: Outputs summary statistics per rule over
#'   validation data and returns a rulelist with a new dataframe-column.
#'
#'   3. [Calculate][calculate.rulelist]: Computes metrics for a rulelist in a
#'   cumulative manner such as `cumulative_coverage`, `cumulative_overlap`,
#'   `cumulative_accuracy`.
#'
#'   4. [Prune][prune.rulelist]: Suggests pruning a rulelist such that some
#'   expectation are met (based on metrics). Example: cumulative_coverage of 80%
#'   can be met with a first few rules.
#'
#'   5. [Reorder][reorder.rulelist]: Reorders a rulelist in order to maximize a
#'   metric.
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
#'   dataframe with some mandatory columns.
#'
#'   2. [set_keys]: Set or Unset 'keys' of a `rulelist`.
#'
#'   3. [to_sql_case]: Outputs a SQL case statement for a `rulelist`.
#'
#'   4. [convert_rule_flavor]: Converts `R`-parsable rule strings to python/SQL
#'   parsable rule strings.
#'
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder][reorder.rulelist]
identity # just a placeholder for 'rulelist' documentation, not exported



#### validate ----

#' @keywords internal
#' raises a meaningful error if there is a problem
#' else returns TRUE invisibly
#' not to be exported
validate_rulelist = function(x){

  checkmate::assert_class(x, "rulelist")

  keys            = attr(x, "keys")
  estimation_type = attr(x, "estimation_type")
  model_type      = attr(x, "model_type")
  validation_data = attr(x, "validation_data")
  y_name          = attr(x, "y_name")
  weight          = attr(x, "weight")

  x = as.data.frame(x)

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

  if (nrow(key_combo_df) != nrow(x)) {
    rlang::abort("`rule_nbr` and 'keys' (if any) together are not unique")
  }
  checkmate::assert_false(anyNA(key_combo_df))

  checkmate::assert_vector(x$rule_nbr, any.missing = FALSE)
  checkmate::assert_character(x$LHS, any.missing = FALSE)
  checkmate::assert_vector(x$RHS, any.missing = FALSE)

  checkmate::assert_string(model_type, null.ok = TRUE)

  checkmate::assert_string(estimation_type)
  checkmate::assert_subset(estimation_type, c("classification", "regression"))

  if (!is.null(validation_data)) {

    checkmate::assert_data_frame(validation_data)

    checkmate::assert_string(y_name)
    checkmate::assert_true(y_name %in% colnames(validation_data))

    if (estimation_type == "classification") {
      checkmate::assert_factor(validation_data[[y_name]], any.missing = FALSE)
    } else if (estimation_type == "regression") {
      checkmate::assert_numeric(validation_data[[y_name]], any.missing = FALSE)
    }

    checkmate::assert_numeric(weight,
                              lower = 0,
                              finite = TRUE,
                              any.missing = FALSE
                              )
    checkmate::assert_true(length(weight) %in% c(1, nrow(validation_data)))
  }

  return(invisible(TRUE))

}

#### as_rulelist ----

#' @name as_rulelist
#' @title as_rulelist generic from [tidyrules][package_tidyrules] package
#' @description as_rulelist generic
#' @param x object to be coerced to a [rulelist]
#' @param ... for methods to use
#' @return A [rulelist]
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder][reorder.rulelist]
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
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder][reorder.rulelist]
#' @export
as_rulelist.data.frame = function(x,
                                  keys = NULL,
                                  model_type = NULL,
                                  estimation_type,
                                  ...
                                  ){

  # set class and attributes
  res = rlang::duplicate(x)

  class(res) = c("rulelist", class(res))
  attr(res, "keys") = keys
  attr(res, "estimation_type") = estimation_type
  attr(res, "model_type") = model_type

  # validate rulelist
  validate_rulelist(res)

  return(res)
}

#### set_keys ----

#' @name set_keys
#' @title Set keys for a [rulelist]
#' @description 'keys' are a set of column(s) which identify a group of rules in
#'   a [rulelist]. Methods like [predict][predict.rulelist],
#'   [augment][augment.rulelist] produce output per key combination.
#'
#' @param x A [rulelist]
#' @param keys (character vector or NULL)
#' @param reset (flag) Whether to reset the keys to sequential numbers starting
#'   with 1 when `keys` is set to NULL
#'
#' @returns A [rulelist] object
#'
#' @details A new [rulelist] is returned with attr `keys` is modified. The input
#'   [rulelist] object is unaltered.
#'
#' @examples
#' model_c5 = C50::C5.0(Attrition ~., data = modeldata::attrition, rules = TRUE)
#' tidy_c5 = tidy(model_c5)
#' tidy_c5 # keys are: "trial_nbr"
#'
#' tidy_c5[["rule_nbr"]] = 1:nrow(tidy_c5)
#' new_tidy_c5 = set_keys(tidy_c5, NULL) # remove all keys
#' new_tidy_c5
#'
#' new_2_tidy_c5 = set_keys(new_tidy_c5, "trial_nbr") # set "trial_nbr" as key
#' new_2_tidy_c5
#'
#' # Note that `tidy_c5` and `new_tidy_c5` are not altered.
#' tidy_c5
#' new_tidy_c5
#'
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder][reorder.rulelist]
#' @family Core Rulelist Utility
#' @export
set_keys = function(x, keys, reset = FALSE){

  validate_rulelist(x)

  res = rlang::duplicate(x)
  attr(res, "keys") = keys

  if (is.null(keys) && reset){
    res[["rule_nbr"]] = 1:nrow(res)
    cli::cli_alert_info("`set_keys` has reset `rule_nbr` column to sequential integers starting with 1.")
  }

  validate_rulelist(res)

  return(res)
}

#### set_validation_data ----

#' @name set_validation_data
#' @title Add `validation_data` to a [rulelist]
#' @description Returns a [rulelist] with three new attributes set:
#'   `validation_data`, `y_name` and `weight`. Methods such as
#'   [augment][augment.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder] require this to be set.
#'
#' @param x A [rulelist]
#' @param validation_data (dataframe) Data to used for computing some metrics.
#'   It is expected to contain `y_name` column.
#' @param y_name (string) Name of the dependent variable column.
#' @param weight (non-negative numeric vector, default: 1) Weight per
#'   observation/row of `validation_data`. This is expected to have same length
#'   as the number of rows in `validation_data`. Only exception is when it is a
#'   single positive number, which means that all rows have equal weight.
#'
#' @returns A [rulelist] with some extra attributes set.
#'
#' @examples
#' att = modeldata::attrition
#' set.seed(100)
#' index = sample(c(TRUE, FALSE), nrow(att), replace = TRUE)
#' model_c5 = C50::C5.0(Attrition ~., data = att[index, ], rules = TRUE)
#'
#' tidy_c5 = tidy(model_c5)
#' tidy_c5
#'
#' tidy_c5_2 = set_validation_data(tidy_c5,
#'                                 validation_data = att[!index, ],
#'                                 y_name = "Attrition",
#'                                 weight = 1 # default
#'                                 )
#' tidy_c5_2
#' tidy_c5 # not altered
#'
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder][reorder.rulelist]
#' @family Core Rulelist Utility
#' @export
set_validation_data = function(x, validation_data, y_name, weight = 1){

    res = rlang::duplicate(x)

    checkmate::assert_data_frame(validation_data, null.ok = TRUE)
    if (!is.null(validation_data)) {
      attr(res, "validation_data") =
        data.table::as.data.table(validation_data)
    }

    attr(res, "y_name") = y_name
    attr(res, "weight") = weight

    validate_rulelist(x)

    return(res)
}

#### print ----

#' @name print.rulelist
#' @title Print method for [rulelist] class
#' @description Prints [rulelist] attributes and first few rows.
#' @param x A [rulelist] object
#' @param banner (flag, default: `TRUE`) Should the banner be displayed
#' @param ... Passed to `tidytable::print`
#' @return input [rulelist] (invisibly)
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder][reorder.rulelist]
#' @export
print.rulelist = function(x, banner = TRUE, ...){

  validate_rulelist(x)
  rulelist = rlang::duplicate(x)

  keys            = attr(rulelist, "keys")
  estimation_type = attr(rulelist, "estimation_type")
  model_type      = attr(rulelist, "model_type")
  validation_data = attr(rulelist, "validation_data")

  text = character(0)
  if (banner) {
    text = c(text, "---- Rulelist --------------------------------")
  }

  if (is.null(keys)) {
    text = c(text,
             paste(cli::symbol$play,
                   "Keys: NULL"
                   )
             )
  } else {
    text = c(text,
             paste(cli::symbol$play,
                   stringr::str_glue("Keys: {keys}")
                   )
             )
    n_combo = nrow(distinct(select(x, all_of(keys))))
    text = c(text,
             paste(cli::symbol$play,
                   stringr::str_glue("Number of distinct keys: {n_combo}")
                   )
             )
  }

  text = c(text,
           paste(cli::symbol$play,
                 stringr::str_glue("Number of rules: {nrow(x)}")
                 )
           )

  if (is.null(model_type)){
    text = c(text,
             paste(cli::symbol$play,
                   stringr::str_glue("Model Type: NULL")
                   )
             )
  } else {
    text = c(text,
             paste(cli::symbol$play,
                   stringr::str_glue("Model type: {model_type}")
                   )
             )
  }

  if (is.null(estimation_type)) {
    text = c(text,
             paste(cli::symbol$play,
                   stringr::str_glue("Estimation type: NULL")
                   )
             )
  } else {
    text = c(text,
             paste(cli::symbol$play,
                   stringr::str_glue("Estimation type: {estimation_type}")
                   )
             )
  }

  if (is.null(validation_data)) {
    text = c(text,
             paste(cli::symbol$play,
                   stringr::str_glue("Is validation data set: FALSE")
                   )
             )
  } else {
    text = c(text,
             paste(cli::symbol$play,
                   stringr::str_glue("Is validation data set: TRUE")
                   )
             )
  }

  print_output = capture.output(print(tibble::as_tibble(x), ...), file = NULL)
  text = c(text, "\n", utils::tail(print_output, -1))

  if (banner) {
    text = c(text, "----------------------------------------------")
  }
  cat(paste(text, collapse = "\n"))

  return(invisible(x))
}

#### plot ----

#' @name plot.rulelist
#' @title Plot method for rulelist
#' @description Plots a heatmap with `rule_nbr`'s on x-side and clusters of
#'   `row_nbr`'s on y-side of a binary matrix with 1 if a rule is applicable for
#'   a row.
#'
#' @param x A [rulelist]
#' @param thres_cluster_rows (positive integer) Maximum number of rows beyond
#'   which a x-side dendrogram is not computed
#' @param dist_metric (string or function, default: "jaccard") Distance metric
#'   for y-side (`rule_nbr`) passed to `method` argument of [proxy::dist]
#' @param ... Arguments to be passed to [pheatmap::pheatmap]
#'
#' @details Number of clusters is set to min(number of unique rows in the
#' row_nbr X rule_nbr matrix and thres_cluster_rows)
#'
#' @examples
#' library("magrittr")
#' att = modeldata::attrition
#' tidy_c5 =
#'   C50::C5.0(Attrition ~., data = att, rules = TRUE) %>%
#'   tidy() %>%
#'   set_validation_data(att, "Attrition") %>%
#'   set_keys(NULL)
#'
#' plot(tidy_c5)
#'
#' @export
plot.rulelist = function(x,
                         thres_cluster_rows = 1e3,
                         dist_metric = "jaccard",
                         ...
                         ){

  validate_rulelist(x)

  checkmate::assert_false(is.null(attr(x, "validation_data")))
  checkmate::assert_true(is.null(attr(x, "keys")))

  validation_data = attr(x, "validation_data")
  y_name = attr(x, "y_name")
  estimation_type = attr(x, "estimation_type")

  df_plot =
    x %>%
    predict(validation_data, multiple = TRUE) %>%
    unnest(rule_nbr) %>%
    drop_na(rule_nbr) %>%
    mutate(value = 1L) %>%
    left_join(validation_data %>%
                select(all_of(y_name)) %>%
                mutate(row_nbr = row_number()),
              by = "row_nbr"
              ) %>%
    mutate(rule_nbr = as.character(rule_nbr)) %>%
    tidytable::pivot_wider(names_from = rule_nbr,
                           values_from = value,
                           values_fill = 0L
                           ) %>%
    arrange(row_nbr)

  mat_obj =
    df_plot %>%
    select(-all_of(c(y_name, "row_nbr"))) %>%
    as.matrix()

  rownames(mat_obj) = 1:nrow(mat_obj)

  row_df = data.frame("y_name" = df_plot[[y_name]])
  colnames(row_df) = y_name
  rownames(row_df) = rownames(mat_obj)

  if (estimation_type == "regression" && is.character(x$RHS)) {
    col_df = NA
  } else {
    col_df = select(as.data.frame(x), rule_nbr, RHS)
    colnames(col_df) = c("rule_nbr", y_name)
    rownames(col_df) = col_df[["rule_nbr"]]
    col_df[["rule_nbr"]] = NULL
  }
  n_unique_rows = nrow(unique(mat_obj))
  n_clusters = min(n_unique_rows, thres_cluster_rows)

  pheatmap::pheatmap(
      mat_obj,
      kmeans_k                 = min(n_unique_rows, n_clusters),
      clustering_distance_cols = proxy::dist(mat_obj,
                                             method = dist_metric,
                                             by_rows = FALSE
                                             ),
      annotation_row           = row_df,
      annotation_col           = col_df,
      ...
      )
}


#### predict ----

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

  # loop over rules and stotre covered rows
  out = vector("list", nrow(rulelist))
  for (rn in 1:nrow(rulelist)) {

    mask = eval(parse(text = rulelist$LHS[rn]), new_data2)
    mask = ifelse(is.na(mask), FALSE, mask)
    out[[rn]] = new_data2$row_nbr[mask]
  }

  # unnest row_nbr (list column of integers)
  res = tidytable::tidytable(rule_nbr = rulelist$rule_nbr,
                             row_nbr = out
                             )

  all_nested_row_nbrs_are_null = all(purrr::map_lgl(res$row_nbr, rlang::is_null))
  if (all_nested_row_nbrs_are_null) {
    res[["row_nbr"]] = NA_integer_
  } else {
    res = unnest(res, row_nbr)
  }

  res = tidytable::full_join(res,
                             tidytable::tidytable(row_nbr = 1:nrow(new_data))
                             )

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
  keys = attr(rulelist, "keys")

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
      nest(data__ = tidytable::everything(), .by = all_of(keys)) %>%
      mutate(rn_df__ =
               purrr::map(data__,
                          ~ predict_all_nokeys_rulelist(.x, new_data)
                          )
             ) %>%
      select(-data__) %>%
      unnest(rn_df__) %>%
      drop_na(row_nbr) %>%
      select(all_of(c("row_nbr", keys, "rule_nbr"))) %>%
      arrange(!!!rlang::syms(c("row_nbr", keys, "rule_nbr"))) %>%
      nest(.by = all_of(c("row_nbr", keys)), .key = "rule_nbr") %>%
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

  # loop through rules and keep removing covered rows from new_data2
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

  # unnest row_nbr (list column of integers)
  res = tidytable::tidytable(rule_nbr = rulelist$rule_nbr,
                             row_nbr = out
                             )

  all_nested_row_nbrs_are_null = all(purrr::map_lgl(res$row_nbr, rlang::is_null))
  if (all_nested_row_nbrs_are_null) {
    res[["row_nbr"]] = NA_integer_
  } else {
    res = unnest(res, row_nbr)
  }

  res = tidytable::full_join(res,
                             tidytable::tidytable(row_nbr = 1:nrow(new_data))
                             )

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
  keys = attr(rulelist, "keys")

  if (is.null(keys)) {

    res =
      predict_nokeys_rulelist(rulelist, new_data) %>%
      arrange(row_nbr) %>%
      select(row_nbr, rule_nbr)

  } else {

    res =
      rulelist %>%
      as.data.frame() %>%
      nest(data__ = tidytable::everything(), .by = all_of(keys)) %>%
      mutate(rn_df__ =
               purrr::map(data__, ~ predict_nokeys_rulelist(.x, new_data))
             ) %>%
      select(-data__) %>%
      unnest(rn_df__) %>%
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
#'
#' @param object A [rulelist]
#' @param new_data (dataframe)
#' @param multiple (flag, default: FALSE) Whether to output all rule numbers
#'   applicable for a row. If FALSE, the first satisfying rule is provided.
#' @param ... unused
#'
#' @returns A  dataframe. See **Details**.
#'
#' @details If a `row_nbr` is covered more than one `rule_nbr` per 'keys', then
#'   `rule_nbr` appearing earlier (as in row order of the [rulelist]) takes
#'   precedence.
#'
#' ## Output Format
#'
#' - When multiple is `FALSE`(default), output is a dataframe with three
#'   or more columns: `row_number` (int), columns corresponding to 'keys',
#'   `rule_nbr` (int).
#'
#' - When multiple is `TRUE`, output is a dataframe with three
#'   or more columns: `row_number` (int), columns corresponding to 'keys',
#'   `rule_nbr` (list column of integers).
#'
#' - If a row number and 'keys' combination is not covered by any rule, then
#'   `rule_nbr` column has missing value.
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
#'
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder][reorder.rulelist]
#' @importFrom stats predict
#' @export
#'
predict.rulelist = function(object, new_data, multiple = FALSE, ...){

  validate_rulelist(object)
  checkmate::assert_data_frame(new_data)
  checkmate::assert_flag(multiple)

  if (multiple) {
    res = predict_all_rulelist(object, new_data)
  } else {
    res = predict_rulelist(object, new_data)
  }

  return(res)
}

#### augment ----

#' @keywords internal
#' @name augment_class_no_keys
#' @title as the name says
#' @description as the name says
#' not to be exported
augment_class_no_keys = function(x, new_data, y_name, weight, ...){

  # raw predictions
  pred_df =
    predict(x, new_data, multiple = TRUE) %>%
    unnest(rule_nbr) %>%
    select(row_nbr, rule_nbr)

  # new_data with rule_nbr and 'keys'
  new_data_with_rule_nbr =
    new_data %>%
    mutate(row_nbr = row_number()) %>%
    mutate(weight__ = local(weight)) %>%
    left_join(pred_df, by = "row_nbr") %>%
    left_join(select(x, rule_nbr, RHS), by = "rule_nbr")

  prevalence_df =
    new_data_with_rule_nbr %>%
    summarise(prevalence_0 = sum(weight__, na.rm = TRUE),
              .by = eval(y_name)
              ) %>%
    drop_na(prevalence_0) %>%
    mutate(prevalence = prevalence_0 / sum(prevalence_0)) %>%
    select(all_of(c(eval(y_name), "prevalence")))

  na_to_false = function(x) ifelse(is.na(x), FALSE, x)

  aggregatees_df =
    new_data_with_rule_nbr %>%
    # bring 'prevalence' column
    left_join(prevalence_df,by = eval(y_name)) %>%
    summarise(
      support = sum(weight__, na.rm = TRUE),
      confidence =
        ( as.character(.data[[y_name]]) == as.character(RHS) ) %>%
        na_to_false() %>%
        weighted.mean(weight__, na.rm = TRUE),
      prevalence = prevalence[1],
      .by = rule_nbr
      ) %>%
    mutate(lift = confidence / prevalence) %>%
    select(-prevalence) %>%
    nest(.by = rule_nbr, .key = "augmented_stats")

  # output has all columns of 'tidy' along with 'augment_stats'
  res =
    x %>%
    left_join(aggregatees_df, by = c("rule_nbr")) %>%
    arrange(rule_nbr)

  return(res)
}

#' @keywords internal
#' @name augment_class_keys
#' @title as the name says
#' @description as the name says
#' not to be exported
augment_class_keys = function(x, new_data, y_name, weight, ...){

  keys = attr(x, "keys")

  # raw predictions
  # columns: row_nbr, rule_nbr, `keys`
  pred_df =
    predict(x, new_data, multi = TRUE) %>%
    unnest(rule_nbr) # columns: row_nbr, rule_nbr, `keys`

  # new_data with rule_nbr and 'keys'
  # columns: row_nbr, rule_nbr, `keys`, RHS, columns of new_data
  new_data_with_rule_nbr =
    # new_data with row_nbr and weight__ columns
    new_data %>%
    mutate(row_nbr = row_number()) %>%
    mutate(weight__ = weight) %>%
    # bring rule_nbr, `keys` (multiple rows per row_nbr might get created)
    inner_join(pred_df, by = "row_nbr") %>%
    # bring RHS column from tidy object
    inner_join(select(x, all_of(c("rule_nbr", keys, "RHS"))),
               by = c(keys, "rule_nbr")
               )

  # prevalence per 'keys'
  prevalence_df =
    new_data_with_rule_nbr %>%
    summarise(prevalence_0 = sum(weight__, na.rm = TRUE),
                         .by = c(keys, eval(y_name))
                         ) %>%
    drop_na(prevalence_0) %>%
    mutate(prevalence = prevalence_0 / sum(prevalence_0, na.rm = TRUE),
                      .by = c(keys)
                      ) %>%
    select(all_of(c(keys, eval(y_name), "prevalence")))

  na_to_false = function(x) ifelse(is.na(x), FALSE, x)

  # add aggregates at rule_nbr and 'keys' level
  aggregatees_df =
    new_data_with_rule_nbr %>%
    left_join(prevalence_df, by = c(keys, eval(y_name))) %>%
    summarise(
      support    = sum(weight__, na.rm = TRUE),
      confidence =
        ( as.character(.data[[y_name]]) == as.character(RHS) ) %>%
        na_to_false() %>%
        weighted.mean(weight__, na.rm = TRUE),
      prevalence = prevalence[1],
      ...,
      .by = c(keys, "rule_nbr")
      ) %>%
    mutate(lift = confidence / prevalence) %>%
    select(-prevalence) %>%
    nest(.by = c("rule_nbr", keys), .key = "augmented_stats")

  # output has all columns of 'tidy' along with 'augment_stats'
  res =
    x %>%
    left_join(aggregatees_df, by = c("rule_nbr", keys)) %>%
    arrange(!!!rlang::syms(c(keys, "rule_nbr"))) %>%
    relocate(all_of(c("rule_nbr", keys)))

  return(res)
}

#' @keywords internal
#' @name augment_regr_no_keys
#' @title as the name says
#' @description as the name says
#' not to be exported
augment_regr_no_keys = function(x, new_data, y_name, weight, ...){

  # raw predictions
  pred_df =
    predict(x, new_data, multiple = TRUE) %>%
    unnest(rule_nbr) %>%
    select(row_nbr, rule_nbr)

  # new_data with rule_nbr and 'keys'
  new_data_with_rule_nbr =
    new_data %>%
    mutate(row_nbr = row_number()) %>%
    mutate(weight__ = local(weight)) %>%
    left_join(pred_df, by = "row_nbr") %>%
    left_join(select(x, rule_nbr, RHS), by = "rule_nbr")

  if (is.character(x$RHS)) {
    new_data_with_rule_nbr =
      new_data_with_rule_nbr %>%
      nest(.by = c("RHS", "row_nbr")) %>%
      mutate(RHS = purrr::map2_dbl(RHS,
                                   data,
                                   ~ eval(parse(text = .x), envir = .y)
                                   )
             ) %>%
      unnest(data)
  }

  aggregatees_df =
      new_data_with_rule_nbr %>%
      summarise(
        support  = sum(weight__, na.rm = TRUE),
        IQR      = DescTools::IQRw(.data[[y_name]], weight__, na.rm = TRUE),
        RMSE     = MetricsWeighted::rmse(actual = .data[[y_name]],
                                         predicted = RHS,
                                         w = weight__,
                                         na.rm = TRUE
                                         ),
        .by = rule_nbr
        ) %>%
      nest(.by = rule_nbr, .key = "augmented_stats")

  # output has all columns of 'tidy' along with 'augment_stats'
    res =
      x %>%
      left_join(aggregatees_df, by = c("rule_nbr")) %>%
      arrange(rule_nbr)

  return(res)
}

#' @keywords internal
#' @name augment_regr_keys
#' @title as the name says
#' @description as the name says
#' not to be exported
augment_regr_keys = function(x, new_data, y_name, weight, ...){

  keys = attr(x, "keys")

  # raw predictions
  # columns: row_nbr, rule_nbr, `keys`
  pred_df =
    predict(x, new_data, multi = TRUE) %>%
    unnest(rule_nbr) # columns: row_nbr, rule_nbr, `keys`

  # new_data with rule_nbr and 'keys'
  # columns: row_nbr, rule_nbr, `keys`, RHS, columns of new_data
  new_data_with_rule_nbr =
    # new_data with row_nbr and weight__ columns
    new_data %>%
    mutate(row_nbr = row_number()) %>%
    mutate(weight__ = weight) %>%
    # bring rule_nbr, `keys` (multiple rows per row_nbr might get created)
    inner_join(pred_df, by = "row_nbr") %>%
    # bring RHS column from tidy object
    inner_join(select(x, all_of(c("rule_nbr", keys, "RHS"))),
               by = c(keys, "rule_nbr")
               )

  if (is.character(x$RHS)) {
    new_data_with_rule_nbr =
      new_data_with_rule_nbr %>%
      nest(.by = c("RHS", keys, "row_nbr")) %>%
      mutate(RHS = purrr::map2_dbl(RHS,
                                   data,
                                   ~ eval(parse(text = .x), envir = .y)
                                   )
             ) %>%
      unnest(data)
  }

  aggregatees_df =
      new_data_with_rule_nbr %>%
      summarise(
        support  = sum(weight__, na.rm = TRUE),
        IQR      = DescTools::IQRw(.data[[y_name]], weight__, na.rm = TRUE),
        RMSE     = MetricsWeighted::rmse(actual = .data[[y_name]],
                                         predicted = RHS,
                                         w = weight__,
                                         na.rm = TRUE
                                         ),
        .by = c(keys, "rule_nbr")
        ) %>%
      nest(.by = c("rule_nbr", keys), .key = "augmented_stats")

  # output has all columns of 'tidy' along with 'augment_stats'
    res =
      x %>%
      left_join(aggregatees_df, by = c("rule_nbr", keys)) %>%
      arrange(!!!rlang::syms(c(keys, "rule_nbr"))) %>%
      relocate(all_of(c("rule_nbr", keys)))

  return(res)
}

#' @name augment
#' @title `augment` is re-export of [generics::augment] from
#'   [tidyrules][package_tidyrules] package
#' @description See [augment.rulelist]
#'
#' @param x A [rulelist]
#' @param ... For methods to use
#'
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder][reorder.rulelist]
#' @importFrom generics augment
#' @export
generics::augment

#' @name augment.rulelist
#' @title Augment a [rulelist]
#' @description `augment` outputs a [rulelist] with an additional column named
#'   `augmented_stats` based on summary statistics calculated using attribute
#'   `validation_data`.
#' @param x A [rulelist]
#' @param ... (expressions) To be send to [tidytable::summarise] for custom
#'   aggregations. See examples.
#' @returns A [rulelist] with a new dataframe-column named `augmented_stats`.
#' @details The dataframe-column `augmented_stats` will have these columns
#'   corresponding to the `estimation_type`:
#'
#' - For `regression`: `support`, `IQR`, `RMSE`
#' - For `classification`: `support`, `confidence`, `lift`
#'
#' along with custom aggregations.
#'
#'
#' @examples
#' # Examples for augment ------------------------------------------------------
#' library("magrittr")
#'
#' # C5 ----
#' att = modeldata::attrition
#' set.seed(100)
#' train_index = sample(c(TRUE, FALSE), nrow(att), replace = TRUE)
#'
#' model_c5 = C50::C5.0(Attrition ~., data = att[train_index, ], rules = TRUE)
#' tidy_c5  =
#'   model_c5 %>%
#'   tidy() %>%
#'   set_validation_data(att[!train_index, ], "Attrition")
#'
#' tidy_c5
#'
#' augment(tidy_c5) %>%
#'   tidytable::unnest(augmented_stats, names_sep = "__") %>%
#'   tidytable::glimpse()
#'
#' # augment with custom aggregator
#' augment(tidy_c5,output_counts = list(table(Attrition))) %>%
#'   tidytable::unnest(augmented_stats, names_sep = "__") %>%
#'   tidytable::glimpse()
#'
#' # rpart ----
#' set.seed(100)
#' train_index = sample(c(TRUE, FALSE), nrow(iris), replace = TRUE)
#'
#' model_class_rpart = rpart::rpart(Species ~ ., data = iris[train_index, ])
#' tidy_class_rpart  = tidy(model_class_rpart) %>%
#'   set_validation_data(iris[!train_index, ], "Species")
#' tidy_class_rpart
#'
#' model_regr_rpart = rpart::rpart(Sepal.Length ~ ., data = iris[train_index, ])
#' tidy_regr_rpart  = tidy(model_regr_rpart) %>%
#'   set_validation_data(iris[!train_index, ], "Sepal.Length")
#' tidy_regr_rpart
#'
#' # augment (classification case)
#' augment(tidy_class_rpart) %>%
#'   tidytable::unnest(augmented_stats, names_sep = "__") %>%
#'   tidytable::glimpse()
#'
#' # augment (regression case)
#' augment(tidy_regr_rpart) %>%
#'   tidytable::unnest(augmented_stats, names_sep = "__") %>%
#'   tidytable::glimpse()
#'
#' # party ----
#' pen = palmerpenguins::penguins %>%
#'   tidytable::drop_na(bill_length_mm)
#' set.seed(100)
#' train_index = sample(c(TRUE, FALSE), nrow(pen), replace = TRUE)
#'
#' model_class_party = partykit::ctree(species ~ ., data = pen[train_index, ])
#' tidy_class_party  = tidy(model_class_party) %>%
#'   set_validation_data(pen[!train_index, ], "species")
#' tidy_class_party
#'
#' model_regr_party =
#'   partykit::ctree(bill_length_mm ~ ., data = pen[train_index, ])
#' tidy_regr_party  = tidy(model_regr_party) %>%
#'   set_validation_data(pen[!train_index, ], "bill_length_mm")
#' tidy_regr_party
#'
#' # augment (classification case)
#' augment(tidy_class_party) %>%
#'   tidytable::unnest(augmented_stats, names_sep = "__") %>%
#'   tidytable::glimpse()
#'
#' # augment (regression case)
#' augment(tidy_regr_party) %>%
#'   tidytable::unnest(augmented_stats, names_sep = "__") %>%
#'   tidytable::glimpse()
#'
#' # cubist ----
#' att         = modeldata::attrition
#' set.seed(100)
#' train_index = sample(c(TRUE, FALSE), nrow(att), replace = TRUE)
#' cols_att    = setdiff(colnames(att), c("MonthlyIncome", "Attrition"))
#'
#' model_cubist = Cubist::cubist(x = att[train_index, cols_att],
#'                               y = att[train_index, "MonthlyIncome"]
#'                               )
#'
#' tidy_cubist = tidy(model_cubist) %>%
#'   set_validation_data(att[!train_index, ], "MonthlyIncome")
#' tidy_cubist
#'
#' augment(tidy_cubist) %>%
#'   tidytable::unnest(augmented_stats, names_sep = "__") %>%
#'   tidytable::glimpse()
#'
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder][reorder.rulelist]
#' @export
augment.rulelist = function(x, ...){

  validate_rulelist(x)
  estimation_type = attr(x, "estimation_type")
  keys            = attr(x, "keys")

  if (is.null(keys)) {
    if (estimation_type == "classification"){
      res = augment_class_no_keys(x,
                                  attr(x, "validation_data"),
                                  attr(x, "y_name"),
                                  attr(x, "weight"),
                                  ...
                                  )
    } else if (estimation_type == "regression") {
      res = augment_regr_no_keys(x,
                                  attr(x, "validation_data"),
                                  attr(x, "y_name"),
                                  attr(x, "weight"),
                                  ...
                                 )
    }

  } else {

    if (estimation_type == "classification"){
      res = augment_class_keys(x,
                               attr(x, "validation_data"),
                               attr(x, "y_name"),
                               attr(x, "weight"),
                               ...
                               )
    } else if (estimation_type == "regression") {
      res = augment_regr_keys(x,
                              attr(x, "validation_data"),
                              attr(x, "y_name"),
                              attr(x, "weight"),
                              ...
                              )
    }
  }

  return(res)
}

#### metrics ----

#' @keywords internal
metric__cumulative_coverage = function(rulelist, new_data, y_name, weight){

  weight_df = tidytable::tidytable(row_nbr = 1:nrow(new_data), weight = weight)

  # loop over rules and get coverage
  predicted = predict(rulelist, new_data)
  in_union = integer(0)
  cum_weighted_coverage = numeric(nrow(rulelist))

  for (i in 1:nrow(rulelist)) {
    row_nbrs =
      predict(rulelist[i, ], new_data) %>%
      drop_na(rule_nbr) %>%
      pull(row_nbr)

    in_union = union(in_union, row_nbrs)
    cum_weighted_coverage[i] = sum(weight_df[row_nbr %in% in_union, ][["weight"]])
  }

  return(cum_weighted_coverage)
}

#' @keywords internal
metric__cumulative_accuracy = function(rulelist, new_data, y_name, weight){

  priority_df =
    rulelist %>%
    select(rule_nbr) %>%
    mutate(priority = 1:nrow(rulelist)) %>%
    select(rule_nbr, priority)

  pred_df =
    predict(rulelist, new_data) %>%
    mutate(weight = local(weight)) %>%
    left_join(priority_df, by = "rule_nbr") %>%
    select(rule_nbr, row_nbr, weight, priority)

  new_data2 =
    new_data %>%
    mutate(row_nbr = 1:n()) %>%
    select(all_of(c("row_nbr", y_name)))

  confidence_till_rule = function(rn){

    pred_df %>%
      tidytable::filter(priority <= rn) %>%
      left_join(new_data2, by = "row_nbr") %>%
      left_join(select(rulelist, rule_nbr, RHS), by = "rule_nbr") %>%
      mutate(hit = as.integer(RHS == .data[[y_name]])) %>%
      summarise(conf = weighted.mean(hit, weight, na.rm = TRUE)) %>%
      `[[`("conf")
  }

  res = purrr::map_dbl(1:nrow(rulelist), confidence_till_rule)
  return(res)
}

#' @keywords internal
metric__cumulative_RMSE = function(rulelist, new_data, y_name, weight){

  priority_df =
    rulelist %>%
    select(rule_nbr) %>%
    mutate(priority = 1:nrow(rulelist)) %>%
    select(rule_nbr, priority)

  pred_df =
    predict(rulelist, new_data) %>%
    left_join(priority_df, by = "rule_nbr") %>%
    mutate(weight = local(weight)) %>%
    select(rule_nbr, row_nbr, weight, priority)

  new_data2 =
    new_data %>%
    mutate(row_nbr = 1:n()) %>%
    select(all_of(c("row_nbr", y_name)))

  rmse_till_rule = function(rn){

    if (is.character(rulelist$RHS)) {
      inter_df =
        pred_df %>%
        tidytable::filter(priority <= rn) %>%
        left_join(mutate(new_data, row_nbr = 1:n()), by = "row_nbr") %>%
        left_join(select(rulelist, rule_nbr, RHS), by = "rule_nbr") %>%
        nest(.by = c("RHS", "rule_nbr", "row_nbr", "priority", "weight")) %>%
        mutate(RHS = purrr::map2_dbl(RHS,
                                     data,
                                     ~ eval(parse(text = .x), envir = .y)
                                     )
               ) %>%
        unnest(data)
    } else {

      inter_df =
        pred_df %>%
        tidytable::filter(priority <= rn) %>%
        left_join(new_data2, by = "row_nbr") %>%
        left_join(select(rulelist, rule_nbr, RHS), by = "rule_nbr")
    }

    inter_df %>%
      summarise(rmse = MetricsWeighted::rmse(RHS,
                                             .data[[y_name]],
                                             weight,
                                             na.rm = TRUE
                                             )
                ) %>%
      `[[`("rmse")
  }

  res = purrr::map_dbl(1:nrow(rulelist), rmse_till_rule)
  return(res)
}

#' @keywords internal
metric__cumulative_overlap = function(rulelist, new_data, y_name, weight){
  weight_df = tidytable::tidytable(row_nbr = 1:nrow(new_data), weight = weight)

  # loop over rules and get coverage
  predicted = predict(rulelist, new_data)
  in_union = integer(0)
  in_overlap = integer(0)
  cum_weighted_overlap = numeric(nrow(rulelist))

  for (i in 1:nrow(rulelist)) {
    row_nbrs =
      predict(rulelist[i, ], new_data) %>%
      drop_na(rule_nbr) %>%
      pull(row_nbr)

    in_overlap = union(in_overlap, intersect(in_union, row_nbrs))
    in_union   = union(in_union, row_nbrs)
    cum_weighted_overlap[i] = sum(weight_df[row_nbr %in% in_overlap, ][["weight"]])
  }

  return(cum_weighted_overlap)
}

#### calculate ----

#' @name calculate
#' @title `calculate` is re-export of [generics::calculate] from
#'   [tidyrules][package_tidyrules] package
#' @description See [calculate.rulelist]
#' @param x A [rulelist]
#' @param ... See [calculate.rulelist]
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder][reorder.rulelist]
#' @importFrom generics calculate
#' @export
#'
generics::calculate


#' @name calculate.rulelist
#' @title `calculate` metrics for a [rulelist]
#' @description Computes some metrics (based on `estimation_type`) in cumulative
#'   window function style over the rulelist (in the same order) ignoring the
#'   keys.
#'
#' @details ## Default Metrics
#' These metrics are calculated by default:
#'
#' - `cumulative_coverage`: For nth rule in the rulelist, number of distinct `row_nbr`s (of `new_data`) covered by nth and all preceding rules (in order). In weighted case, we sum the weights corresponding to the distinct `row_nbr`s.
#'
#' - `cumulative_overlap`: Up til nth rule in the rulelist, number of distinct `row_nbr`s (of `new_data`) already covered by some preceding rule (in order). In weighted case, we sum the weights corresponding to the distinct `row_nbr`s.
#'
#' For classification:
#'
#' - `cumulative_accuracy`: For nth rule in the rulelist, fraction of `row_nbr`s such that `RHS` matches the `y_name` column (of `new_data`) by nth and all preceding rules (in order). In weighted case, weighted accuracy is computed.
#'
#' For regression:
#'
#' - `cumulative_RMSE`: For nth rule in the rulelist, weighted RMSE of all predictions (`RHS`) predicted by nth rule and all preceding rules.
#'
#' ## Custom metrics
#'
#' Custom metrics to be computed should be passed a named list of function(s) in
#' `...`. The custom metric function should take these arguments in same order:
#' `rulelist`, `new_data`, `y_name`, `weight`. The custom metric function should
#' return a numeric vector of same length as the number of rows of rulelist.
#'
#' @param x A [rulelist]
#' @param metrics_to_exclude (character vector) Names of metrics to exclude
#'
#' @param ... Named list of custom metrics. See 'details'.
#'
#' @returns A dataframe of metrics with a `rule_nbr` column.
#'
#' @examples
#' library("magrittr")
#' model_c5  = C50::C5.0(Attrition ~., data = modeldata::attrition, rules = TRUE)
#' tidy_c5   = tidy(model_c5) %>%
#'             set_validation_data(modeldata::attrition, "Attrition") %>%
#'             set_keys(NULL)
#'
#' # calculate default metrics (classification)
#' calculate(tidy_c5)
#'
#' model_rpart = rpart::rpart(MonthlyIncome ~., data = modeldata::attrition)
#' tidy_rpart  =
#'   tidy(model_rpart) %>%
#'   set_validation_data(modeldata::attrition, "MonthlyIncome") %>%
#'   set_keys(NULL)
#'
#' # calculate default metrics (regression)
#' calculate(tidy_rpart)
#'
#' # calculate default metrics with a custom metric
#' #' custom function to get cumulative MAE
#' library("tidytable")
#' get_cumulative_MAE = function(rulelist, new_data, y_name, weight){
#'
#'   priority_df =
#'     rulelist %>%
#'     select(rule_nbr) %>%
#'     mutate(priority = 1:nrow(rulelist)) %>%
#'     select(rule_nbr, priority)
#'
#'   pred_df =
#'     predict(rulelist, new_data) %>%
#'     left_join(priority_df, by = "rule_nbr") %>%
#'     mutate(weight = local(weight)) %>%
#'     select(rule_nbr, row_nbr, weight, priority)
#'
#'   new_data2 =
#'     new_data %>%
#'     mutate(row_nbr = 1:n()) %>%
#'     select(all_of(c("row_nbr", y_name)))
#'
#'   rmse_till_rule = function(rn){
#'
#'     if (is.character(rulelist$RHS)) {
#'       inter_df =
#'         pred_df %>%
#'         tidytable::filter(priority <= rn) %>%
#'         left_join(mutate(new_data, row_nbr = 1:n()), by = "row_nbr") %>%
#'         left_join(select(rulelist, rule_nbr, RHS), by = "rule_nbr") %>%
#'         nest(.by = c("RHS", "rule_nbr", "row_nbr", "priority", "weight")) %>%
#'         mutate(RHS = purrr::map2_dbl(RHS,
#'                                      data,
#'                                      ~ eval(parse(text = .x), envir = .y)
#'                                      )
#'                ) %>%
#'         unnest(data)
#'     } else {
#'
#'       inter_df =
#'         pred_df %>%
#'         tidytable::filter(priority <= rn) %>%
#'         left_join(new_data2, by = "row_nbr") %>%
#'         left_join(select(rulelist, rule_nbr, RHS), by = "rule_nbr")
#'     }
#'
#'     inter_df %>%
#'       summarise(rmse = MetricsWeighted::mae(RHS,
#'                                              .data[[y_name]],
#'                                              weight,
#'                                              na.rm = TRUE
#'                                              )
#'                 ) %>%
#'       `[[`("rmse")
#'   }
#'
#'   res = purrr::map_dbl(1:nrow(rulelist), rmse_till_rule)
#'   return(res)
#' }
#'
#' calculate(tidy_rpart,
#'           metrics_to_exclude = NULL,
#'           list("cumulative_mae" = get_cumulative_MAE)
#'           )
#'
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder][reorder.rulelist]
#' @export
calculate.rulelist = function(x,
                              metrics_to_exclude = NULL,
                              ...
                              ){

  # checks
  validate_rulelist(x)

  if (is.null(attr(x, "validation_data"))) {
    cli::cli_alert_danger("validation_data is not present! Set using `set_validation_data`")
    rlang::abort()
  }
  checkmate::assert_character(metrics_to_exclude, null.ok = TRUE)

  # ignore keys
  keys = attr(x, "keys")
  if (!is.null(keys)) {
    cli::cli_alert_warning("'keys' will be ignored in `calculate`")
    if (inherits(try(set_keys(x, NULL), silent = TRUE), "try-error")) {
      x = set_keys(x, NULL, reset = TRUE)
    } else {
      x = set_keys(x, NULL)
    }
  }

  # set metric names to compute
  metric_names = c("cumulative_coverage", "cumulative_overlap")
  metric_names = switch(
    attr(x, "estimation_type"),
    classification = c(metric_names, "cumulative_accuracy"),
    regression = c(metric_names, "cumulative_RMSE")
    )
  metric_names = setdiff(metric_names, metrics_to_exclude)

  # compute metrics
  res = tidytable::tidytable(rule_nbr = x$rule_nbr)

  for (a_metric_name in metric_names) {
    metric_func = get(paste("metric", a_metric_name, sep = "__"))
    out = metric_func(x,
                      attr(x, "validation_data"),
                      attr(x, "y_name"),
                      attr(x, "weight")
                      )
    res[[a_metric_name]] = out
  }

  # compute udf metrics
  extra_metrics = list(...)
  if (length(extra_metrics)){
    extra_metrics = list(...)[[1]]
  }

  if (length(extra_metrics) > 0) {
    # should be a named list of functions
    checkmate::assert_list(extra_metrics,
                           any.missing = FALSE,
                           types = "function"
                           )
    checkmate::assert_names(names(extra_metrics), type = "unique")
    checkmate::assert(!any(names(extra_metrics) %in% metric_names))

    for (a_metric_name in names(extra_metrics)) {
      metric_func = extra_metrics[[a_metric_name]]
      out = metric_func(x,
                        attr(x, "validation_data"),
                        attr(x, "y_name"),
                        attr(x, "weight")
                        )
      res[[a_metric_name]] = out
    }
  }

  return(res)
}


#### prune ----

#' @name prune
#' @title `prune` is re-export of [generics::prune] from
#'   [tidyrules][package_tidyrules] package
#' @description See [prune.rulelist]
#' @param tree A [rulelist]
#' @param ... See [prune.rulelist]
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder][reorder.rulelist]
#' @importFrom generics prune
#' @export
generics::prune

#' @name prune.rulelist
#' @title `prune` rules of a [rulelist]
#' @description Prune the [rulelist] by suggesting to keep first 'k' rules based
#'   on metrics computed by [calculate][calculate.rulelist]
#'
#' @param tree A [rulelist]
#' @param metrics_to_exclude (character vector or NULL) Names of metrics not to
#'   be calculated. See [calculate][calculate.rulelist] for the list of default
#'   metrics.
#' @param stop_expr_string (string default: "relative__cumulative_coverage >=
#'   0.9") Parsable condition
#' @param min_n_rules (positive integer) Minimum number of rules to keep
#' @param ... Named list of custom metrics passed to
#'   [calculate][calculate.rulelist]
#'
#' @details 1. Metrics are computed using [calculate][calculate.rulelist]. 2.
#'   Relative metrics (prepended by 'relative__') are calculated by dividing
#'   each metric by its max value. 3. The first rule in rulelist order which
#'   meets the 'stop_expr_string' criteria is stored (say 'pos'). Print method
#'   suggests to keep rules until pos.
#'
#' @returns Object of class 'prune_ruleslist' with these components: 1. pruned:
#'   ruleset keeping only first 'pos' rows. 2. n_pruned_rules: pos. If stop
#'   criteria is never met, then pos = nrow(ruleset) 3. n_total_rules:
#'   nrow(ruleset), 4. metrics_df: Dataframe with metrics and relative metrics
#'   5. stop_expr_string
#'
#' @examples
#' library("magrittr")
#' model_c5  = C50::C5.0(Attrition ~., data = modeldata::attrition, rules = TRUE)
#' tidy_c5   = tidy(model_c5) %>%
#'             set_validation_data(modeldata::attrition, "Attrition") %>%
#'             set_keys(NULL)
#'
#' #' prune with defaults
#' prune_obj = prune(tidy_c5)
#' #' note that all other metrics are visible in the print output
#' prune_obj
#' plot(prune_obj)
#' prune_obj$pruned
#'
#' #' prune with a different stop_expr_string threshold
#' prune_obj = prune(tidy_c5,
#'                   stop_expr_string = "relative__cumulative_coverage >= 0.2"
#'                   )
#' prune_obj #' as expected, has smaller then 10 rules as compared to default args
#' plot(prune_obj)
#' prune_obj$pruned
#'
#' #' prune with a different stop_expr_string metric
#' st = "relative__cumulative_overlap <= 0.7 & relative__cumulative_overlap > 0"
#' prune_obj = prune(tidy_c5, stop_expr_string = st)
#' prune_obj #' as expected, has smaller then 10 rules as compared to default args
#' plot(prune_obj)
#' prune_obj$pruned
#'
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder][reorder.rulelist]
#' @export
prune.rulelist = function(
    tree,
    metrics_to_exclude = NULL,
    stop_expr_string = "relative__cumulative_coverage >= 0.9",
    min_n_rules = 1,
    ...
    ){
  x = tree
  validate_rulelist(x)

  # get metrics
  metrics_df = calculate(x, metrics_to_exclude, ...)

  # create "relative__" metrics with minmax scaling
  relative_metrics_df =
    metrics_df %>%
    mutate(across(-rule_nbr,
                  list(relative = ~ .x / max(.x, na.rm = TRUE)),
                  .names = "{.fn}__{.col}"
                  )
           )

  mask = eval(parse(text = stop_expr_string), envir = relative_metrics_df)
  pos  = purrr::detect_index(mask, isTRUE)
  if (pos == 0){ pos = nrow(x)}
  pos  = max(min_n_rules, pos)

  res = list(pruned           = head(x, pos),
             n_pruned_rules   = pos,
             n_total_rules    = nrow(x),
             metrics_df       = relative_metrics_df,
             stop_expr_string = stop_expr_string
             )
  class(res) = c("prune_rulelist", class(res))
  return(res)
}

#' @name print.prune_rulelist
#' @title Print method for `prune_rulelist` class
#' @description Print method for `prune_rulelist` class
#' @param x A 'prune_rulelist' object
#' @param ... unused
#' @export
print.prune_rulelist = function(x, ...) {

  cli::cli_rule("Prune Suggestion")
  cli::cli_text()
  if (x$n_pruned_rules < x$n_total_rules) {
    cli::cli_alert_success(
      glue::glue("Keep first {x$n_pruned_rules} out of {x$n_total_rules}"))
  } else {
    cli::cli_alert_danger(
      glue::glue("Stop criteria is not met. Pruning is not possible."))
  }

  cli::cli_text()
  cli::cli_alert_info(glue::glue("Metrics after {x$n_pruned_rules} rules: "))

  x$metrics_df %>%
    slice(x$n_pruned_rules) %>%
    tidytable::pivot_longer(-rule_nbr, names_to = "metric", values_to = "value") %>%
    print()

  cli::cli_text()
  cli::cli_alert_info("Run `plot(x)` for details; `x$pruned` to get pruned rulelist")
  cli::cli_rule()

  return(invisible(x))
}

#' @name plot.prune_rulelist
#' @title Plot method for `prune_rulelist` class
#' @description Plot method for `prune_rulelist` class
#' @param x A 'prune_rulelist' object
#' @param ... unused
#' @returns ggplot2 object (invisibly)
#' @export
plot.prune_rulelist = function(x, ...) {

  data_for_plot =
    x$metrics_df %>%
    mutate(rule_nbr = factor(as.character(rule_nbr),
                             levels = as.character(rule_nbr)
                              )
           ) %>%
    select(rule_nbr, tidytable::starts_with("relative__")) %>%
    pivot_longer(-rule_nbr, names_to = "metric", values_to = "value")

  n_total_rules  = x[["n_total_rules"]]
  n_pruned_rules = x[["n_pruned_rules"]]

  plot(x    = 1:n_total_rules,
       y    = runif(n_total_rules),
       type = "n", # No points initially
       xaxt = "n", # Don't draw x-axis yet
       yaxt = "n",
       xlab = "rule_nbr",
       ylab = "value"
       )

  for (metric_level in unique(data_for_plot$metric)) {

    subset = data_for_plot[data_for_plot$metric == metric_level,]
    # Plot lines with color based on metric
    lines(subset$rule_nbr,
          subset$value,
          col = which(unique(data_for_plot$metric) == metric_level),
          type = "l"
          )
  }
  axis(1, at = 1:n_total_rules, labels = unique(data_for_plot$rule_nbr), las = 2)
  axis(2, at = NULL, ylim = c(-0.1, 1.1))
  abline(v = n_pruned_rules, col = "darkgreen", lty = 3)

  legend("bottomright",
         legend = unique(data_for_plot$metric),
         col = 1:length(unique(data_for_plot$metric)),
         pch = 19
         )
}

#### reorder ----

#' @name reorder
#' @title reorder generic
#' @description reorder generic for rulelist
#' @param x A [rulelist]
#' @param ... See [reorder.rulelist]
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder][reorder.rulelist]
#' @importFrom stats reorder
#' @export
stats::reorder

#' @name reorder.rulelist
#' @title Reorder the rules/rows of a [rulelist]
#' @description Implements a greedy strategy to add one rule at a time which
#'   maximizes/minimizes a metric.
#'
#' @param x A [rulelist]
#' @param metric (character vector or named list) Name of metrics or a custom
#'   function(s). See [calculate][calculate.rulelist]. The 'n+1'th metric is
#'   used when there is a match at 'nth' level, similar to [base::order]. If
#'   there is a match at final level, row order of the rulelist comes into play.
#' @param minimize (logical vector) Whether to minimize. Either TRUE/FALSE or a
#'   logical vector of same length as metric
#' @param init (positive integer) Initial number of rows after which reordering
#'   should begin
#' @param ... passed to [calculate][calculate.rulelist]
#'
#' @examples
#' library("magrittr")
#' att = modeldata::attrition
#' tidy_c5 =
#'   C50::C5.0(Attrition ~., data = att, rules = TRUE) %>%
#'   tidy() %>%
#'   set_validation_data(att, "Attrition") %>%
#'   set_keys(NULL) %>%
#'   head(5)
#'
#' # with defaults
#' reorder(tidy_c5)
#'
#' # use 'cumulative_overlap' to break ties (if any)
#' reorder(tidy_c5, metric = c("cumulative_coverage", "cumulative_overlap"))
#'
#' # reorder after 2 rules
#' reorder(tidy_c5, init = 2)
#'
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder][reorder.rulelist]
#' @export
#'
reorder.rulelist = function(x,
                            metric = "cumulative_coverage",
                            minimize = FALSE,
                            init = NULL,
                            ...
                            ){
  # checks
  validate_rulelist(x)

  keys            = attr(x, "keys")
  validation_data = attr(x, "validation_data")
  estimation_type = attr(x, "estimation_type")
  model_type      = attr(x, "model_type")
  y_name          = attr(x, "y_name")
  weight          = attr(x, "weight")

  # ignore keys
  if (!is.null(keys)) {
    cli::cli_alert_warning("'keys' will be ignored in `reorder`")
    if (inherits(try(set_keys(x, NULL), silent = TRUE), "try-error")) {
      x = set_keys(x, NULL, reset = TRUE)
    } else {
      x = set_keys(x, NULL)
    }
    keys = attr(x, "keys")
  }

  checkmate::assert(!is.null(validation_data))
  checkmate::assert_character(metric)
  # checks:done

  # utility function to set to rulelist
  to_rulelist = function(df){
    df %>%
      as_rulelist(keys = keys,
                  model_type = model_type,
                  estimation_type = estimation_type
                  ) %>%
      set_validation_data(validation_data, y_name, weight)
  }

  # handle init
  checkmate::assert_integerish(init,
                               len = 1,
                               lower = 1,
                               upper = max(1, nrow(x) - 2),
                               null.ok = TRUE
                               )
  if (!is.null(init)) {
    reordered_df = as.data.frame(x[1:init, ])
    x = x[(init + 1):(nrow(x)), ] # x changes at this point
  } else {
    reordered_df = NULL
  }

  # core process
  splitted = split(as.data.frame(x), 1:nrow(x))
  reordered_metrics = vector("list", length = nrow(x))

  # wrapper where metric gets computed
  wrapper_metric_fun = function(single_rule_df){

    single_row_metric_df =
      reordered_df %>%
      bind_rows(single_rule_df) %>%
      to_rulelist() %>%
      calculate(...) %>%
      select(all_of(metric)) %>%
      tail(1)

    return(single_row_metric_df)
  }

  # get init metrics
  if (!is.null(init)) {
    init_metrics = calculate(to_rulelist(reordered_df), ...) %>%
      select(all_of(metric))
  } else {
    init_metrics = NULL
  }

  # loop through surviving rules
  cli::cli_progress_bar("Reordering ...", clear = FALSE)
  for (i in 1:nrow(x)) {
    rule_metrics           = purrr::map_dfr(splitted, wrapper_metric_fun)
    ord                    = do.call(base::order,
                                     c(rule_metrics,
                                       list(decreasing = !minimize)
                                       )
                                     )
    pos                    = which(ord == 1)
    reordered_metrics[[i]] = rule_metrics[pos, ]
    reordered_df           = bind_rows(reordered_df, splitted[[pos]])
    splitted[[pos]]        = NULL

    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  # put metrics df in same shape as x
  reordered_metrics =
    bind_rows(reordered_metrics) %>%
    bind_rows(init_metrics, .)

  # put metrics to the right of reordered x
  reordered_df = bind_cols(reordered_df, reordered_metrics)

  # return
  return(to_rulelist(reordered_df))
}
