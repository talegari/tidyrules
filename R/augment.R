################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

#' @keywords internal
#' @name augment_class_no_keys
#' @title as the name says
#' @description as the name says
#' not to be exported
augment_class_no_keys = function(x, new_data, y_name, weight = 1L, ...){

  # raw predictions
  pred_df =
    x %>%
    select(rule_nbr, LHS) %>%
    predict(new_data, multiple = TRUE) %>%
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

  aggregatees_df =
    new_data_with_rule_nbr %>%
    # bring 'prevalence' column
    left_join(prevalence_df,by = eval(y_name)) %>%
    summarise(
      support = sum(weight__, na.rm = TRUE),
      confidence = weighted.mean(ifelse(is.na(eval(y_name) == RHS), FALSE, TRUE),
                                 weight__,
                                 na.rm = TRUE
                                 ),
      lift = weighted.mean(ifelse(is.na(eval(y_name) == RHS), FALSE, TRUE),
                           weight__,
                           na.rm = TRUE
                           ) / prevalence[1],
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
#' @name augment_class_keys
#' @title as the name says
#' @description as the name says
#' not to be exported
augment_class_keys = function(x, new_data, y_name, weight = 1L, ...){

  keys = attr(x, "keys")

  # raw predictions
  # columns: row_nbr, rule_nbr, `keys`
  pred_df =
    x %>%
    select(all_of(c("rule_nbr", "LHS", keys))) %>%
    predict(new_data, multi = TRUE) %>%
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

  # add aggregates at rule_nbr and 'keys' level
  aggregatees_df =
    new_data_with_rule_nbr %>%
    left_join(prevalence_df, by = c(keys, eval(y_name))) %>%
    summarise(
      support    = sum(weight__, na.rm = TRUE),
      confidence = weighted.mean(ifelse(is.na(eval(y_name) == RHS), FALSE, TRUE),
                                   weight__,
                                   na.rm = TRUE
                                   ),
      lift = weighted.mean(ifelse(is.na(eval(y_name) == RHS), FALSE, TRUE),
                           weight__,
                           na.rm = TRUE
                           ) / prevalence[1],
      ...,
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

#' @keywords internal
#' @name augment_regr_no_keys
#' @title as the name says
#' @description as the name says
#' not to be exported
augment_regr_no_keys = function(x, new_data, y_name, weight = 1L, ...){

  # raw predictions
  pred_df =
    x %>%
    select(rule_nbr, LHS) %>%
    predict(new_data, multiple = TRUE) %>%
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
augment_regr_keys = function(x, new_data, y_name, weight = 1L, ...){

  keys = attr(x, "keys")

  # raw predictions
  # columns: row_nbr, rule_nbr, `keys`
  pred_df =
    x %>%
    select(all_of(c("rule_nbr", "LHS", keys))) %>%
    predict(new_data, multi = TRUE) %>%
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
#' @param x A [rulelist]
#' @param ... For methods to use
#' @seealso [rulelist], [tidy], [augment][augment.rulelist], [predict][predict.rulelist]
#' @importFrom generics augment
#' @family Augment
#' @export
generics::augment

#' @name augment.rulelist
#' @title Augment a [rulelist]
#' @description `augment` outputs a [rulelist] with an additional column named
#'   `augmented_stats` based on summary statistics calculated using `new_data`.
#' @param x A [rulelist]
#' @param new_data (dataframe) with column named `y_name` present
#' @param y_name (string) Column name representing the dependent variable
#' @param weight (numeric, default: 1) Positive weight vector with length equal
#'   to one or number of rows of 'new_data'
#' @param ... (expressions) To be send to [tidytable::summarise] for custom
#'   aggregations. See examples.
#' @returns A [rulelist] with a new dataframe-column named `augmented_stats`.
#' @details The dataframe-column `augmented_stats` will have these columns
#' corresponding to the `estimation_type`:
#'
#' - For `regression`: `support`, `IQR`, `RMSE`
#' - For `classification`: `support`, `confidence`, `lift`
#'
#' All these metrics are computed in a weighted sense. Arg `weight` is 1 by
#' default.
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
#' tidy_c5  = tidy(model_c5)
#' tidy_c5
#'
#' # augment
#' augmented = augment(tidy_c5, new_data = att[!train_index, ], y_name = "Attrition")
#'
#' augmented %>%
#'   tidytable::unnest(augmented_stats, names_sep = "__") %>%
#'   tidytable::glimpse()
#'
#' # augment with custom aggregator
#' augmented =
#'   augment(tidy_c5,
#'           new_data = att[!train_index, ],
#'           y_name = "Attrition",
#'           output_counts = list(table(Attrition))
#'           )
#'
#' augmented %>%
#'   tidytable::unnest(augmented_stats, names_sep = "__") %>%
#'   tidytable::glimpse()
#'
#' # rpart ----
#' set.seed(100)
#' train_index = sample(c(TRUE, FALSE), nrow(iris), replace = TRUE)
#'
#' model_class_rpart = rpart::rpart(Species ~ ., data = iris[train_index, ])
#' tidy_class_rpart  = tidy(model_class_rpart)
#' tidy_class_rpart
#'
#' model_regr_rpart = rpart::rpart(Sepal.Length ~ ., data = iris[train_index, ])
#' tidy_regr_rpart  = tidy(model_regr_rpart)
#' tidy_regr_rpart
#'
#' #' augment (classification case)
#' augmented =
#'   augment(tidy_class_rpart,
#'           new_data = iris[!train_index, ],
#'           y_name = "Species"
#'           )
#' augmented
#'
#' augmented %>%
#'   tidytable::unnest(augmented_stats, names_sep = "__") %>%
#'   tidytable::glimpse()
#'
#' #' augment (regression case)
#' augmented =
#'   augment(tidy_regr_rpart,
#'           new_data = iris[!train_index, ],
#'           y_name = "Sepal.Length"
#'           )
#' augmented
#'
#' augmented %>%
#'   tidytable::unnest(augmented_stats, names_sep = "__") %>%
#'   tidytable::glimpse()
#'
#' # party ----
#' pen         = palmerpenguins::penguins
#' set.seed(100)
#' train_index = sample(c(TRUE, FALSE), nrow(pen), replace = TRUE)
#'
#' model_class_party = partykit::ctree(species ~ ., data = pen[train_index, ])
#' tidy_class_party  = tidy(model_class_party)
#' tidy_class_party
#'
#' model_regr_party = partykit::ctree(bill_length_mm ~ ., data = pen[train_index, ])
#' tidy_regr_party  = tidy(model_regr_party)
#' tidy_regr_party
#'
#' #' augment (classification case)
#' augmented =
#'   augment(tidy_class_party,
#'           new_data = pen[!train_index, ],
#'           y_name = "species"
#'           )
#' augmented
#'
#' augmented %>%
#'   tidytable::unnest(augmented_stats, names_sep = "__") %>%
#'   tidytable::glimpse()
#'
#' #' augment (regression case)
#' augmented =
#'   augment(tidy_regr_party,
#'           new_data = tidytable::drop_na(pen[!train_index, ], bill_length_mm),
#'           y_name = "bill_length_mm"
#'           )
#' augmented
#'
#' augmented %>%
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
#' tidy_cubist = tidy(model_cubist)
#' tidy_cubist
#'
#' augmented =
#'   augment(tidy_cubist,
#'           new_data = att[!train_index, ],
#'           y_name = "MonthlyIncome"
#'           )
#' augmented
#'
#' augmented %>%
#'   tidytable::unnest(augmented_stats, names_sep = "__") %>%
#'   tidytable::glimpse()
#'
#' @seealso [rulelist], [tidy], [augment][augment.rulelist], [predict][predict.rulelist]
#' @family Augment
#' @export
augment.rulelist = function(x, new_data, y_name, weight = 1L, ...){

  checkmate::assert_string(y_name)
  checkmate::assert_data_frame(new_data)
  checkmate::assert_true(y_name %in% colnames(new_data))
  checkmate::assert_numeric(weight,
                            lower = 1e-8,
                            finite = TRUE,
                            any.missing = FALSE
                            )
  checkmate::assert_true(length(weight) %in% c(1, nrow(new_data)))
  checkmate::assert_false(anyNA(new_data[[y_name]]))


  estimation_type = attr(x, "estimation_type")
  keys            = attr(x, "keys")

  if (is.null(keys)) {
    if (estimation_type == "classification"){
      res = augment_class_no_keys(x, new_data, y_name, weight, ...)
    } else if (estimation_type == "regression") {
      res = augment_regr_no_keys(x, new_data, y_name, weight, ...)
    } else {
      rlang::abort("unknown 'estimation_type'")
    }

  } else {

    if (estimation_type == "classification"){
      res = augment_class_keys(x, new_data, y_name, weight, ...)
    } else if (estimation_type == "regression") {
      res = augment_regr_keys(x, new_data, y_name, weight, ...)
    } else {
      rlang::abort("unknown 'estimation_type'")
    }
  }

  attr(res, "data") = new_data
  return(res)
}
