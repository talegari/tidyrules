################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

#' @name tidy.rpart
#' @title Obtain rules as a ruleset/tidytable from a rpart model
#' @description Each row corresponds to a rule. A rule can be copied into
#'   `dplyr::filter` to filter the observations corresponding to a rule
#' @param x rpart model
#' @param ... Other arguments (currently unused)
#' @details NOTE: For rpart rules, one should build the model without
#' \bold{ordered factor} variable. We recommend you to convert \bold{ordered
#' factor} to \bold{factor} or \bold{integer} class.
#' @return A tidytable where each row corresponds to a rule. The columns are:
#'   rule_nbr, LHS, RHS, support, confidence (for classification only), lift
#'   (for classification only)
#' @examples
#' rpart_class = rpart::rpart(Species ~ .,data = iris)
#' rpart_class
#' tidy(rpart_class)
#'
#' rpart_regr = rpart::rpart(Sepal.Length ~ .,data = iris)
#' rpart_regr
#' tidy(rpart_regr)
#' @export

tidy.rpart = function(x, ...){

  ##### assertions and prep ####################################################
  arguments = list(...)

  # supported 'rpart' classes
  method_rpart = x$method
  # classification: class, regression: anova
  checkmate::assert_choice(method_rpart, c("class", "anova"))

  # build with y = TRUE
  if (is.null(x$y)){
    rlang::abort("rpart model should be built using argument `y = TRUE`.")
  }

  # column names from the x: This will be used at the end to handle the
  # variables with a space
  col_names =
    attr(x$terms, which = "term.labels") %>%
    stringr::str_remove_all(pattern = "`")

  # throw error if there are consecutive spaces in the column names
  if (any(stringr::str_count(col_names, "  ") > 0)){
    rlang::abort(
      "Variable names should not have two or more consecutive spaces.")
  }

  #### core extraction work ####################################################

  # convert to class "party"
  party_obj = partykit::as.party(x)

  # extract rules
  rules =
    list.rules.party(party_obj) %>%
    stringr::str_replace_all(pattern = "\\\"","'") %>%
    stringr::str_remove_all(pattern = ", 'NA'") %>%
    stringr::str_remove_all(pattern = "'NA',") %>%
    stringr::str_remove_all(pattern = "'NA'") %>%
    stringr::str_squish() %>%
    stringr::str_split(" & ") %>%
    purrr::map(~ stringr::str_c("( ", .x, " )")) %>%
    purrr::map_chr(~ stringr::str_c(.x, collapse = " & "))

  terminal_nodes = partykit::nodeids(party_obj, terminal = TRUE)

  # create metrics df
  if (method_rpart == "class"){
    prevalence     = as.numeric(prop.table(table(x$y)))

    res =
      x$frame[terminal_nodes, c("n", "dev", "yval")] %>%
      tidytable::mutate(confidence = (n + 1 - dev) / (n + 2)) %>%
      tidytable::rename(support = n, predict_class = yval) %>%
      tidytable::mutate(RHS = attr(x, "ylevels")[predict_class]) %>%
      tidytable::mutate(prevalence = prevalence[predict_class]) %>%
      tidytable::mutate(lift = confidence / prevalence) %>%
      tidytable::mutate(LHS = rules)

  } else if (method_rpart == "anova"){
    res =
      x$frame[terminal_nodes, c("n","yval")] %>%
      tidytable::rename(support = n, RHS = yval) %>%
      tidytable::mutate(LHS = rules)
  }

  #### finalize output #########################################################

  # replace variable names with spaces within backquotes
  for (i in 1:length(col_names)) {
    res[["LHS"]] =
      stringr::str_replace_all(res[["LHS"]],
                               col_names[i],
                               addBackquotes(col_names[i])
                               )
  }

  #### return ##################################################################

  res[["rule_nbr"]] = 1:nrow(res)

  if (method_rpart == "class"){
    res =
      res %>%
      tidytable::select(rule_nbr, LHS, RHS, support, confidence, lift)

  } else if (method_rpart == "anova") {
    res =
      res %>%
      tidytable::select(rule_nbr, LHS, RHS, support)
  }

  class(res) = c("ruleset", class(res))

  attr(res, "keys")            = NULL
  attr(res, "model_type")      = "rpart"
  if (method_rpart == "class"){
    attr(res, "estimation_type") = "classification"
  } else if (method_rpart == "anova"){
    attr(res, "estimation_type") = "regression"
  }

  return(res)
}
