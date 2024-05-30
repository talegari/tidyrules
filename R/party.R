################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

#' @name tidy.constparty
#' @title Obtain rules as a ruleset/tidytable from a party model
#' @description Each row corresponds to a rule. A rule can be copied into
#'   `dplyr::filter` to filter the observations corresponding to a rule
#' @param x party model
#' @param ... Other arguments (currently unused)
#' @details These party models are supported: regression (y is numeric),
#'   classification (y is factor)
#' @return A tidytable where each row corresponds to a rule. The columns are:
#'   rule_nbr, LHS, RHS, support, confidence (for classification only), lift
#'   (for classification only)
#' @examples
#' model_party_cl = partykit::ctree(species ~ .,data = palmerpenguins::penguins)
#' model_party_cl
#' tidy(model_party_cl)
#'
#' model_party_re = partykit::ctree(bill_length_mm ~ .,
#'                                  data = palmerpenguins::penguins
#'                                  )
#' model_party_re
#' tidy(model_party_re)
#' @export

tidy.constparty = function(x, ...){

  ##### assertions and prep ####################################################
  arguments = list(...)

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

  # detect method using 'fitted'
  fitted_df           = tidytable::as_tidytable(x$fitted)
  colnames(fitted_df) = c("terminal_node_id", "weight", "response")
  fitted_df[["terminal_node_id"]] = as.character(fitted_df[["terminal_node_id"]])

  y_class = class(fitted_df[["response"]])
  if (y_class == "factor") {
    type = "classification"
  } else if (y_class %in% c("numeric", "integer")) {
    type = "regression"
  } else {
    rlang::inform("tidy supports only classification and regression 'party' models")
    rlang::abort("Unsupported party object")
  }

  #### core extraction work ####################################################

  # extract rules
  raw_rules = list.rules.party(x)

  rules_df =
    raw_rules %>%
    stringr::str_replace_all(pattern = "\\\"","'") %>%
    stringr::str_remove_all(pattern = ", 'NA'") %>%
    stringr::str_remove_all(pattern = "'NA',") %>%
    stringr::str_remove_all(pattern = "'NA'") %>%
    stringr::str_squish() %>%
    stringr::str_split(" & ") %>%
    purrr::map(~ stringr::str_c("( ", .x, " )")) %>%
    purrr::map_chr(~ stringr::str_c(.x, collapse = " & ")) %>%
    tidytable::tidytable(LHS = .) %>%
    tidytable::mutate(terminal_node_id = names(raw_rules))

  # create metrics df
  if (type == "classification"){

    terminal_response_df =
      fitted_df %>%
      tidytable::summarise(sum_weight = sum(weight, na.rm = TRUE),
                           .by = c(terminal_node_id, response)
                           ) %>%
      tidytable::slice_max(n = 1,
                           order_by = sum_weight,
                           by = terminal_node_id,
                           with_ties = FALSE
                           ) %>%
      tidytable::select(terminal_node_id,
                        winning_response = response
                        )

    prevalence_df =
      fitted_df %>%
      tidytable::summarise(prevalence = sum(weight, na.rm = TRUE),
                           .by = response
                           ) %>%
      tidytable::mutate(prevalence = prevalence / sum(prevalence)) %>%
      tidytable::select(response, prevalence)

    res =
      fitted_df %>%
      # bring 'winning_response' column
      tidytable::left_join(terminal_response_df,
                           by = "terminal_node_id"
                           ) %>%
      # bring 'prevalence' column
      tidytable::left_join(prevalence_df,
                           by = c("winning_response" = "response")
                           ) %>%
      tidytable::summarise(
        support = sum(weight),
        confidence = weighted.mean(response == winning_response, weight, na.rm = TRUE),
        lift = weighted.mean(response == winning_response, weight, na.rm = TRUE) / prevalence[1],
        RHS = winning_response[1],
        .by = terminal_node_id
        ) %>%
      tidytable::left_join(rules_df, by = "terminal_node_id") %>%
      tidytable::arrange(tidytable::desc(confidence)) %>%
      tidytable::mutate(., rule_nbr = 1:nrow(.)) %>%
      tidytable::select(rule_nbr, LHS, RHS,
                        support, confidence, lift,
                        terminal_node_id
                        )

  } else if (type == "regression"){

    res =
      fitted_df %>%
      tidytable::mutate(average = weighted.mean(response, weight, na.rm = TRUE),
                        .by = terminal_node_id
                        ) %>%
      tidytable::summarise(
        support  = sum(weight),
        IQR      = DescTools::IQRw(response, weight, na.rm = TRUE),
        RMSE     = MetricsWeighted::rmse(actual = response,
                                         predicted = average,
                                         w = weight,
                                         na.rm = TRUE
                                         ),
        average = mean(average),
        .by = terminal_node_id
        ) %>%
      tidytable::left_join(rules_df, by = "terminal_node_id") %>%
      tidytable::arrange(tidytable::desc(RMSE)) %>%
      tidytable::mutate(., rule_nbr = 1:nrow(.)) %>%
      tidytable::select(rule_nbr, LHS, RHS = average,
                        support, IQR, RMSE,
                        terminal_node_id
                        )
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

  class(res) = c("ruleset", class(res))

  attr(res, "keys")            = NULL
  attr(res, "model_type")      = "constparty"
  attr(res, "estimation_type") = type

  return(res)
}
