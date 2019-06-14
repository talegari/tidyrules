################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

#' @name tidyRules.rpart
#' @title Obtain rules as a tidy tibble from a rpart model
#' @description Each row corresponds to a rule. A rule can be copied into
#'   `dplyr::filter` to filter the observations corresponding to a rule
#' @author Amith Kumar U R, \email{amith54@@gmail.com}
#' @param object Fitted model object with rules
#' @param ... Other arguments (currently unused)
#' @details NOTE: For rpart rules, one should build the model without
#' \bold{ordered factor} variable. We recommend you to convert \bold{ordered
#' factor} to \bold{factor} or \bold{integer} class.
#' @return A tibble where each row corresponds to a rule. The columns are:
#'   support, confidence, lift, LHS, RHS
#' @examples
#' iris_rpart <- rpart::rpart(Species ~ .,data = iris)
#' tidyRules(iris_rpart)
#' @export
tidyRules.rpart <- function(object, ...){

  # check for rpart object
  stopifnot(inherits(object, "rpart"))

  # Stop if only root node is present in the object
  if(nrow(object$frame) == 1){
    stop(paste0("Only root is present in the rpart object"

                )
         )
  }

  # Stop if any ordered factor is present:
  # partykit doesn't handle the ordered factors while processing rules.
  if(sum(object$ordered) > 0){
    stop(paste0("Ordered variables detected!!"
               , "convert ordered variables"
               , " to factor or numberic before model fit"))
  }

  if(is.null(object$y)){
    stop(
      stringr::str_c(
        "Unable to find target variable in the model object!! "
        , "Model should be built using argument `y = TRUE`."
      )
    )
  }

  # column names from the object: This will be used at the end to handle the
  # variables with a space.
  col_names <- stringr::str_remove_all(attr(object$terms,which = "term.labels")
                                       , pattern = "`")

  # throw error if there are consecutive spaces in the column names ----
  if(any(stringr::str_count(col_names, "  ") > 0)){
    stop("Variable names should not have two or more consecutive spaces.")
  }

  # convert to class "party"
  party_obj <- partykit::as.party(object)

  # extracting rules
  rules <- list.rules.party(party_obj) %>%
    stringr::str_replace_all(pattern = "\\\"","'") %>%
    stringr::str_remove_all(pattern = ", 'NA'") %>%
    stringr::str_remove_all(pattern = "'NA',") %>%
    stringr::str_remove_all(pattern = "'NA'") %>%
    stringr::str_squish()

  # terminal nodes from party object
  terminal_nodes <- partykit::nodeids(party_obj, terminal = T)

  # extract metrics from rpart object
  metrics <- object$frame[terminal_nodes,c("n","dev","yval")]
  metrics$confidence <- (metrics$n + 1 - metrics$dev) / (metrics$n + 2)

  metrics <- metrics[,c("n","yval","confidence")] %>%
    magrittr::set_colnames(c("support","predict_class","confidence"))

  # prevelance for lift calculation
  prevelance <- object$y %>%
    table() %>%
    prop.table() %>%
    as.numeric()

  # Actual labels for RHS
  metrics$RHS <- attr(object, "ylevels")[metrics$predict_class]

  metrics$prevelance <- prevelance[metrics$predict_class]

  metrics$lift <- metrics$confidence / metrics$prevelance

  metrics$LHS <- rules


  tidy_rules <- metrics

  # replace variable names with spaces within backquotes ----
  for(i in 1:length(col_names)){
    tidy_rules[["LHS"]] <- stringr::str_replace_all(
      tidy_rules[["LHS"]]
      , col_names[i]
      , addBackquotes(col_names[i])
    )
  }

  # return ----
  tidy_rules <- tibble::rowid_to_column(tidy_rules, "id")
  tidy_rules <- tidy_rules[, c("id"
                               , "LHS"
                               , "RHS"
                               , "support"
                               , "confidence"
                               , "lift")
                           ] %>%
    tibble::as_tibble()

  return(tidy_rules)

}

