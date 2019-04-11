#' @name tidyRules
#' @title Obtain rules as a tidy tibble
#' @description Each row corresponds to a rule. A rule can be copied into
#'   `dplyr::filter` to filter the observations corresponding to a rule
#' @details tidyRule supports these rule based models: C5, Cubist, rpart, pre
#' @author Srikanth KS, \email{sri.teach@@gmail.com}
#' @param object Fitted model object with rules
#' @param ... Other arguments (currently unused)
#' @return A tibble where each row corresponds to a rule
#' @export
tidyRules <- function(object, ...){

  UseMethod("tidyRules", object)

}
