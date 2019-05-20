################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

#' @name tidyRules
#' @title Obtain rules as a tidy tibble
#' @description Each row corresponds to a rule. A rule can be copied into
#'   `dplyr::filter` to filter the observations corresponding to a rule
#' @details tidyRule supports these rule based models: C5, Cubist and rpart.
#' @author Srikanth KS, \email{sri.teach@@gmail.com}
#' @param object Fitted model object with rules
#' @param ... Other arguments (currently unused)
#' @param col_classes Named list or a named character vector of column classes.
#'   Column names of the data used for modeling form the names and the
#'   respective classes for the value. One way of obtaining this is by running
#'   `lapply(data, class)`.
#' @return A tibble where each row corresponds to a rule
#' @export
tidyRules <- function(object, col_classes = NULL, ...){

  UseMethod("tidyRules", object)

}
