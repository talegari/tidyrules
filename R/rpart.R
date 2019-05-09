#' @name tidyRules.rpart
#' @title Obtain rules as a tidy tibble from a rpart model
#' @description Each row corresponds to a rule. A rule can be copied into
#'   `dplyr::filter` to filter the observations corresponding to a rule
#' @author Amith Kumar U R, \email{amith54@@gmail.com}
#' @param object Fitted model object with rules
#' @param ... Other arguments (currently unused)
#' @return A tibble where each row corresponds to a rule. The columns are:
#'   support, confidence, lift, LHS, RHS
#' @examples
#' data("attrition", package = "rsample")
#' attrition <- tibble::as_tibble(attrition)
#' rpart_model <- rpart::rpart(Attrition ~., data = attrition, rules = TRUE)
#' summary(rpart_model)
#' tidyRules(rpart_model)
#' @export
tidyRules.rpart <- function(object, ...){

  stopifnot(inherits(object, "rpart"))
  # convert to class "party"
  party_obj <- partykit::as.party(object)

  # extracting rules
  rules <- partykit:::.list.rules.party(party_obj) %>%
    str_replace_all(pattern = "\\\"","'") %>%
    str_remove_all(pattern = "'NA',") %>%
    str_remove_all(pattern = ", 'NA'") %>%
    str_remove_all(pattern = "'NA'") %>%
    str_squish()

  # terminal nodes from party object
  terminal_nodes <- partykit::nodeids(party_obj, terminal = T)

  # extract metrics from rpart object
  metrics <- object$frame[terminal_nodes,c("n","dev","yval")]
  metrics$confidence <- (metrics$n + 1 - metrics$dev) / (metrics$n + 2)

  metrics <- metrics %>%
    dplyr::select(n,yval,confidence) %>%
    magrittr::set_colnames(c("support","predict_class","confidence"))

  # extract exact labels
  extracted_data <- partykit::data_party(party_obj)[,c("(fitted)"
                                                       ,"(response)")] %>%
    magrittr::set_colnames(c("fitted","response"))

  extracted_data$numeric_response <- object$y

  # Actual labels for RHS
  exact_labels <- extracted_data %>%
    dplyr::distinct(response,numeric_response)

  # prevelance for lift calculation
  prevelance <- prop.table(table(extracted_data$numeric_response)) %>%
    as.data.frame() %>%
    magrittr::set_colnames(c("class","freq")) %>%
    dplyr::mutate(class = as.numeric(class))

  # final metric df
  metrics <- metrics %>%
    dplyr::inner_join(exact_labels
                      , by = c("predict_class" = "numeric_response")) %>%
    dplyr::inner_join(prevelance, by = c("predict_class" = "class")) %>%
    dplyr::mutate(lift = confidence / freq) %>%
    dplyr::select(-c(predict_class,freq)) %>%
    dplyr::rename(RHS = response)

  # tidy rules
  tidy_rules <- cbind(metrics,LHS = rules)
  tidy_rules %>%
    dplyr::select(support,confidence,lift,LHS,RHS) %>%
    dplyr::mutate_if(is.factor,as.character) %>%
    tibble::as_tibble()

}

