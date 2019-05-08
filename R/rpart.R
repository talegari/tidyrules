tidyRules.rpart <- function(object, ...){

  # for dplyr dot
  . <- NULL

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

  prevelance <- prop.table(table(extracted_data$numeric_response)) %>%
    as.data.frame() %>%
    magrittr::set_colnames(c("class","freq")) %>%
    dplyr::mutate(class = as.numeric(class))

  exact_labels <- extracted_data %>%
    dplyr::distinct(response,numeric_response)

  metrics <- metrics %>%
    dplyr::inner_join(exact_labels
                      , by = c("predict_class" = "numeric_response")) %>%
    dplyr::inner_join(prevelance, by = c("predict_class" = "class")) %>%
    dplyr::mutate(lift = confidence / freq) %>%
    dplyr::select(-c(predict_class,freq)) %>%
    dplyr::rename(RHS = response)

  tidy_rules <- cbind(metrics,LHS = rules)

  tidy_rules %>%
    dplyr::select(support,confidence,lift,LHS,RHS)

}

