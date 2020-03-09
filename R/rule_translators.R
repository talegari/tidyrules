#' @name ruleRToPython
#' @title Convert a R parsable rule to python parsable rule
#' @description Expected to be passed to `pd.query` method of pandas dataframe
#' @param rule (chr vector) R parsable rule(s)
#' @return (chr vector) Python parsable rule(s)
ruleRToPython = function(rule){

  res = rule %>%
    stringr::str_replace_all("%in%", "in") %>%
    stringr::str_replace_all("c\\(", "[") %>%
    stringr::str_replace_all("\\)", "]") %>%
    stringr::str_replace_all("&", "and")


  return(res)
}

#' @name ruleRToSQL
#' @title Convert a R parsable rule to SQL parsable rule
#' @description Expected to be passed after SQL 'WHERE' clause
#' @param rule (chr vector) R parsable rule(s)
#' @return (chr vector) SQL parsable rule(s) as a 'WHERE' clause
ruleRToSQL = function(rule){

  res = rule %>%
    stringr::str_replace_all("==", "=") %>%
    stringr::str_replace_all("%in%", "IN") %>%
    stringr::str_replace_all("c\\(", "(") %>%
    stringr::str_replace_all("&", "AND")


  return(res)
}
