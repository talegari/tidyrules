#' @name convert_rule_flavor
#' @title Convert a R parsable rule to python/sql parsable rule
#' @description Convert a R parsable rule to python/sql parsable rule
#' @param rule (chr vector) R parsable rule(s)
#' @param flavor (string) One among: 'python', 'sql'
#' @return (chr vector) of rules
#' @export
convert_rule_flavor = function(rule, flavor){

  checkmate::assert_character(rule)
  checkmate::assert_string(flavor)
  flavor = stringr::str_to_lower(flavor)
  checkmate::assert_choice(flavor, c("python", "sql"))

  if (flavor == "python"){
    res =
      rule %>%
      stringr::str_replace_all("\\( ", "") %>%
      stringr::str_replace_all(" \\)", "") %>%

      stringr::str_replace_all("%in%", "in") %>%
      stringr::str_replace_all("c\\(", "[") %>%
      stringr::str_replace_all("\\)", "]") %>%

      stringr::str_replace_all("&", " ) and (") %>%

      stringr::str_c("( ", ., " )") %>%
      stringr::str_squish()

  } else if (flavor == "sql"){
    res =
      rule %>%
      stringr::str_replace_all("\\( ", "") %>%
      stringr::str_replace_all(" \\)", "") %>%

      stringr::str_replace_all("%in%", "IN") %>%
      stringr::str_replace_all("c\\(", "[") %>%
      stringr::str_replace_all("\\)", "]") %>%

      stringr::str_replace_all("&", " ) AND (") %>%

      stringr::str_c("( ", ., " )") %>%
      stringr::str_squish()
  }

  attr(res, "flavor") = flavor
  return(res)
}
