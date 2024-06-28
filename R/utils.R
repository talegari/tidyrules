#*******************************************************************************
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
#*******************************************************************************

#' @keywords internal
#' @name positionSpaceOutsideSinglequotes
#' @title Position of space outside single quotes
#' @description (vectorised) Detect the position of space in a string not within
#'   a pair of single quotes
#' @param string A character vector
#' @return A integer vector of positions
#' @examples
#' \donttest{
#' tidyrules:::positionSpaceOutsideSinglequotes(c("hello", "hel' 'o "))
#' }
#'
positionSpaceOutsideSinglequotes = Vectorize(
  function(string){

    checkmate::assert_string(string)

    fullsplit               = strsplit(string, "")[[1]]
    is_singlequote          = (fullsplit == "'")
    parity_singlequote_left = cumsum(fullsplit == "'") %% 2
    space_position          = which(fullsplit == " ")

    space_position[parity_singlequote_left[space_position] == 0]
  },
  USE.NAMES = FALSE
)

#' @keywords internal
#' @name removeEmptyLines
#' @title Remove empty lines
#' @description Remove empty strings from a character vector
#' @param strings A character vector
#' @return A character vector
#' @examples
#' \donttest{
#' tidyrules:::removeEmptyLines(c("abc", "", "d"))
#' }
#'
removeEmptyLines = function(strings){
  strings[!(strings == "")]
}

#' @keywords internal
#' @name strSplitSingle
#' @title String split a string
#' @description and return a character vector (not a list)
#' @param string A string
#' @param pattern Passed as-is to 'stringr::str_split'
#' @return A character vector
#' @examples
#' \donttest{
#' tidyrules:::strSplitSingle("abc,d", ",")
#' }
#'
strSplitSingle = function(string, pattern){

  checkmate::assert_string(string)
  stringr::str_split(string, pattern)[[1]]

}

#' @keywords internal
#' @name strHead
#' @title Vectorized semantic equivalent of 'head' for a string
#' @description Picks the substring starting from the first character
#' @param string string
#' @param n (integer) Number of characters
#' @details 'n' can be in the interval \[-len + 1, len\] (both ends inclusive)
#' @return A string
#' @examples
#' \donttest{
#' tidyrules:::strHead(c("string", "string2"), 2)
#' tidyrules:::strHead(c("string", "string2"), -1)
#' }
#'
strHead = Vectorize(
  function(string, n){

    checkmate::assert_string(string)

    len = stringr::str_length(string)
    checkmate::assert_integerish(n, len = 1)
    checkmate::assert(n != 0)
    if(n < 0){
      n = len + n
    }

    return( stringr::str_sub(string, 1, n) )
  },
  vectorize.args = "string",
  USE.NAMES = FALSE
  )

#' @keywords internal
#' @name strTail
#' @title Vectorized semantic equivalent of tail for a string
#' @description Picks the substring starting from the first character
#' @param string string
#' @param n (integer) Number of characters
#' @details 'n' can be in the interval \[-len + 1, len\] (both ends inclusive)
#' @return A string
#' @examples
#' \donttest{
#' tidyrules:::strTail(c("string", "string2"), 2)
#' tidyrules:::strTail(c("string", "string2"), -1)
#' }
#'
strTail = Vectorize(
  function(string, n){

    checkmate::assert_string(string)

    len = stringr::str_length(string)
    checkmate::assert_integerish(n, len = 1)
    checkmate::assert(n != 0)
    if (n < 0){
      n = len + n
    }

    return( stringr::str_sub(string, len - n + 1, len) )
  },
  vectorize.args = "string",
  USE.NAMES = FALSE
  )

#' @keywords internal
#' @name addBackquotes
#' @title Add backquotes
#' @description (vectorized) Add backquotes when a string has a space in it
#' @param string character vector
#' @return character vector
#' @examples
#' \donttest{
#' tidyrules:::addBackquotes(c("ab", "a b"))
#' }
#'
addBackquotes = Vectorize(
  function(string){
    checkmate::assert_string(string)

    res = string
    if (stringr::str_count(string, "\\s") > 0){
      if(strHead(string, 1) != "`" && strTail(string, 1) != "`"){
          res = stringr::str_c("`", string, "`")
      }
    }
    return(res)
  },
  USE.NAMES = FALSE
  )

#' @keywords internal
#' @name strReplaceReduce
#' @title Sequential string replace
#' @description Sequential string replace via reduce
#' @param string string
#' @param pattern pattern
#' @param replacement replacement
#' @return character vector
#' @examples
#' \donttest{
#' tidyrules:::strReplaceReduce("abcd", c("ab", "dc"), c("cd", "ab"))
#' }
#'
strReplaceReduce = function(string, pattern, replacement){

  stopifnot(length(pattern) == length(replacement))
  io =
    list(pattern, replacement) %>%
    purrr::transpose() %>%
    purrr::map(unlist)

  purrr::reduce(io,
                function(x, y) stringr::str_replace_all(x, y[[1]], y[[2]]),
                .init = string
                )
}

#' @keywords internal
#' @name varSpec
#' @title Get variable specification for a Cubist/C5 object
#' @description Obtain variable names, type (numeric, ordered, factor) and
#'   levels as a tidytable
#' @param object Cubist/C5 object
#' @return A tidytable with three columns: variable(character), type(character)
#'   and levels(a list-column). For numeric variables, levels are set to NA.
#' @examples
#' \dontrun{
#' data("attrition", package = "modeldata")
#' cols_att = setdiff(colnames(attrition), c("MonthlyIncome", "Attrition"))
#'
#' cb_att = Cubist::cubist(x = attrition[, cols_att],
#'                         y = attrition[["MonthlyIncome"]]
#'                         )
#' varSpec(cb_att)
#' }
varSpec = function(object){

  # 1. split ny newline
  # 2. remove a few header lines
  # 3. get variables and details

  lines_raw =
    object[["names"]] %>%
    strSplitSingle("\\n")

  outcome_line_number = stringr::str_which(lines_raw, "^outcome:")

  lines =
    lines_raw[-(1:outcome_line_number)] %>%
    removeEmptyLines()

  split_lines =
    lines %>%
    stringr::str_split(":") %>%
    purrr::transpose()

  variables =
    split_lines %>%
    magrittr::extract2(1) %>%
    unlist() %>%
    stringr::str_replace_all("\\\\", "") # clean up variable names

  details =
    split_lines %>%
    magrittr::extract2(2) %>%
    unlist() %>%
    stringr::str_trim()

  # handle a detail depending on its type
  handleDetail = function(adetail){

    if (adetail == "continuous."){
      # handle numeric/integer
      out = list(type = "numeric", levels = NA_character_)

    } else if (stringr::str_detect(adetail, "^\\[ordered\\]")){
      # handle ordered factors

      levels =
        adetail %>%
        strSplitSingle("\\[ordered\\]") %>%
        magrittr::extract(2) %>%
        strHead(-1) %>%
        strSplitSingle(",") %>%
        stringr::str_trim()

      out = list(type = "ordered", levels = levels)

    } else { # handle unordered factors

      levels =
        adetail %>%
        strHead(-1) %>%
        strSplitSingle(",") %>%
        stringr::str_trim()

      out = list(type = "factor", levels = levels)
    }

  return(out)
  }

  details_cleaned =
    details %>%
    purrr::map(handleDetail) %>%
    purrr::transpose()

  details_cleaned[["type"]]     = unlist(details_cleaned[["type"]])
  details_cleaned[["variable"]] = variables

  res = tidytable::as_tidytable(details_cleaned)
  return(res)
}

#' @name convert_rule_flavor
#' @title Convert a R parsable rule to python/sql parsable rule
#' @description Convert a R parsable rule to python/sql parsable rule
#' @param rule (chr vector) R parsable rule(s)
#' @param flavor (string) One among: 'python', 'sql'
#' @return (chr vector) of rules
#' @seealso [rulelist], [tidy], [augment][augment.rulelist], [predict][predict.rulelist], [to_sql_case]
#' @family Auxiliary Rulelist Utility
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
      stringr::str_replace_all("==", "=") %>%

      stringr::str_replace_all("\\( ", "") %>%
      stringr::str_replace_all(" \\)", "") %>%

      stringr::str_replace_all("%in%", "IN") %>%
      stringr::str_replace_all("c\\(", "(") %>%
      stringr::str_replace_all("\\)", ")") %>%

      stringr::str_replace_all("&", " ) AND (") %>%

      stringr::str_c("( ", ., " )") %>%
      stringr::str_squish()
  }

  attr(res, "flavor") = flavor
  return(res)
}

#' @name to_sql_case
#' @title Extract SQL case statement from a [rulelist]
#' @description Extract SQL case statement from a [rulelist]
#' @param rulelist A [rulelist] object
#' @param rhs_column_name (string, default: "RHS") Name of the column in the
#'   rulelist to be used as RHS (WHEN some_rule THEN rhs) in the sql case
#'   statement
#' @param output_colname (string, default: "output") Name of the output column
#'   created by the SQL statement (used in case ... AS output_column)
#' @return (string invisibly) SQL case statement
#' @details As a side-effect, the SQL statement is cat to stdout. The output
#' contains newline character.
#' @examples
#' model_c5 = C50::C5.0(Attrition ~., data = modeldata::attrition, rules = TRUE)
#' tidy(model_c5)
#' to_sql_case(tidy(model_c5))
#' @seealso [rulelist], [tidy], [augment][augment.rulelist], [predict][predict.rulelist], [convert_rule_flavor]
#' @family Auxiliary Rulelist Utility
#' @export
to_sql_case = function(rulelist,
                       rhs_column_name = "RHS",
                       output_colname = "output"
                       ){

  checkmate::assert_class(rulelist, "rulelist")
  rhs_is_string = inherits(rulelist[[rhs_column_name]], c("character", "factor"))
  lhs_sql = convert_rule_flavor(rulelist$LHS, flavor = "sql")
  out = "CASE"

  for (rn in seq_len(nrow(rulelist))) {

    if (rhs_is_string) {
      lhs = glue::glue("WHEN {lhs_sql[rn]} THEN '{rulelist[[rhs_column_name]][rn]}'")
    } else {
      lhs = glue::glue("WHEN {lhs_sql[rn]} THEN {rulelist[[rhs_column_name]][rn]}")
    }
    out = paste(out, lhs, sep = "\n")
  }
  out = paste(out, "ELSE NULL", sep = "\n")
  out = paste(out, glue::glue("END AS {output_colname}"), sep = "\n")

  cli::cli_code(out, language = "SQL")

  return(invisible(out))
}
