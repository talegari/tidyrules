################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

#' @name varSpec
#' @title Get variable specification for a Cubist/C5 object
#' @description Obtain variable names, type (numeric, ordered, factor) and
#'   levels as a tibble
#' @author Srikanth KS, \email{sri.teach@@gmail.com}
#' @param object Cubist/C5 object
#' @return A tibble with three columns: variable(character), type(character) and
#'   levels(a list-column). For numeric variables, levels are set to NA.
#' @examples
#' data("attrition", package = "rsample")
#' attrition <- tibble::as_tibble(attrition)
#' cols_att <- setdiff(colnames(attrition), c("MonthlyIncome", "Attrition"))
#'
#' cb_att <-
#'   Cubist::cubist(x = attrition[, cols_att],y = attrition[["MonthlyIncome"]])
#' varSpec(cb_att)
#' @export
varSpec <- function(object){

  # 1. split ny newline
  # 2. remove a few header lines
  # 3. get variables and details

  lines_raw <- object[["names"]] %>%
    strSplitSingle("\\n")

  outcome_line_number <- stringr::str_which(lines_raw, "^outcome:")

  lines <- lines_raw[-(1:outcome_line_number)] %>%
    removeEmptyLines()

  split_lines <- lines %>%
    stringr::str_split(":") %>%
    purrr::transpose()

  variables <- split_lines %>%
    magrittr::extract2(1) %>%
    unlist() %>%
    stringr::str_replace_all("\\\\", "") # clean up variable names

  details <-  split_lines %>%
    magrittr::extract2(2) %>%
    unlist() %>%
    stringr::str_trim()

  # handle a detail depending on its type
  handleDetail <- function(adetail){

    if(adetail == "continuous."){
      # handle numeric/integer
      out <- list(type = "numeric", levels = NA_character_)

    } else if(stringr::str_detect(adetail, "^\\[ordered\\]")){
      # handle ordered factors

      levels <- adetail %>%
        strSplitSingle("\\[ordered\\]") %>%
        magrittr::extract(2) %>%
        strHead(-1) %>%
        strSplitSingle(",") %>%
        stringr::str_trim()

      out <- list(type = "ordered", levels = levels)

    } else { # handle unordered factors

      levels <- adetail %>%
        strHead(-1) %>%
        strSplitSingle(",") %>%
        stringr::str_trim()

      out <- list(type = "factor", levels = levels)
    }
  return(out)
  }

  details_cleaned <- details %>%
    purrr::map(handleDetail) %>%
    purrr::transpose()

  details_cleaned[["type"]]     <- unlist(details_cleaned[["type"]])
  details_cleaned[["variable"]] <- variables

  return(tibble::as_tibble(details_cleaned))
}
