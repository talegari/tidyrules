#' @name tidyRules.cubist
#' @title Obtain rules as a tidy tibble from a cubist model
#' @description Each row corresponds to a rule. A rule can be copied into
#'   `dplyr::filter` to filter the observations corresponding to a rule
#' @author Srikanth KS, \email{sri.teach@@gmail.com}
#' @param object Fitted model object with rules
#' @param col_classes Named list or a named character vector of column classes.
#'   Column names of the data used for modeling form the names and the
#'   respective classes for the value. One way of obtaining this is by running
#'   `lapply(data, class)`.
#' @param ... Other arguments (currently unused)
#' @return A tibble where each row corresponds to a rule. The columns are:
#'   support, mean, min, max, error, lhs, rhs and committee
#' @details When col_classes argument is missing, an educated guess is made
#' about class by parsing the RHS of sub-rule. This might sometimes not lead to
#' a parsable rule.
#' @examples
#' data("attrition", package = "rsample")
#' attrition <- tibble::as_tibble(attrition)
#' cols_att <- setdiff(colnames(attrition), c("MonthlyIncome", "Attrition"))
#'
#' cb_att <-
#'   Cubist::cubist(x = attrition[, cols_att],y = attrition[["MonthlyIncome"]])
#' tr_att <- tidyRules(cb_att)
#' tr_att
#' @export

tidyRules.cubist <- function(object, col_classes = NULL, ...){

  # checks for column classes
  if(!is.null(col_classes)){
    stopifnot(inherits(col_classes, c("list", "character")))
    stopifnot(!is.null(names(col_classes))) # should have names
  }

  # get column names
  columnNames <- object[["names"]] %>%
    stringr::str_split("\\n") %>%
    magrittr::extract2(1) %>%
    utils::tail(-5) %>% # first five rows are always headers
    # pick to the left of ":"
    purrr::map_chr(function(s) stringr::str_split(s, ":")[[1]][[1]]) %>%
    stringr::str_replace_all("\\\\", "") %>%
    removeEmptyLines()

  # handle column names with spaces
  namesWithSpace <- columnNames[(stringr::str_detect(columnNames, "\\s"))]

  # ordering is required because we do not want to replace smaller strings
  # ex: suppose 'hello world' and 'hello world india' are two columns
  # First replacement of 'hello world' by 'hello_world' will prevent
  # 'hello_world_india' from replacing 'hello world india'
  # edit: I find this unnecessary, clean it up later
  if(length(namesWithSpace) > 0){
    stop("At least one column name has a 'space' in it")
  }

  # split by newline and remove emptylines
  lev_1 <- object[["output"]] %>%
    stringr::str_split("\\n") %>%
    magrittr::extract2(1) %>%
    removeEmptyLines()

  # remove everything from 'Evaluation on training data' onwards
  evalLine <- stringr::str_detect(lev_1
                                  , "^Evaluation on training data"
                                  ) %>%
    which()
  lev_2    <- lev_1[-(evalLine:length(lev_1))]


  # detect starts and ends of rules
  rule_starts <- stringr::str_detect(stringr::str_trim(lev_2), "^Rule\\s") %>%
    which()
  # end of a rule is a line before the next rule start
  rule_ends   <- c(utils::tail(rule_starts, -1) - 1, length(lev_2))

  # create a rule list for cubist
  get_rules_cubist <- function(single_raw_rule){

    #print(single_raw_rule)

    # a raw rule looks like this:
    #
    # [1] "  Rule 7: [87 cases, mean 15824.0, range 12061 to 17924, est err 694.4]"
    # [2] "    if"
    # [3] "\tJobLevel <= 4"
    # [4] "\tJobRole in {Manager, Research_Director}"
    # [5] "\tTotalWorkingYears > 14"
    # [6] "    then"
    # [7] "\toutcome = 4166.4 + 3467 JobLevel - 23 Age - 0.011 MonthlyRate"

    # example with equal sign inside
    #     Rule 1/14: [35 cases, mean 5.364152, range 4.963788 to 5.521399, est err 0.039525]
    #
    #     if
    # 	Year_Built > 1952
    # 	Bsmt_Exposure in {Av, Mn, No, No_Basement}
    # 	Gr_Liv_Area <= 1692
    # 	Kitchen_Qual = Excellent
    #     then
    # 	outcome = 2.533961 + 0.000252 Gr_Liv_Area + 0.0025 Year_Built
    # 	          + 0.002 Year_Remod_Add + 0.000105 Garage_Area
    # 	          - 0.00054 Lot_Frontage - 3.8e-05 Bsmt_Unf_SF
    # 	          + 2.4e-05 Total_Bsmt_SF + 7e-07 Lot_Area - 0.005 Bedroom_AbvGr
    # 	          + 0.003 Garage_Cars + 0.003 Fireplaces + 0.07 Longitude
    # 	          + 0.001 TotRms_AbvGrd

    res <- list()

    # locate the position of square bracket and collect stats
    firstLine <- stringr::str_squish(single_raw_rule[1])
    openingSquareBracketPosition <- stringr::str_locate(firstLine, "\\[") %>%
      magrittr::extract(1, 1)

    # All stats are at the begining of the rule
    stat <-
      # between square brackets
      stringr::str_sub(firstLine
                       , openingSquareBracketPosition + 1
                       , stringr::str_length(firstLine) - 1
                      ) %>%
      stringr::str_split("\\,") %>%
      magrittr::extract2(1) %>%
      stringr::str_squish()

    res[["support"]] <- stat[1] %>%
      stringr::str_split("\\s") %>%
      magrittr::extract2(1) %>%
      magrittr::extract(1) %>%
      as.integer()

    res[["mean"]] <- stat[2] %>%
      stringr::str_split(" ") %>%
      magrittr::extract2(1) %>%
      magrittr::extract(2) %>%
      as.numeric()

    res[["min"]] <- stat[3] %>%
      stringr::str_split(" ") %>%
      magrittr::extract2(1) %>%
      magrittr::extract(2) %>%
      as.numeric()

    res[["max"]] <- stat[3] %>%
      stringr::str_split(" ") %>%
      magrittr::extract2(1) %>%
      magrittr::extract(4) %>%
      as.numeric()

    res[["error"]] <- stat[4] %>%
      stringr::str_split(" ") %>%
      magrittr::extract2(1) %>%
      magrittr::extract(3) %>%
      as.numeric()

    # get LHS
    btw_if_then <- seq(which(stringr::str_trim(single_raw_rule) == "if") + 1
                       , which(stringr::str_trim(single_raw_rule) == "then") - 1
                       )

    # unclean LHS strings, one condition per string
    lhsStrings <-  single_raw_rule[btw_if_then] %>%
      stringr::str_replace_all("\\t", "\\\\n") %>%
      stringr::str_squish() %>%
      stringr::str_c(collapse = " ") %>%
      stringr::str_split("\\\\n") %>%
      magrittr::extract2(1) %>%
      removeEmptyLines() %>%
      stringr::str_squish()

    # function to get the one clean rule string
    getRuleString <- function(string){

      # to avoid CRAN notes
      . <- NULL

      # if  there is ' in {' in the string
      if(stringr::str_detect(string, "\\sin\\s\\{")){

        # split with ' in {'
        var_lvls <- stringr::str_split(string, "\\sin\\s\\{")[[1]]

        # get the contents inside curly braces
        lvls <- var_lvls[2] %>%
          # omit the closing curly bracket
          stringr::str_sub(1, stringr::str_length(var_lvls[2]) - 1) %>%
          stringr::str_split(",") %>%
          magrittr::extract2(1) %>%
          stringr::str_squish() %>%
          purrr::map_chr(function(x) stringr::str_c("'", x, "'")) %>%
          stringr::str_c(collapse = ", ") %>%  # note the space next to comma
          stringr::str_c("c(", ., ")")

        # get the variable
        var <- var_lvls[1] %>%
          stringr::str_squish()

        rs <- stringr::str_c(var, " %in% ", lvls)

      } else {

        # handle '=' case
        contains_equals <- stringr::str_detect(string, " = ")
        if(contains_equals){

          sub_rule <- stringr::str_split(string, "=") %>%
            magrittr::extract2(1) %>%
            stringr::str_squish()

          if(!is.null(col_classes)){
            the_class <- col_classes[[ sub_rule[1] ]]
          } else {
            # when col_classes is not specified
            # try coercing to numeric. When not numeric, it results in NA
            coerced <- suppressWarnings(as.numeric(sub_rule[2]))
            if(is.na(coerced)){
              the_class <- "character"
            }
          }

          if(the_class %in% c("character", "factor", "ordered")){
              sub_rule[2] <- stringr::str_c("'", sub_rule[2], "'")
          }

        rs <- stringr::str_c(sub_rule, collapse = " == ")

        } else {

          # nothing to do
          rs <- string

        }

      } # end of handle '=' case

      return(rs)

    }

    # clean up LHS as string
    res[["lhs"]] <- purrr::map_chr(lhsStrings, getRuleString) %>%
    stringr::str_c(collapse = " & ") # note spaces next to AND

    # get RHS
    afterThen <- seq(which(stringr::str_trim(single_raw_rule) == "then") + 1
                     , length(single_raw_rule)
                     )

    # handle brackets around signs
    res[["rhs"]] <- single_raw_rule[afterThen] %>%
      stringr::str_replace_all("\\t", "") %>%
      stringr::str_c(collapse = " ") %>%
      stringr::str_squish() %>%
      stringr::str_replace("outcome = ", "") %>%
      # remove spaces around +- signs
      stringr::str_replace_all("\\s\\+\\s", "+") %>%
      stringr::str_replace_all("\\s\\-\\s", "-") %>%
      stringr::str_replace_all("\\s", " * ") %>%
      stringr::str_replace_all("\\+", ") + (") %>%
      stringr::str_replace_all("\\-", ") - (")

    # quotes aroud each addenum
    res[["rhs"]] <- stringr::str_c("(", res[["rhs"]], ")") %>%
      # honour negative intercept
      stringr::str_replace("\\(\\)\\s\\-\\s\\(", "(-")

    return(res)
}

  # see if rules have commitees and create commitees vector
  rule_number_splits <-
    stringr::str_split(stringr::str_trim(lev_2)[rule_starts], ":") %>%
    purrr::map_chr(function(x) x[[1]]) %>%
    stringr::str_split("\\s") %>%
    purrr::map_chr(function(x) x[[2]]) %>%
    stringr::str_split("/") %>%
    simplify2array() %>%
    as.integer()

  if(length(rule_number_splits) > length(rule_starts)){
    committees <- rule_number_splits[seq(1
                                         , by = 2
                                         , length.out = length(rule_starts)
                                         )]
  } else {
    committees <- rep(1L, length(rule_starts))
  }

  # create parsable rules from raw rules
  res <-
    purrr::map(1:length(rule_starts)
               , function(i) lev_2[rule_starts[i]:rule_ends[i]]
               ) %>%
    purrr::map(get_rules_cubist) %>%
    purrr::transpose() %>%
    purrr::map(unlist) %>%
    tibble::as_tibble()

  res[["committee"]] <- committees

  return(res)
}
