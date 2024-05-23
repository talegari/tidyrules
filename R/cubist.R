################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

#' @name tidy.cubist
#' @title Obtain rules as a ruleset/tidytable from a cubist model
#' @description Each row corresponds to a rule per committee.
#' @param x Cubist model
#' @param ... Other arguments (currently unused)
#' @return A ruleset/tidytable where each row corresponds to a rule. The columns
#'   are: rule_nbr, committee, LHS, RHS, support, mean, min, max, error
#' @details When col_classes argument is missing, an educated guess is made
#'   about class by parsing the RHS of sub-rule. This might sometimes not lead
#'   to a parsable rule.
#' @examples
#' data("attrition", package = "modeldata")
#' cols_att = setdiff(colnames(attrition), c("MonthlyIncome", "Attrition"))
#'
#' cb_att = Cubist::cubist(x = attrition[, cols_att],
#'                         y = attrition[["MonthlyIncome"]]
#'                         )
#' summary(cb_att)
#' tidy(cb_att)
#' @export

tidy.cubist = function(x, ...){

  #### core rule extraction ####################################################
  # output from the model
  output = x[["output"]]

  # get variable specification
  var_spec           = varSpec(x)
  variable_names     = var_spec[["variable"]]
  col_classes        = var_spec[["type"]]
  names(col_classes) = variable_names

  # throw error if there is consecutive spaces
  # output from the model squishes the spaces
  if(any(stringr::str_count(variable_names, "  ") > 0)){
    rlang::abort("Variable names should not two or more consecutive spaces.")
  }

  variable_names_with_ =
    stringr::str_replace_all(variable_names, "\\s", "_")

  # split by newline and remove emptylines
  lev_1 =
    x[["output"]] %>%
    strSplitSingle("\\n") %>%
    removeEmptyLines()

  # remove everything from 'Evaluation on training data' onwards
  evalLine = stringr::str_which(lev_1, "^Evaluation on training data")
  lev_2    =
    lev_1[-(evalLine:length(lev_1))] %>%
    stringr::str_subset("^(?!Model).*$")

  # detect starts and ends of rules
  rule_starts = stringr::str_which(stringr::str_trim(lev_2), "^Rule\\s")
  # end of a rule is a line before the next rule start
  rule_ends   = c(utils::tail(rule_starts, -1) - 1, length(lev_2))

  # create a rule list for cubist
  get_rules_cubist = function(single_raw_rule){

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

    res = list()

    # locate the position of square bracket and collect stats
    firstLine = stringr::str_squish(single_raw_rule[1])
    openingSquareBracketPosition = stringr::str_locate(firstLine, "\\[")[1, 1]

    # All stats are at the begining of the rule
    stat =
      # between square brackets
      stringr::str_sub(firstLine
                       , openingSquareBracketPosition + 1
                       , stringr::str_length(firstLine) - 1 # closing ] bracket
                      ) %>%
      strSplitSingle("\\,") %>%
      stringr::str_trim()

    res[["support"]] = stat[1] %>%
      strSplitSingle("\\s") %>%
      magrittr::extract(1) %>%
      as.integer()

    res[["mean"]] = stat[2] %>%
      strSplitSingle(" ") %>%
      magrittr::extract(2) %>%
      as.numeric()

    res[["min"]] = stat[3] %>%
      strSplitSingle(" ") %>%
      magrittr::extract(2) %>%
      as.numeric()

    res[["max"]] = stat[3] %>%
      strSplitSingle(" ") %>%
      magrittr::extract(4) %>%
      as.numeric()

    res[["error"]] = stat[4] %>%
      strSplitSingle(" ") %>%
      magrittr::extract(3) %>%
      as.numeric()

    # is if-then missing (only outcome is there)
    if_exists = any(stringr::str_trim(single_raw_rule) == "if")

    if (if_exists){
      # get LHS
      btw_if_then =
        seq(which(stringr::str_trim(single_raw_rule) == "if") + 1,
            which(stringr::str_trim(single_raw_rule) == "then") - 1
            )

      # unclean LHS strings, one condition per string
      lhsStrings =
        single_raw_rule[btw_if_then] %>%
        stringr::str_replace_all("\\t", "\\\\n") %>%
        stringr::str_trim() %>%
        stringr::str_c(collapse = " ") %>%
        strSplitSingle("\\\\n") %>%
        removeEmptyLines() %>%
        stringr::str_trim()

      # function to get the one clean rule string
      getRuleString = function(string){

        # to avoid CRAN notes
        . = NULL

        # if  there is ' in {' in the string
        if(stringr::str_detect(string, "\\sin\\s\\{")){

          # split with ' in {'
          var_lvls = strSplitSingle(string, "\\sin\\s\\{")

          # get the contents inside curly braces
          lvls =
            var_lvls[2] %>%
            # omit the closing curly bracket
            strHead(-1) %>%
            strSplitSingle(",") %>%
            stringr::str_trim() %>%
            purrr::map_chr(function(x) stringr::str_c("'", x, "'")) %>%
            stringr::str_c(collapse = ", ") %>%  # note the space next to comma
            stringr::str_c("c(", ., ")")

          # get the variable
          var = stringr::str_trim(var_lvls[1])
          rs  = stringr::str_c(var, " %in% ", lvls)

        } else {

          # handle '=' case
          contains_equals = stringr::str_detect(string, " = ")

          if (contains_equals){

            sub_rule = strSplitSingle(string, "=") %>%
              stringr::str_trim()

            if(!(col_classes[sub_rule[1]] == "numeric")){
              sub_rule[2] = stringr::str_c("'", sub_rule[2], "'")
            }

            rs = stringr::str_c(sub_rule, collapse = " == ")
          } else {
            # nothing to do
            rs = string
          }

        } # end of handle '=' case

        return(rs)

      }

      # clean up LHS as string
      res[["LHS"]] =
        purrr::map_chr(lhsStrings, getRuleString) %>%
        stringr::str_c("( ", ., " )") %>%
        stringr::str_c(collapse = " & ") # note spaces next to AND
    } else {

      res[["LHS"]] = NA

    }

    # get RHS
    # then might not exist: still retaining old name 'afterThen'
    if (if_exists){
      afterThen = seq(which(stringr::str_trim(single_raw_rule) == "then") + 1,
                      length(single_raw_rule)
                      )
    } else {
      afterThen = seq(
        which(stringr::str_detect(stringr::str_trim(single_raw_rule),
                                  "^outcome"
                                  )
              ),
        length(single_raw_rule)
        )
    }

    # handle brackets around signs
    res[["RHS"]] =
      single_raw_rule[afterThen] %>%
      stringr::str_replace_all("\\t", "") %>%
      stringr::str_trim() %>%
      stringr::str_c(collapse = " ") %>%
      stringr::str_squish() %>%
      stringr::str_replace("outcome = ", "") %>%
      # remove spaces around +- signs
      stringr::str_replace_all("\\s\\+\\s", "++") %>%
      stringr::str_replace_all("\\s\\-\\s", "--") %>%
      strReplaceReduce(variable_names, variable_names_with_) %>%
      stringr::str_replace_all("\\s", " * ") %>%
      stringr::str_replace_all("\\+\\+", ") + (") %>%
      stringr::str_replace_all("\\-\\-", ") - (")

    # quotes aroud each addenum
    res[["RHS"]] =
      stringr::str_c("(", res[["RHS"]], ")") %>%
      # honour negative intercept
      stringr::str_replace("\\(\\)\\s\\-\\s\\(", "(-")

    return(res)
  }

  # see if rules have commitees and create commitees vector
  rule_number_splits =
    stringr::str_split(stringr::str_trim(lev_2)[rule_starts], ":") %>%
    purrr::map_chr(function(x) x[[1]]) %>%
    stringr::str_split("\\s") %>%
    purrr::map_chr(function(x) x[[2]]) %>%
    stringr::str_split("/") %>%
    simplify2array() %>%
    as.integer()

  if (length(rule_number_splits) > length(rule_starts)){
    committees =
      rule_number_splits[seq(1, by = 2, length.out = length(rule_starts))]
  } else {
    committees = rep(1L, length(rule_starts))
  }

  # create parsable rules from raw rules
  res =
    purrr::map(1:length(rule_starts),
               function(i) lev_2[rule_starts[i]:rule_ends[i]]
               ) %>%
    purrr::map(get_rules_cubist) %>%
    purrr::transpose() %>%
    purrr::map(unlist) %>%
    tidytable::as_tidytable()

  #### prepare and return ######################################################
  # replace variable names with spaces within backquotes
  for (i in 1:length(variable_names)){
    res[["LHS"]] =
      stringr::str_replace_all(res[["LHS"]],
                               variable_names[i],
                               addBackquotes(variable_names[i])
                               )

    res[["RHS"]] = stringr::str_replace_all(res[["RHS"]],
                                            variable_names_with_[i],
                                            addBackquotes(variable_names[i])
                                            )
  }

  res =
    res %>%
    tidytable::mutate(committee = local(committees)) %>%
    tidytable::arrange(desc(support), .by = committee) %>%
    tidytable::mutate(rule_nbr = tidytable::row_number(), .by = committee)

  res = res[, c("rule_nbr", "committee",
                "LHS", "RHS",
                "support", "mean", "min", "max", "error"
                )
             ]

  class(res) = c("ruleset", class(res))

  attr(res, "keys")            = "committee"
  attr(res, "model_type")      = "cubist"
  attr(res, "estimation_type") = "regression"

  return(res)
}
