################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

#' @name tidyRules.C5.0
#' @title Obtain rules as a tidy tibble from a C5.0 model
#' @description Each row corresponds to a rule. A rule can be copied into
#'   `dplyr::filter` to filter the observations corresponding to a rule
#' @author Srikanth KS, \email{sri.teach@@gmail.com}
#' @param object Fitted model object with rules
#' @param ... Other arguments (See details)
#' @return A tibble where each row corresponds to a rule. The columns are:
#'   support, confidence, lift, lhs, rhs, n_conditions
#' @details Optional named argument laplace(flag, default: TRUE) is supported.
#'   This computes confidence with laplace correction as documented under
#'   'Rulesets' here: [C5 doc](https://www.rulequest.com/see5-unix.html)
#' @examples
#' data("attrition", package = "rsample")
#' attrition <- tibble::as_tibble(attrition)
#' c5_model <- C50::C5.0(Attrition ~., data = attrition, rules = TRUE)
#' summary(c5_model)
#' tidyRules(c5_model)
#' @export

tidyRules.C5.0 <- function(object, ...){

  # evaluate ...
  arguments = list(...)
  if(is.null(arguments[["laplace"]])){
    arguments[["laplace"]] = TRUE
  }

  # for magrittr dot ----
  . <- NULL

  # check for a rule based model ----
  stopifnot(inherits(object, "C5.0"))
  if(!object[["rbm"]]){
    stop(
      stringr:: str_c(
        "Unable to find rules in the C5.0 model."
        , "Model should be built with rules using `rules = TRUE` argument."
        )
      )
  }

  # output of the model ----
  output <- object[["output"]]

  # get variable specification ----
  var_spec           <- varSpec(object)
  variable_names     <- var_spec[["variable"]]
  col_classes        <- var_spec[["type"]]
  names(col_classes) <- variable_names

  # throw error if there is consecutive spaces ----
  # output from the model squishes the spaces
  if(any(stringr::str_count(variable_names, "  ") > 0)){
    stop("Variable names should not two or more consecutive spaces.")
  }

  # extract rules part ----
  spl <- output %>%
    stringr::str_replace_all("\t", "") %>% # remove tab spaces
    stringr::str_replace_all("\n ", "") %>% # handle multiline lineitems
    strSplitSingle("\n")                     # split along newlines

  # detect where the rules start
  start_rules_position <-
    which(stringr::str_detect(spl, "^Rule ")) %>%
    min()

  # detect where the rules end
  end_rules_position <-
    which(stringr::str_detect(spl, "^Evaluation on training data")) %>%
    magrittr::subtract(1) %>%
    min()

  # get the rules part
  spl <- spl[start_rules_position:end_rules_position] %>%
    stringr::str_squish() %>%
    removeEmptyLines()

  # get raw rules by splitting ----

  # every rule starts with 'Rule'
  cuts      <-  which(stringr::str_detect(spl, "^Rule "))

  # end of rule is a line before the start of next rule
  #cuts2     <- c(utils::tail(cuts, -1) - 1, length(spl))
  cuts2     <- which(stringr::str_detect(spl, "^\\-\\> "))

  # split rules
  rules_raw <- purrr::map2(cuts, cuts2, function(x, y) spl[x:y])

  # function to get a parsable rule from a raw rule ----
  getRules <- function(single_raw_rule){

    # empty list container
    rule <- list()

    # get stats from first line ----
    first_line <- single_raw_rule[1]

    # A typical first line looks like:
    #**************************************************
    # "Rule 0/1: (521/30, lift 1.1)", ":"
    #**************************************************

    index <- strSplitSingle(first_line, ":") %>%
      magrittr::extract(1) %>%
      strSplitSingle("\\s") %>%
      magrittr::extract(2) %>%
      strSplitSingle("/")

    if(length(index) == 2){
      rule[["rule_number"]]  <- as.integer(index[2])
      rule[["trial_number"]] <- as.integer(index[1]) + 1L
    } else {
      rule[["rule_number"]] <- as.integer(index)
      rule[["trial_number"]] <- 1L
    }

    stats <- strSplitSingle(first_line, ":") %>%
      #stringr::str_split("Rule 0/1: (521/30, lift 1.1)", ":") %>%
      magrittr::extract(2) %>%
      strSplitSingle("\\(") %>%
      magrittr::extract(2) %>%
      strSplitSingle("\\)") %>%
      magrittr::extract(1) %>%
      strSplitSingle(",") %>%
      stringr::str_squish()

    support_confidence <- strSplitSingle(stats[1], "/")
    if(length(support_confidence) > 1){

      # extract support
      rule[["support"]]    <- as.integer(support_confidence[1])

      # compute confidence (not extract)
      if(arguments[["laplace"]]){

        # C5 doc computes confidence using laplace correction
        # (n-m+1)/(n+2)
        # n: number of obs in leaf
        # m: number of musclassifications among n
        rule[["confidence"]] <- rule[["support"]] %>%
          magrittr::subtract(as.integer(support_confidence[2])) %>%
          magrittr::add(1) %>%
          magrittr::divide_by(rule[["support"]] + 2) %>%
          round(4)

      } else {

        # without laplace correction
        # simply: (n-m)/n
        rule[["confidence"]] <- rule[["support"]] %>%
          magrittr::subtract(as.integer(support_confidence[2])) %>%
          magrittr::divide_by(rule[["support"]]) %>%
          round(4)
      }

    } else {

      rule[["support"]]    <- as.integer(support_confidence)
      # see comments for laplace above
      if(arguments[["laplace"]]){
        rule[["confidence"]] = (rule[["support"]] + 1)/(rule[["support"]] + 2)
      } else{
        rule[["confidence"]] = 1
      }
    }

    rule[["lift"]] <- strSplitSingle(stats[2], "\\s") %>%
      magrittr::extract(2) %>%
      as.numeric()

    # curate a single line item of the rule ----
    line_item_curator <- function(line_item){

      # in unforeseen cases just return the rule string
      # let the parsing test catch it
      line_item_rule <- line_item

      # 'in' separator for a single line item of rule
      # ex1: JobInvolvement in [Low-Medium] for ordered factors
      # ex2: JobRole in {Laboratory_Technician, Sales_Representative}
      if(stringr::str_detect(line_item, "\\sin\\s")){
        split_line_item <- strSplitSingle(line_item, "\\sin\\s")
        lhs_line_item   <- split_line_item[1]
        rhs_line_item   <- split_line_item[2]

        # unordered factor case
        if(stringr::str_detect(line_item, "\\{")){
          rhs_line_item <- rhs_line_item %>%
            strHead(-1) %>%                     # remove quotes
            strTail(-1) %>%
            strSplitSingle(",") %>%             # split the list by comma
            stringr::str_trim() %>%             # trim if any
                                                # add quotes around levels
            purrr::map_chr(function(x) stringr::str_c("'", x, "'")) %>%
            stringr::str_c(collapse = ", ") %>% # bind with comma
            stringr::str_c("c(", ., ")")        # create 'c' structure

          line_item_rule <- stringr::str_c(lhs_line_item
                                           , " %in% "
                                           , rhs_line_item
                                           )
        }

        # unordered factor case
        if(stringr::str_detect(line_item, "\\[")){
          rhs_line_item <- rhs_line_item %>%
            strHead(-1) %>%
            strTail(-1)

          # more than one hyphen means some factor level has hyphen
          if(stringr::str_count(rhs_line_item, "-") > 1){
            stop("factor levels cannot have '-'.")
          }

          rhs_line_item <- rhs_line_item %>%
            strSplitSingle("-") %>%
            stringr::str_squish() # in case there is space

          # get the levels of the variable
          levels <- var_spec[var_spec[["variable"]] == lhs_line_item, ] %>%
            as.list() %>%
            magrittr::extract2("levels") %>%
            magrittr::extract2(1)

          # get all levels between start and end level
          start_level <- which(levels == rhs_line_item[1])
          end_level   <- which(levels == rhs_line_item[2])

          # construct RHS of the line item
          rhs_line_item <- levels[start_level:end_level] %>%
            stringr::str_c("'", ., "'") %>%
            stringr::str_c(collapse = ", ") %>%
            stringr::str_c("c(", ., ")")

          # complete line rule
          line_item_rule <- stringr::str_c(lhs_line_item
                                           , " %in% "
                                           , rhs_line_item
                                           )
        }
      }

      # handle '=' case
      # ex: MaritalStatus = Single
      contains_equals <- stringr::str_detect(line_item, " = ")
      if(contains_equals){

        sub_rule <- strSplitSingle(line_item, "=") %>%
          stringr::str_trim()

        the_class <- col_classes[[ sub_rule[1] ]]

        # quote if non-numeric
        if(!(the_class %in% c("numeric", "integer"))){
            sub_rule[2] <- stringr::str_c("'", sub_rule[2], "'")
        }

        line_item_rule <- stringr::str_c(sub_rule, collapse = " == ")

      }

      return(line_item_rule)
    }

    # create LHS and RHS ----
    rule[["LHS"]] <- single_raw_rule %>%
      utils::tail(-1) %>%                # remove first stats line
      utils::head(-1) %>%                # remove the RHS line
      purrr::map(line_item_curator) %>%  # get clean rule lines
      stringr::str_c(collapse = " & ")   # concat them with '&'

    rule[["RHS"]] <- single_raw_rule %>%
      utils::tail(1) %>%                 # get the RHS line
      stringr::str_squish() %>%          # remove multispaces
      strSplitSingle("\\s") %>%          # split by space
      magrittr::extract(3)               # extract the RHS name

    # return cleaned rule ----
    return(rule)
  }

  # apply rule tidying for each rule and return tibble ----
  res <- purrr::map(rules_raw, getRules) %>%
    purrr::transpose() %>%
    purrr::simplify_all() %>%
    tibble::as_tibble()

  # replace variable names with spaces within backquotes ----
  for(i in 1:length(variable_names)){
    res[["LHS"]] <- stringr::str_replace_all(
      res[["LHS"]]
      , variable_names[i]
      , addBackquotes(variable_names[i])
      )
  }

  # return ----
  res <- tibble::rowid_to_column(res, "id")
  res <- res[, c("id", "LHS", "RHS", "support", "confidence"
                 , "lift", "rule_number", "trial_number")
             ]
  return(res)
}

