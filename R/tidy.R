################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

#' @name tidy
#' @title `tidy` is re-export of [generics::tidy] from
#'   [tidyrules][package_tidyrules] package
#' @description `tidy` applied on a supported model fit creates a [rulelist].
#'   **See Also** section links to documentation of specific methods.
#'
#' @param x A supported model object
#' @param ... For model specific implementations to use
#'
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder][reorder.rulelist]
#' @importFrom generics tidy
#' @family Core Tidy Utility
#' @export
#'
generics::tidy

#' @name tidy.C5.0
#' @title Get the [rulelist] from a [C5][C50::C5.0] model
#' @description Each row corresponds to a rule per `trial_nbr`
#' @param x [C50::C5.0] model fitted with `rules = TRUE`
#' @param ... Other arguments (See details)
#' @return A [rulelist] object
#' @details
#' - The output columns are: `rule_nbr`, `trial_nbr`, `LHS`, `RHS`,
#' `support`, `confidence`, `lift`.
#' - Rules per `trial_nbr` are sorted in this order: `desc(confidence)`,
#' `desc(lift)`, `desc(support)`.
#'
#' Optional named arguments:
#' - `laplace` (flag, default: TRUE) is supported. This
#' computes confidence with laplace correction as documented under 'Rulesets'
#' here: [C5 doc](https://www.rulequest.com/see5-unix.html).
#'
#' @examples
#' model_c5 = C50::C5.0(Attrition ~., data = modeldata::attrition, rules = TRUE)
#' tidy(model_c5)
#'
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder][reorder.rulelist]
#' @family Core Tidy Utility
#' @export
#'
tidy.C5.0 = function(x, ...){

  #### checks #################################################################

  arguments = list(...)
  arguments[["laplace"]] = arguments[["laplace"]] %||% TRUE

  # for magrittr dot
  . = NULL

  if (!x[["rbm"]]){
    rlang::abort("Model should be built using `rules = TRUE` argument.")
  }

  # output of the model
  output = x[["output"]]

  # get variable specification
  var_spec           = varSpec(x)
  variable_names     = var_spec[["variable"]]
  col_classes        = var_spec[["type"]]
  names(col_classes) = variable_names

  # throw error if there is consecutive spaces
  # output from the model squishes the spaces
  if (any(stringr::str_count(variable_names, "  ") > 0)){
    rlang::abort("Variable names should not two or more consecutive spaces.")
  }

  #### core logic ##############################################################
  # extract rules part
  spl =
    output %>%
    stringr::str_replace_all("\t", "") %>%  # remove tab spaces
    stringr::str_replace_all("\n ", "") %>% # handle multiline lineitems
    strSplitSingle("\n")                    # split along newlines

  # detect where the rules start
  start_rules_position = min(which(stringr::str_detect(spl, "^Rule ")))

  # detect where the rules end
  end_rules_position =
    stringr::str_detect(spl, "^Evaluation on training data") %>%
    which() %>%
    magrittr::subtract(1) %>%
    min()

  # get the rules part
  spl = spl[start_rules_position:end_rules_position] %>%
    stringr::str_squish() %>%
    removeEmptyLines()

  ## get raw rules by splitting

  # every rule starts with 'Rule'
  cuts  = which(stringr::str_detect(spl, "^Rule "))
  # end of rule is a line before the start of next rule
  cuts2 = which(stringr::str_detect(spl, "^\\-\\> "))

  # split rules
  rules_raw = purrr::map2(cuts, cuts2, function(x, y) spl[x:y])

  ## function to get a parsable rule from a raw rule
  getRules = function(single_raw_rule){

    # empty list container
    rule = list()

    # get stats from first line ----
    first_line = single_raw_rule[1]

    # A typical first line looks like:
    #**************************************************
    # "Rule 0/1: (521/30, lift 1.1)", ":"
    #**************************************************

    index = strSplitSingle(first_line, ":") %>%
      magrittr::extract(1) %>%
      strSplitSingle("\\s") %>%
      magrittr::extract(2) %>%
      strSplitSingle("/")

    if (length(index) == 2){
      rule[["rule_number"]]  = as.integer(index[2])
      rule[["trial_number"]] = as.integer(index[1]) + 1L
    } else {
      rule[["rule_number"]] = as.integer(index)
      rule[["trial_number"]] = 1L
    }

    stats =
      strSplitSingle(first_line, ":") %>%
      magrittr::extract(2) %>%
      strSplitSingle("\\(") %>%
      magrittr::extract(2) %>%
      strSplitSingle("\\)") %>%
      magrittr::extract(1) %>%
      strSplitSingle(",") %>%
      stringr::str_squish()

    support_confidence = strSplitSingle(stats[1], "/")
    if (length(support_confidence) > 1){

      # extract support
      rule[["support"]] = as.integer(support_confidence[1])

      # compute confidence (not extract)
      if (arguments[["laplace"]]){

        # C5 doc computes confidence using laplace correction
        # (n-m+1)/(n+2)
        # n: number of obs in leaf
        # m: number of musclassifications among n
        rule[["confidence"]] =
          rule[["support"]] %>%
          magrittr::subtract(as.integer(support_confidence[2])) %>%
          magrittr::add(1) %>%
          magrittr::divide_by(rule[["support"]] + 2) %>%
          round(4)

      } else {

        # without laplace correction
        # simply: (n-m)/n
        rule[["confidence"]] =
          rule[["support"]] %>%
          magrittr::subtract(as.integer(support_confidence[2])) %>%
          magrittr::divide_by(rule[["support"]]) %>%
          round(4)
      }

    } else {

      rule[["support"]] = as.integer(support_confidence)
      # see comments for laplace above
      if (arguments[["laplace"]]){
        rule[["confidence"]] = (rule[["support"]] + 1)/(rule[["support"]] + 2)
      } else{
        rule[["confidence"]] = 1
      }
    }

    rule[["lift"]] =
      strSplitSingle(stats[2], "\\s") %>%
      magrittr::extract(2) %>%
      as.numeric()

    # curate a single line item of the rule ----
    line_item_curator = function(line_item){

      # in unforeseen cases just return the rule string
      # let the parsing test catch it
      line_item_rule = line_item

      # 'in' separator for a single line item of rule
      # ex1: JobInvolvement in [Low-Medium] for ordered factors
      # ex2: JobRole in {Laboratory_Technician, Sales_Representative}
      if (stringr::str_detect(line_item, "\\sin\\s")){
        split_line_item = strSplitSingle(line_item, "\\sin\\s")
        lhs_line_item   = split_line_item[1]
        rhs_line_item   = split_line_item[2]

        # unordered factor case
        if (stringr::str_detect(line_item, "\\{")){
          rhs_line_item =
            rhs_line_item %>%
            strHead(-1) %>%                     # remove quotes
            strTail(-1) %>%
            strSplitSingle(",") %>%             # split the list by comma
            stringr::str_trim() %>%             # trim if any
                                                # add quotes around levels
            purrr::map_chr(function(x) stringr::str_c("'", x, "'")) %>%
            stringr::str_c(collapse = ", ") %>% # bind with comma
            stringr::str_c("c(", ., ")")        # create 'c' structure

          line_item_rule = stringr::str_c(lhs_line_item,
                                          " %in% ",
                                          rhs_line_item
                                          )
        }

        # unordered factor case
        if (stringr::str_detect(line_item, "\\[")){
          rhs_line_item =
            rhs_line_item %>%
            strHead(-1) %>%
            strTail(-1)

          # more than one hyphen means some factor level has hyphen
          if (stringr::str_count(rhs_line_item, "-") > 1){
            rlang::abort("factor levels cannot have '-'.")
          }

          rhs_line_item = rhs_line_item %>%
            strSplitSingle("-") %>%
            stringr::str_squish() # in case there is space

          # get the levels of the variable
          levels =
            var_spec[var_spec[["variable"]] == lhs_line_item, ] %>%
            as.list() %>%
            magrittr::extract2("levels") %>%
            magrittr::extract2(1)

          # get all levels between start and end level
          start_level = which(levels == rhs_line_item[1])
          end_level   = which(levels == rhs_line_item[2])

          # construct RHS of the line item
          rhs_line_item =
            levels[start_level:end_level] %>%
            stringr::str_c("'", ., "'") %>%
            stringr::str_c(collapse = ", ") %>%
            stringr::str_c("c(", ., ")")

          # complete line rule
          line_item_rule = stringr::str_c(lhs_line_item,
                                          " %in% ",
                                          rhs_line_item
                                          )
        }
      }

      # handle '=' case
      # ex: MaritalStatus = Single
      contains_equals = stringr::str_detect(line_item, " = ")
      if (contains_equals){

        sub_rule =
          strSplitSingle(line_item, "=") %>%
          stringr::str_trim()

        the_class = col_classes[[ sub_rule[1] ]]

        # quote if non-numeric
        if (!(the_class %in% c("numeric", "integer"))){
            sub_rule[2] = stringr::str_c("'", sub_rule[2], "'")
        }

        line_item_rule = stringr::str_c(sub_rule, collapse = " == ")

      }

      line_item_rule = paste0("( ", line_item_rule, " )")
      return(line_item_rule)
    }

    # create LHS and RHS ----
    rule[["LHS"]] =
      single_raw_rule %>%
      utils::tail(-1) %>%                # remove first stats line
      utils::head(-1) %>%                # remove the RHS line
      purrr::map(line_item_curator) %>%  # get clean rule lines
      stringr::str_c(collapse = " & ")   # concat them with '&'

    rule[["RHS"]] =
      single_raw_rule %>%
      utils::tail(1) %>%                 # get the RHS line
      stringr::str_squish() %>%          # remove multispaces
      strSplitSingle("\\s") %>%          # split by space
      magrittr::extract(3)               # extract the RHS name

    # return cleaned rule ----
    return(rule)
  }

  # apply rule tidying for each rule and return tibble
  res =
    purrr::map(rules_raw, getRules) %>%
    purrr::transpose() %>%
    purrr::simplify_all() %>%
    tidytable::as_tidytable()

  #### finalize output #########################################################
  # replace variable names with spaces within backquotes
  for (i in 1:length(variable_names)){
    res[["LHS"]] =
      stringr::str_replace_all(res[["LHS"]],
                               variable_names[i],
                               addBackquotes(variable_names[i])
                               )
  }

  #### return ##################################################################
  res =
    res %>%
    select(trial_nbr = trial_number,
           LHS, RHS,
           support, confidence, lift
           ) %>%
    arrange(trial_nbr, desc(confidence), desc(lift), desc(support)) %>%
    mutate(rule_nbr = 1:n(), .by = trial_nbr) %>%
    mutate(RHS = factor(RHS)) %>%
    relocate(rule_nbr, trial_nbr)

  class(res) = c("rulelist", class(res))

  attr(res, "keys")            = "trial_nbr"
  attr(res, "model_type")      = "C5"
  attr(res, "estimation_type") = "classification"

  return(res)
}

#' @name tidy.rpart
#' @title Get the [rulelist] from a [rpart][rpart::rpart] model
#' @description Each row corresponds to a rule
#' @param x [rpart::rpart] model
#' @param ... Other arguments (currently unused)
#' @return A [rulelist] object
#' @details For rpart rules, one should build the model without [ordered
#' factor][base::ordered] variable. We recommend you to convert [ordered
#' factor][base::ordered] to [factor][base::factor] or *integer* class.
#'
#' For [rpart::rpart] classification model:
#' - Output columns are: `rule_nbr`, `LHS`, `RHS`, `support`, `confidence`, `lift`.
#' - The rules are sorted in this order: `desc(confidence)`, `desc(lift)`,
#' `desc(support)`.
#'
#' For [rpart::rpart] regression(anova) model:
#' - Output columns are: `rule_nbr`, `LHS`, `RHS`, `support`.
#' - The rules are sorted in this order: `desc(support)`.
#' @examples
#' model_class_rpart = rpart::rpart(Species ~ ., data = iris)
#' tidy(model_class_rpart)
#'
#' model_regr_rpart = rpart::rpart(Sepal.Length ~ ., data = iris)
#' tidy(model_regr_rpart)
#'
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder][reorder.rulelist]
#' @family Core Tidy Utility
#' @export
#'
tidy.rpart = function(x, ...){

  ##### assertions and prep ####################################################
  arguments = list(...)

  # supported 'rpart' classes
  method_rpart = x$method
  # classification: class, regression: anova
  checkmate::assert_choice(method_rpart, c("class", "anova"))

  # build with y = TRUE
  if (is.null(x$y)){
    rlang::abort("rpart model should be built using argument `y = TRUE`.")
  }

  # column names from the x: This will be used at the end to handle the
  # variables with a space
  col_names =
    attr(x$terms, which = "term.labels") %>%
    stringr::str_remove_all(pattern = "`")

  # throw error if there are consecutive spaces in the column names
  if (any(stringr::str_count(col_names, "  ") > 0)){
    rlang::abort(
      "Variable names should not have two or more consecutive spaces.")
  }

  #### core extraction work ####################################################

  # convert to class "party"
  party_obj = partykit::as.party(x)

  # extract rules
  rules =
    list.rules.party(party_obj) %>%
    stringr::str_replace_all(pattern = "\\\"","'") %>%
    stringr::str_remove_all(pattern = ", 'NA'") %>%
    stringr::str_remove_all(pattern = "'NA',") %>%
    stringr::str_remove_all(pattern = "'NA'") %>%
    stringr::str_squish() %>%
    stringr::str_split(" & ") %>%
    purrr::map(~ stringr::str_c("( ", .x, " )")) %>%
    purrr::map_chr(~ stringr::str_c(.x, collapse = " & "))

  terminal_nodes = partykit::nodeids(party_obj, terminal = TRUE)

  # create metrics df
  if (method_rpart == "class"){
    prevalence     = as.numeric(prop.table(table(x$y)))

    res =
      x$frame[terminal_nodes, c("n", "dev", "yval")] %>%
      tidytable::mutate(confidence = (n + 1 - dev) / (n + 2)) %>%
      tidytable::rename(support = n, predict_class = yval) %>%
      tidytable::mutate(RHS = attr(x, "ylevels")[predict_class]) %>%
      tidytable::mutate(prevalence = prevalence[predict_class]) %>%
      tidytable::mutate(lift = confidence / prevalence) %>%
      tidytable::mutate(LHS = rules)

  } else if (method_rpart == "anova"){
    res =
      x$frame[terminal_nodes, c("n","yval")] %>%
      tidytable::rename(support = n, RHS = yval) %>%
      tidytable::mutate(LHS = rules)
  }

  #### finalize output #########################################################

  # replace variable names with spaces within backquotes
  for (i in 1:length(col_names)) {
    res[["LHS"]] =
      stringr::str_replace_all(res[["LHS"]],
                               col_names[i],
                               addBackquotes(col_names[i])
                               )
  }

  #### return ##################################################################

  res[["rule_nbr"]] = 1:nrow(res)

  if (method_rpart == "class"){
    res =
      res %>%
      select(LHS, RHS, support, confidence, lift) %>%
      arrange(desc(confidence), desc(lift), desc(support)) %>%
      mutate(rule_nbr = 1:n()) %>%
      mutate(RHS = factor(RHS)) %>%
      relocate(rule_nbr)

  } else if (method_rpart == "anova") {
    res =
      res %>%
      select(rule_nbr, LHS, RHS, support) %>%
      arrange(desc(support)) %>%
      mutate(rule_nbr = 1:n()) %>%
      relocate(rule_nbr)
  }

  class(res) = c("rulelist", class(res))

  attr(res, "keys")            = NULL
  attr(res, "model_type")      = "rpart"
  if (method_rpart == "class"){
    attr(res, "estimation_type") = "classification"
  } else if (method_rpart == "anova"){
    attr(res, "estimation_type") = "regression"
  }

  return(res)
}

#' @name tidy.constparty
#' @title Get the [rulelist] from a [party][partykit::party] model
#' @description Each row corresponds to a rule
#' @param x [partykit::party] model typically built using [partykit::ctree]
#' @param ... Other arguments (currently unused)
#' @return A [rulelist] object
#' @details These types of [party][partykit::party] models are supported:
#' `regression` (y is numeric), `classification` (y is factor)
#'
#' For [party][partykit::party] classification model:
#'
#' - Output columns are: `rule_nbr`, `LHS`, `RHS`, `support`, `confidence`, `lift`, `terminal_node_id`.
#' - Rules are sorted in this order: `desc(confidence)`, `desc(lift)`,
#' `desc(support)`.
#'
#' For [party][partykit::party] regression model:
#'
#' - Output columns are: `rule_nbr`, `LHS`, `RHS`, `support`, `IQR`, `RMSE`, `terminal_node_id`.
#' - Rules are sorted in this order: `RMSE`, `desc(support)`.
#' @examples
#' pen = palmerpenguins::penguins
#' model_class_party = partykit::ctree(species ~ ., data = pen)
#' tidy(model_class_party)

#' model_regr_party = partykit::ctree(bill_length_mm ~ ., data = pen)
#' tidy(model_regr_party)
#'
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder][reorder.rulelist]
#' @export
#'
tidy.constparty = function(x, ...){

  ##### assertions and prep ####################################################
  arguments = list(...)

  # column names from the x: This will be used at the end to handle the
  # variables with a space
  col_names =
    attr(x$terms, which = "term.labels") %>%
    stringr::str_remove_all(pattern = "`")

  # throw error if there are consecutive spaces in the column names
  if (any(stringr::str_count(col_names, "  ") > 0)){
    rlang::abort(
      "Variable names should not have two or more consecutive spaces.")
  }

  # detect method using 'fitted'
  fitted_df           = tidytable::as_tidytable(x$fitted)
  colnames(fitted_df) = c("terminal_node_id", "weight", "response")
  fitted_df[["terminal_node_id"]] = as.character(fitted_df[["terminal_node_id"]])

  y_class = class(fitted_df[["response"]])
  if (y_class == "factor") {
    type = "classification"
  } else if (y_class %in% c("numeric", "integer")) {
    type = "regression"
  } else {
    rlang::inform("tidy supports only classification and regression 'party' models")
    rlang::abort("Unsupported party object")
  }

  #### core extraction work ####################################################

  # extract rules
  raw_rules = list.rules.party(x)

  rules_df =
    raw_rules %>%
    stringr::str_replace_all(pattern = "\\\"","'") %>%
    stringr::str_remove_all(pattern = ", 'NA'") %>%
    stringr::str_remove_all(pattern = "'NA',") %>%
    stringr::str_remove_all(pattern = "'NA'") %>%
    stringr::str_squish() %>%
    stringr::str_split(" & ") %>%
    purrr::map(~ stringr::str_c("( ", .x, " )")) %>%
    purrr::map_chr(~ stringr::str_c(.x, collapse = " & ")) %>%
    tidytable::tidytable(LHS = .) %>%
    tidytable::mutate(terminal_node_id = names(raw_rules))

  # create metrics df
  if (type == "classification"){

    terminal_response_df =
      fitted_df %>%
      tidytable::summarise(sum_weight = sum(weight, na.rm = TRUE),
                           .by = c(terminal_node_id, response)
                           ) %>%
      tidytable::slice_max(n = 1,
                           order_by = sum_weight,
                           by = terminal_node_id,
                           with_ties = FALSE
                           ) %>%
      tidytable::select(terminal_node_id,
                        winning_response = response
                        )

    prevalence_df =
      fitted_df %>%
      tidytable::summarise(prevalence = sum(weight, na.rm = TRUE),
                           .by = response
                           ) %>%
      tidytable::mutate(prevalence = prevalence / sum(prevalence)) %>%
      tidytable::select(response, prevalence)

    res =
      fitted_df %>%
      # bring 'winning_response' column
      tidytable::left_join(terminal_response_df,
                           by = "terminal_node_id"
                           ) %>%
      # bring 'prevalence' column
      tidytable::left_join(prevalence_df,
                           by = c("winning_response" = "response")
                           ) %>%
      tidytable::summarise(
        support = sum(weight),
        confidence = weighted.mean(response == winning_response, weight, na.rm = TRUE),
        lift = weighted.mean(response == winning_response, weight, na.rm = TRUE) / prevalence[1],
        RHS = winning_response[1],
        .by = terminal_node_id
        ) %>%
      tidytable::left_join(rules_df, by = "terminal_node_id") %>%
      tidytable::arrange(desc(confidence), desc(lift), desc(support)) %>%
      tidytable::mutate(., rule_nbr = 1:nrow(.)) %>%
      mutate(RHS = factor(RHS)) %>%
      tidytable::select(rule_nbr, LHS, RHS,
                        support, confidence, lift,
                        terminal_node_id
                        )

  } else if (type == "regression"){

    res =
      fitted_df %>%
      tidytable::mutate(average = weighted.mean(response, weight, na.rm = TRUE),
                        .by = terminal_node_id
                        ) %>%
      tidytable::summarise(
        support  = sum(weight),
        IQR      = DescTools::IQRw(response, weight, na.rm = TRUE),
        RMSE     = MetricsWeighted::rmse(actual = response,
                                         predicted = average,
                                         w = weight,
                                         na.rm = TRUE
                                         ),
        average = mean(average),
        .by = terminal_node_id
        ) %>%
      tidytable::left_join(rules_df, by = "terminal_node_id") %>%
      tidytable::arrange(RMSE, desc(support)) %>%
      tidytable::mutate(., rule_nbr = 1:nrow(.)) %>%
      tidytable::select(rule_nbr, LHS, RHS = average,
                        support, IQR, RMSE,
                        terminal_node_id
                        )
  }

  #### finalize output #########################################################

  # replace variable names with spaces within backquotes
  for (i in 1:length(col_names)) {
    res[["LHS"]] =
      stringr::str_replace_all(res[["LHS"]],
                               col_names[i],
                               addBackquotes(col_names[i])
                               )
  }

  #### return ##################################################################

  class(res) = c("rulelist", class(res))

  attr(res, "keys")            = NULL
  attr(res, "model_type")      = "constparty"
  attr(res, "estimation_type") = type

  return(res)
}

#' @name tidy.cubist
#' @title Get the [rulelist] from a [cubist][Cubist::cubist] model
#' @description Each row corresponds to a rule per `committee`
#' @param x [Cubist::cubist] model
#' @param ... Other arguments (currently unused)
#' @return A [rulelist] object
#' @details
#' - The output columns are: `rule_nbr`, `committee`, `LHS`, `RHS`, `support`, `mean`, `min`, `max`, `error`.
#'
#' - Rules are sorted in this order per committee:
#' `error`, `desc(support)`
#' @examples
#' att = modeldata::attrition
#' cols_att    = setdiff(colnames(att), c("MonthlyIncome", "Attrition"))
#' model_cubist = Cubist::cubist(x = att[, cols_att],
#'                               y = att[["MonthlyIncome"]]
#'                               )
#' tidy(model_cubist)
#'
#' @seealso [rulelist], [tidy], [augment][augment.rulelist],
#'   [predict][predict.rulelist], [calculate][calculate.rulelist],
#'   [prune][prune.rulelist], [reorder][reorder.rulelist]
#' @family Core Tidy Utility
#' @export
#'
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
    tidytable::arrange(desc(error), .by = committee) %>%
    tidytable::mutate(rule_nbr = tidytable::row_number(), .by = committee)

  res =
    res %>%
    select(committee, LHS, RHS,
           support, mean, min, max, error
           ) %>%
    arrange(committee, error, desc(support)) %>%
    mutate(rule_nbr = 1:n(), .by = committee) %>%
    relocate(rule_nbr, committee)

  class(res) = c("rulelist", class(res))

  attr(res, "keys")            = "committee"
  attr(res, "model_type")      = "cubist"
  attr(res, "estimation_type") = "regression"

  return(res)
}
