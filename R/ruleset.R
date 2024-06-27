#*******************************************************************************
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
#*******************************************************************************

#' @name ruleset
#' @title Ruleset
#' @description ruleset class is a piggyback class that inherits [rulelist]
#'   class for convenience of [print] and [predict] methods.
identity # just a placeholder for 'ruleset' documentation, not exported

#' @name as_ruleset
#' @title Get a ruleset from a rulelist
#' @description Returns a ruleset object
#' @param rulelist A [rulelist]
#' @returns A [ruleset]
#'
#' @examples
#' model_class_party = partykit::ctree(species ~ .,
#'                                     data = palmerpenguins::penguins
#'                                     )
#' as_ruleset(tidy(model_class_party))
#'
#' @seealso [rulelist]
#' @export
as_ruleset = function(rulelist){

  validate_rulelist(rulelist)

  x = rlang::duplicate(rulelist)
  class(x) = c("ruleset", class(x))

  return(x)
}

#' @name print.ruleset
#' @title Print method for ruleset class
#' @description Prints the ruleset object
#' @param x A [rulelist]
#' @param banner (flag, default: `TRUE`) Should the banner be displayed
#' @param ... Passed to `print.rulelist`
#' @returns (invisibly) Returns the ruleset object
#'
#' @examples
#' model_class_party = partykit::ctree(species ~ .,
#'                                     data = palmerpenguins::penguins
#'                                     )
#' as_ruleset(tidy(model_class_party))
#'
#' @seealso [print.rulelist]
#' @export
print.ruleset = function(x, banner = TRUE, ...){

  ruleset = rlang::duplicate(x)

  text = character(0)
  if (banner) {
    text = c(text, "---- Ruleset -------------------------------")
  }

  class(ruleset) = setdiff(class(ruleset), "ruleset")
  text = c(text,
           capture.output(print(ruleset, banner = FALSE, ...),
                          file = NULL
                          )
           )

  if (banner) {
    text = c(text, "--------------------------------------------")
  }
  cat(paste(text, collapse = "\n"))

  return(invisible(x))
}

#' @name predict.ruleset
#' @title `predict` method for a [ruleset]
#' @description Predicts multiple `rule_nbr`(s) applicable for a `row_nbr` (per
#'   key) in new_data
#'
#' @param object A [ruleset]
#' @param new_data (dataframe)
#' @param ... unused
#'
#' @returns A dataframe with three or more columns: `row_number` (int), columns
#'   corresponding to 'keys', `rule_nbr` (list column of integers). If a row
#'   number and 'keys' combination is not covered by any rule, then `rule_nbr`
#'   column has missing value.
#'
#' @examples
#' model_c5 = C50::C5.0(species ~.,
#'                      data = palmerpenguins::penguins,
#'                      trials = 5,
#'                      rules = TRUE
#'                      )
#' tidy_c5_ruleset = as_ruleset(tidy(model_c5))
#' tidy_c5_ruleset
#'
#' predict(tidy_c5_ruleset, palmerpenguins::penguins)
#'
#' @seealso [predict.rulelist]
#' @importFrom stats predict
#' @export
predict.ruleset = function(object, new_data, ...){

  x = rlang::duplicate(object)
  class(x) = setdiff(class(x), "ruleset")

  # now 'ruleset' is a rulelist
  res = predict(x, new_data, multiple = TRUE, ...)

  return(res)
}
