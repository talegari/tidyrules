################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

#' @name package_tidyrules
#' @title About 'tidyrules' package
#' @description Obtain rules as tidy dataframes

#' @importFrom magrittr %>%
"_PACKAGE"

is.integerish <- getFromNamespace("is.integerish", "assertthat")

list.rules.party <- getFromNamespace(".list.rules.party", "partykit")
