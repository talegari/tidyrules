################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

#' @name package_tidyrules
#' @title About 'tidyrules' package
#' @description Obtain rules as tidy dataframes
#' @importFrom magrittr %>%
#' @importFrom rlang %||%
#' @importFrom data.table :=
#' @importFrom utils data
#' @importFrom stats IQR
#' @importFrom stats weighted.mean
"_PACKAGE"

list.rules.party = getFromNamespace(".list.rules.party", "partykit")
