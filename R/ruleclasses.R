################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

#' @export
print.ruleset = function(x, ...){

  rlang::inform(paste0("# A ruleset/tidytable with keys: ",
                       paste(attr(x, "keys"), collapse = ", ")
                       )
                )

  class(x) = setdiff(class(x), "ruleset")
  print(x, ...)
  class(x) = c("ruleset", class(x))

  return(invisible(x))
}

#' @export
print.rulelist = function(x, ...){

  rlang::inform(paste0("# A rulelist/tidytable with keys: ",
                       paste(attr(x, "keys"), collapse = ", ")
                       )
                )

  class(x) = setdiff(class(x), "rulelist")
  print(x, ...)
  class(x) = c("rulelist", class(x))

  return(invisible(x))
}
