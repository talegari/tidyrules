# utils
################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

#' @name positionSpaceOutsideSinglequotes
#' @title Position of space outside single quotes
#' @description (vectorised) Detect the position of space in a string not within
#'   a pair of single quotes
#' @param string A chracter vector
#' @return A integer vector of positions
#' @examples
#' \donttest{
#' tidyrules:::positionSpaceOutsideSinglequotes(c("hello", "hel' 'o "))
#' }
#'
positionSpaceOutsideSinglequotes <- Vectorize(
  function(string){

    assertthat::assert_that(assertthat::is.string(string))

    fullsplit               <- strsplit(string, "")[[1]]
    is_singlequote          <- (fullsplit == "'")
    parity_singlequote_left <- cumsum(fullsplit == "'") %% 2
    space_position          <- which(fullsplit == " ")

    space_position[parity_singlequote_left[space_position] == 0]
  }
  , USE.NAMES = FALSE
)

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
removeEmptyLines <- function(strings){
  strings[!(strings == "")]
}

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
strSplitSingle <- function(string, pattern){

  assertthat::assert_that(assertthat::is.string(string))
  stringr::str_split(string, pattern)[[1]]

}

#' @name strHead
#' @title Vectorized semantic equivalent of 'head' for a string
#' @description Picks the substring starting from the first character
#' @param string string
#' @param n (integer) Number of characters
#' @details 'n' can be in the interval [-len + 1, len] (both ends inclusive)
#' @return A string
#' @examples
#' \donttest{
#' tidyrules:::strHead(c("string", "string2"), 2)
#' tidyrules:::strHead(c("string", "string2"), -1)
#' }
#'
strHead <- Vectorize(
  function(string, n){

    assertthat::assert_that(assertthat::is.string(string))
    len <- stringr::str_length(string)
    assertthat::assert_that(is.integerish(n) && length(n) == 1 && n != 0
                            , msg = "'n' should be an integer"
                            )
    if(n < 0){
      n <- len + n
    }

    return( stringr::str_sub(string, 1, n) )
  }
  , vectorize.args = "string"
  , USE.NAMES = FALSE
  )



#' @name strTail
#' @title Vectorized semantic equivalent of tail for a string
#' @description Picks the substring starting from the first character
#' @param string string
#' @param n (integer) Number of characters
#' @details 'n' can be in the interval [-len + 1, len] (both ends inclusive)
#' @return A string
#' @examples
#' \donttest{
#' tidyrules:::strTail(c("string", "string2"), 2)
#' tidyrules:::strTail(c("string", "string2"), -1)
#' }
#'
strTail <- Vectorize(
  function(string, n){

    assertthat::assert_that(assertthat::is.string(string))
    len <- stringr::str_length(string)
    assertthat::assert_that(is.integerish(n) && length(n) == 1 && n != 0
                            , msg = "'n' should be an integer"
                            )
    if(n < 0){
      n <- len + n
    }

    return( stringr::str_sub(string, len - n + 1, len) )
  }
  , vectorize.args = "string"
  , USE.NAMES = FALSE
  )
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
addBackquotes <- Vectorize(
  function(string){
    res <- string
    if(stringr::str_count(string, "\\s") > 0){
      if(strHead(string, 1) != "`" && strTail(string, 1) != "`"){
          res <- stringr::str_c("`", string, "`")
      }
    }
    return(res)
  }
  , USE.NAMES = FALSE
  )

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
strReplaceReduce <- function(string, pattern, replacement){

  stopifnot(length(pattern) == length(replacement))
  io <- list(pattern, replacement) %>%
    purrr::transpose() %>%
    purrr::map(unlist)

  purrr::reduce(io
                 , function(x, y) stringr::str_replace_all(x
                                                           , y[[1]]
                                                           , y[[2]]
                                                           )
                 , .init = string
                 )
}
