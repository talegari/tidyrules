# detect the position of space(" ") in a string not within a pair of single quotes
positionSpaceOutsideSinglequotes_ <- function(string){

  assertthat::assert_that(assertthat::is.string(string))

  fullsplit               <- strsplit(string, "")[[1]]
  is_singlequote          <- (fullsplit == "'")
  parity_singlequote_left <- cumsum(fullsplit == "'") %% 2
  space_position          <- which(fullsplit == " ")

  space_position[parity_singlequote_left[space_position] == 0]
}
positionSpaceOutsideSinglequotes <-
  Vectorize(positionSpaceOutsideSinglequotes_)

# split string at positions
# the ones at the position will not be retained
strPositionSplit_ <- function(string, positions){

  assertthat::assert_that(assertthat::is.string(string))
  assertthat::assert_that(is.integerish(positions))
  assertthat::assert_that(length(positions) == length(unique(positions)))
  positions <- sort(positions)
  ell <- stringr::str_length(string)
  if(max(positions) > ell){
    stop("Positions to split should be less than the length of the string")
  }

  starts <- c(1L, positions + 1L)
  ends   <- c(positions - 1L, ell)
  wrongs <- (starts > ends)

  if(sum(wrongs) < ell){
    starts <- starts[!wrongs]
    ends   <- ends[!wrongs]
  } else {
    return(character(0))
  }

  return(stringr::str_sub(string, starts, ends))
}
strPositionSplit <- Vectorize(strPositionSplit_)

# remove singlequotes in a string
removeSinglequotes_ <- function(string){
  assertthat::assert_that(assertthat::is.string(string))
  string[string != "'"]
}
removeSinglequotes <- Vectorize(removeSinglequotes_)

# remove empty lines
removeEmptyLines <- function(strings){
  strings[!(strings == "")]
}

# strSplitSingle
strSplitSingle <- function(string, pattern){

  assertthat::assert_that(assertthat::is.string(string))
  stringr::str_split(string, pattern)[[1]]

}


strHead_ <- function(string, n){

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

#' @name strHead
#' @title Vectorized semantic equivalent of 'head' for a string
#' @description Picks the substring starting from the first character
#' @param string string
#' @param n (integer) Number of characters
#' @details 'n' can be in the interval [-len + 1, len] (both ends inclusive)
#' @return A string
#' @examples
#' strHead(c("string", "string2"), 2)
#' strHead(c("string", "string2"), -1)
#' @export
#'
strHead <- Vectorize(strHead_, vectorize.args = "string", USE.NAMES = FALSE)

strTail_ <- function(string, n){

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

#' @name strTail
#' @title Vetorized semantic equivalent of tail for a string
#' @description Picks the substring starting from the first character
#' @param string string
#' @param n (integer) Number of characters
#' @details 'n' can be in the interval [-len + 1, len] (both ends inclusive)
#' @return A string
#' @examples
#' strTail(c("string", "string2"), 2)
#' strTail(c("string", "string2"), -1)
#' @export
#'
strTail <- Vectorize(strTail_, vectorize.args = "string", USE.NAMES = FALSE)

addBackquotes_ <- function(string){
  res <- string
  if(stringr::str_count(string, "\\s") > 0){
    if(strHead(string, 1) != "`" && strTail(string, 1) != "`"){
        res <- stringr::str_c("`", string, "`")
    }
  }

  return(res)
}

addBackquotes <- Vectorize(addBackquotes_, USE.NAMES = FALSE)

strReplaceReduce <- function(string, pattern, replacement){

  stopifnot(length(pattern) ==  length(replacement))
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
