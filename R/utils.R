# detect the position of space(" ") in a string not within a pair of single quotes
detect_space_outside_singlequotes <- function(string){

  fullsplit             <- strsplit(string, "")[[1]]
  isSingleQuote         <- fullsplit == "'"
  paritySingleQuoteLeft <- cumsum(fullsplit == "'") %% 2
  spacePosition         <- which(fullsplit == " ")

  spacePosition[paritySingleQuoteLeft[spacePosition] == 0]
}

# string split given the positions
strsplit_at <- function(string, positions){
  substring(string
            , c(1, positions + 1)
            , c(positions, nchar(string))
  )
}

# string split where seperator is space which is not within a pair of single quotes
strsplit_space_ <- function(string){
  trimws( strsplit_at(string, detect_space_outside_singlequotes(string)) )
}

# vectorized version of strsplit_space
strsplit_space <- Vectorize(strsplit_space_)

# remove singlequotes in a string
remove_singlequotes <- function(string){
  strsplit(string, "'") %>% unlist() %>% paste0(collapse = "")
}

# remove empty lines
removeEmptyLines <- function(strings){
  strings[!(strings == "")]
}