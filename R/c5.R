#' @name tidyRules.C5.0
#' @title Obtain rules as a tidy tibble from a C5.0 model
#' @description Each row corresponds to a rule. A rule can be copied into
#'   `dplyr::filter` to filter the observations corresponding to a rule
#' @author Srikanth KS, \email{sri.teach@@gmail.com}
#' @param object Fitted model object with rules
#' @param ... Other arguments (currently unused)
#' @return A tibble where each row corresponds to a rule. The columns are:
#'   support, confidence, lift, lhs, rhs, n_conditions
#' @examples
#' data("attrition", package = "rsample")
#' attrition <- tibble::as_tibble(attrition)
#' c5_model <- C50::C5.0(Attrition ~., data = attrition, rules = TRUE)
#' summary(c5_model)
#' tidyRules(c5_model)
#' @export

tidyRules.C5.0 <- function(object , ...){

  # for dplyr dot
  . <- NULL

  stopifnot(inherits(object, "C5.0"))
  if(is.null(object$rules)){
    stop("Unable to find rules in the C5.0 model. It should be built with rules using rules = TRUE argument")
  }

  spl <- object$rules %>%
    gsub("\\\"", "'", .) %>%
    strsplit(split = "\n") %>%
    `[[`(1)

  cuts      <-  grep("^conds", spl)
  cuts2     <- c(utils::tail(cuts, -1) - 1, length(spl))
  rules_raw <- Map(function(x, y) spl[x:y], cuts, cuts2)

  getRules <- function(single_raw_rule){

    res <- list()
    srr_split <- strsplit_space(single_raw_rule)
    stat      <- srr_split[[1]]

    res[["n_conditions"]] <- stat[1] %>%
      strsplit("=") %>%
      unlist() %>%
      utils::tail(1) %>%
      remove_singlequotes() %>%
      as.integer()

    res[["support"]] <- stat[2] %>%
      strsplit("=") %>%
      unlist() %>%
      utils::tail(1) %>%
      remove_singlequotes() %>%
      as.integer()

    res[["confidence"]] <- stat[3] %>%
      strsplit("=") %>%
      unlist() %>%
      utils::tail(1) %>%
      remove_singlequotes() %>%
      as.integer() %>%
      magrittr::divide_by(res[["support"]])

    res[["lift"]] <- stat[4] %>%
      strsplit("=") %>%
      unlist() %>%
      utils::tail(1) %>%
      remove_singlequotes() %>%
      as.numeric()

    res[["RHS"]] <- stat[5] %>%
      strsplit("=") %>%
      unlist() %>%
      utils::tail(1) %>%
      remove_singlequotes()

    form_condition_string <- function(index){

      line <- srr_split[[index]]

      type <- line[1] %>%
        strsplit("=") %>%
        unlist() %>%
        utils::tail(1) %>%
        remove_singlequotes() %>%
        paste0("a", .)

      attribute <- line[2] %>%
        strsplit("=") %>%
        unlist() %>%
        utils::tail(1) %>%
        remove_singlequotes()

      condition <- switch(
        type
        , a1 = {

          value <- line[3] %>%
            strsplit("=") %>%
            unlist() %>%
            utils::tail(1)

          paste(attribute, value, sep = " == ")

        }

        , a2 = {

          value <- line[3] %>%
            strsplit("=") %>%
            unlist() %>%
            utils::tail(1) %>%
            remove_singlequotes()

          sign <- line[4] %>%
            strsplit("=") %>%
            unlist() %>%
            utils::tail(1) %>%
            remove_singlequotes()

          paste(attribute, sign, value, sep = " ")

        }

        , a3 = {

          value <- line[3] %>%
            strsplit("=") %>%
            unlist() %>%
            utils::tail(1)

          paste(attribute, " %in% ", "c(", value, ")", sep = "")

        }

      )

    }

    res[["LHS"]] <- sapply(2:length(single_raw_rule), form_condition_string) %>%
      paste0(collapse = " & ")

    return(res)
  }

  res <- rules_raw %>%
    lapply(getRules) %>%
    lapply(tibble::as_tibble) %>%
    dplyr::bind_rows() %>%
    dplyr::select(c("support", "confidence", "lift", "LHS", "RHS", "n_conditions"))

  return(res)
}
