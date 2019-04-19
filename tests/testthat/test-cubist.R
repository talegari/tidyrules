context("test-cubist")

# setup some models ----
# attrition
data("attrition", package = "rsample")
attrition <- tibble::as_tibble(attrition)
cols_att <- setdiff(colnames(attrition), c("MonthlyIncome", "Attrition"))

cb_att <-
  Cubist::cubist(x = attrition[, cols_att],
                 y = attrition[["MonthlyIncome"]]
                 )
tr_att <- tidyRules(cb_att)

# attrition with commitees
cb_att_2 <-
  Cubist::cubist(x = attrition[, cols_att],
                 y = attrition[["MonthlyIncome"]],
                 committees = 7
                 )
tr_att_2 <- tidyRules(cb_att_2)

# ames housing
ames   <- AmesHousing::make_ames()
cb_ames <- Cubist::cubist(x = ames[, setdiff(colnames(ames), c("Sale_Price"))],
                          y = log10(ames[["Sale_Price"]]),
                          committees = 3
                          )
tr_ames <- tidyRules(cb_ames)

# ames housing with with column classes
cb_ames_2 <- Cubist::cubist(x = ames[, setdiff(colnames(ames), c("Sale_Price"))],
                          y = log10(ames[["Sale_Price"]])
                          )

tr_ames_2 <- tidyRules(
  cb_ames_2
  , col_classes = lapply(ames[, setdiff(colnames(ames), c("Sale_Price"))], class))

# get an error message when a column name has a space in it
data("Boston", package = "MASS")
names(Boston)[1] <- "cri m"
cb_boston <- Cubist::cubist(x = Boston[, -14], y = Boston[[14]])

# function to check whether a rule is filterable
ruleFilterable <- function(rule, data){
  dplyr::filter(data, eval(parse(text = rule)))
}

# function to check whether all rules are filterable
allRulesFilterable <- function(tr, data){
  parse_status <- sapply(
    tr[["lhs"]]
    , function(arule){
        inherits(try(ruleFilterable(arule, data)
                     , silent = TRUE
                     )
                 , "tbl_df"
                 )
      }
    )
  return(parse_status)
}

# test output type ----

test_that("creates tibble", {
  expect_is(tr_att, "tbl_df")
  expect_is(tr_att_2, "tbl_df")
  expect_is(tr_ames, "tbl_df")
  expect_is(tr_ames_2, "tbl_df")
})

# test NA ----
test_that("Are NA present", {
  expect_false(anyNA(tr_att))
  expect_false(anyNA(tr_att_2))
  expect_false(anyNA(tr_ames))
  expect_false(anyNA(tr_ames_2))
})

# test parsable ----
test_that("rules are parsable", {
  expect_true(all(allRulesFilterable(tr_att, attrition)))
  expect_true(all(allRulesFilterable(tr_att_2, attrition)))
  expect_true(all(allRulesFilterable(tr_ames, ames)))
  expect_true(all(allRulesFilterable(tr_ames_2, ames)))
})

# expect error when a column has space in it ----
test_that("error when space", {
  expect_error(tidyRules(cb_boston))
})
