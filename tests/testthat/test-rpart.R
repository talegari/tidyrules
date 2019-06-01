################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

context("test-rpart")

# setup some models ----
# attrition
data("attrition", package = "rsample")
attrition_1 <- attrition %>%
  dplyr::mutate_if(is.ordered, function(x) x <- factor(x,ordered = F)) %>%
  dplyr::mutate(Attrition = factor(Attrition, levels = c("No","Yes")))

rpart_att <- rpart::rpart(Attrition ~ ., data = attrition_1)
tr_att <- tidyRules(rpart_att)

# with ordered variables
attrition_2 <- attrition %>%
  dplyr::mutate(Attrition = factor(Attrition, levels = c("No","Yes")))

rpart_att_1 <- rpart::rpart(Attrition ~ ., data = attrition_2)

# attrition with maxdepth
rpart_att_2 <- rpart::rpart(Attrition ~ .
                            , data = attrition_1)

tr_att_2 <- tidyRules(rpart_att_2)

# BreastCancer
data(BreastCancer, package = "mlbench")
bc <- BreastCancer %>%
  dplyr::select(-Id) %>%
  dplyr::mutate_if(is.ordered, function(x) x <- factor(x,ordered = F))

bc_1m <- rpart::rpart(Class ~ ., data = bc)

tr_bc_1 <- tidyRules(bc_1m)

# variables with spaces
bc2 <- bc

colnames(bc2)[which(colnames(bc2) == "Cell.size")] <- "Cell size"
colnames(bc2)[which(colnames(bc2) == "Cell.shape")] <- "Cell shape"

bc_2m <- rpart::rpart(Class ~ ., data = bc2)

tr_bc_2 <- tidyRules(bc_2m)

# function to check whether a rule is filterable
ruleFilterable <- function(rule, data){
  dplyr::filter(data, eval(parse(text = rule)))
}

# function to check whether all rules are filterable
allRulesFilterable <- function(tr, data){
  parse_status <- sapply(
    tr[["LHS"]]
    , function(arule){
      trydf <- try(ruleFilterable(arule, data)
                   , silent = TRUE
      )
      if(nrow(trydf) == 0){
        print(arule)
      }
      inherits(trydf, "data.frame")
    }
  )
  return(parse_status)
}

# test for error while ordered features are present ----
test_that("check error",{
  expect_error(tidyRules(rpart_att_1))})

# test output type ----

test_that("creates tibble", {
  expect_is(tr_att, "tbl_df")
  expect_is(tr_att_2, "tbl_df")
  expect_is(tr_bc_1, "tbl_df")
  expect_is(tr_bc_2, "tbl_df")
})

# test NA ----
test_that("Are NA present", {
  expect_false(anyNA(tr_att))
  expect_false(anyNA(tr_att_2))
  expect_false(anyNA(tr_bc_1))
  expect_false(anyNA(tr_bc_2))
})

# test parsable ----
test_that("rules are parsable", {
  expect_true(all(allRulesFilterable(tr_att, attrition)))
  expect_true(all(allRulesFilterable(tr_att_2, attrition)))
  expect_true(all(allRulesFilterable(tr_bc_1, bc)))
  expect_true(all(allRulesFilterable(tr_bc_2, bc2)))
})

