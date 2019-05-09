context("test-rpart")

# setup some models ----
# attrition
data("attrition", package = "rsample")
attrition <- tibble::as_tibble(attrition)

rpart_att <- rpart::rpart(Attrition ~ ., data = attrition, y = T)
tr_att <- tidyRules(rpart_att)

# attrition with trials
rpart_att_2 <- rpart::rpart(Attrition ~ ., data = attrition)
tr_att_2 <- tidyRules(rpart_att_2)

# # ames housing
# # ames has some space in Sale_Type levels
# ames   <- AmesHousing::make_ames()
# ames
# cb_ames <- rpart::rpart(MS_SubClass ~ .
#                         , data = ames
#                         , control = rpart::rpart.control(maxdepth = 4
#                                                          , maxsurrogate = 6
#                                                          , usesurrogate = 3
#                                                          , maxcompete = 5))
# tr_ames <- tidyRules(cb_ames)
#
# # column name has a space in it
# ames   <- AmesHousing::make_ames()
# ames_2 <- ames
# colnames(ames_2)[which(colnames(ames_2) == "Bldg_Type")] <- "Bldg Type"
# colnames(ames_2)[which(colnames(ames_2) == "House_Style")] <- "House Style"
# rpart_ames_2 <- rpart::rpart(MS_SubClass ~ ., data = ames_2)
# tr_ames_2 <- tidyRules(rpart_ames_2)

# BreastCancer
data(BreastCancer, package = "mlbench")
bc <- BreastCancer %>% dplyr::select(-Id)

bc_1m <- rpart::rpart(Class ~ .
                     , data = bc
                     , control = rpart::rpart.control(maxdepth = 2
                                                      , maxsurrogate = 2
                                                      , usesurrogate = 2
                                                      , maxcompete = 2))

tr_bc_1 <- tidyRules(bc_1m)

# variables with spaces
colnames(bc)[which(colnames(bc) == "Cell.size")] <- "Cell size"
colnames(bc)[which(colnames(bc) == "Cell.shape")] <- "Cell shape"

bc_2m <- rpart::rpart(Class ~ .
                     , data = bc
                     )

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
  expect_true(all(allRulesFilterable(tr_ames_2, ames_2)))
})

