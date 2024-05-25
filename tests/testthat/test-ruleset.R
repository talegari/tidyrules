################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

context("test-ruleset")

model_rpart = rpart::rpart(species ~ .,
                           data = palmerpenguins::penguins
                           )
tidy_rpart = tidy(model_rpart)
tidy_rpart

output_1 = predict(tidy_rpart, palmerpenguins::penguins)
output_1

output_2 = predict(tidy_rpart, palmerpenguins::penguins, raw = TRUE)
output_2 # `rule_nbr` is a list-column of integer vectors

test_that("creates a dataframe", {
  expect_is(output_1, "data.frame")
  expect_is(output_2, "data.frame")
})

test_that("should not miss any row_nbr", {
  expect_true(all(1:nrow(palmerpenguins::penguins) %in% output_1$row_nbr))
})