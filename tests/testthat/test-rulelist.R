################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

context("test-rulelist")

model_c5 = C50::C5.0(species ~.,
                     data = palmerpenguins::penguins,
                     trials = 5,
                     rules = TRUE
                     )
tidy_c5 = tidy(model_c5)
tidy_c5

output_1 = predict(tidy_c5, palmerpenguins::penguins)
output_1 # different rules per 'keys' (`trial_nbr` here)

output_2 = predict(tidy_c5, palmerpenguins::penguins, raw = TRUE)
output_2 # `rule_nbr` is a list-column of integer vectors

test_that("creates a dataframe", {
  expect_is(output_1, "data.frame")
  expect_is(output_2, "data.frame")
})

test_that("should not miss any row_nbr", {
  expect_true(all(1:nrow(palmerpenguins::penguins) %in% output_1$row_nbr))
})