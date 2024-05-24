################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

# Structure
#
# Model/fitted object to rules should happens via 'tidy' call
# We get the generic from generics::tidy
# Rules object will be one among: ruleset/rulelist.
# This is a wrapper over tidytable/dataframe.
#
# Methods for rulelist/set: print, predict, augment
# At high level, predict returns the rule_nbr for a row_nbr in new_data
# At high level, augment (TODO) returns some metrics on new_data as new column
#
# Models:
#
# C5
#   - (rulelist when fitted with rules = TRUE) -- implemented
#   - (ruleset when fitted with rules = FALSE) -- NOT implemented
#
# rpart
#   - (ruleset with classification aka class) -- implemented
#   - (ruleset with regression aka anova)     -- implemented
#   - (ruleset with poisson)                  -- NOT implemented
#   - (ruleset with survival)                 -- NOT implemented
#   - (ruleset with exp)                      -- NOT implemented
#   - (ruleset with used defined split)       -- NOT implemented
#
# party
#   - (ruleset with classification)           -- NOT implemented
#   - (ruleset with regression)               -- NOT implemented
#   - (ruleset with survival)                 -- NOT implemented
#   - (ruleset with used defined split)       -- NOT implemented
#
# cubist
#   - (ruleset with regression)               -- implemented
#
# ranger
#   - (rulelist)                              -- NOT implemented
#
# sirus
#   - (ruleset ??)                            -- NOT implemented