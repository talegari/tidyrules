################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

################################################################################
# description of the cubist return object
################################################################################
# data: rowwise concatenated data
# names: new line separated variables, type and levels
# caseWeights: flag
# model: has rules
# output: has rules which can be used for printing with writeLines
# control: list of control values
# committees: Number of committees
# maxd: some related to seed?
# dims: dimensions of the data
# splits: splits of a rule as a dataframe
# usage: Variables used in conditions and the model as a dataframe
# call: call
# coefficients: matrix of rule X varibale form, intercept included
# vars: List of all and used variables

################################################################################
# description of the C5 return object
################################################################################
# names: new line separated variables, type and levels
# cost: Cost if provided
# costMatrix: if provided
# caseWeights: flag
# control: list of control values
# trials: named vector with reuested and actual
# rbm: flag whether it is a rule based model
# boostingResults: a parsed version of the boosting table(s) shown in the output (have not checked this)
# size: n integer vector of the tree/rule size (or sizes in the case of boosting)
# dims: dimensions of the data
# call: call
# levels: levels of the outcome factor variable
# output: has rules which can be used for printing with writeLines
# tree : tree (have not checked this)
# predictors: Names of variables
# rules: Rules in a slightly hard to parse format
# terms: object of class terms and formula
# xlevels: levels of factor/ordered variables