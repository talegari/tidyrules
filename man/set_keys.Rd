% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rulelist.R
\name{set_keys}
\alias{set_keys}
\title{Set keys for a \link{rulelist}}
\usage{
set_keys(x, keys, reset = FALSE)
}
\arguments{
\item{x}{A \link{rulelist}}

\item{keys}{(character vector or NULL)}

\item{reset}{(flag) Whether to reset the keys to sequential numbers starting
with 1 when \code{keys} is set to NULL}
}
\value{
A \link{rulelist} object
}
\description{
'keys' are a set of column(s) which identify a group of rules in
a \link{rulelist}. Methods like \link[=predict.rulelist]{predict},
\link[=augment.rulelist]{augment} produce output per key combination.
}
\details{
A new \link{rulelist} is returned with attr \code{keys} is modified. The input
\link{rulelist} object is unaltered.
}
\examples{
model_c5 = C50::C5.0(Attrition ~., data = modeldata::attrition, rules = TRUE)
tidy_c5 = tidy(model_c5)
tidy_c5 # keys are: "trial_nbr"

tidy_c5[["rule_nbr"]] = 1:nrow(tidy_c5)
new_tidy_c5 = set_keys(tidy_c5, NULL) # remove all keys
new_tidy_c5

new_2_tidy_c5 = set_keys(new_tidy_c5, "trial_nbr") # set "trial_nbr" as key
new_2_tidy_c5

# Note that `tidy_c5` and `new_tidy_c5` are not altered.
tidy_c5
new_tidy_c5

}
\seealso{
\link{rulelist}, \link{tidy}, \link[=augment.rulelist]{augment},
\link[=predict.rulelist]{predict}, \link[=calculate.rulelist]{calculate},
\link[=prune.rulelist]{prune}, \link[=reorder.rulelist]{reorder}

Other Core Rulelist Utility: 
\code{\link{set_validation_data}()}
}
\concept{Core Rulelist Utility}
