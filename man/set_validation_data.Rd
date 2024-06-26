% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rulelist.R
\name{set_validation_data}
\alias{set_validation_data}
\title{Add \code{validation_data} to a \link{rulelist}}
\usage{
set_validation_data(x, validation_data, y_name, weight = 1)
}
\arguments{
\item{x}{A \link{rulelist}}

\item{validation_data}{(dataframe) Data to used for computing some metrics.
It is expected to contain \code{y_name} column.}

\item{y_name}{(string) Name of the dependent variable column.}

\item{weight}{(non-negative numeric vector, default: 1) Weight per
observation/row of \code{validation_data}. This is expected to have same length
as the number of rows in \code{validation_data}. Only exception is when it is a
single positive number, which means that all rows have equal weight.}
}
\value{
A \link{rulelist} with some extra attributes set.
}
\description{
Returns a \link{rulelist} with three new attributes set:
\code{validation_data}, \code{y_name} and \code{weight}. Methods such as
\link[=augment.rulelist]{augment}, \link[=calculate.rulelist]{calculate},
\link[=prune.rulelist]{prune}, \link{reorder} require this to be set.
}
\examples{
att = modeldata::attrition
set.seed(100)
index = sample(c(TRUE, FALSE), nrow(att), replace = TRUE)
model_c5 = C50::C5.0(Attrition ~., data = att[index, ], rules = TRUE)

tidy_c5 = tidy(model_c5)
tidy_c5

tidy_c5_2 = set_validation_data(tidy_c5,
                                validation_data = att[!index, ],
                                y_name = "Attrition",
                                weight = 1 # default
                                )
tidy_c5_2
tidy_c5 # not altered

}
\seealso{
\link{rulelist}, \link{tidy}, \link[=augment.rulelist]{augment},
\link[=predict.rulelist]{predict}, \link[=calculate.rulelist]{calculate},
\link[=prune.rulelist]{prune}, \link[=reorder.rulelist]{reorder}

Other Core Rulelist Utility: 
\code{\link{set_keys}()}
}
\concept{Core Rulelist Utility}
