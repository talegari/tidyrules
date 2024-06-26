% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rulelist.R
\name{reorder.rulelist}
\alias{reorder.rulelist}
\title{Reorder the rules/rows of a \link{rulelist}}
\usage{
\method{reorder}{rulelist}(x, metric = "cumulative_coverage", minimize = FALSE, init = NULL, ...)
}
\arguments{
\item{x}{A \link{rulelist}}

\item{metric}{(character vector or named list) Name of metrics or a custom
function(s). See \link[=calculate.rulelist]{calculate}. The 'n+1'th metric is
used when there is a match at 'nth' level, similar to \link[base:order]{base::order}. If
there is a match at final level, row order of the rulelist comes into play.}

\item{minimize}{(logical vector) Whether to minimize. Either TRUE/FALSE or a
logical vector of same length as metric}

\item{init}{(positive integer) Initial number of rows after which reordering
should begin}

\item{...}{passed to \link[=calculate.rulelist]{calculate}}
}
\description{
Implements a greedy strategy to add one rule at a time which
maximizes/minimizes a metric.
}
\examples{
library("magrittr")
att = modeldata::attrition
tidy_c5 =
  C50::C5.0(Attrition ~., data = att, rules = TRUE) \%>\%
  tidy() \%>\%
  set_validation_data(att, "Attrition") \%>\%
  set_keys(NULL) \%>\%
  head(5)

# with defaults
reorder(tidy_c5)

# use 'cumulative_overlap' to break ties (if any)
reorder(tidy_c5, metric = c("cumulative_coverage", "cumulative_overlap"))

# reorder after 2 rules
reorder(tidy_c5, init = 2)

}
\seealso{
\link{rulelist}, \link{tidy}, \link[=augment.rulelist]{augment},
\link[=predict.rulelist]{predict}, \link[=calculate.rulelist]{calculate},
\link[=prune.rulelist]{prune}, \link[=reorder.rulelist]{reorder}
}
