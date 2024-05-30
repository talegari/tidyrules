% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rule_translators.R
\name{convert_rule_flavor}
\alias{convert_rule_flavor}
\title{Convert a R parsable rule to python/sql parsable rule}
\usage{
convert_rule_flavor(rule, flavor)
}
\arguments{
\item{rule}{(chr vector) R parsable rule(s)}

\item{flavor}{(string) One among: 'python', 'sql'}
}
\value{
(chr vector) of rules
}
\description{
Convert a R parsable rule to python/sql parsable rule
}