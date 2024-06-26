% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rulelist.R
\name{rulelist}
\alias{rulelist}
\title{Rulelist}
\description{
\subsection{Structure}{

A \code{rulelist} is ordered list of rules stored as a dataframe. Each row,
specifies a rule (LHS), expected outcome (RHS) and some other details.

It has these mandatory columns:
\itemize{
\item \code{rule_nbr}: (integer vector) Rule number
\item \code{LHS}: (character vector) A rule is a string that can be parsed using \code{\link[base:parse]{base::parse()}}
\item \code{RHS}: (character vector or a literal)
}
}

\subsection{Example}{

\if{html}{\out{<div class="sourceCode">}}\preformatted{| rule_nbr|LHS                                                                  |RHS       | support| confidence|     lift|
|--------:|:--------------------------------------------------------------------|:---------|-------:|----------:|--------:|
|        1|( island \%in\% c('Biscoe') ) & ( flipper_length_mm > 203 )            |Gentoo    |     122|  1.0000000| 2.774193|
|        2|( island \%in\% c('Biscoe') ) & ( flipper_length_mm <= 203 )           |Adelie    |      46|  0.9565217| 2.164760|
|        3|( island \%in\% c('Dream', 'Torgersen') ) & ( bill_length_mm > 44.1 )  |Chinstrap |      65|  0.9538462| 4.825339|
|        4|( island \%in\% c('Dream', 'Torgersen') ) & ( bill_length_mm <= 44.1 ) |Adelie    |     111|  0.9459459| 2.140825|
}\if{html}{\out{</div>}}
}

\subsection{Create a rulelist}{

A \code{rulelist} can be created using \code{\link[=tidy]{tidy()}} on some supported model fits
(run: \code{utils::methods(tidy)}). It can also be created manually from a
existing dataframe using \link[=as_rulelist.data.frame]{as_rulelist}.
}

\subsection{Keys and attributes}{

Columns identified as 'keys' along with \code{rule_nbr} form a unique
combination
-- a group of rules. For example, rule-based C5 model with multiple trials
creates rules per each \code{trial_nbr}. \code{predict} method understands 'keys',
thereby provides/predicts a rule number (for each row in new data / test
data) within the same \code{trial_nbr}.

A rulelist has these mandatory attributes:
\itemize{
\item \code{estimation_type}: One among \code{regression}, \code{classification}

A rulelist has these optional attributes:
\item \code{keys}: (character vector)Names of the column that forms a key.
\item \code{model_type}: (string) Name of the model
}

\subsection{Set Validation data}{

This helps a few methods like \link{augment}, \link{calculate}, \link{prune}, \link{reorder}
require few additional attributes which can be set using
\link{set_validation_data}.
}

\subsection{Methods for rulelist}{
\enumerate{
\item \link[=predict.rulelist]{Predict}: Given a dataframe (possibly without a
dependent variable column aka 'test data'), predicts the first rule (as
ordered in the rulelist) per 'keys' that is applicable for each row. When
\code{multiple = TRUE}, returns all rules applicable for a row (per key).
\item \link[=augment.rulelist]{Augment}: Outputs summary statistics per rule over
validation data and returns a rulelist with a new dataframe-column.
\item \link[=calculate.rulelist]{Calculate}: Computes metrics for a rulelist in a
cumulative manner such as \code{cumulative_coverage}, \code{cumulative_overlap},
\code{cumulative_accuracy}.
\item \link[=prune.rulelist]{Prune}: Suggests pruning a rulelist such that some
expectation are met (based on metrics). Example: cumulative_coverage of 80\%
can be met with a first few rules.
\item \link[=reorder.rulelist]{Reorder}: Reorders a rulelist in order to maximize a
metric.
}
}

\subsection{Manipulating a rulelist}{

Rulelists are essentially dataframes. Hence, any dataframe operations which
preferably preserve attributes will output a rulelist. \link{as_rulelist} and
\link{as.data.frame} will help in moving back and forth between rulelist and
dataframe worlds.
}

\subsection{Utilities for a rulelist}{
\enumerate{
\item \link[=as_rulelist.data.frame]{as_rulelist}: Create a \code{rulelist} from a
dataframe with some mandatory columns.
\item \link{set_keys}: Set or Unset 'keys' of a \code{rulelist}.
\item \link{to_sql_case}: Outputs a SQL case statement for a \code{rulelist}.
\item \link{convert_rule_flavor}: Converts \code{R}-parsable rule strings to python/SQL
parsable rule strings.
}
}
}
}
\seealso{
\link{rulelist}, \link{tidy}, \link[=augment.rulelist]{augment},
\link[=predict.rulelist]{predict}, \link[=calculate.rulelist]{calculate},
\link[=prune.rulelist]{prune}, \link[=reorder.rulelist]{reorder}
}
