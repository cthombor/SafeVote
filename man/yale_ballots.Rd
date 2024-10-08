% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{yale_ballots}
\alias{yale_ballots}
\title{Yale Faculty Senate 2016}
\format{
A data frame with 479 observations and 44 candidates.
}
\usage{
data(yale_ballots)
}
\description{
This data follows the structure of a 2016 Yale
Faculty Senate election, with candidate names anonymised and permuted.
Imported to SafeVote from \href{https://github.com/jayemerson/STV}{STV v1.0.2},
after applying the 'STV::cleanBallots' method to remove the ten empty
rows.
}
\keyword{datasets}
