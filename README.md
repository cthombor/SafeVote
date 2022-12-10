
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SafeVote

<!-- badges: start -->
<!-- badges: end -->

The goals of SafeVote are to investigate the safety of announcing
preliminary results from an election, and to allow experimental study of
the safety of a complete ranking of all candidates (as in a party list)
that is derived from a small-scale election with preferential ballots.

## Installation

You can install the development version of SafeVote from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cthombor/SafeVote")
```

## Examples

This mod of
[vote_2.3.2](https://cran.r-project.org/web/packages/vote/index.html)
reports the margins of victory in an election. In cases where there are
near-ties, the value of the `safety` parameter (default 1.0) will affect
the completeness of the safeRank ordering of the candidates.

    library(SafeVote)
    data(food_election)
    stv(food_election, complete.ranking=TRUE, safety=0.5)

A few safety-testing routines are supplied, to support experimental
study of the statistical behaviour of ballot counting methods. For
example:

    data(dublin_west)
    testDeletions(dublin_west, countMethod="stv", rankMethod="elected",
                  countArgs=list(nseats=3))
