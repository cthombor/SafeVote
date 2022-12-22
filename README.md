
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
    plot(testFraction(dublin_west))

![](man/figures/testFraction.png)

        data(food_election)
        testAdditions(food_election)
        #> 
        #> Adding up to 1 stv ballots = ( 2 5 3 4 1 )
        #> Testing progress:  1
        #> Experimental results:
        #>      nBallots Oranges Pears Chocolate Strawberries Sweets
        #> [1,]       20       2     2         1            2      2
        #> [2,]       21       2     2         1            2      2

        # Display a possible evolution of the election results, when ballots
        # are counted in a randomised order.
        xr <- testDeletions(dublin_west,dinc=25,dstart=29988,quiet=FALSE,
          countArgs=list(safety=0.0,complete.ranking=TRUE,nseats=3))
        save(xr,file="../s0di25ns3.rdata")
        plot(xr,title="testDeletions", file="s0di25ns3")

![](man/figures/s0di25ns3.png)
