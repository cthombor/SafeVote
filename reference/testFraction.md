# Bootstrapping experiment, with fractional counts of a ballot box.

Starting from some number ('astart') of randomly-selected ballots, an
increasingly-large collection of randomly-selected ballots are counted.
The ballots are chosen independently without replacement for each
experimental unit; if you want to count decreasingly-sized portions of a
single sample of ballots, use [`testDeletions()`](testDeletions.md).

## Usage

``` r
testFraction(
  votes = NULL,
  astart = NULL,
  ainc = NULL,
  arep = NULL,
  trep = NULL,
  rankMethod = "safeRank",
  countMethod = "stv",
  countArgs = list(),
  exptName = NULL,
  equiet = FALSE,
  everbose = FALSE
)
```

## Arguments

- votes:

  A numeric matrix: one row per ballot, one column per candidate

- astart:

  Starting number of ballots (min 2)

- ainc:

  Number of ballots to be added in each step. Must be non-negative.

- arep:

  Number of repetitions of the test on each step. Required to be
  non-'NULL' if 'ainc=0 && is.null(trep)'.

- trep:

  Limit on the total number of simulated elections. Required to be
  non-'NULL' if 'ainc=0 && is.null(arep)'.

- rankMethod:

  "safeRank" (default), "elected", or "rank". "rank" is a total ranking
  of the candidates, with ties broken at random. "elected" assigns
  rank=1 to elected candidates, rank=2 for eliminated candidates.

- countMethod:

  countMethod "stv" (default) or "condorcet"

- countArgs:

  List of args to be passed to 'countMethod' (in addition to 'votes')

- exptName:

  stem-name of experimental units *e.g.* "E". If 'NULL', then a
  3-character string of capital letters is chosen at random.

- equiet:

  'TRUE' to suppress all experimental output

- everbose:

  'TRUE' to produce diagnostic output from the experiment

## Value

'SafeRankExpt' object of experimental results. See
[`new_SafeRankExpt()`](new_SafeRankExpt.md)

## Examples

``` r
testFraction(food_election, countMethod="condorcet",
             countArgs=list(safety=0.5,complete.ranking=TRUE))
#> Progress in counting condorcet ballots:
#>  10%, 20%, 30%, 40%, 50%, 60%, 70%, 80%, 90%, 100%
#> 
#> Results of testFraction at 2026-03-23 17:51:42
#> 
#> Dataset = food_election, countMethod = condorcet, rankMethod = safeRank
#> 
#> |          | safety| complete.ranking|
#> |:---------|------:|----------------:|
#> |countArgs |    0.5|             TRUE|
#> 
#> 
#> |             | astart| ainc| arep|
#> |:------------|------:|----:|----:|
#> |otherFactors |      2|    2|    1|
#> 
#> Experiment ID, number of ballots in simulated election, ranks, winning margins:
#> 
#> |exptID | nBallots| Oranges| Pears| Chocolate| Strawberries| Sweets| m.Oranges| m.Pears| m.Chocolate| m.Strawberries| m.Sweets|
#> |:------|--------:|-------:|-----:|---------:|------------:|------:|---------:|-------:|-----------:|--------------:|--------:|
#> |SLS1   |        2|       3|     3|         1|            3|      2|         0|       0|           2|              0|        6|
#> |SLS2   |        4|       3|     3|         1|            2|      3|         0|       1|           2|              2|        2|
#> |SLS3   |        6|       1|     3|         1|            3|      3|         5|       1|           6|              0|        3|
#> |SLS4   |        8|       3|     4|         1|            2|      4|         3|       1|           9|              8|        0|
#> |SLS5   |       10|       2|     2|         1|            2|      2|         4|       0|          12|              6|        2|
#> |SLS6   |       12|       2|     5|         1|            2|      2|        12|       0|          12|             16|       13|
#> |SLS7   |       14|       3|     5|         1|            2|      3|         6|       0|          18|              4|        5|
#> |SLS8   |       16|       3|     4|         1|            2|      4|         5|       0|          16|             14|        1|
#> |SLS9   |       18|       3|     5|         1|            2|      3|        10|       0|          19|              7|        8|
#> |SLS10  |       20|       3|     5|         1|            2|      4|         6|       0|          20|              6|        8|
testFraction(dublin_west, astart=20, ainc=10, arep=2, trep=3, 
             countMethod="stv", rankMethod="elected", equiet=FALSE)
#> Progress in counting stv ballots:
#>  33.3%, 66.7%, 100%
#> 
#> Results of testFraction at 2026-03-23 17:51:42
#> 
#> Dataset = dublin_west, countMethod = stv, rankMethod = elected
#> 
#> |             | astart| ainc| arep|
#> |:------------|------:|----:|----:|
#> |otherFactors |     20|   10|    2|
#> 
#> Experiment ID, number of ballots in simulated election, ranks, winning margins:
#> 
#> |exptID | nBallots| Bonnie| Burton| Ryan| Higgins| Lenihan| McDonald| Morrissey| Smyth| Terry| m.Bonnie| m.Burton|   m.Ryan| m.Higgins| m.Lenihan| m.McDonald| m.Morrissey|   m.Smyth|  m.Terry|
#> |:------|--------:|------:|------:|----:|-------:|-------:|--------:|---------:|-----:|-----:|--------:|--------:|--------:|---------:|---------:|----------:|-----------:|---------:|--------:|
#> |SUV1   |       20|      2|      1|    2|       1|       1|        2|         2|     2|     1|        3| 1.979580| 0.000000|  1.979580|  2.979579|   3.270405|   0.0000000| 1.0000000| 1.274170|
#> |SUV2   |       30|      2|      1|    1|       1|       1|        2|         2|     2|     2|        0| 3.273823| 1.774073|  1.254765|  5.273823|   4.923228|   0.4229778| 0.9232278| 3.614889|
#> |SUV3   |       20|      2|      2|    1|       1|       1|        2|         2|     2|     1|        1| 0.599550| 3.933258|  2.933258|  5.266925|   1.000000|   1.0660333| 0.0000000| 4.266925|
```
