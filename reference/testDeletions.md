# Assess the safety of a preliminary result for an election

Ballots are deleted at random from the ballot-box, with election results
computed once per 'dinc' ballot-deletions. The experiment terminates
after a specified number of ballots have been deleted, or a specified
number of ballot-counts have occurred. Note: these ballot-counts are
correlated. Use [`testFraction()`](testFraction.md) to experiment with
independently-drawn samples from the ballot-box.

## Usage

``` r
testDeletions(
  votes,
  countMethod = "stv",
  countArgs = list(),
  dstart = NULL,
  dinc = NULL,
  dlimit = NULL,
  drep = NULL,
  rankMethod = "safeRank",
  exptName = NULL,
  equiet = FALSE,
  everbose = FALSE
)
```

## Arguments

- votes:

  A set of ballots, as in
  [vote_2.3.2](https://CRAN.R-project.org/package=vote)

- countMethod:

  "stv" (default) or "condorcet"

- countArgs:

  List of args to be passed to 'countMethod' (in addition to 'votes')

- dstart:

  Number of ballots in the first ballot-count (selected at random from
  'votes', without replacement)

- dinc:

  Number of ballots to be deleted in subsequent steps

- dlimit:

  Maximum number of ballots to delete (in addition to 'dstart')

- drep:

  Maximum number of elections (required if 'dinc=0')

- rankMethod:

  "safeRank" (default), "elected", or "rank". "rank" is a total ranking
  of the candidates, with ties broken at random. "elected" assigns
  rank=1 to elected candidates, rank=2 to eliminated candidates.

- exptName:

  stem-name of experimental units *e.g.* "E". If 'NULL', then a
  3-character string of capital letters is chosen at random.

- equiet:

  TRUE to suppress all experimental output

- everbose:

  TRUE to produce diagnostic output from the experiment

## Value

'SafeRankExpt' object of experimental results. See
[`new_SafeRankExpt()`](new_SafeRankExpt.md)

## Examples

``` r
testDeletions(food_election, countMethod="stv",
  countArgs=list(complete.ranking=TRUE))
#> Number of ballots counted by stv: 20, 18, 16, 14, 12, 10, 8, 6, 4, 2
#> 
#> Results of testDeletions at 2026-03-23 18:19:36
#> 
#> Dataset = food_election, countMethod = stv, rankMethod = safeRank
#> 
#> |          | complete.ranking|
#> |:---------|----------------:|
#> |countArgs |             TRUE|
#> 
#> 
#> |             | dstart| dinc| dlimit| drep|
#> |:------------|------:|----:|------:|----:|
#> |otherFactors |     20|    2|      2|   10|
#> 
#> Unit factors: initSample, removedBallots
#> 
#> Experiment ID, number of ballots in simulated election, ranks, winning margins:
#> 
#> |exptID | nBallots| Oranges| Pears| Chocolate| Strawberries| Sweets| m.Oranges|  m.Pears| m.Chocolate| m.Strawberries|  m.Sweets|
#> |:------|--------:|-------:|-----:|---------:|------------:|------:|---------:|--------:|-----------:|--------------:|---------:|
#> |ZCB0   |       20|       2|     2|         1|            2|      2| 4.0000000| 2.000000|    8.000000|      6.7771111| 3.8885556|
#> |ZCB1   |       18|       2|     2|         1|            2|      2| 4.0000000| 2.000000|    6.000000|      5.8993000| 3.0997000|
#> |ZCB2   |       16|       2|     2|         1|            2|      2| 3.0000000| 2.000000|    6.000000|      5.2215556| 3.1107778|
#> |ZCB3   |       14|       2|     2|         1|            2|      2| 3.0000000| 1.000000|    5.000000|      5.2492500| 2.4164167|
#> |ZCB4   |       12|       1|     1|         1|            1|      1| 3.0000000| 1.000000|    7.332500|      4.3325000| 1.6665000|
#> |ZCB5   |       10|       1|     1|         1|            1|      1| 2.0000000| 1.000000|    6.665867|      3.6658667| 1.6664667|
#> |ZCB6   |        8|       2|     2|         1|            2|      2| 1.0000000| 1.000000|    3.000000|      2.9992500| 1.6664167|
#> |ZCB7   |        6|       1|     1|         1|            1|      1| 0.6656667| 1.665667|    4.998333|      2.9983333| 2.3320000|
#> |ZCB8   |        4|       2|     2|         1|            2|      2| 0.0000000| 0.000000|    2.000000|      2.5548889| 0.7774444|
#> |ZCB9   |        2|       2|     2|         1|            2|      2| 0.0000000| 0.000000|    2.000000|      0.8328333| 0.8328333|
```
