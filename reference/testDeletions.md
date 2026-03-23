# Assess the safety of a preliminary result for an election

Ballots are deleted at random from the ballot-box, with election results
computed once per `dinc` ballot-deletions. The experiment terminates
after a specified number of ballots have been deleted, or a specified
number of ballot-counts have occurred. Note: these ballot-counts are
correlated. Use
[testFraction()](https://cthombor.github.io/SafeVote/reference/testFraction)
to experiment with independently-drawn samples from the ballot-box.

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

  List of args to be passed to `countMethod` (in addition to `votes`)

- dstart:

  Number of ballots in the first ballot-count (selected at random from
  `votes`, without replacement)

- dinc:

  Number of ballots to be deleted in subsequent steps

- dlimit:

  Maximum number of ballots to delete (in addition to `dstart`)

- drep:

  Maximum number of elections (required if `dinc=0`)

- rankMethod:

  "safeRank" (default), "elected", or "rank". "rank" is a total ranking
  of the candidates, with ties broken at random. "elected" assigns
  rank=1 to elected candidates, rank=2 for eliminated candidates.

- exptName:

  stem-name of experimental units *e.g.* "E". If `NULL`, then a
  3-character string of capital letters is chosen at random.

- equiet:

  TRUE to suppress all experimental output

- everbose:

  TRUE to produce diagnostic output from the experiment

## Value

[SafeRankExpt](https://cthombor.github.io/SafeVote/reference/new_SafeRankExpt)
object, describing this experiment and its results

## Examples

``` r
data(food_election)
testDeletions(food_election)
#> Number of ballots counted by stv: 20, 18, 16, 14, 12, 10, 8, 6, 4, 2
#> 
#> Results of testDeletions at 2026-03-23 17:40:14
#> 
#> Dataset = food_election, countMethod = stv, rankMethod = safeRank
#> 
#> |             | dstart| dinc| dlimit| drep|
#> |:------------|------:|----:|------:|----:|
#> |otherFactors |     20|    2|      2|   10|
#> 
#> Unit factors: initSample, removedBallots
#> 
#> Experiment ID, number of ballots in simulated election, ranks, winning margins:
#> 
#> |exptID | nBallots| Oranges| Pears| Chocolate| Strawberries| Sweets| m.Oranges| m.Pears| m.Chocolate| m.Strawberries|  m.Sweets|
#> |:------|--------:|-------:|-----:|---------:|------------:|------:|---------:|-------:|-----------:|--------------:|---------:|
#> |ZCB0   |       20|       2|     2|         1|            2|      2|         6|       0|       8.000|      4.5548889| 2.7774444|
#> |ZCB1   |       18|       2|     2|         1|            2|      2|         6|       0|       6.000|      3.7993000| 2.1997000|
#> |ZCB2   |       16|       2|     2|         1|            2|      2|         5|       0|       6.000|      3.4437778| 2.2218889|
#> |ZCB3   |       14|       2|     2|         1|            2|      2|         4|       0|       5.000|      3.4992500| 1.8330833|
#> |ZCB4   |       12|       1|     1|         1|            1|      1|         4|       0|       7.000|      2.6658333| 1.3331667|
#> |ZCB5   |       10|       1|     1|         1|            1|      1|         3|       0|       6.000|      2.3325333| 1.3331333|
#> |ZCB6   |        8|       2|     2|         1|            2|      2|         2|       0|       3.000|      1.9992500| 0.0000000|
#> |ZCB7   |        6|       1|     1|         1|            1|      1|         0|       1|       3.666|      1.6660000| 1.3330000|
#> |ZCB8   |        4|       2|     2|         1|            2|      2|         0|       0|       2.000|      2.1104444| 0.5552222|
#> |ZCB9   |        2|       2|     2|         1|            2|      2|         0|       0|       2.000|      0.6661667| 0.6661667|
testDeletions(food_election, countMethod="stv",
  countArgs=list(complete.ranking=TRUE))
#> Number of ballots counted by stv: 20, 18, 16, 14, 12, 10, 8, 6, 4, 2
#> 
#> Results of testDeletions at 2026-03-23 17:40:14
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
#> |RLV0   |       20|       2|     2|         1|            2|      2| 4.0000000| 2.000000|      8.0000|      6.7771111| 3.8885556|
#> |RLV1   |       18|       2|     2|         1|            2|      2| 4.0000000| 2.000000|      6.0000|      5.8993000| 3.0997000|
#> |RLV2   |       16|       2|     2|         1|            2|      2| 3.0000000| 2.000000|      6.0000|      5.2215556| 3.1107778|
#> |RLV3   |       14|       2|     2|         1|            2|      2| 3.0000000| 2.000000|      4.0000|      3.6660952| 2.9995714|
#> |RLV4   |       12|       2|     2|         1|            2|      2| 2.0000000| 1.000000|      5.0000|      3.8565714| 3.1424286|
#> |RLV5   |       10|       2|     2|         1|            2|      2| 1.0000000| 1.000000|      5.0000|      3.1661667| 3.1661667|
#> |RLV6   |        8|       2|     2|         1|            2|      2| 1.0000000| 1.000000|      4.0000|      1.4662667| 3.1994000|
#> |RLV7   |        6|       2|     2|         1|            2|      2| 0.6656667| 1.665667|      3.0000|      2.1651667| 3.1651667|
#> |RLV8   |        4|       2|     2|         1|            2|      2| 0.5546389| 1.000000|      2.0000|      1.5548889| 0.7774444|
#> |RLV9   |        2|       1|     1|         1|            1|      1| 0.6656667| 1.000200|      1.0002|      0.6658667| 0.0000000|
```
