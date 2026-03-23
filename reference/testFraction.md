# Bootstrapping experiment, with fractional counts of a ballot box.

Starting from some number (`astart`) of randomly-selected ballots, an
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
  non-`NULL` if `ainc=0` && is.null(trep)\`.

- trep:

  Limit on the total number of simulated elections. Required to be
  non-`NULL` if `ainc=0 && is.null(arep)`.

- rankMethod:

  "safeRank" (default), "elected", or "rank". "rank" is a total ranking
  of the candidates, with ties broken at random. "elected" assigns
  rank=1 to elected candidates, rank=2 for eliminated candidates.

- countMethod:

  countMethod "stv" (default) or "condorcet"

- countArgs:

  List of args to be passed to `countMethod` (in addition to `votes`)

- exptName:

  stem-name of experimental units *e.g.* "E". If `NULL`, then a
  3-character string of capital letters is chosen at random.

- equiet:

  `TRUE` to suppress all experimental output

- everbose:

  `TRUE` to produce diagnostic output from the experiment

## Value

a
[SafeRankExpt](https://cthombor.github.io/SafeVote/reference/new_SafeRankExpt)
object of experimental results.

## Examples

``` r
data(food_election)
testFraction(food_election, countMethod="condorcet",
             countArgs=list(safety=0.5,complete.ranking=TRUE))
#> Progress in counting condorcet ballots:
#>  10%, 20%, 30%, 40%, 50%, 60%, 70%, 80%, 90%, 100%
#> 
#> Results of testFraction at 2026-03-23 17:40:15
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
#> |OKT1   |        2|       4|     4|         1|            2|      2|         0|       0|           5|              6|        3|
#> |OKT2   |        4|       3|     3|         1|            1|      3|         0|       1|           4|              3|        0|
#> |OKT3   |        6|       2|     5|         1|            2|      2|         8|       0|           4|              7|        6|
#> |OKT4   |        8|       1|     5|         1|            4|      1|         7|       0|           5|              7|        3|
#> |OKT5   |       10|       3|     5|         1|            2|      3|         8|       0|          17|              6|        9|
#> |OKT6   |       12|       2|     5|         1|            2|      2|        11|       0|          10|              5|       10|
#> |OKT7   |       14|       2|     4|         1|            2|      4|         8|       0|           9|              9|        2|
#> |OKT8   |       16|       3|     4|         1|            2|      4|         8|       0|          11|              7|        2|
#> |OKT9   |       18|       2|     5|         1|            2|      4|         6|       0|          18|             12|        8|
#> |OKT10  |       20|       3|     5|         1|            2|      4|         6|       0|          20|              6|        8|
testFraction(dublin_west, astart=20, ainc=10, arep=2, trep=3, 
             countMethod="stv", rankMethod="elected", equiet=FALSE)
#> Progress in counting stv ballots:
#>  33.3%, 66.7%, 100%
#> 
#> Results of testFraction at 2026-03-23 17:40:15
#> 
#> Dataset = dublin_west, countMethod = stv, rankMethod = elected
#> 
#> |             | astart| ainc| arep|
#> |:------------|------:|----:|----:|
#> |otherFactors |     20|   10|    2|
#> 
#> Experiment ID, number of ballots in simulated election, ranks, winning margins:
#> 
#> |exptID | nBallots| Bonnie| Burton| Ryan| Higgins| Lenihan| McDonald| Morrissey| Smyth| Terry|  m.Bonnie|  m.Burton|    m.Ryan| m.Higgins| m.Lenihan| m.McDonald| m.Morrissey| m.Smyth|   m.Terry|
#> |:------|--------:|------:|------:|----:|-------:|-------:|--------:|---------:|-----:|-----:|---------:|---------:|---------:|---------:|---------:|----------:|-----------:|-------:|---------:|
#> |ATJ1   |       20|      2|      2|    1|       1|       1|        1|         2|     2|     2| 0.0260056| 3.5301390| 0.7576946|  2.287453|  1.688053|   3.287453|   1.0000000|       0| 2.0440168|
#> |ATJ2   |       30|      2|      1|    1|       1|       1|        2|         2|     2|     2| 0.9181389| 0.8824781| 2.2601184|  4.029811| 11.029811|   3.760623|   5.2222379|       0| 0.4565235|
#> |ATJ3   |       20|      2|      1|    2|       1|       1|        1|         2|     2|     2| 3.3996000| 1.6311954| 3.7992000|  3.830995|  3.830995|   0.446673|   0.9692046|       0| 0.7694046|
```
