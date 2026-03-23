# Test the sensitivity of a result to tactical voting.

Ballots are added until a specified number of simulated elections
('arep') have been held. If a 'favoured' candidate is specified, then
the ballot-box is stuffed with ballots awarding first-preference to this
candidate. Alternatively, a 'tacticalBallot' may be specified. If both
'favoured' and 'tacticalBallot' are 'NULL', then a random candidate is
selected as the favoured one.

## Usage

``` r
testAdditions(
  votes,
  ainc = 1,
  arep = NULL,
  favoured = NULL,
  tacticalBallot = NULL,
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

  A set of ballots, as in
  [vote_2.3.2](https://CRAN.R-project.org/package=vote)

- ainc:

  Number of ballots to be added in each step

- arep:

  Maximum number of ballot-stuffed elections to run

- favoured:

  Name of the candidate being "plumped". If 'NULL', a random candidate
  is selected from among the candidates not initially top-ranked. All
  other candidates are fully-ranked at random, with an identical ballot
  paper being stuffed multiple times. An integer value for 'favoured' is
  interpreted as an index into the candidate names.

- tacticalBallot:

  A ballot paper i.e. a vector of length 'ncol(ballots)'. If this
  argument is non-'NULL', it takes precedence over 'favoured' when the
  ballot box is being stuffed.

- rankMethod:

  "safeRank" (default), "elected", or "rank". "rank" is a total ranking
  of the candidates, with ties broken at random. "elected" assigns
  rank=1 to elected candidates, rank=2 for eliminated candidates.

- countMethod:

  countMethod "stv" (default) or "condorcet"

- countArgs:

  List of args to be passed to countMethod (in addition to votes)

- exptName:

  stem-name of experimental units *e.g.* "E". If 'NULL', then a
  3-character string of capital letters is chosen at random.

- equiet:

  'TRUE' to suppress all experimental output

- everbose:

  'TRUE' to produce diagnostic output from the experiment

## Value

'SafeRankExpt' object, containing a matrix of experimental results, of
dimension \\n\\ by \\2m+1\\, where \\n\\ is the number of elections and
\\m\\ is the number of candidates. The first column is named "nBallots".
Other columns indicate the ranking of the eponymous candidate, and their
margin over the next-lower-ranked candidate. See
[`new_SafeRankExpt()`](new_SafeRankExpt.md)

## Examples

``` r
testAdditions(food_election, arep = 2, favoured = "Strawberries", 
  countArgs = list(safety = 0))
#> 
#> Adding up to 2 stv ballots = ( 5 2 4 1 3 )
#> Testing progress:  1, 2
#> 
#> Results of testAdditions at 2026-03-23 17:51:42
#> 
#> Dataset = food_election, countMethod = stv, rankMethod = safeRank
#> 
#> |          | safety|
#> |:---------|------:|
#> |countArgs |      0|
#> 
#> 
#> |             | ainc| arep|                                                         tacticalBallot|
#> |:------------|----:|----:|----------------------------------------------------------------------:|
#> |otherFactors |    1|    2| c(Oranges = 5, Pears = 2, Chocolate = 4, Strawberries = 1, Sweets = 3)|
#> 
#> Experiment ID, number of ballots in simulated election, ranks, winning margins:
#> 
#> |exptID | nBallots| Oranges| Pears| Chocolate| Strawberries| Sweets| m.Oranges| m.Pears| m.Chocolate| m.Strawberries| m.Sweets|
#> |:------|--------:|-------:|-----:|---------:|------------:|------:|---------:|-------:|-----------:|--------------:|--------:|
#> |KEU0   |       20|       2|     5|         1|            3|      4| 1.4451111|       0|           8|      1.7774444| 2.777444|
#> |KEU1   |       21|       2|     5|         1|            3|      4| 0.6673333|       0|           8|      2.6663333| 2.666333|
#> |KEU2   |       22|       3|     5|         1|            2|      4| 3.4447778|       0|           8|      0.1104444| 2.555222|
```
