# Count preferential ballots using an STV method

The 'votes' parameter is as described in [`condorcet()`](condorcet.md)
with the following additional semantics.

## Usage

``` r
stv(
  votes,
  nseats = NULL,
  eps = 0.001,
  equal.ranking = FALSE,
  fsep = "\t",
  ties = c("f", "b"),
  quota.hare = FALSE,
  constant.quota = FALSE,
  win.by.elim = TRUE,
  group.nseats = NULL,
  group.members = NULL,
  complete.ranking = FALSE,
  invalid.partial = FALSE,
  verbose = FALSE,
  seed = NULL,
  quiet = FALSE,
  digits = 3,
  backwards.compatible = FALSE,
  safety = 1,
  ...
)
```

## Arguments

- votes:

  an array with one column per candidate and one row per ballot, as
  described in [`condorcet()`](condorcet.md)

- nseats:

  the number of seats to be filled in this election

- eps:

  fuzz-factor when comparing fractional votes. The default of 0.001 is
  preserved from the legacy code, injecting substantial validity hazards
  into the codebase. We have not attempted to mitigate any of these
  hazards in 'SafeVote v1.0.0'. We prefer instead to retain
  backwards-compatibility with the legacy code in 'vote_2.3-2' in the
  knowledge that, even if these hazards were adequately addressed, the
  resulting code is unlikely to be reliable at replicating the results
  of any other implementation of any of the many variants of "STV"
  counting methods. Please see the description of the 'a53_hil' dataset
  in this package for some preliminary findings on the magnitude of the
  vote-count-variances which may be injected by differing
  implementations of broadly-similar "STV" counting methods.

- equal.ranking:

  if 'TRUE', equal preferences are allowed.

- fsep:

  column-separator for output

- ties:

  vector of tie-breaking methods: ”f” for forward, ”b” for backward

- quota.hare:

  'TRUE' if Hare quota, 'FALSE' if Droop quota (default)

- constant.quota:

  'TRUE' if quota is held constant. Over-rides 'quota.hare'. Default is
  'FALSE'

- win.by.elim:

  'TRUE' (default) if the quota is waived when there are no more
  candidates than vacant seats. Note: there is no lower limit when the
  quota is waived, so a candidate may be elected on zero votes.

- group.nseats:

  number of seats reserved to members of a group

- group.members:

  vector of members of the group with reserved seats

- complete.ranking:

  is 'TRUE' by default. This parameter is retained solely for backwards
  compatibility with
  [`vote::stv()`](https://rdrr.io/pkg/vote/man/stv.html). It has no
  effect on elections in which 'nseats' is explicitly specified in the
  call to `stv()`.

- invalid.partial:

  'TRUE' if ballots which do not specify a complete ranking of
  candidates are informal (aka "invalid") *i.e.* ignored (with a
  warning). Default is 'FALSE'.

- verbose:

  'TRUE' for diagnostic output

- seed:

  integer seed for tie-breaking. Warning: if non-'NULL', the PRNG for R
  is reseeded prior to *every* random tie-break among the
  possibly-elected candidates. We have preserved this functionality in
  this branch to allow regression against the legacy codebase of
  [`vote::stv()`](https://rdrr.io/pkg/vote/man/stv.html). In `stv()` the
  default value for seed is 'NULL' rather than the legacy value of 1234,
  to mitigate the validity hazard of PRNG reseedings during a stochastic
  experiment.

- quiet:

  'TRUE' to suppress console output, and also output to Viewer

- digits:

  number of significant digits in the output table

- backwards.compatible:

  'TRUE' to regress against vote2_3.2 by disabling \$margins, \$fuzz,
  \$rankingTable, \$safeRank

- safety:

  number of standard deviations on vote-counts, when producing a
  safeRank by clustering near-ties in a complete ranking

- ...:

  undocumented intent (preserved from legacy code)

## Value

object of class 'vote.stv'. Note: the winning margins in this object are
valid for the elected candidates and their (total) ranking, but must be
adjusted within tiegroups to be valid for the candidates' (possibly
partial) safeRank.

## Details

By default the preferences are not allowed to contain duplicates per
ballot. However, if the argument 'equal.ranking' is set to 'TRUE',
ballots are allowed to have the same ranking for multiple candidates.
The desired format is such that for each preference \$i\$ that does not
have any duplicate, there must be exactly \$i – 1\$ preferences \$j\$
with \$0 \< j \< i\$. For example, valid ordered preferences are '1; 1;
3; 4; …', or '1; 2; 3; 3; 3; 6; …', but NOT '1; 1; 2; 3; …', or NOT '1;
2; 3; 3; 3; 5; 6; …'. If the data contain such invalid votes, they are
automatically corrected and a warning is issued by calling the
'correct.ranking' function.

If equal ranking is not allowed ('equal.ranking = FALSE'), the argument
'invalid.partial' can be used to make ballots containing duplicates or
gaps partially valid. If it is 'TRUE', a ballot is considered valid up
to a preference that is in normal case not allowed. For example, ballots
'1; 2; 3; 4; 4; 6' or '1; 2; 3; 5; 6; 7' would be both converted into
'1; 2; 3; 0; 0; 0', because the ballots contain valid ranking only up to
the third preference.

By default, ties in the STV algorithm are resolved using the forwards
tie-breaking method, see Newland and Briton (Section 5.2.5). Argument
'ties' can be set to ”b” in order to use the backwards tie-breaking
method, see O’Neill (2004). In addition, both methods are complemented
by the following “ordered” method: Prior to the STV election candidates
are ordered by the number of first preferences. Equal ranks are resolved
by moving to the number of second preferences, then third and so on.
Remaining ties are broken by random draws. Such complete ordering is
used to break any tie that cannot be resolved by the forwards or
backwards method. If there is at least one tie during the processing,
the output contains a row indicating in which count a tie-break happened
(see the 'ties' element in the Value section for an explanation of the
symbols).

The ordered tiebreaking described above can be analysed from outside of
the 'stv' function by using the 'ordered.tiebreak' function for Viewing
the a-priori ordering (the highest number is the best and lowest is the
worst). Such ranking is produced by comparing candidates along the
columns of the matrix returned by 'ordered.preferences'.

## Examples

``` r
summary(stv(food_election, safety = 0.0, quiet=TRUE))
#> 
#> Results of Single transferable vote
#> ===================================                           
#> Number of valid votes:   20
#> Number of invalid votes:  0
#> Number of candidates:     5
#> Number of seats:          2
#> Number of unfilled seats: 0
#> 
#> Fuzz on safeRank:     0
#> Complete ranking:
#> 
#> |             | TotalRank|SafeRank |  Margin  |
#> |:------------|---------:|:--------|:--------:|
#> |Oranges      |         2|2        | 1.445111 |
#> |Pears        |         5|5        | 0.000000 |
#> |Chocolate    |         1|1        | 8.000000 |
#> |Strawberries |         3|3        | 1.777444 |
#> |Sweets       |         4|4        | 2.777444 |
#> 
#> Vote transfers:
#> 
#> |             |         1| 2-trans|     2| 3-trans|      3| 4-trans|       4|
#> |:------------|---------:|-------:|-----:|-------:|------:|-------:|-------:|
#> |Oranges      |     4.000|   0.000| 4.000|       2|  6.000|   0.000|   6.000|
#> |Pears        |     2.000|   0.000| 2.000|      -2|       |        |        |
#> |Chocolate    |    12.000|  -5.332|      |        |       |        |        |
#> |Strawberries |     1.000|   3.555| 4.555|       0|  4.555|   0.000|   4.555|
#> |Sweets       |     1.000|   1.777| 2.777|       0|  2.777|  -2.777|        |
#> |Tie-breaks   |          |        |      |        |       |        |        |
#> |Elected      | Chocolate|        |      |        |       |        | Oranges|
#> |Eliminated   |          |        | Pears|        | Sweets|        |        |
#> 
#> Elected: Chocolate, Oranges 
#> 
summary(stv(food_election, nseats = 2, quiet=TRUE))
#> 
#> Results of Single transferable vote
#> ===================================                           
#> Number of valid votes:   20
#> Number of invalid votes:  0
#> Number of candidates:     5
#> Number of seats:          2
#> Number of unfilled seats: 0
#> 
#> Fuzz on safeRank:     4.472136
#> Complete ranking:
#> 
#> |             | TotalRank|SafeRank |  Margin  |
#> |:------------|---------:|:--------|:--------:|
#> |Oranges      |         2|2        | 1.445111 |
#> |Pears        |         5|2        | 0.000000 |
#> |Chocolate    |         1|1        | 8.000000 |
#> |Strawberries |         3|2        | 1.777444 |
#> |Sweets       |         4|2        | 2.777444 |
#> 
#> Vote transfers:
#> 
#> |             |         1| 2-trans|     2| 3-trans|      3| 4-trans|       4|
#> |:------------|---------:|-------:|-----:|-------:|------:|-------:|-------:|
#> |Oranges      |     4.000|   0.000| 4.000|       2|  6.000|   0.000|   6.000|
#> |Pears        |     2.000|   0.000| 2.000|      -2|       |        |        |
#> |Chocolate    |    12.000|  -5.332|      |        |       |        |        |
#> |Strawberries |     1.000|   3.555| 4.555|       0|  4.555|   0.000|   4.555|
#> |Sweets       |     1.000|   1.777| 2.777|       0|  2.777|  -2.777|        |
#> |Tie-breaks   |          |        |      |        |       |        |        |
#> |Elected      | Chocolate|        |      |        |       |        | Oranges|
#> |Eliminated   |          |        | Pears|        | Sweets|        |        |
#> 
#> Elected: Chocolate, Oranges 
#> 
```
