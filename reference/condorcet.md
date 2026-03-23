# Count votes using the Condorcet voting method.

The Condorcet method elects the candidate who wins a majority of the
ranked vote in every head to head election against each of the other
candidates. A Condorcet winner is a candidate who beats all other
candidates in pairwise comparisons. Analogously, a Condorcet loser is a
candidate who loses against all other candidates. Neither Condorcet
winner nor loser might exist.

## Usage

``` r
condorcet(
  votes,
  runoff = FALSE,
  nseats = 1,
  safety = 1,
  fsep = "\t",
  quiet = FALSE,
  ...
)
```

## Arguments

- votes:

  A [matrix](https://rdrr.io/r/base/matrix.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html) containing the
  votes. Rows correspond to the votes, columns correspond to the
  candidates. If `votes` is a character string, it is interpreted as a
  file name from which the votes are to be read. See
  [below](https://cthombor.github.io/SafeVote/reference/condorcet#details).

- runoff:

  Logical. If [TRUE](https://rdrr.io/r/base/logical.html) and no
  Condorcet winner exists, the election goes into a run-off, see
  [below](https://cthombor.github.io/SafeVote/reference/condorcet#details).

- nseats:

  the number of seats to be filled in this election

- safety:

  Parameter for a clustering heuristic on a total ranking of the
  candidates. Conjecture: the default of `1.0` ensures a separation of
  one s.d. between clusters, when `votes` are i.u.d. permutations on the
  candidates.

- fsep:

  If `votes` is a file name, this argument gives the column separator in
  the file.

- quiet:

  If [TRUE](https://rdrr.io/r/base/logical.html) no output is printed.

- ...:

  Undocumented intent (preserved from legacy code)

## Value

Object of class `SafeVote.condorcet`

## Details

If the runoff argument is set to `TRUE` and no Condorcet winner exists,
two or more candidates with the most pairwise wins are selected and the
method is applied to such subset. If more than two candidates are in
such run-off, the selection is performed repeatedly, until either a
winner is selected or no more selection is possible.

The input data votes is structured the same way as for the [stv](stv.md)
method: Row `i` contains the preferences of voter `i` numbered
`1; 2; : : : ; r; 0; 0; 0; 0`, in some order, while equal preferences
are allowed. The columns correspond to the candidates. The
[dimnames](https://rdrr.io/r/base/dimnames.html) of the columns are the
names of the candidates; if these are not supplied then the candidates
are lettered `A, B, C, ...`. If the dataset contains missing values
([NA](https://rdrr.io/r/base/NA.html)), they are replaced by zeros.

If a ballot has equally-ranked candidates, its rankings are tested for
validity: for each preference \\i\\ which does not have any duplicate,
there are exactly \\i - 1\\ preferences \\j\\ with \\0 \< j \< i\\. If
any ballot `x` fails this validity test, it is automatically corrected
(aka "converted") into a valid ballot using
`x <- rank(x, ties.method = "min")`, and a warning is issued.

This method also computes a Borda ranking of all candidates, using
tournament-style scoring. This ranking is "fuzzed" into a `safeRank`,
with approximately 1 s.d. of fuzz when `safety=1.0` and voter
preferences are i.u.d. A warning is thrown if a `safeRank` violates the
(extended) Condorcet principle: that Candidate \\i\\ is more highly
ranked than Candidate \\j\\ only if a majority of voters agree with
this.

## Examples

``` r
{
data(food_election)
condorcet(food_election)
}
#> 
#> Results of Condorcet voting
#> ===========================                           
#> Number of valid votes:   20
#> Number of invalid votes:  0
#> Number of candidates:     5
#> Number of seats:          1
#> 
#> 
#> |             | Oranges| Pears| Chocolate| Strawberries| Sweets| Total| Score | BordaRank | margin| SafeRank| Winner| Loser|
#> |:------------|-------:|-----:|---------:|------------:|------:|-----:|:-----:|:---------:|------:|--------:|------:|-----:|
#> |Oranges      |       0|     1|         0|            0|      1|     2|  22   |     3     |      6|        2|       |      |
#> |Pears        |       0|     0|         0|            0|      0|     0|   8   |     5     |      0|        2|       |     x|
#> |Chocolate    |       1|     1|         0|            1|      1|     4|  48   |     1     |     20|        1|      x|      |
#> |Strawberries |       1|     1|         0|            0|      1|     3|  28   |     2     |      6|        2|       |      |
#> |Sweets       |       0|     1|         0|            0|      0|     1|  16   |     4     |      8|        2|       |      |
#> 
#> safeRank fuzz on Borda scores: 8.944272
#> Gaps in Borda scores: min 6 mean 9.6 max 20 ; all 6 6 8 8 20
#> Condorcet winner: Chocolate
#> Condorcet loser: Pears
#> 
```
