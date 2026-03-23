# extract margins from the results of a ballot count

extract margins from the results of a ballot count

## Usage

``` r
extractMargins(marginNames, crRanks, cr)
```

## Arguments

- marginNames:

  list of colnames of the margins in our SafeRank result

- crRanks:

  ranks of candidates, not necessarily total

- cr:

  structure returned by a ballot-counting method

  Margins are adjusted for tied candidates, such that candidates within
  a tie group have margins indicative of their relative strengths.
  Extremely small margins are indicative of floating-point roundoff
  errors.

## Value

named list of margins
