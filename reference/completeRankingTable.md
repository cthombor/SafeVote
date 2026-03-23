# internal method to analyse the partial results of an stv() ballot count, to discover a complete ranking of all candidates. The ranking may depend on the value of nseats, because this affects how votes are transferred.

internal method to analyse the partial results of an stv() ballot count,
to discover a complete ranking of all candidates. The ranking may depend
on the value of nseats, because this affects how votes are transferred.

## Usage

``` r
completeRankingTable(object, quiet, verbose)
```

## Arguments

- object:

  partial results

- quiet:

  TRUE to suppress console output

- verbose:

  TRUE to produce diagnostic output

## Value

data.frame with columns TotalRank, Margin, Candidate, Elected, SafeRank
