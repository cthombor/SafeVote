# Extracts the invalid.votes member (if any) from the result of a count

This method was added to stv in Jan 2022 – it was named in a warning
message but had apparently either never been implemented, or had been
"lost" through versioning.

## Usage

``` r
invalid.votes(x)
```

## Arguments

- x:

  value returned by stv, condorcet, approval, plurality, or score

## Value

matrix with one column per candidate and one row per invalid ballot
