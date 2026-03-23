# Extract a ranking vector by name from the results of a ballot count

Extract a ranking vector by name from the results of a ballot count

## Usage

``` r
extractRank(rankMethod, cr)
```

## Arguments

- rankMethod:

  "safeRank", "elected", or "rank"

- cr:

  structure returned by a ballot-counting method

## Value

a numeric ranking vector, in order of colnames(cr\$data)
