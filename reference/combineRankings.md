# the least upper bound on a pair of rankings

the least upper bound on a pair of rankings

## Usage

``` r
combineRankings(r1, r2)
```

## Arguments

- r1, r2:

  numeric vectors

## Value

the most complete (but possibly partial) ranking which is consistent
with both r1 and r2. Uses `ties.method="min"`

## Examples

``` r
combineRankings( c(3,1,2), c(2,1,3) )
#> [1] 2 1 2
```
