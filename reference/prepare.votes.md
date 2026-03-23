# Coerce input 'data' into a matrix

Coerce input 'data' into a matrix

## Usage

``` r
prepare.votes(data, fsep = "\n")
```

## Arguments

- data:

  possibly a .csv file, possibly an R object

- fsep:

  separation character for .csv e.g. tab or comma

## Value

a matrix with one row per ballot, one column per candidate, with named
rows and columns
