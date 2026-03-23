# parameter-checking method for nseats (internal)

parameter-checking method for nseats (internal)

## Usage

``` r
check.nseats(
  nseats = NULL,
  ncandidates,
  default = 1,
  mcan = NULL,
  complete.ranking = FALSE
)
```

## Arguments

- nseats:

  initially-specified number of seats to be filled in an election

- ncandidates:

  the number of candidates standing for election

- default:

  the return value of this function when nseats=NULL

- mcan:

  a deprecated name for nseats

- complete.ranking:

  when TRUE, the return value is in 1..ncandidates When FALSE, the
  return value is in 1..ncandidates-1 (for backwards compatibility)

## Value

a valid non-NULL value for the number of seats to be filled
