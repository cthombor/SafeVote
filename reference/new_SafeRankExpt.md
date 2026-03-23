# Constructor for the results of a SafeRank experiment

Constructor for the results of a SafeRank experiment

## Usage

``` r
new_SafeRankExpt(
  rankNames = list(),
  marginNames = list(),
  countMethod = character(0),
  rankMethod = character(0),
  datasetName = character(0),
  experimentalMethod = character(0),
  countArgs = list(),
  nseats = integer(0),
  otherFactors = list(),
  unitFactors = list()
)
```

## Arguments

- rankNames:

  colnames for per-candidate ranks

- marginNames:

  colnames for per-candidate margins

- countMethod:

  secondary factor: counting method e.g. "stv"

- rankMethod:

  secondary factor: ranking method e.g. "elected"

- datasetName:

  secondary factor: name of the dataset of ballots

- experimentalMethod:

  secondary factor: name of the method which simulated these elections
  e.g. "testFraction"

- countArgs:

  secondary factor: args passed to countMethod

- nseats:

  secondary factor: number of seats to be filled

- otherFactors:

  other secondary factors, e.g. parameters to experimentalMethod

- unitFactors:

  per-unit factors derived from PRNG of the experimental harness, e.g
  describing the ballots randomly deleted during testDeletions

## Value

object of class SafeRankExpt
