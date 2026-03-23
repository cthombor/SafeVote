# Yale Faculty Senate 2016

This data follows the structure of a 2016 Yale Faculty Senate election,
with candidate names anonymised and permuted. Imported to SafeVote from
[STV v1.0.2](https://github.com/jayemerson/STV), after applying the
[`STV::cleanBallots`](https://rdrr.io/pkg/STV/man/cleanBallots.html)
method to remove the ten empty rows.

## Usage

``` r
data(yale_ballots)
```

## Format

A data frame with 479 observations and 44 candidates.
