# Dublin West vote count, identifying the top 3 candidates

Dublin West vote count, identifying the top 3 candidates

## Usage

``` r
dublin_west_vote_count_3seats
```

## Format

A copy of the output of the stv() method of vote 2.5-2, when it tallies
the ballots of candidates standing in the Dublin West election of 2002.
The runtime of that count is approximately 2 minutes on my laptop,
making it painful to regress against vote::stv(dublin_west, nseats = 3,
complete.ranking = TRUE).
