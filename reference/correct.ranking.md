# Amend ballots with equal or incomplete preferences

The 'correct.ranking' function returns a modified set of ballots. Its
argument 'partial' determines if ballots are partially set to '0'
('TRUE'), or if it is a complete re-ranking, as allowed when
'equal.ranking = TRUE'. It can be used by calling it explicitly. It is
called by 'stv' if 'equal.ranking = TRUE' or 'invalid.partial = TRUE'.
It is also called from within the 'condorcet' function with the default
value ('FALSE') for 'partial', i.e. interpreting any '0' as a last=
preference.

## Usage

``` r
correct.ranking(votes, partial = FALSE, quiet = FALSE)
```

## Arguments

- votes:

  original contents of ballot box

- partial:

  if 'FALSE' (default), each ballot is interpreted, if possible, as a
  complete (but not necessarily total) ranking of the candidates. If
  'TRUE', a ballot will contain a '0' on unranked candidates.

- quiet:

  suppress diagnostics

## Value

corrected ballots
