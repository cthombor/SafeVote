# summarises vote-totals for subsequent printing

summarises vote-totals for subsequent printing

## Usage

``` r
.summary.SafeVote(object, larger.wins = TRUE, reorder = TRUE)
```

## Arguments

- object:

  vector of total votes per candidate

- larger.wins:

  TRUE if candidates are "voted in" rather than voted-out

- reorder:

  TRUE if output data.frame columns should be in rank-order

## Value

a data.frame with three columns and nc+1 rows, where nc is the number of
candidates. The first column contains candidate names and a final entry
named "Sum". The second column contains vote totals. The third column is
a vector of chars which indicate whether the candidate has been elected.
The data.frame has four named attributes carrying election parameters.

TODO: refactor into a modern dialect of R, perhaps by defining a
constructor for an election_info S3 object with a summary method and a
print method
