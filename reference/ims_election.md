# IMS Election

Datasets containing anonymized votes for a past Council election of the
Institute of Mathematical Statistics (IMS). The dataset ims_election is
the original dataset used with single transferable vote, where candidate
names have been changed.

## Usage

``` r
ims_election
```

## Format

A data frame with 620 observations and 10 candidates (names were made
up). Each record corresponds to one ballot. The IMS Council voting is
done using the STV method, and thus the ims_election dataset contains
ballots with candidates being ranked between 1 and 10 with zeros
allowed.
