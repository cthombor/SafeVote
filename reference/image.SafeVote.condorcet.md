# The image function visualizes the joint distribution of two preferences (if 'all.pref=FALSE') given 'xpref' and 'ypref', as well as the marginal distribution of all preferences (if 'all.pref=TRUE'). The joint distribution can be shown as proportions (if 'proportion=TRUE') or raw vote counts (if 'proportion=FALSE').

The image function visualizes the joint distribution of two preferences
(if 'all.pref=FALSE') given 'xpref' and 'ypref', as well as the marginal
distribution of all preferences (if 'all.pref=TRUE'). The joint
distribution can be shown as proportions (if 'proportion=TRUE') or raw
vote counts (if 'proportion=FALSE').

## Usage

``` r
# S3 method for class 'SafeVote.condorcet'
image(x, ...)
```

## Arguments

- x:

  object of type SafeVote.condorcet

- ...:

  See arguments for [image.SafeVote.stv](image.SafeVote.stv.md),
  especially 'xpref', 'ypref', 'all.pref' and 'proportion'.

## Value

image object, with side-effect in RStudio Plots pane
