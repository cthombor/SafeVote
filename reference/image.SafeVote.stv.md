# visualisation of joint and marginal distributions in STV preferences

visualisation of joint and marginal distributions in STV preferences

## Usage

``` r
# S3 method for class 'SafeVote.stv'
image(x, xpref = 2, ypref = 1, all.pref = FALSE, proportion = TRUE, ...)
```

## Arguments

- x:

  STV results to be visualised

- xpref, ypref:

  candidates shown in a joint distribution plot

- all.pref:

  plot the joint distribution of two preferences (if 'all.pref=FALSE')
  or the marginal distribution of all preferences (if 'all.pref=TRUE').

- proportion:

  The joint distribution can be shown either as proportions (if
  'proportion=TRUE') or raw vote counts (if 'proportion=FALSE').

- ...:

  args passed to fields::image.plot()

## Value

image object, with side-effect in RStudio Plots pane
