# plot() method for the result of an stv() ballot-count

The `plot` function shows the evolution of the total score for each
candidate as well as the quota.

## Usage

``` r
# S3 method for class 'SafeVote.stv'
plot(x, xlab = "Count", ylab = "Preferences", point.size = 2, ...)
```

## Arguments

- x:

  stv results

- xlab, ylab:

  axis labels

- point.size:

  diameter of elected/eliminated points

- ...:

  params for generic plot()

## Value

graphics object, with side-effect in RStudio's Plots pane
