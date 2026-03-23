# plot() method for the result of an experiment with varying numbers of ballots

The "adjusted rank" of a candidate is their ranking \\r\\ plus their
scaled "winning margin". The scaled margin is \\e^{-cx/\sqrt{n}}\\,
where \\x\\ is the adjusted margin (i.e. the number of votes by which
this candidate is ahead of the next-weaker candidate, adjusted for the
number of ballots \\n\\ and the number of seats \\s\\), and \\c\>0\\ is
the margin-scaling parameter `cMargin`.

## Usage

``` r
# S3 method for class 'SafeRankExpt'
plot(
  x,
  facetWrap = FALSE,
  nResults = NA,
  anBallots = 0,
  cMargin = 1,
  xlab = "Ballots",
  ylab = "Adjusted Rank",
  title = NULL,
  subtitle = "(default)",
  line = TRUE,
  boxPlot = FALSE,
  boxPlotCutInterval = 10,
  pointSize = 1,
  ...
)
```

## Arguments

- x:

  object containing experimental results

- facetWrap:

  TRUE provides per-candidate scatterplots

- nResults:

  number of candidates whose results are plotted (omitting the
  least-favoured candidates first)

- anBallots, cMargin:

  parameters in the rank-adjustment formula

- xlab, ylab:

  axis labels

- title:

  overall title for the plot. Default: NULL

- subtitle:

  subtitle for the plot. Default: value of nSeats and any non-zero
  rank-adjustment parameters

- line:

  TRUE will connect points with lines, and will disable jitter

- boxPlot:

  TRUE for a boxplot, rather than the default xy-scatter

- boxPlotCutInterval:

  parameter of boxplot, default 10

- pointSize:

  diameter of points

- ...:

  params for generic plot()

## Value

graphics object, with side-effect in RStudio Plots pane

## Details

The default value of `cMargin=1.0` draws visual attention to candidates
with a very small winning margin, as their adjusted rank is very near to
\\r+1\\. Candidates with anything more than a small winning margin have
only a small rank adjustment, due to the exponential scaling.

A scaling linear in \\s/n\\ is applied to margins when `anBallots>0`.
Such a linear scaling may be a helpful way to visualise the winning
margins in STV elections because the margin of victory for an elected
candidate is typically not much larger than the quota of \\n/(s+1)\\
(Droop) or \\n/s\\ (Hare). The linear scaling factor is \\as/n\\, where
\\a\\ is the value of `anBallots`, \\s\\ is the number of seats, and
\\n\\ is the number of ballots. For plotting on the (inverted) adjusted
rank scale, the linearly-scaled margin is added to the candidate's rank.
Note that the linearly-scaled margins are zero when \\a=0\\, and thus
have no effect on the adjusted rank. You might want to increase the
value of `anBallots`, starting from 1.0, until the winning candidate's
adjusted rank is 1.0 when all ballots are counted, then confirm that the
adjusted ranks of other candidates are still congruent with their
ranking (i.e. that the rank-adjustment is less than 1 in all cases
except perhaps on an initial transient with small numbers of ballots).

When both `anBallots` and `cMargins` are non-zero, the ranks are
adjusted with both exponentially-scaled margins and linearly-scaled
margins. The resulting plot would be difficult to interpret in a valid
way.

Todo: Accept a list of SafeVoteExpt objects.

Todo: Multiple counts with the same number of ballots could be
summarised with a box-and-whisker graphic, rather than a set of jittered
points.

Todo: Consider developing a linear scaling that is appropriate for
plotting stochastic experimental data derived from Condorcet elections.
