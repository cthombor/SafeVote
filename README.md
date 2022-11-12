# SafeVote
Mod of vote_2.3-2 (https://cran.r-project.org/web/packages/vote/index.html) to test the safety of
results from an STV or Condorcet count of a set of ballots.

Features:

Three routines for assessing the safety: 1) deletion of random ballots, 2) addition of "plumping"
ballots, and 3) selection of a randomly-selected fraction of the ballots.

Experimental implementation of SafeRank: a (possibly partial) ranking derived by clustering a
total ranking from a Condorcet count or an STV counting process.

Reformatting of whitespace and bracketing in existing code, for legibility and consistency.

Addition of roxygen2 comments.
