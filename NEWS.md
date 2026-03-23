# SafeVote 1.0.2 (2026-03-24)

* Updated to allow regression against the current version (2.5-2) of vote -- 
which had added a "weights" feature.  The count of Dublin West ballots
by vote 2.5-2 is so slow (approx 2 minutes on my laptop) that I have stored
its results, and all regression tests now complete within 15 seconds.

* Reworked the view routines

# SafeVote 1.0.1 (2024-10-04)

## DOCUMENTATION FIXES

* I'm now person("Clark","Thomborson") rather than person("Clark Thomborson")
* I have reformatted NEWS.md, as required by github's pkgdown::build_news()

# SafeVote 1.0.0 (2023-01-18)

- Initial release to CRAN

