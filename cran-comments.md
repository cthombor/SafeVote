## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* My tests include some ad-hoc regressions against vote 2.3-2 and stv 1.02.
* I have tested only within RStudio on Win11.
* After receiving the pre-test results, I have adjusted the version number, 
  URLs and URIs.  
* Apologies in advance if the cross-links within my package documentation are 
  still unacceptable -- it's my first experience with a CRAN submission and
  ROxygen2.
* Revised Description.  Repaired three faulty cross-links.
* Revised Description as requested.  Added @return fields to the ROxygen2
  comments of a few functions which lacked /value comments.  Amended the tests 
  of the internal method check.nseats which had been ported from legacy code, 
  using :: rather than ::: decorations.
* Using my professional email address rather than my personal one on
  this submission.
* Deleted the legacy tests of check.nseats.  They add little value to the
  codebase, and I can't be bothered to figure out how to persuade test_check()
  to test an internal method without using `:::`.

