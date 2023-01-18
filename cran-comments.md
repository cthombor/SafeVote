## R CMD check results
There were no ERRORs or WARNINGs. 

The only change since my previous submission is to use apostrophes
(ASCII 0x27), rather than 0x92 right single-quote chars of CP-1252
extended ASCII, to delimit the name of the package in DESCRIPTION.
Apologies to the hard-working CRAN volunteer who alerted me to this
error!

There were 3 NOTEs (when using devtools::check_rhub())

❯ checking CRAN incoming feasibility ... [13s] NOTE
  Maintainer: 'Clark Thomborson <c.thomborson@auckland.ac.nz>'
  
  New submission

❯ checking HTML version of manual ... [11s] NOTE
  Skipping checking math rendering: package 'V8' unavailable

❯ checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

* The math-rendering NOTE seems likely to be the issue I had reported in 
https://github.com/r-lib/pkgdown/issues/2261#issuecomment-1375175672,
It's certainly the case that $\sqrt{x}$ is still not rendering properly
in https://cthombor.github.io/SafeVote/, despite my many attempts to find
a workaround.  I'm unable to find the lastMiKTeXException file at rhub,
so will try again in  a few days, and if V8 is still unavailable I'll
file an issue report.

* Because I'm unable to find a workaround for
<DOI:10.1023/A:1005082925477>, I have deleted this URI and will submit
an issue report regarding the exception:

[HttpException (0x80004005): A potentially dangerous Request.Path value was detected from the client (<).]
   System.Web.HttpRequest.ValidateInputIfRequiredByConfig() +11790877
   System.Web.PipelineStepManager.ValidateHelper(HttpContext context) +54
   
* <DOI:10.1023/A&#58;1005082925477> is not a workaround.  

* https://dx.doi.org/10.1023/A:1005082925477 redirects to
https://link.springer.com/article/10.1023/A:1005082925477
