## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

We have checked this in win builder and on OS X with no issues. 

* This is a resubmission that fixes two NOTES:

1. Missing dependency on R >= 4.1.0 because package code uses the pipe
  |> or function shorthand \(...) syntax added in R 4.1.0.
  
We have updated the R dependency

2. Found the following URLs which should use \doi (with the DOI name only):
  File 'patriot_act.Rd':
    https://doi.org/10.7910/DVN/I9GSKI
  
We have updated to \doi{10.7910/DVN/I9GSKI}
