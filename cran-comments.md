## Test environments
* Local Linux Mint install, R 3.6.1
* win-builder (release)

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs during local or win-builder checks.

There wsas 1 ERROR on r-patched-solaris-x86 (https://cloud.r-project.org/web/checks/check_results_pmml.html):

     > library(testthat)
     > library(pmml, quietly = T)
     >
     > test_check("pmml")
     Error: segfault from C stack overflow

We are not sure how to address this issue.