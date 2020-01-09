## Test environments
* Local Linux Mint install, R 3.6.2
* win-builder (devel)

## R CMD check results

There were no ERRORs or WARNINGs during local or win-builder checks.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Dmitriy Bolotov <dmitriy.bolotov@softwareag.com>'

New maintainer:
  Dmitriy Bolotov <dmitriy.bolotov@softwareag.com>
Old maintainer(s):
  Dmitriy Bolotov <rpmmlsupport@softwareag.com>


There was 1 ERROR on r-patched-solaris-x86 (https://cloud.r-project.org/web/checks/check_results_pmml.html):

     > library(testthat)
     > library(pmml, quietly = T)
     >
     > test_check("pmml")
     Error: segfault from C stack overflow

We are not sure how to address this issue.