## Test environments
* Local Linux Mint install, R 3.6.3
* win-builder (devel)
* windows-x86_64-devel, fedora-gcc-devel via rhub

## R CMD check results

There were no ERRORs or WARNINGs during local or win-builder checks.

There were several ERRORs on https://cran.r-project.org/web/checks/check_results_neighbr.html

These are due to the upcoming stringsAsFactors change in R 4.0.0. We addressed these issues
by setting stringsAsFactors = TRUE for calls to data.frame() and read.table().