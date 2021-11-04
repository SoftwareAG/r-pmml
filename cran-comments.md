## Test environments
* Local Linux Mint 20 install, R 4.1.1
* win-builder (devel)
* windows-x86_64-devel, debian-clang-devel, fedora-gcc-devel via rhub

## R CMD check results
No errors, warnings, or notes.

## Additional Notes
* There is an ERROR for r-patched-solaris-x86, shown on this page: https://cloud.r-project.org/web/checks/check_results_pmml.html. This is due to a suggested package ('glmnet') missing from solaris. We are not sure how to fix this.

* There is a NOTE for win-devel, but the word "gravesee" is spelled correctly:
checking CRAN incoming feasibility ... NOTE
Maintainer: 'Dmitriy Bolotov <dmitriy.bolotov@softwareag.com>'

Possibly misspelled words in DESCRIPTION:
  gravesee (22:568)


