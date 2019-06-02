## Test environments
* Local Linux Mint install, R 3.6.0
* win-builder (devel and release)

## R CMD check results

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Dmitriy Bolotov <rpmmlsupport@softwareag.com>'

New maintainer:
  Dmitriy Bolotov <rpmmlsupport@softwareag.com>
Old maintainer(s):
  Tridivesh Jena <rpmmlsupport@softwareag.com>

The maintainer has changed.


There was 1 NOTE during reverse dependency checks:

Changes to worse in reverse depends:

Package: fpmoutliers
Check: R code for possible problems
New result: NOTE
  generatePMML: no visible global function definition for â€˜pmml.itemsetsâ€™
  Undefined global functions or variables:
    pmml.itemsets

This note is due to the method not being explicitly exported in the namespace. We have added an explicit export.