## Test environments
* Local Linux Mint install, R 3.6.3
* win-builder (release and devel)
* windows-x86_64-devel, fedora-gcc-devel via rhub

## R CMD check results

There was 1 NOTE on win-release:

Found the following (possibly) invalid URLs:
  URL: https://www.softwareag.com/corporate/products/az/zementis/default.html
    From: DESCRIPTION
          man/pmml-package.Rd
    Status: Error
    Message: libcurl error code 60:
      	SSL certificate problem: unable to get local issuer certificate
      	(Status without verification: OK)

We are not sure how to fix this; the URL works on our browsers.