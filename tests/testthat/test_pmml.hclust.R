
# necessary to unload rattle because it has a different "audit" dataset
teardown({
  detach("package:rattle", unload = TRUE)
  detach("package:amap", unload = TRUE)
})

test_that("error when object is not hclust", {
  skip_if_not_installed("amap")
  library(amap)
  expect_error(pmml.hclust("foo"), "Not a legitimate hclust object")
})


test_that("appropriate number of clusters is created", {
  skip_if_not_installed("amap")
  skip_if_not_installed("rattle")
  library(amap)
  library(rattle)
  fit <- hclusterpar(iris[, -5])
  centerInfo <- rattle::centers.hclust(iris[, -5], fit, 3)
  p_fit <- pmml(fit, centers = centerInfo)
  expect_equal(xmlGetAttr(p_fit[[3]], name = "numberOfClusters"), "3")

  centerInfo <- rattle::centers.hclust(iris[, -5], fit, 5)
  p_fit <- pmml(fit, centers = centerInfo)
  expect_equal(xmlGetAttr(p_fit[[3]], name = "numberOfClusters"), "5")
})
