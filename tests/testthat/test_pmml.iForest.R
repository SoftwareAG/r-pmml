context("test pmml.iForest converter")

data(iris)

test_that("confirm non-existent category is still automatically created by iForest function", {
  skip_on_cran()
  
  library(isofor)
  mod <- iForest(iris, nt = 2, phi = 30)
  model_pmml <- pmml(model = mod)
  expect_equal(length(model_pmml[[2]]), 5)
  expect_equal(as.character(model_pmml[[2]][[5]][[4]])[2], ".")
})
