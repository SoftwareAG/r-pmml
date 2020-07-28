library(nnet)
data(iris)

test_that("error when object is not multinom", {
  expect_error(pmml.multinom("foo"), "Not a legitimate multinom object")
})