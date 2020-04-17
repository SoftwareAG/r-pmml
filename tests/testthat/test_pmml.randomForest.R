library(randomForest)

test_that("error when object is not randomForest", {
  a <- "foo"
  expect_error(pmml.randomForest(a), "Not a legitimate randomForest object")
})

test_that("no error occurs in doc example", {
  iris_rf <- randomForest(Species ~ ., data = iris, ntree = 20)
  expect_error(pmml(iris_rf), NA) # expect no error
})