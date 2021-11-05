library(neighbr)
data(iris)

teardown({
  detach("package:neighbr", unload = TRUE)
})

test_that("error when transform argument is not null", {
  set.seed(1131231)
  train_set <- iris[1:147, ]
  test_set <- iris[148:150, !names(iris) %in% c("Species")]

  expect_error(
    pmml(knn(
      train_set = train_set, test_set = test_set,
      k = 3,
      categorical_target = "Species",
      comparison_measure = "squared_euclidean"
    ), transforms = "NOT NULL"),
    "transforms currently not supported for knn models"
  )
})

test_that("error when object is not neighbr", {
  a <- "foo"
  expect_error(pmml.neighbr(a), "Not a legitimate neighbr object")
})

test_that("pmml.neighbr produces expected elements for function_name=mixed", {
  data(iris)
  iris$ID <- c(1:150)
  train_set <- iris[1:140, ]
  test_set <- iris[141:150, -c(4, 5, 6)]

  fit <- knn(
    train_set = train_set, test_set = test_set,
    k = 3,
    categorical_target = "Species",
    continuous_target = "Petal.Width",
    comparison_measure = "squared_euclidean",
    return_ranked_neighbors = 3,
    id = "ID"
  )

  fit_pmml <- pmml(fit)

  # Expect ComparisonMeasure kind to be "distance" and squaredEuclidean
  expect_equal(toString(fit_pmml[[3]][[4]]), "<ComparisonMeasure kind=\"distance\">\n <squaredEuclidean/>\n</ComparisonMeasure>")
  # Expect the inputs to be Sepal.Length, Sepal.Width, and Petal.Length, and compareFunction is "absDiff"
  expect_equal(toString(fit_pmml[[3]][[5]]), "<KNNInputs>\n <KNNInput field=\"Sepal.Length\" compareFunction=\"absDiff\"/>\n <KNNInput field=\"Sepal.Width\" compareFunction=\"absDiff\"/>\n <KNNInput field=\"Petal.Length\" compareFunction=\"absDiff\"/>\n</KNNInputs>")
  # Expect that MiningSchema contains two MiningFields with usageType="predicted"
  expect_equal(toString(fit_pmml[[3]][[1]]), "<MiningSchema>\n <MiningField name=\"Sepal.Length\" usageType=\"active\"/>\n <MiningField name=\"Sepal.Width\" usageType=\"active\"/>\n <MiningField name=\"Petal.Length\" usageType=\"active\"/>\n <MiningField name=\"Species\" usageType=\"predicted\"/>\n <MiningField name=\"Petal.Width\" usageType=\"predicted\"/>\n</MiningSchema>")
})

test_that("pmml.neighbr produces expected elements for function_name=regression", {
  data(iris)

  iris <- iris[, -which(names(iris) %in% c("Species"))]

  iris$ID <- c(1:150)
  train_set <- iris[1:140, ]
  test_set <- iris[141:150, -c(4, 5, 6)]

  fit <- knn(
    train_set = train_set, test_set = test_set,
    k = 3,
    continuous_target = "Petal.Width",
    comparison_measure = "euclidean",
    return_ranked_neighbors = 3,
    id = "ID"
  )

  fit_pmml <- pmml(fit)

  # Expect ComparisonMeasure kind to be "distance" and euclidean
  expect_equal(toString(fit_pmml[[3]][[4]]), "<ComparisonMeasure kind=\"distance\">\n <euclidean/>\n</ComparisonMeasure>")
  # Expect the inputs to be Sepal.Length, Sepal.Width, and Petal.Length, and compareFunction is "absDiff"
  expect_equal(toString(fit_pmml[[3]][[5]]), "<KNNInputs>\n <KNNInput field=\"Sepal.Length\" compareFunction=\"absDiff\"/>\n <KNNInput field=\"Sepal.Width\" compareFunction=\"absDiff\"/>\n <KNNInput field=\"Petal.Length\" compareFunction=\"absDiff\"/>\n</KNNInputs>")
  # Expect that MiningSchema contains one MiningField with usageType="predicted"
  expect_equal(toString(fit_pmml[[3]][[1]]), "<MiningSchema>\n <MiningField name=\"Sepal.Length\" usageType=\"active\"/>\n <MiningField name=\"Sepal.Width\" usageType=\"active\"/>\n <MiningField name=\"Petal.Length\" usageType=\"active\"/>\n <MiningField name=\"Petal.Width\" usageType=\"predicted\"/>\n</MiningSchema>")
})


test_that("pmml.neighbr produces expected elements for function_name=classification", {
  data(iris)

  iris$ID <- c(1:150)
  train_set <- iris[1:140, ]
  test_set <- iris[141:150, -c(5, 6)]

  fit <- knn(
    train_set = train_set, test_set = test_set,
    k = 3,
    categorical_target = "Species",
    continuous_target = NULL,
    comparison_measure = "euclidean",
    return_ranked_neighbors = 3,
    id = "ID"
  )

  fit_pmml <- pmml(fit)

  # Expect ComparisonMeasure kind to be "distance" and euclidean
  expect_equal(toString(fit_pmml[[3]][[4]]), "<ComparisonMeasure kind=\"distance\">\n <euclidean/>\n</ComparisonMeasure>")
  # Expect the inputs to be Sepal.Length, Sepal.Width, Petal.Length, and Petal.Width, and compareFunction is "absDiff"
  expect_equal(toString(fit_pmml[[3]][[5]]), "<KNNInputs>\n <KNNInput field=\"Sepal.Length\" compareFunction=\"absDiff\"/>\n <KNNInput field=\"Sepal.Width\" compareFunction=\"absDiff\"/>\n <KNNInput field=\"Petal.Length\" compareFunction=\"absDiff\"/>\n <KNNInput field=\"Petal.Width\" compareFunction=\"absDiff\"/>\n</KNNInputs>")
  # Expect that MiningSchema contains one MiningField with usageType="predicted"
  expect_equal(toString(fit_pmml[[3]][[1]]), "<MiningSchema>\n <MiningField name=\"Sepal.Length\" usageType=\"active\"/>\n <MiningField name=\"Sepal.Width\" usageType=\"active\"/>\n <MiningField name=\"Petal.Length\" usageType=\"active\"/>\n <MiningField name=\"Petal.Width\" usageType=\"active\"/>\n <MiningField name=\"Species\" usageType=\"predicted\"/>\n</MiningSchema>")
})

test_that("pmml.neighbr produces expected elements for function_name=clustering", {
  data(iris)

  iris <- iris[, -which(names(iris) %in% c("Species"))]

  iris$ID <- c(1:150)
  train_set <- iris[1:140, ]
  test_set <- iris[141:150, -c(5)]

  fit <- knn(
    train_set = train_set, test_set = test_set,
    k = 3,
    categorical_target = NULL,
    continuous_target = NULL,
    comparison_measure = "euclidean",
    return_ranked_neighbors = 3,
    id = "ID"
  )

  fit_pmml <- pmml(fit)

  # Expect ComparisonMeasure kind to be "distance" and euclidean
  expect_equal(toString(fit_pmml[[3]][[4]]), "<ComparisonMeasure kind=\"distance\">\n <euclidean/>\n</ComparisonMeasure>")
  # Expect the inputs to be Sepal.Length, Sepal.Width, Petal.Length, and Petal.Width, and compareFunction is "absDiff"
  expect_equal(toString(fit_pmml[[3]][[5]]), "<KNNInputs>\n <KNNInput field=\"Sepal.Length\" compareFunction=\"absDiff\"/>\n <KNNInput field=\"Sepal.Width\" compareFunction=\"absDiff\"/>\n <KNNInput field=\"Petal.Length\" compareFunction=\"absDiff\"/>\n <KNNInput field=\"Petal.Width\" compareFunction=\"absDiff\"/>\n</KNNInputs>")
  # Expect that MiningSchema contains no MiningFields with usageType="predicted"
  expect_equal(toString(fit_pmml[[3]][[1]]), "<MiningSchema>\n <MiningField name=\"Sepal.Length\" usageType=\"active\"/>\n <MiningField name=\"Sepal.Width\" usageType=\"active\"/>\n <MiningField name=\"Petal.Length\" usageType=\"active\"/>\n <MiningField name=\"Petal.Width\" usageType=\"active\"/>\n</MiningSchema>")
})




test_that("error when transforms are not NULL", {
  data(iris)
  iris$ID <- c(1:150)
  train_set <- iris[1:140, ]
  test_set <- iris[141:150, -c(4, 5, 6)]

  fit <- knn(
    train_set = train_set, test_set = test_set,
    k = 3,
    categorical_target = "Species",
    continuous_target = "Petal.Width",
    comparison_measure = "squared_euclidean",
    return_ranked_neighbors = 3,
    id = "ID"
  )

  expect_error(pmml(fit, transforms = "foo"), "transforms currently not supported for knn models")
})
