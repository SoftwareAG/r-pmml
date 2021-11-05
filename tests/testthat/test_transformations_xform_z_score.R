data(iris)
data(audit)

tmp_file <- tempfile()
teardown(unlink(tmp_file))

test_that("xform_z_score centers and scales for derived fields equal specific values", {
  iris_box <- xform_wrap(iris)
  iris_box <- xform_z_score(iris_box, "1")
  iris_box <- xform_z_score(iris_box, "2")
  expect_equal(iris_box$field_data["derived_Sepal.Length", "transform"], "zxform")
  expect_equal(iris_box$field_data["derived_Sepal.Width", "transform"], "zxform")
  expect_equal(iris_box$field_data["derived_Sepal.Length", "centers"], 5.843333, tolerance = 1e-6)
  expect_equal(iris_box$field_data["derived_Sepal.Length", "scales"], 0.8280661, tolerance = 1e-6)
  expect_equal(iris_box$field_data["derived_Sepal.Width", "centers"], 3.057333, tolerance = 1e-6)
  expect_equal(iris_box$field_data["derived_Sepal.Width", "scales"], 0.4358663, tolerance = 1e-6)
})

test_that("PMML with xform_z_score has correct localTransformations", {
  iris_box <- xform_wrap(iris)
  iris_box <- xform_z_score(iris_box, xform_info = "Sepal.Width")
  iris_box <- xform_z_score(iris_box, xform_info = "Sepal.Length")

  # linear regression
  fit <- lm(Petal.Width ~ ., iris_box$data[, -5])
  fit_pmml <- pmml(fit, transforms = iris_box)
  
  expect_equal(xmlGetAttr(fit_pmml[[3]][[3]][[1]], "name"), "derived_Sepal.Width")
  expect_equal(xmlGetAttr(fit_pmml[[3]][[3]][[1]], "dataType"), "double")
  expect_equal(xmlGetAttr(fit_pmml[[3]][[3]][[1]], "optype"), "continuous")
  
  expect_equal(xmlGetAttr(fit_pmml[[3]][[3]][[1]][[1]], name = "field"), "Sepal.Width")
  
  # one-class svm
  library(e1071)
  fit_2 <- svm(iris_box$data[, 6:7], y = NULL, type = "one-classification")
  fit_pmml_2 <- pmml(fit_2, dataset = iris_box$data[, 6:7], transforms = iris_box)
  
  expect_equal(xmlGetAttr(fit_pmml_2[[3]][[3]][[3]][[1]], "name"), "derived_Sepal.Width")
  expect_equal(xmlGetAttr(fit_pmml_2[[3]][[3]][[3]][[1]], "dataType"), "double")
  expect_equal(xmlGetAttr(fit_pmml_2[[3]][[3]][[3]][[1]], "optype"), "continuous")
  expect_equal(xmlGetAttr(fit_pmml_2[[3]][[3]][[3]][[1]][[1]], "field"), "Sepal.Width")
  
  expect_equal(xmlGetAttr(fit_pmml_2[[3]][[3]][[3]][[2]], "name"), "derived_Sepal.Length")
  expect_equal(xmlGetAttr(fit_pmml_2[[3]][[3]][[3]][[2]][[1]], "field"), "Sepal.Length")
  
  })