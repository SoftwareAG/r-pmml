rn <- .pmmlRootNode()

test_that("version attribute in PMML root node has value 4.4.1", {
  expect_equal(xmlGetAttr(rn, name = "version"), "4.4.1")
})

test_that("xsi:schemaLocation in PMML root node points to pmml-4-4", {
  expect_equal(
    xmlGetAttr(rn, name = "xsi:schemaLocation"),
    "http://www.dmg.org/PMML-4_4 http://www.dmg.org/pmml/v4-4/pmml-4-4.xsd"
  )
})

test_that(".pmmlLocalTransformations sets dataType and optype provided in transforms", {
  iris_box_1 <- xform_wrap(iris)
  iris_box_1 <- xform_function(
    wrap_object = iris_box_1,
    orig_field_name = "Sepal.Length",
    new_field_name = "Sepal.Length.Transformed",
    new_field_data_type = "factor",
    expression = "Sepal.Length * 0.1"
  )

  iris_box_1 <- xform_function(
    wrap_object = iris_box_1,
    orig_field_name = "Sepal.Width",
    new_field_name = "Sepal.Width.Transformed",
    new_field_data_type = "numeric",
    expression = "Sepal.Width + 3.5"
  )

  fit_1 <- lm(Petal.Length ~ Sepal.Length.Transformed + Sepal.Width.Transformed,
    data = iris_box_1$data
  )
  fit_pmml_1 <- pmml(fit_1, transforms = iris_box_1)

  expect_equal(xmlGetAttr(fit_pmml_1[[3]][[3]][[1]], name = "dataType"), "string")
  expect_equal(xmlGetAttr(fit_pmml_1[[3]][[3]][[1]], name = "optype"), "categorical")
  expect_equal(xmlGetAttr(fit_pmml_1[[3]][[3]][[2]], name = "dataType"), "double")
  expect_equal(xmlGetAttr(fit_pmml_1[[3]][[3]][[2]], name = "optype"), "continuous")
})

test_that(".pmmlLocalTransformationsAD sets dataType and optype provided in transforms", {
  # .pmmlLocalTransformationsAD is only used by pmml.svm(). e1071::svm()
  # requires training data to be numeric.

  skip_if_not_installed("e1071")
  library(e1071)
  iris_box_1 <- xform_wrap(iris[, 1:4])
  iris_box_1 <- xform_function(
    wrap_object = iris_box_1,
    orig_field_name = "Sepal.Length",
    new_field_name = "Sepal.Length.Transformed",
    new_field_data_type = "numeric",
    expression = "Sepal.Length * 0.1"
  )

  iris_box_1 <- xform_function(
    wrap_object = iris_box_1,
    orig_field_name = "Sepal.Width",
    new_field_name = "Sepal.Width.Transformed",
    new_field_data_type = "numeric",
    expression = "Sepal.Width + 3.5"
  )

  fit_1 <- svm(x = iris_box_1$data[, 5:6], y = NULL, type = "one-classification")

  fit_pmml_1 <- pmml(fit_1, dataset = iris_box_1$data[, 5:6], transforms = iris_box_1)

  expect_equal(xmlGetAttr(fit_pmml_1[[3]][[3]][[3]][[1]], name = "dataType"), "double")
  expect_equal(xmlGetAttr(fit_pmml_1[[3]][[3]][[3]][[1]], name = "optype"), "continuous")
  expect_equal(xmlGetAttr(fit_pmml_1[[3]][[3]][[3]][[2]], name = "dataType"), "double")
  expect_equal(xmlGetAttr(fit_pmml_1[[3]][[3]][[3]][[2]], name = "optype"), "continuous")
})


test_that(".pmmlHeader() adds modelVersion attribute when model_version is not NULL", {
  header <- .pmmlHeader(
    description = "Test model", copyright = NULL,
    model_version = "someVersion", app_name = "App Name"
  )
  expect_equal(xmlGetAttr(header, name = "modelVersion"), "someVersion")

  header2 <- .pmmlHeader(
    description = "Test model", copyright = NULL,
    model_version = c("someVersion"), app_name = "App Name"
  )
  expect_equal(xmlGetAttr(header2, name = "modelVersion"), "someVersion")
})

test_that(".pmmlHeader() does not add modelVersion attribute when model_version is NULL", {
  header <- .pmmlHeader(
    description = "Test model", copyright = NULL,
    model_version = NULL, app_name = "App Name"
  )
  # XML::xmlNode does not add an attribute if its value is NULL
  expect_equal(xmlGetAttr(header, name = "modelVersion"), NULL)
})

test_that(".pmmlHeader() errors if model_version is not a character vector of length 1", {
  expect_error(
    .pmmlHeader(
      description = "Test model", copyright = NULL,
      model_version = 123, app_name = "App Name"
    ),
    'model_version must be of type "character" and of length 1.'
  )

  expect_error(
    .pmmlHeader(
      description = "Test model", copyright = NULL,
      model_version = c(1, 2, 3), app_name = "App Name"
    ),
    'model_version must be of type "character" and of length 1.'
  )

  expect_error(
    .pmmlHeader(
      description = "Test model", copyright = NULL,
      model_version = c("adf", 1), app_name = "App Name"
    ),
    'model_version must be of type "character" and of length 1.'
  )

  expect_error(
    .pmmlHeader(
      description = "Test model", copyright = NULL,
      model_version = c("adf", "asdf"), app_name = "App Name"
    ),
    'model_version must be of type "character" and of length 1.'
  )
  # >>>>>>> master
})
