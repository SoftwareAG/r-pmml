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
  
  expect_equal(toString(fit_pmml[[3]][[3]]), "<LocalTransformations>\n <DerivedField name=\"derived_Sepal.Width\" dataType=\"double\" optype=\"continuous\">\n  <NormContinuous field=\"Sepal.Width\">\n   <LinearNorm orig=\"3.05733333333333\" norm=\"0\"/>\n   <LinearNorm orig=\"3.49319961827003\" norm=\"1\"/>\n  </NormContinuous>\n </DerivedField>\n <DerivedField name=\"derived_Sepal.Length\" dataType=\"double\" optype=\"continuous\">\n  <NormContinuous field=\"Sepal.Length\">\n   <LinearNorm orig=\"5.84333333333333\" norm=\"0\"/>\n   <LinearNorm orig=\"6.6713994613112\" norm=\"1\"/>\n  </NormContinuous>\n </DerivedField>\n</LocalTransformations>")
  
  # one-class svm
  library(e1071)
  fit_2 <- svm(iris_box$data[, 6:7], y = NULL, type = "one-classification")
  fit_pmml_2 <- pmml(fit_2, dataset = iris_box$data[, 6:7], transforms = iris_box)
  
  expect_equal(toString(fit_pmml_2[[3]][[3]][[3]]), "<LocalTransformations>\n <DerivedField name=\"derived_Sepal.Width\" dataType=\"double\" optype=\"continuous\">\n  <NormContinuous field=\"Sepal.Width\">\n   <LinearNorm orig=\"3.05733333333333\" norm=\"0\"/>\n   <LinearNorm orig=\"3.49319961827003\" norm=\"1\"/>\n  </NormContinuous>\n </DerivedField>\n <DerivedField name=\"derived_Sepal.Length\" dataType=\"double\" optype=\"continuous\">\n  <NormContinuous field=\"Sepal.Length\">\n   <LinearNorm orig=\"5.84333333333333\" norm=\"0\"/>\n   <LinearNorm orig=\"6.6713994613112\" norm=\"1\"/>\n  </NormContinuous>\n </DerivedField>\n <DerivedField name=\"algorithm_derived_nc_derived_Sepal.Width\" dataType=\"double\" optype=\"continuous\">\n  <NormContinuous field=\"derived_Sepal.Width\">\n   <LinearNorm orig=\"0\" norm=\"-2.03540887847945e-16\"/>\n   <LinearNorm orig=\"2.03540887847945e-16\" norm=\"0\"/>\n  </NormContinuous>\n </DerivedField>\n <DerivedField name=\"algorithm_derived_nc_derived_Sepal.Length\" dataType=\"double\" optype=\"continuous\">\n  <NormContinuous field=\"derived_Sepal.Length\">\n   <LinearNorm orig=\"-4.48067509021636e-16\" norm=\"0\"/>\n   <LinearNorm orig=\"0\" norm=\"4.48067509021636e-16\"/>\n  </NormContinuous>\n </DerivedField>\n</LocalTransformations>")
})