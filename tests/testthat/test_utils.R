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
  iris_box_1 <- xform_function(wrap_object = iris_box_1,
                               orig_field_name = "Sepal.Length",
                               new_field_name = "Sepal.Length.Transformed",
                               new_field_data_type = "factor",
                               expression = "Sepal.Length * 0.1"
  )
  
  iris_box_1 <- xform_function(wrap_object = iris_box_1,
                               orig_field_name = "Sepal.Width",
                               new_field_name = "Sepal.Width.Transformed",
                               new_field_data_type = "numeric",
                               expression = "Sepal.Width + 3.5"
  )

  fit_1 <- lm(Petal.Length ~ Sepal.Length.Transformed + Sepal.Width.Transformed,
              data = iris_box_1$data)
  fit_pmml_1 <- pmml(fit_1, transforms = iris_box_1)
  
  expect_equal(xmlGetAttr(fit_pmml_1[[3]][[3]][[1]], name = "dataType"), "string")
  expect_equal(xmlGetAttr(fit_pmml_1[[3]][[3]][[1]], name = "optype"), "categorical")
  expect_equal(xmlGetAttr(fit_pmml_1[[3]][[3]][[2]], name = "dataType"), "double")
  expect_equal(xmlGetAttr(fit_pmml_1[[3]][[3]][[2]], name = "optype"), "continuous")
  
})
