test_that("xform_function works correctly", {
  iris_box <- xform_wrap(iris)
  iris_box <- xform_function(iris_box,
                             orig_field_name = "Sepal.Length",
                             new_field_name = "Sepal.Length.Transformed",
                             expression = "(Sepal.Length^2)/100"
  )
  expect_equal(iris_box$field_data["Sepal.Length.Transformed", "orig_field_name"], "Sepal.Length")
  expect_equal(iris_box$field_data["Sepal.Length.Transformed", "xform_function"], "(Sepal.Length^2)/100")
  expect_equal(iris_box$data$Sepal.Length.Transformed[[1]], 0.2601)
})

test_that("xform_function does not create an unnecessary NA factor", {
  iris_box_xf <- xform_wrap(iris)
  iris_box_xf <- xform_function(iris_box_xf,
                                orig_field_name = "Sepal.Width",
                                new_field_name = "Sepal.Width.sq",
                                expression = "Sepal.Width^2",
                                new_field_data_type = "numeric"
  )
  expect_equal(levels(iris_box_xf$field_data$dataType), c("factor", "numeric"))
  
  iris_box_xf_2 <- xform_wrap(iris)
  iris_box_xf_2 <- xform_function(iris_box_xf_2,
                                  orig_field_name = "Sepal.Length,Petal.Length",
                                  new_field_name = "Length.Ratio",
                                  expression = "Sepal.Length / Petal.Length"
  )
  expect_equal(levels(iris_box_xf_2$field_data$dataType), c("factor", "numeric"))
})

test_that("Error when new_field_data_type is not numeric or character", {
  iris_box_2 <- xform_wrap(iris)
  expect_error(xform_function(wrap_object = iris_box_2,
                               orig_field_name = "Sepal.Length",
                               new_field_name = "Sepal.Length.Transformed",
                               new_field_data_type = "foo",
                               expression = "(Sepal.Length^2)/100"),
               'new_field_data_type must be "numeric" or "character".')
})

test_that("field_data$dataType and class of new data column match new_field_data_type", {
  iris_box_3 <- xform_wrap(iris)
  iris_box_3 <- xform_function(wrap_object = iris_box_3,
                              orig_field_name = "Sepal.Length",
                              new_field_name = "Sepal.Length.Transformed",
                              new_field_data_type = "numeric",
                              expression = "(Sepal.Length^2)/100")
  
  iris_box_3 <- xform_function(wrap_object = iris_box_3,
                               orig_field_name = "Sepal.Width",
                               new_field_name = "Sepal.Width.Char",
                               new_field_data_type = "character",
                               expression = "Sepal.Width")
  
  expect_equal(class(iris_box_3$data$Sepal.Length.Transformed), "numeric")
  expect_equal(class(iris_box_3$data$Sepal.Width.Char), "character")
  
  expect_equal(as.character(iris_box_3$field_data['Sepal.Length.Transformed','dataType']), "numeric")
  expect_equal(as.character(iris_box_3$field_data['Sepal.Width.Char','dataType']), "character")
  
})


