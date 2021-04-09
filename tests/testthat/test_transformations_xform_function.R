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

test_that("Error when new_field_data_type is not numeric or factor", {
  iris_box_2 <- xform_wrap(iris)
  expect_error(xform_function(wrap_object = iris_box_2,
                               orig_field_name = "Sepal.Length",
                               new_field_name = "Sepal.Length.Transformed",
                               new_field_data_type = "foo",
                               expression = "(Sepal.Length^2)/100"),
               'new_field_data_type must be "numeric" or "factor".')
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
                               new_field_name = "Sepal.Width.Factor",
                               new_field_data_type = "factor",
                               expression = "Sepal.Width")
  
  expect_equal(class(iris_box_3$data$Sepal.Length.Transformed), "numeric")
  expect_equal(class(iris_box_3$data$Sepal.Width.Factor), "factor")
  
  expect_equal(as.character(iris_box_3$field_data['Sepal.Length.Transformed','dataType']), "numeric")
  expect_equal(as.character(iris_box_3$field_data['Sepal.Width.Factor','dataType']), "factor")
  
})

test_that("xform_function preserves factor names factor input is unchanged", {
  iris_box_4 <- xform_wrap(iris)

  iris_box_4 <- xform_function(wrap_object = iris_box_4,
                               orig_field_name = "Species",
                               new_field_name = "Species_transf",
                               new_field_data_type = "factor",
                               expression = "Species")
  
  expect_equal(levels(iris_box_4$data$Species_transf), levels(iris_box_4$data$Species))
  
  fit <- lm(Sepal.Width ~ Species_transf, data = iris_box_4$data)
  p_fit <- pmml(fit, transforms = iris_box_4)
  
  # The value attribute in RegressionTable should have proper level names, not integers
  expect_equal(xmlGetAttr(p_fit[[3]][[4]][[1]], "value"), "setosa")
  expect_equal(xmlGetAttr(p_fit[[3]][[4]][[2]], "value"), "versicolor")
  expect_equal(xmlGetAttr(p_fit[[3]][[4]][[3]], "value"), "virginica")
  
})

test_that("xform_function preserves factor names when output is a factor", {
  iris_box_4a <- xform_wrap(iris)
  
  iris_box_4a <- xform_function(wrap_object = iris_box_4a,
                               orig_field_name = "Sepal.Length",
                               new_field_name = "Sepal.Length_transf",
                               new_field_data_type = "factor",
                               expression = "if(Sepal.Length<5.1) {'level_A'} else if (Sepal.Length>6.6) {'level_B'} else {'level_C'}")
  
  expect_equal(levels(iris_box_4a$data$Sepal.Length_transf), c("level_A", "level_B", "level_C"))
  
  fit <- lm(Sepal.Width ~ Sepal.Length_transf, data = iris_box_4a$data)
  p_fit <- pmml(fit, transforms = iris_box_4a)
  
  # The value attribute in RegressionTable should have proper level names, not integers
  expect_equal(xmlGetAttr(p_fit[[3]][[4]][[1]], "value"), "level_A")
  expect_equal(xmlGetAttr(p_fit[[3]][[4]][[2]], "value"), "level_B")
  expect_equal(xmlGetAttr(p_fit[[3]][[4]][[3]], "value"), "level_C")
})


test_that("xform_function preserves R factor names when input is numeric", {
  # The unique values in the original Sepal.Length field are sorted and converted
  # to character before comparing with the levels in Sepal.Length_transf
  
  iris_box_5 <- xform_wrap(iris)
  iris_box_5 <- xform_function(wrap_object = iris_box_5,
                               orig_field_name = "Sepal.Length",
                               new_field_name = "Sepal.Length_transf",
                               new_field_data_type = "factor",
                               expression = "Sepal.Length")
  
  char_array <- as.character(sort(unique(iris_box_5$data$Sepal.Length)))
  expect_equal(levels(iris_box_5$data$Sepal.Length_transf), char_array)
  
})

