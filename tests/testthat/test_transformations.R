data(iris)
data(audit)

tmp_file <- tempfile()
teardown(unlink(tmp_file))

test_that("xform_wrap box$field_data contains specific column names", {
  iris_box <- xform_wrap(iris)
  expect_equal(names(iris_box$field_data), c(
    "type", "dataType", "orig_field_name",
    "sampleMin", "sampleMax", "xformedMin",
    "xformedMax", "centers", "scales",
    "fieldsMap", "transform", "default",
    "missingValue", "xform_function"
  ))
})

test_that("xform_wrap does not convert all columns to factors for tibbles", {
  rtbl <- as_tibble(iris)
  rtbl_box <- xform_wrap(rtbl)
  expect_equal(
    rtbl_box$field_data[, 2],
    as.factor(c(
      "numeric", "numeric", "numeric",
      "numeric", "factor"
    ))
  )
})



# test_that("xform_z_score centers and scales for derived fields equal specific values", {
#   iris_box <- xform_wrap(iris)
#   iris_box <- xform_z_score(iris_box, "1")
#   iris_box <- xform_z_score(iris_box, "2")
#   expect_equal(iris_box$field_data["derived_Sepal.Length", "transform"], "zxform")
#   expect_equal(iris_box$field_data["derived_Sepal.Width", "transform"], "zxform")
#   expect_equal(iris_box$field_data["derived_Sepal.Length", "centers"], 5.843333, tolerance = 1e-6)
#   expect_equal(iris_box$field_data["derived_Sepal.Length", "scales"], 0.8280661, tolerance = 1e-6)
#   expect_equal(iris_box$field_data["derived_Sepal.Width", "centers"], 3.057333, tolerance = 1e-6)
#   expect_equal(iris_box$field_data["derived_Sepal.Width", "scales"], 0.4358663, tolerance = 1e-6)
# })

test_that("xform_min_max normalizes all data to be between 0 and 1", {
  iris_box <- xform_wrap(iris)
  iris_box <- xform_min_max(iris_box)
  expect_equal(iris_box$field_data["derived_Sepal.Length", "transform"], "minmax")
  expect_equal(iris_box$field_data["derived_Sepal.Width", "transform"], "minmax")
  expect_equal(iris_box$field_data["derived_Petal.Length", "transform"], "minmax")
  expect_equal(iris_box$field_data["derived_Petal.Width", "transform"], "minmax")
  expect_equal(min(iris_box$data$derived_Sepal.Length), 0)
  expect_equal(max(iris_box$data$derived_Sepal.Length), 1)
  expect_equal(min(iris_box$data$derived_Sepal.Width), 0)
  expect_equal(min(iris_box$data$derived_Petal.Length), 0)
  expect_equal(max(iris_box$data$derived_Petal.Length), 1)
  expect_equal(min(iris_box$data$derived_Petal.Width), 0)
  expect_equal(max(iris_box$data$derived_Petal.Width), 1)
})

test_that("xform_norm_discrete box$field_data$fieldsMap contains values of setosa, versicolor, virginica", {
  iris_box <- xform_wrap(iris)
  iris_box <- xform_norm_discrete(iris_box, xform_info = "Species")
  expect_equal(iris_box$field_data["Species_setosa", "transform"], "NormDiscrete")
  expect_equal(iris_box$field_data["Species_versicolor", "transform"], "NormDiscrete")
  expect_equal(iris_box$field_data["Species_virginica", "transform"], "NormDiscrete")
  expect_equal(iris_box$field_data["Species_setosa", "fieldsMap"][[1]], "setosa")
  expect_equal(iris_box$field_data["Species_versicolor", "fieldsMap"][[1]], "versicolor")
  expect_equal(iris_box$field_data["Species_virginica", "fieldsMap"][[1]], "virginica")
})

test_that("rename_wrap_var box$field_data and box$data contain renamed variable", {
  iris_box <- xform_wrap(iris)
  iris_box <- rename_wrap_var(wrap_object = iris_box, xform_info = "column1->SL")
  expect_equal(row.names(iris_box$field_data)[[1]], "SL")
  expect_equal(names(iris_box$data)[[1]], "SL")
})


test_that(".init_wrap_params adds NA columns", {
  iris_box <- xform_wrap(iris)

  iris_box$field_data$xformedMax <- NULL
  iris_box$field_data$centers <- NULL
  iris_box$field_data$fieldsMap <- NULL
  iris_box$field_data$transform <- NULL
  iris_box$field_data$default <- NULL
  iris_box$field_data$xform_function <- NULL

  iris_box_updated <- .init_wrap_params(iris_box)

  expect_equal(iris_box_updated$field_data$xformedMax, rep(NA, 5))


  # iris_box <- rename_wrap_var(wrap_object = iris_box,
  #                             xform_info = "column1->SL")
})
