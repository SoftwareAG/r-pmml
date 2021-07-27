data(iris)

test_that("xform_discretize produces correct field_data and data values", {
  iris_box <- xform_wrap(iris)
  t <- list()
  m <- data.frame(rbind(
    c("Petal.Length", "dis_pl", "leftInterval", "leftValue", "rightInterval", "rightValue"),
    c("double", "integer", "string", "double", "string", "double"),
    c("0)", 0, "open", NA, "Open", 0),
    c(NA, 1, "closed", 0, "Open", 1),
    c(NA, 2, "closed", 1, "Open", 2),
    c(NA, 3, "closed", 2, "Open", 3),
    c(NA, 4, "closed", 3, "Open", 4),
    c("[4", 5, "closed", 4, "Open", NA)
  ), stringsAsFactors = TRUE)
  t[[1]] <- m
  def <- c(11)
  mis <- c(22)
  iris_box <- xform_discretize(iris_box, xform_info = t, default_value = def, map_missing_to = mis)
  
  expect_equal(iris_box$field_data["dis_pl", "transform"], "discretize")
  expect_equal(iris_box$field_data["dis_pl", "default"], 11)
  expect_equal(iris_box$field_data["dis_pl", "missingValue"], 22)
  expect_true(iris_box$data$dis_pl[[1]] == 2)
})

test_that("xform_discretize produces correct discretization for a closed left interval", {
  iris_box <- xform_wrap(iris)
  t <- list()
  m <- data.frame(rbind(
    c("Petal.Length", "dis_pl", "leftInterval", "leftValue", "rightInterval", "rightValue"),
    c("double", "integer", "string", "double", "string", "double"),
    c("0)", 0, "open", NA, "Open", 0),
    c(NA, 1, "closed", 0, "Open", 1),
    c(NA, 2, "closed", 1, "Open", 2),
    c(NA, 3, "closed", 2, "Open", 3),
    c(NA, 4, "closed", 3, "Open", 4),
    c("[4", 5, "closed", 4, "Open", NA)
  ))
  t[[1]] <- m
  def <- c(11)
  mis <- c(22)
  iris_box <- xform_discretize(iris_box, xform_info = t, default_value = def, map_missing_to = mis)
  
  f <- iris_box$data$dis_pl[iris_box$data$Petal.Length == 4]
  expect_equal(as.numeric(levels(f))[f], c(5, 5, 5, 5, 5)) # test that value 4 is transformed to 5
})

test_that("xform_discretize produces correct discretization for a closed right interval", {
  iris_box <- xform_wrap(iris)
  t <- list()
  m <- data.frame(rbind(
    c("Petal.Length", "dis_pl", "leftInterval", "leftValue", "rightInterval", "rightValue"),
    c("double", "integer", "string", "double", "string", "double"),
    c("0]", 0, "open", NA, "Closed", 0),
    c(NA, 1, "open", 0, "Closed", 1),
    c(NA, 2, "open", 1, "Closed", 2),
    c(NA, 3, "open", 2, "Closed", 3),
    c(NA, 4, "open", 3, "Closed", 4),
    c("(4", 5, "open", 4, "Open", NA)
  ))
  t[[1]] <- m
  def <- c(11)
  mis <- c(22)
  iris_box <- xform_discretize(iris_box, xform_info = t, default_value = def, map_missing_to = mis)
  
  f <- iris_box$data$dis_pl[iris_box$data$Petal.Length == 4]
  expect_equal(as.numeric(levels(f))[f], c(4, 4, 4, 4, 4)) # test that value 4 is transformed to 4
})