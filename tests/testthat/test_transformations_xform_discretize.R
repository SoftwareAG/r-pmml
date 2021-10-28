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



test_that("PMML with xform_discretize has correct localTransformations", {
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
  
  # linear regression
  fit <- lm(Petal.Width ~ ., iris_box$data[, -5])
  fit_pmml <- pmml(fit, transforms = iris_box)
  expect_equal(toString(fit_pmml[[3]][[3]]), "<LocalTransformations>\n <DerivedField name=\"dis_pl\" dataType=\"double\" optype=\"continuous\">\n  <Discretize field=\"Petal.Length\" mapMissingTo=\"22\" defaultValue=\"11\">\n   <DiscretizeBin binValue=\"0\">\n    <Interval closure=\"openOpen\" rightMargin=\"0\"/>\n   </DiscretizeBin>\n   <DiscretizeBin binValue=\"1\">\n    <Interval closure=\"closedOpen\" leftMargin=\"0\" rightMargin=\"1\"/>\n   </DiscretizeBin>\n   <DiscretizeBin binValue=\"2\">\n    <Interval closure=\"closedOpen\" leftMargin=\"1\" rightMargin=\"2\"/>\n   </DiscretizeBin>\n   <DiscretizeBin binValue=\"3\">\n    <Interval closure=\"closedOpen\" leftMargin=\"2\" rightMargin=\"3\"/>\n   </DiscretizeBin>\n   <DiscretizeBin binValue=\"4\">\n    <Interval closure=\"closedOpen\" leftMargin=\"3\" rightMargin=\"4\"/>\n   </DiscretizeBin>\n   <DiscretizeBin binValue=\"5\">\n    <Interval closure=\"closedOpen\" leftMargin=\"4\"/>\n   </DiscretizeBin>\n  </Discretize>\n </DerivedField>\n</LocalTransformations>")

  # one-class svm; this transformation is not usable because svm expects numeric, not factor input
  library(e1071)
  fit_2 <- svm(iris_box$data[, 1:4], y = NULL, type = "one-classification")
  fit_pmml_2 <- pmml(fit_2, dataset = iris_box$data[, 1:4], transforms = iris_box)
  expect_equal(toString(fit_pmml_2[[3]][[3]][[3]]), "<LocalTransformations>\n <DerivedField name=\"dis_pl\" dataType=\"double\" optype=\"continuous\">\n  <Discretize field=\"Petal.Length\" mapMissingTo=\"22\" defaultValue=\"11\">\n   <DiscretizeBin binValue=\"0\">\n    <Interval closure=\"openOpen\" rightMargin=\"0\"/>\n   </DiscretizeBin>\n   <DiscretizeBin binValue=\"1\">\n    <Interval closure=\"closedOpen\" leftMargin=\"0\" rightMargin=\"1\"/>\n   </DiscretizeBin>\n   <DiscretizeBin binValue=\"2\">\n    <Interval closure=\"closedOpen\" leftMargin=\"1\" rightMargin=\"2\"/>\n   </DiscretizeBin>\n   <DiscretizeBin binValue=\"3\">\n    <Interval closure=\"closedOpen\" leftMargin=\"2\" rightMargin=\"3\"/>\n   </DiscretizeBin>\n   <DiscretizeBin binValue=\"4\">\n    <Interval closure=\"closedOpen\" leftMargin=\"3\" rightMargin=\"4\"/>\n   </DiscretizeBin>\n   <DiscretizeBin binValue=\"5\">\n    <Interval closure=\"closedOpen\" leftMargin=\"4\"/>\n   </DiscretizeBin>\n  </Discretize>\n </DerivedField>\n <DerivedField name=\"algorithm_derived_nc_Sepal.Length\" dataType=\"double\" optype=\"continuous\">\n  <NormContinuous field=\"Sepal.Length\">\n   <LinearNorm orig=\"0\" norm=\"-7.05660228803556\"/>\n   <LinearNorm orig=\"5.84333333333333\" norm=\"0\"/>\n  </NormContinuous>\n </DerivedField>\n <DerivedField name=\"algorithm_derived_nc_Sepal.Width\" dataType=\"double\" optype=\"continuous\">\n  <NormContinuous field=\"Sepal.Width\">\n   <LinearNorm orig=\"0\" norm=\"-7.01438362863362\"/>\n   <LinearNorm orig=\"3.05733333333333\" norm=\"0\"/>\n  </NormContinuous>\n </DerivedField>\n <DerivedField name=\"algorithm_derived_nc_Petal.Length\" dataType=\"double\" optype=\"continuous\">\n  <NormContinuous field=\"Petal.Length\">\n   <LinearNorm orig=\"0\" norm=\"-2.12881876228992\"/>\n   <LinearNorm orig=\"3.758\" norm=\"0\"/>\n  </NormContinuous>\n </DerivedField>\n <DerivedField name=\"algorithm_derived_nc_Petal.Width\" dataType=\"double\" optype=\"continuous\">\n  <NormContinuous field=\"Petal.Width\">\n   <LinearNorm orig=\"0\" norm=\"-1.57343750141496\"/>\n   <LinearNorm orig=\"1.19933333333333\" norm=\"0\"/>\n  </NormContinuous>\n </DerivedField>\n</LocalTransformations>")
  
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

test_that("xform_discretize works with table from .csv file", {
  # Example from documentation
  iris_box <- xform_wrap(iris)
  
  expect_error(xform_discretize(iris_box,
                                xform_info = "[Sepal.Length -> dsl][double -> string]",
                                table = "intervals.csv", map_missing_to = "0"),
               NA)
})

test_that("xform_discretize does not give error when 1st column of data matrix is a factor", {
  iris2 <- iris
  iris2[, 6] <- iris2[, 1]
  colnames(iris2)[6] <- "Sepal.Length"
  iris2[, 1] <- iris2[, 5]
  iris2[, 5] <- NULL
  colnames(iris2)[1] <- "Species"
  iris_box <- xform_wrap(iris2)
  
  t <- list()
  m <- data.frame(rbind(
    c("Petal.Length", "dis_pl", "leftInterval", "leftValue", "rightInterval", "rightValue"),
    c("double", "integer", "string", "double", "string", "double"),
    c(NA, 0, "open", NA, "Open", 0),
    c(NA, 1, "closed", 0, "Closed", 1),
    c(NA, 2, "open", 1, "Closed", 2),
    c(NA, 3, "open", 2, "Open", 3),
    c(NA, 4, "closed", 3, "Open", 4),
    c(NA, 5, "closed", 4, "Open", NA)
  ))
  t[[1]] <- m
  def <- c(11)
  mis <- c(22)
  
  iris_box <- xform_discretize(iris_box, xform_info = t, default_value = def, map_missing_to = mis)
  expect_equal(iris_box$field_data[6, 11], "discretize")
})
