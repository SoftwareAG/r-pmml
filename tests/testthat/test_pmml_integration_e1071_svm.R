
data(iris)
data(audit)
audit <- na.omit(audit)
audit_factor <- audit
audit_factor[, 13] <- as.factor(audit_factor[, 13])
iris_mini_dot <- read.csv("iris_mini_dot.csv", stringsAsFactors = TRUE)
iris_p <- read.csv("iris.csv", stringsAsFactors = TRUE)

expect_equal_nn <- function(...) {
  # expect_equal without name checking
  expect_equal(..., check.names = FALSE)
}

svm_ad_predict <- function(fit, newdata) {
  # For e1071::svm anomaly detection models, format the output correctly for matching R with Zementis Server.
  # Output "svm_predict_anomaly" and "anomaly_score" fields for comparison with Zementis Server.
  preds <- predict(fit, newdata = newdata, decision.values = TRUE)
  svm_predict_anomaly <- as.logical(preds)
  anomaly_score <- attr(preds, "decision.values")
  preds_df <- data.frame(svm_predict_anomaly, anomaly_score, stringsAsFactors = FALSE)
  names(preds_df) <- c("svm_predict_anomaly", "anomaly_score")
  return(preds_df)
}

test_that("AnomalyDetectionModel/e1071 one-classification PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

  library(e1071)
  library(zementisr)



  # do not run tests where scale=TRUE

  # fit <- svm(iris[, 1:3], y = NULL, type = "one-classification", scale = TRUE)
  # p_fit <- pmml(fit, dataset = iris[, 1:3])
  # r_pred <- svm_ad_predict(fit, iris[, 1:3])
  # up_stat <- upload_model(p_fit)
  # z_pred <- predict_pmml_batch(iris[, 1:3], up_stat$model_name)
  # delete_model(up_stat$model_name)
  # expect_equal_nn(z_pred$outputs$anomalyScore, r_pred$anomaly_score)
  # # expect failure because PMML predicts anomaly while R predicts inlier
  # expect_failure(expect_equal_nn(z_pred$outputs$anomaly, r_pred$svm_predict_anomaly))

  fit <- svm(iris[, 1:4], y = NULL, type = "one-classification", nu = 0.10, scale = FALSE, kernel = "linear")
  p_fit <- pmml(fit, dataset = iris[, 1:4], detect_anomaly = FALSE)
  r_pred <- svm_ad_predict(fit, iris[, 1:4])
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris[, 1:4], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$anomalyScore, r_pred$anomaly_score)
  expect_equal_nn(z_pred$outputs$inlier, r_pred$svm_predict_anomaly)


  # fit <- svm(iris[, 1:4], y = NULL, type = "one-classification", nu = 0.11, scale = TRUE, kernel = "polynomial")
  # p_fit <- pmml(fit, dataset = iris[, 1:4], detect_anomaly = FALSE)
  # r_pred <- svm_ad_predict(fit, iris[, 1:4])
  # up_stat <- upload_model(p_fit)
  # z_pred <- predict_pmml_batch(iris[, 1:4], up_stat$model_name)
  # delete_model(up_stat$model_name)
  # expect_equal_nn(z_pred$outputs$anomalyScore, r_pred$anomaly_score)
  # expect_equal_nn(z_pred$outputs$inlier, r_pred$svm_predict_anomaly)

  # export above model with detect_anomaly = TRUE
  p_fit <- pmml(fit, dataset = iris[, 1:4], detect_anomaly = TRUE)
  r_pred <- svm_ad_predict(fit, iris[, 1:4])
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris[, 1:4], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$anomalyScore, r_pred$anomaly_score)
  # expect failure because PMML predicts anomaly while R predicts inlier
  expect_failure(expect_equal_nn(z_pred$outputs$anomaly, r_pred$svm_predict_anomaly))


  # fit <- svm(iris[, 1:4], y = NULL, type = "one-classification", nu = 0.21, kernel = "sigmoid")
  # p_fit <- pmml(fit, dataset = iris[, 1:4], detect_anomaly = FALSE)
  # r_pred <- svm_ad_predict(fit, iris[, 1:4])
  # up_stat <- upload_model(p_fit)
  # z_pred <- predict_pmml_batch(iris[, 1:4], up_stat$model_name)
  # delete_model(up_stat$model_name)
  # expect_equal_nn(z_pred$outputs$anomalyScore, r_pred$anomaly_score)
  # expect_equal_nn(z_pred$outputs$inlier, r_pred$svm_predict_anomaly)
  #
  #
  # iris_y <- as.numeric(iris$Species == "setosa")
  # fit <- svm(iris[, 1:4], y = iris_y, type = "one-classification", nu = 0.15, kernel = "sigmoid")
  # p_fit <- pmml(fit, dataset = iris[, 1:4], detect_anomaly = FALSE)
  # r_pred <- svm_ad_predict(fit, iris[, 1:4])
  # up_stat <- upload_model(p_fit)
  # z_pred <- predict_pmml_batch(iris[, 1:4], up_stat$model_name)
  # delete_model(up_stat$model_name)
  # expect_equal_nn(z_pred$outputs$anomalyScore, r_pred$anomaly_score)
  # expect_equal_nn(z_pred$outputs$inlier, r_pred$svm_predict_anomaly)
  #
  # fit <- svm(audit[100:400, c("Income", "Deductions")],
  #   y = NULL, type = "one-classification",
  #   nu = 0.10, scale = TRUE, kernel = "linear"
  # )
  # p_fit <- pmml(fit, dataset = audit[, c("Income", "Deductions")], detect_anomaly = FALSE)
  # r_pred <- svm_ad_predict(fit, audit[, c("Income", "Deductions")])
  # up_stat <- upload_model(p_fit)
  # z_pred <- predict_pmml_batch(audit[, c("Income", "Deductions")], up_stat$model_name)
  # delete_model(up_stat$model_name)
  # expect_equal_nn(z_pred$outputs$anomalyScore, r_pred$anomaly_score)
  # expect_equal_nn(z_pred$outputs$inlier, r_pred$svm_predict_anomaly)


  audit_numeric <- audit[1:500, c("Age", "Income", "Deductions", "Hours", "Adjustment", "Adjusted")]
  audit_numeric$Age <- as.numeric(audit_numeric$Age)
  audit_numeric$Hours <- as.numeric(audit_numeric$Hours)
  audit_numeric$Adjustment <- as.numeric(audit_numeric$Adjustment)
  audit_numeric$Adjusted <- as.numeric(audit_numeric$Adjusted)
  fit <- svm(audit_numeric, y = NULL, type = "one-classification", nu = 0.10, scale = FALSE, kernel = "radial")
  p_fit <- pmml(fit, dataset = audit_numeric, detect_anomaly = FALSE)
  r_pred <- svm_ad_predict(fit, audit_numeric)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_numeric, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$anomalyScore, r_pred$anomaly_score)
  expect_equal_nn(z_pred$outputs$inlier, r_pred$svm_predict_anomaly)


  audit_numeric <- audit[600:900, c("Age", "Income", "Deductions", "Hours", "Adjustment", "Adjusted")]
  audit_numeric$Age <- as.numeric(audit_numeric$Age)
  audit_numeric$Hours <- as.numeric(audit_numeric$Hours)
  audit_numeric$Adjustment <- as.numeric(audit_numeric$Adjustment)
  audit_numeric$Adjusted <- as.numeric(audit_numeric$Adjusted)
  fit <- svm(audit_numeric, y = NULL, type = "one-classification", nu = 0.10, scale = FALSE, kernel = "radial")
  p_fit <- pmml(fit, dataset = audit_numeric, detect_anomaly = FALSE)
  r_pred <- svm_ad_predict(fit, audit_numeric)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_numeric, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$anomalyScore, r_pred$anomaly_score)
  expect_equal_nn(z_pred$outputs$inlier, r_pred$svm_predict_anomaly)

  # box_obj <- xform_wrap(iris[, 1:4])
  # fit <- svm(box_obj$data, y = NULL, type = "one-classification")
  # p_fit <- pmml(fit, dataset = iris[, 1:4], transforms = box_obj, detect_anomaly = FALSE)
  # r_pred <- svm_ad_predict(fit, box_obj$data)
  # up_stat <- upload_model(p_fit)
  # z_pred <- predict_pmml_batch(iris[, 1:4], up_stat$model_name)
  # delete_model(up_stat$model_name)
  # expect_equal_nn(z_pred$outputs$anomalyScore, r_pred$anomaly_score)
  # expect_equal_nn(z_pred$outputs$inlier, r_pred$svm_predict_anomaly)
  #
  #
  # box_obj <- xform_wrap(iris[, 1:4])
  # box_obj <- xform_z_score(box_obj)
  # fit <- svm(box_obj$data[, 5:8], y = NULL, type = "one-classification")
  # p_fit <- pmml(fit, dataset = box_obj$data[, 5:8], transforms = box_obj, detect_anomaly = FALSE)
  # r_pred <- svm_ad_predict(fit, box_obj$data[, 5:8])
  # up_stat <- upload_model(p_fit)
  # z_pred <- predict_pmml_batch(iris[, 1:4], up_stat$model_name)
  # delete_model(up_stat$model_name)
  # expect_equal_nn(z_pred$outputs$anomalyScore, r_pred$anomaly_score)
  # expect_equal_nn(z_pred$outputs$inlier, r_pred$svm_predict_anomaly)


  # box_obj <- xform_wrap(iris[, 1:4])
  # box_obj <- xform_z_score(box_obj, xform_info = "column1->d1")
  # box_obj <- xform_z_score(box_obj, xform_info = "column2->d2")
  # box_obj <- xform_z_score(box_obj, xform_info = "column3->d3")
  # box_obj <- xform_z_score(box_obj, xform_info = "column4->d4")
  # box_obj <- xform_min_max(box_obj, xform_info = "d1->dd1")
  # box_obj <- xform_min_max(box_obj, xform_info = "d2->dd2")
  # box_obj <- xform_min_max(box_obj, xform_info = "d3->dd3")
  # box_obj <- xform_min_max(box_obj, xform_info = "d4->dd4")
  # box_obj <- xform_z_score(box_obj, xform_info = "dd1->ddd1")
  # box_obj <- xform_z_score(box_obj, xform_info = "dd2->ddd2")
  # box_obj <- xform_z_score(box_obj, xform_info = "dd3->ddd3")
  # box_obj <- xform_z_score(box_obj, xform_info = "dd4->ddd4")
  # fit <- svm(box_obj$data[, 13:16], y = NULL, type = "one-classification")
  # p_fit <- pmml(fit, dataset = box_obj$data[, 13:16], transforms = box_obj, detect_anomaly = FALSE)
  # r_pred <- svm_ad_predict(fit, box_obj$data[, 13:16])
  # up_stat <- upload_model(p_fit)
  # z_pred <- predict_pmml_batch(iris[, 1:4], up_stat$model_name)
  # delete_model(up_stat$model_name)
  # expect_equal_nn(z_pred$outputs$anomalyScore, r_pred$anomaly_score)
  # expect_equal_nn(z_pred$outputs$inlier, r_pred$svm_predict_anomaly)


  # box_obj <- xform_wrap(iris_p[, 1:4])
  # box_obj <- xform_discretize(box_obj,
  #   xform_info = "[petal_length->dis_pl][double->integer]",
  #   table = "iris_discretize_pl.csv", map_missing_to = "0", default_value = "1"
  # )
  # box_obj <- xform_discretize(box_obj,
  #   xform_info = "[petal_width->dis_pw][double->integer]",
  #   table = "iris_discretize_pw.csv", map_missing_to = "0", default_value = "1"
  # )
  # box_obj <- xform_discretize(box_obj,
  #   xform_info = "[sepal_length->dis_sl][double->integer]",
  #   table = "iris_discretize_sl.csv", map_missing_to = "0", default_value = "1"
  # )
  # box_obj <- xform_discretize(box_obj,
  #   xform_info = "[sepal_width->dis_sw][double->integer]",
  #   table = "iris_discretize_sw.csv", map_missing_to = "0", default_value = "1"
  # )
  # suppressWarnings(fit <- svm(box_obj$data[, 5:8], y = NULL, type = "one-classification"))
  # p_fit <- pmml(fit, dataset = box_obj$data[, 5:8], transforms = box_obj, detect_anomaly = FALSE)
  # r_pred <- svm_ad_predict(fit, box_obj$data[, 5:8])
  # up_stat <- upload_model(p_fit)
  # z_pred <- predict_pmml_batch(iris_p[, 1:4], up_stat$model_name)
  # delete_model(up_stat$model_name)
  # expect_equal_nn(z_pred$outputs$anomalyScore, r_pred$anomaly_score)
  # expect_equal_nn(z_pred$outputs$inlier, r_pred$svm_predict_anomaly)


  # box_obj <- xform_wrap(audit)
  # box_obj <- xform_z_score(box_obj, xform_info = "column2->d_Age")
  # box_obj <- xform_z_score(box_obj, xform_info = "column7->d_Income")
  # box_obj <- xform_z_score(box_obj, xform_info = "column9->d_Deductions")
  # box_obj <- xform_z_score(box_obj, xform_info = "column10->d_Hours")
  # box_obj <- xform_min_max(box_obj, xform_info = "d_Age->dd_Age")
  # box_obj <- xform_min_max(box_obj, xform_info = "d_Income->dd_Income")
  # box_obj <- xform_min_max(box_obj, xform_info = "d_Deductions->dd_Deductions")
  # box_obj <- xform_min_max(box_obj, xform_info = "d_Hours->dd_Hours")
  # box_obj <- xform_z_score(box_obj, xform_info = "dd_Age->ddd_Age")
  # box_obj <- xform_z_score(box_obj, xform_info = "dd_Income->ddd_Income")
  # box_obj <- xform_z_score(box_obj, xform_info = "dd_Deductions->ddd_Deductions")
  # box_obj <- xform_z_score(box_obj, xform_info = "dd_Hours->ddd_Hours")
  # box_obj <- xform_norm_discrete(box_obj, input_var = "Employment")
  # box_obj <- xform_map(box_obj,
  #   xform_info = "[Marital-> d_Marital][string->double]",
  #   table = "audit_marital_table.csv",
  #   default_value = "-1", map_missing_to = "1"
  # )
  # fit <- svm(box_obj$data[, c(22, 23, 25)],
  #   y = NULL, type = "one-classification", nu = 0.10,
  #   scale = TRUE, kernel = "linear"
  # )
  # p_fit <- pmml(fit, dataset = box_obj$data[, c(22, 23, 25)], transforms = box_obj, detect_anomaly = FALSE)
  # r_pred <- svm_ad_predict(fit, box_obj$data[, c(22, 23, 25)])
  # up_stat <- upload_model(p_fit)
  # z_pred <- predict_pmml_batch(audit, up_stat$model_name)
  # delete_model(up_stat$model_name)
  # expect_equal_nn(z_pred$outputs$anomalyScore, r_pred$anomaly_score)
  # expect_equal_nn(z_pred$outputs$inlier, r_pred$svm_predict_anomaly)


  box_obj <- xform_wrap(audit[, c("Income", "Deductions")])
  fit <- svm(box_obj$data, y = NULL, type = "one-classification")
  p_fit <- pmml(fit, dataset = box_obj$data, transforms = box_obj, detect_anomaly = FALSE)
  r_pred <- svm_ad_predict(fit, box_obj$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$anomalyScore, r_pred$anomaly_score)
  expect_equal_nn(z_pred$outputs$inlier, r_pred$svm_predict_anomaly)

  box_obj <- xform_wrap(audit[, c("Income", "Deductions")])
  box_obj <- xform_function(
    wrap_object = box_obj,
    orig_field_name = "Income",
    new_field_name = "Income.Transformed",
    new_field_data_type = "numeric",
    expression = "Income * 0.1 + 300"
  )

  fit <- svm(box_obj$data, y = NULL, type = "one-classification")
  p_fit <- pmml(fit, dataset = box_obj$data, transforms = box_obj, detect_anomaly = FALSE)
  r_pred <- svm_ad_predict(fit, box_obj$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$anomalyScore, r_pred$anomaly_score)
  expect_equal_nn(z_pred$outputs$inlier, r_pred$svm_predict_anomaly)
})

test_that("SupportVectorMachineModel/e1071 PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

  library(e1071)
  library(zementisr)

  fit <- svm(Petal.Width ~ ., data = iris[, 1:4], kernel = "linear")
  p_fit <- pmml(fit)
  r_pred <- predict(fit, iris[, 1:4])
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris[, 1:4], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$svm_predict_function, r_pred)


  fit <- svm(Adjusted ~ Age + Income + Hours, data = audit[1:900, ])
  p_fit <- pmml(fit)
  r_pred <- predict(fit, audit[1:900, ])
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit[1:900, ], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$svm_predict_function, r_pred)


  fit <- svm(Sex ~ ., data = audit[200:700, 2:9], scale = FALSE)
  p_fit <- pmml(fit)
  r_pred <- as.character(predict(fit, audit[200:700, 2:9]))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit[200:700, 2:9], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Sex, r_pred)


  audit_logical <- audit[1:800, c(2, 8, 13)]
  audit_logical$Adjusted <- as.logical(audit_logical$Adjusted)
  fit <- svm(Sex ~ ., data = audit_logical, scale = FALSE)
  p_fit <- pmml(fit)
  r_pred <- as.character(predict(fit, audit_logical))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_logical, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Sex, r_pred)


  fit <- svm(as.factor(Adjusted) ~ Age + Income + Deductions + Hours, data = audit[1:800, ])
  p_fit <- pmml(fit)
  r_pred <- as.character(predict(fit, audit[1:800, ]))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit[1:800, ], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred)


  fit <- svm(as.factor(Adjusted) ~ ., data = audit[1:700, ])
  p_fit <- pmml(fit)
  r_pred <- as.character(predict(fit, audit[1:700, ]))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit[1:700, ], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred)


  fit <- svm(Marital ~ Income + Deductions, data = audit[1:700, ], kernel = "polynomial")
  p_fit <- pmml(fit)
  r_pred <- as.character(predict(fit, audit[1:700, ]))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit[1:700, ], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Marital, r_pred)


  fit <- svm(Species ~ ., data = iris)
  p_fit <- pmml(fit)
  r_pred <- as.character(predict(fit, iris))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Species, r_pred)


  fit <- svm(sepal_length ~ ., data = iris_mini_dot)
  p_fit <- pmml(fit)
  r_pred <- predict(fit, iris_mini_dot)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_mini_dot, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$svm_predict_function, r_pred)


  fit <- svm(Species ~ ., data = iris, scale = FALSE, probability = TRUE)
  p_fit <- pmml(fit)
  r_pred <- as.character(predict(fit, iris))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Species, r_pred)


  fit <- svm(Species ~ ., data = iris, kernel = "linear")
  p_fit <- pmml(fit)
  r_pred <- as.character(predict(fit, iris))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Species, r_pred)


  fit <- svm(Species ~ ., data = iris, kernel = "polynomial")
  p_fit <- pmml(fit)
  r_pred <- as.character(predict(fit, iris))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Species, r_pred)


  fit <- svm(Species ~ ., data = iris, kernel = "sigmoid")
  p_fit <- pmml(fit)
  r_pred <- as.character(predict(fit, iris))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Species, r_pred)





  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_length->dis_pl][double->integer]",
    table = "iris_discretize_pl.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_width->dis_pw][double->integer]",
    table = "iris_discretize_pw.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[sepal_length->dis_sl][double->integer]",
    table = "iris_discretize_sl.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[sepal_width->dis_sw][double->integer]",
    table = "iris_discretize_sw.csv", map_missing_to = "0", default_value = "1"
  )
  suppressWarnings(fit <- svm(class ~ dis_pl + dis_pw + dis_sl + dis_sw, data = box_obj$data))
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- as.character(predict(fit, box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_class, r_pred)


  box_obj <- xform_wrap(audit)
  box_obj <- xform_z_score(box_obj, xform_info = "column2->d_Age")
  box_obj <- xform_z_score(box_obj, xform_info = "column7->d_Income")
  box_obj <- xform_z_score(box_obj, xform_info = "column9->d_Deductions")
  box_obj <- xform_z_score(box_obj, xform_info = "column10->d_Hours")
  box_obj <- xform_min_max(box_obj, xform_info = "d_Age->dd_Age")
  box_obj <- xform_min_max(box_obj, xform_info = "d_Income->dd_Income")
  box_obj <- xform_min_max(box_obj, xform_info = "d_Deductions->dd_Deductions")
  box_obj <- xform_min_max(box_obj, xform_info = "d_Hours->dd_Hours")
  box_obj <- xform_z_score(box_obj, xform_info = "dd_Age->ddd_Age")
  box_obj <- xform_z_score(box_obj, xform_info = "dd_Income->ddd_Income")
  box_obj <- xform_z_score(box_obj, xform_info = "dd_Deductions->ddd_Deductions")
  box_obj <- xform_z_score(box_obj, xform_info = "dd_Hours->ddd_Hours")
  box_obj <- xform_norm_discrete(box_obj, input_var = "Employment")
  box_obj <- xform_map(box_obj,
    xform_info = "[Marital-> d_Marital][string->double]",
    table = "audit_marital_table.csv", default_value = "-1", map_missing_to = "1"
  )
  fit <- svm(Adjusted ~ ddd_Age + ddd_Income + ddd_Hours, data = box_obj$data)
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- predict(fit, box_obj$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$svm_predict_function, r_pred)



  box_obj <- xform_wrap(audit_factor)
  box_obj <- xform_z_score(box_obj, xform_info = "column2->d_Age")
  box_obj <- xform_z_score(box_obj, xform_info = "column7->d_Income")
  box_obj <- xform_z_score(box_obj, xform_info = "column9->d_Deductions")
  box_obj <- xform_z_score(box_obj, xform_info = "column10->d_Hours")
  box_obj <- xform_min_max(box_obj, xform_info = "d_Age->dd_Age")
  box_obj <- xform_min_max(box_obj, xform_info = "d_Income->dd_Income")
  box_obj <- xform_min_max(box_obj, xform_info = "d_Deductions->dd_Deductions")
  box_obj <- xform_min_max(box_obj, xform_info = "d_Hours->dd_Hours")
  box_obj <- xform_z_score(box_obj, xform_info = "dd_Age->ddd_Age")
  box_obj <- xform_z_score(box_obj, xform_info = "dd_Income->ddd_Income")
  box_obj <- xform_z_score(box_obj, xform_info = "dd_Deductions->ddd_Deductions")
  box_obj <- xform_z_score(box_obj, xform_info = "dd_Hours->ddd_Hours")
  box_obj <- xform_norm_discrete(box_obj, input_var = "Employment")
  box_obj <- xform_map(box_obj,
    xform_info = "[Marital-> d_Marital][string->double]",
    table = "audit_marital_table.csv", default_value = "-1", map_missing_to = "1"
  )
  fit <- svm(Adjusted ~ ., data = box_obj$data[, -c(1, 2, 7, 9, 10, 3, 5)])
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- as.character(predict(fit, box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred)


  fit <- svm(as.factor(Adjusted) ~ ddd_Age + ddd_Income + ddd_Deductions + ddd_Hours, data = box_obj$data)
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- as.character(predict(fit, box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred)


  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_length->dis_pl][double->integer]",
    table = "iris_discretize_pl.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_width->dis_pw][double->integer]",
    table = "iris_discretize_pw.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[sepal_length->dis_sl][double->integer]",
    table = "iris_discretize_sl.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[sepal_width->dis_sw][double->integer]",
    table = "iris_discretize_sw.csv", map_missing_to = "0", default_value = "1"
  )
  suppressWarnings(fit <- svm(class ~ dis_pl + dis_pw + dis_sl + dis_sw, data = box_obj$data))
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- as.character(predict(fit, box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_class, r_pred)


  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_z_score(box_obj, xform_info = "column1->d1")
  box_obj <- xform_z_score(box_obj, xform_info = "column2->d2")
  box_obj <- xform_z_score(box_obj, xform_info = "column3->d3")
  box_obj <- xform_z_score(box_obj, xform_info = "column4->d4")
  box_obj <- xform_min_max(box_obj, xform_info = "d1->dd1")
  box_obj <- xform_min_max(box_obj, xform_info = "d2->dd2")
  box_obj <- xform_min_max(box_obj, xform_info = "d3->dd3")
  box_obj <- xform_min_max(box_obj, xform_info = "d4->dd4")
  box_obj <- xform_z_score(box_obj, xform_info = "dd1->ddd1")
  box_obj <- xform_z_score(box_obj, xform_info = "dd2->ddd2")
  box_obj <- xform_z_score(box_obj, xform_info = "dd3->ddd3")
  box_obj <- xform_z_score(box_obj, xform_info = "dd4->ddd4")
  fit <- svm(class ~ ddd1 + ddd2 + ddd3 + ddd4, data = box_obj$data)
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- as.character(predict(fit, box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_class, r_pred)



  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_z_score(box_obj)
  fit <- svm(class ~ derived_petal_length + derived_petal_width + derived_sepal_length + derived_sepal_width,
    data = box_obj$data
  )
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- as.character(predict(fit, box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_class, r_pred)



  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_z_score(box_obj, xform_info = "column1->d1")
  box_obj <- xform_z_score(box_obj, xform_info = "column2->d2")
  box_obj <- xform_z_score(box_obj, xform_info = "column3->d3")
  box_obj <- xform_z_score(box_obj, xform_info = "column4->d4")
  box_obj <- xform_min_max(box_obj, xform_info = "d1->dd1")
  box_obj <- xform_min_max(box_obj, xform_info = "d2->dd2")
  box_obj <- xform_min_max(box_obj, xform_info = "d3->dd3")
  box_obj <- xform_min_max(box_obj, xform_info = "d4->dd4")
  box_obj <- xform_z_score(box_obj, xform_info = "dd1->ddd1")
  box_obj <- xform_z_score(box_obj, xform_info = "dd2->ddd2")
  box_obj <- xform_z_score(box_obj, xform_info = "dd3->ddd3")
  box_obj <- xform_z_score(box_obj, xform_info = "dd4->ddd4")
  box_obj <- xform_map(box_obj,
    xform_info = "[class->d_class][string->double]",
    table = "iris_p_class_table.csv", default_value = "-1", map_missing_to = "1"
  )
  box_obj <- xform_norm_discrete(box_obj, input_var = "class")
  fit <- svm(class ~ ddd1 + ddd2 + ddd3 + ddd4, data = box_obj$data)
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- as.character(predict(fit, box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_class, r_pred)



  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_length->dis_pl][double->integer]",
    table = "iris_discretize_pl.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_width->dis_pw][double->integer]",
    table = "iris_discretize_pw.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[sepal_length->dis_sl][double->integer]",
    table = "iris_discretize_sl.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[sepal_width->dis_sw][double->integer]",
    table = "iris_discretize_sw.csv", map_missing_to = "0", default_value = "1"
  )
  suppressWarnings(fit <- svm(class ~ dis_pl + dis_pw + dis_sl + dis_sw, data = box_obj$data))
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- as.character(predict(fit, box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_class, r_pred)
})
