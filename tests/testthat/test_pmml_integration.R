context("test integration")

library(zementisr)
library(amap)
library(isofor)
library(clue)
library(data.table)
library(glmnet)
library(ada)
library(gbm)
library(caret)
library(randomForest)
library(xgboost)
library(Matrix)
library(e1071)
library(neighbr)
library(nnet)
library(kernlab)
library(forecast)


data(iris)
data(audit)
data("WWWusage")
data("AirPassengers")
data("USAccDeaths")
data("JohnsonJohnson")
data("sunspots")
audit_factor <- audit
audit_factor[, 13] <- as.factor(audit_factor[, 13])
iris_p <- read.csv("iris.csv", stringsAsFactors = TRUE)
audit <- na.omit(audit)
elnino <- read.csv("elnino.csv", stringsAsFactors = TRUE)
heart <- read.csv("heart.csv", stringsAsFactors = TRUE)
glm_issue3543_data <- read.csv("glm_issue3543_data.csv", stringsAsFactors = TRUE)
credit_class <- read.csv("credit_class.csv", stringsAsFactors = TRUE)
covtype2 <- read.csv("covtype2.csv", header = TRUE, stringsAsFactors = TRUE)
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
credit_class_01 <- read.csv("credit_class_01.csv", stringsAsFactors = TRUE)
audit_nor_logical <- na.omit(read.csv("audit_nor_logical.csv", stringsAsFactors = TRUE))
audit_nor <- na.omit(read.csv("audit_nor.csv"))
audit_nor_fake_logical <- na.omit(read.csv("audit_nor_fake_logical.csv", stringsAsFactors = TRUE))
random_data_small <- read.csv("random_data_small.csv", stringsAsFactors = TRUE)
iris_nor <- read.csv("iris_nor.csv", stringsAsFactors = TRUE)
bank <- na.omit(read.csv("bank.csv", stringsAsFactors = TRUE))
audit_r_build_in <- na.omit(read.csv("audit_r_build_in.csv", stringsAsFactors = TRUE))
insurance <- na.omit(read.csv("insurance.csv", stringsAsFactors = TRUE))
iris_bin <- read.csv("iris_bin.csv", stringsAsFactors = TRUE)
house_votes <- na.omit(read.csv("house_votes_84.csv", stringsAsFactors = TRUE))
iris_mini_dot <- read.csv("iris_mini_dot.csv", stringsAsFactors = TRUE)
petfood <- read.csv("petfood.csv", stringsAsFactors = TRUE)
job_cat <- read.csv("job_cat.csv", stringsAsFactors = TRUE)
job_cat_index <- read.csv("job_cat_index.csv", stringsAsFactors = TRUE)
iris_nor_logical <- read.csv("iris_nor_logical.csv", stringsAsFactors = TRUE)
factor_40k <- read.csv("factor_40k.csv", stringsAsFactors = TRUE)
numeric_10k <- na.omit(read.csv("numeric_10k.csv", stringsAsFactors = TRUE))
factor_10k <- read.csv("factor_10k.csv", stringsAsFactors = TRUE)
numeric_no_na_10k <- read.csv("numeric_no_na_10k.csv", stringsAsFactors = TRUE)


forecast_with_cpi <- function(model, h) {
  # create data frame of point forecast and CPI
  pr <- forecast(model, h)
  dframe <- data.frame(
    "Predicted_ts_value" = as.numeric(pr$mean),
    "cpi_80_lower" = as.numeric(pr$lower[, 1]),
    "cpi_80_upper" = as.numeric(pr$upper[, 1]),
    "cpi_95_lower" = as.numeric(pr$lower[, 2]),
    "cpi_95_upper" = as.numeric(pr$upper[, 2])
  )
  return(dframe)
}

expect_equal_df <- function(z_pred, r_pred) {
  # expect_equal for data frames with point and CPI

  # rearrange z_pred columns into same order as that of r_pred
  z_pred <- z_pred[c(
    "Predicted_ts_value", "cpi_80_lower",
    "cpi_80_upper", "cpi_95_lower", "cpi_95_upper"
  )]

  expect_equal_nn(z_pred, r_pred)
}

expect_equal_df_2 <- function(z_pred_out, r_pred) {
  # expect_equal for data frames with point and CPI where Zementis output is a JSON string.
  
  z_pred_out <- z_pred_out[NROW(z_pred_out),] # only use the last row
  
  # rearrange z_pred columns into same order as that of r_pred;
  # using names(r_pred) accounts for names with different CPI
  z_pred_out <- z_pred_out[names(r_pred)]
  
  z_pred_transf <- data.frame(matrix(NA, nrow = NCOL(z_pred_out[[1]]),
                      ncol = NCOL(r_pred)), stringsAsFactors = TRUE)
  colnames(z_pred_transf) <- names(r_pred)
  
  for (x in names(r_pred)) {
    z_pred_transf[x] <- as.numeric(t(z_pred_out[[x]]))
  }
  
  expect_equal_nn(z_pred_transf, r_pred)
  
  
}



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

single_col_h_df <- function(h) {
  # For ARIMA models, create a data frame with a single column of h (number of steps ahead) values.
  dframe <- data.frame("h" = c(1:h), stringsAsFactors = TRUE)
  return(dframe)
}

h_20 <- single_col_h_df(20)
h_20_one_line <- data.frame("h" = c(20))

# Temp files for xgboost
xgb_tmp_01_save <- tempfile()
xgb_tmp_01_dump <- tempfile()


# setup({
#   # Delete any PMML files on Zementis Server
#   mods <- get_models()
#   lapply(mods, delete_model)
# })


teardown(unlink(c(xgb_tmp_01_save, xgb_tmp_01_dump), recursive = TRUE))

test_that("TimeSeriesModel/forecast PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

  # non-seasonal tests - with CLS and CPI
  fit <- auto.arima(sunspots) # creates non-seasonal model
  p_fit <- pmml(fit, model_name = "arima_auto_01")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_2(z_pred$outputs, r_pred)

  fit <- auto.arima(JohnsonJohnson) # creates seasonal model
  p_fit <- pmml(fit, model_name = "arima_auto_02", exact_least_squares = FALSE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20,]), r_pred)

  fit <- Arima(AirPassengers, order = c(2, 1, 2))
  p_fit <- pmml(fit, model_name = "arima_212")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_2(z_pred$outputs, r_pred)

  fit <- Arima(AirPassengers, order = c(1, 1, 1))
  p_fit <- pmml(fit, model_name = "arima_111")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_2(z_pred$outputs, r_pred)

  fit <- Arima(WWWusage, order = c(2, 0, 2))
  p_fit <- pmml(fit, model_name = "arima_202")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_2(z_pred$outputs, r_pred)

  # fit <- Arima(WWWusage, order = c(3, 1, 1), include.drift = TRUE)
  # p_fit <- pmml(fit, model_name = "arima_311")
  # r_pred <- forecast_with_cpi(fit, 20)
  # up_stat <- upload_model(p_fit)
  # z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  # delete_model(up_stat$model_name)
  # expect_equal_df(z_pred$outputs, r_pred)

  fit <- Arima(USAccDeaths, order = c(1, 2, 0))
  p_fit <- pmml(fit, model_name = "arima_120")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_2(z_pred$outputs, r_pred)

  # non-seasonal tests with d=2 - expect mismatch
  fit <- Arima(AirPassengers, order = c(2, 2, 2))
  p_fit <- pmml(fit, model_name = "arima_222")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_failure(expect_equal_df_2(z_pred$outputs, r_pred))

  fit <- Arima(USAccDeaths, order = c(1, 2, 3))
  p_fit <- pmml(fit, model_name = "arima_123")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_failure(expect_equal_df_2(z_pred$outputs, r_pred))

  # seasonal with CLS
  fit <- Arima(JohnsonJohnson, order = c(1, 1, 0), seasonal = c(0, 1, 1))
  p_fit <- pmml(fit, model_name = "arima_110011", exact_least_squares = FALSE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20,]), r_pred)
  
  fit <- Arima(JohnsonJohnson, order = c(0, 1, 0), seasonal = c(0, 1, 0))
  p_fit <- pmml(fit, model_name = "arima_010010", exact_least_squares = FALSE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20,]), r_pred)
  
  fit <- Arima(JohnsonJohnson, order = c(0, 1, 0), seasonal = c(0, 1, 2))
  p_fit <- pmml(fit, model_name = "arima_010012", exact_least_squares = FALSE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20,]), r_pred)
  
  fit <- Arima(AirPassengers, order = c(0, 1, 1), seasonal = c(0, 1, 1))
  p_fit <- pmml(fit, model_name = "arima_011011", exact_least_squares = FALSE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20,]), r_pred)
  
  fit <- Arima(JohnsonJohnson, order = c(1, 1, 1), seasonal = c(1, 1, 1))
  p_fit <- pmml(fit, model_name = "arima_111111", exact_least_squares = FALSE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20,]), r_pred)

  fit <- Arima(JohnsonJohnson, order = c(0, 0, 1), seasonal = c(0, 1, 0))
  p_fit <- pmml(fit, model_name = "arima_001010", exact_least_squares = FALSE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20,]), r_pred)
  
  fit <- Arima(sunspots, order = c(1, 0, 0), seasonal = c(1, 0, 0))
  p_fit <- pmml(fit, model_name = "arima_100100", exact_least_squares = FALSE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20,]), r_pred)
  
  # expect the following test to fail
  fit <- Arima(JohnsonJohnson, order = c(1, 1, 0), seasonal = c(0, 0, 1))
  p_fit <- pmml(fit, model_name = "arima_110001", exact_least_squares = FALSE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_failure(expect_equal_nn(as.numeric(
    z_pred$outputs$Predicted_ts_value[20,]), r_pred))

  # tests with seasonal ELS
  fit <- Arima(JohnsonJohnson, order = c(1, 1, 0), seasonal = c(0, 0, 1))
  p_fit <- pmml(fit, model_name = "arima_110001", exact_least_squares = TRUE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20,]), r_pred)
  
  fit <- Arima(JohnsonJohnson, order = c(4, 1, 4), seasonal = c(1, 1, 1))
  p_fit <- pmml(fit, model_name = "arima_414111", exact_least_squares = TRUE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20,]), r_pred)
  
  fit <- Arima(USAccDeaths, order = c(2, 1, 1), seasonal = c(2, 1, 4))
  p_fit <- pmml(fit, model_name = "arima_211214", exact_least_squares = TRUE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20,]), r_pred)
  
  fit <- Arima(USAccDeaths, order = c(2, 2, 3), seasonal = c(0, 1, 1))
  p_fit <- pmml(fit, model_name = "arima_223011", exact_least_squares = TRUE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20,]), r_pred)
  
  fit <- Arima(USAccDeaths, order = c(2, 0, 0), seasonal = c(2, 0, 0))
  p_fit <- pmml(fit, model_name = "arima_200200", exact_least_squares = TRUE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20,]), r_pred)
  
  fit <- Arima(WWWusage, order = c(3, 0, 3), seasonal = c(3, 0, 3))
  p_fit <- pmml(fit, model_name = "arima_303303", exact_least_squares = TRUE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20,]), r_pred)
  
  fit <- Arima(JohnsonJohnson, order = c(0, 0, 4), seasonal = c(0, 2, 4))
  p_fit <- pmml(fit, model_name = "arima_004024", exact_least_squares = TRUE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20,]), r_pred)
  
  # seasonal model with (0,0,0) non-seasonal component
  fit <- Arima(JohnsonJohnson, order = c(0, 0, 0), seasonal = c(0, 2, 4))
  p_fit <- pmml(fit, model_name = "arima_000024", exact_least_squares = FALSE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20,]), r_pred)
})


test_that("AnomalyDetectionModel/iForest PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

  anomaly_threshold <- 0.6
  fit <- iForest(iris, nt = 7, phi = 30)
  p_fit <- pmml(fit, anomaly_threshold = anomaly_threshold)
  r_pred <- predict(fit, iris)
  r_pred_anomaly <- r_pred >= anomaly_threshold
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$anomalyScore, r_pred, tolerance = 1e-5)
  expect_equal_nn(z_pred$outputs$anomaly, r_pred_anomaly)

  anomaly_threshold <- 0.5
  fit <- iForest(as.matrix(iris[, 1:4]), nt = 6, phi = 27)
  p_fit <- pmml(fit, anomaly_threshold = anomaly_threshold)
  r_pred <- predict(fit, as.matrix(iris[, 1:4]))
  r_pred_anomaly <- r_pred >= anomaly_threshold
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris[, 1:4], up_stat$model_name) # cannot use matrix, must be data frame or file
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$anomalyScore, r_pred, tolerance = 1e-5)
  expect_equal_nn(z_pred$outputs$anomaly, r_pred_anomaly)


  anomaly_threshold <- 0.4
  box_obj <- xform_wrap(audit[, -1])
  box_obj <- xform_norm_discrete(box_obj, xform_info = "Sex")
  box_obj <- xform_function(box_obj, orig_field_name = "Age,Hours", new_field_name = "Agrs", expression = "Age/Hours")
  fit <- iForest(box_obj$data[, -c(1, 7, 9, 10)], nt = 5, phi = 420)
  p_fit <- pmml(fit, transforms = box_obj, anomaly_threshold = anomaly_threshold)
  r_pred <- predict(fit, box_obj$data[, -c(1, 7, 9, 10)])
  r_pred_anomaly <- r_pred >= anomaly_threshold
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$anomalyScore, r_pred)
  expect_equal_nn(z_pred$outputs$anomaly, r_pred_anomaly)
})


test_that("ClusteringModel/stats kmeans PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

  fit <- kmeans(audit[, c(2, 7, 9, 10, 12)], 2)
  p_fit <- pmml(fit)
  r_pred <- sprintf("%.0f", cl_predict(fit, audit[, c(2, 7, 9, 10, 12)]))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit[, c(2, 7, 9, 10, 12)], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$predictedValue, r_pred)


  fit <- kmeans(iris[, 1:4], 3)
  p_fit <- pmml(fit)
  r_pred <- sprintf("%.0f", cl_predict(fit, iris[, 1:4]))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$predictedValue, r_pred)


  box_obj <- xform_wrap(iris)
  box_obj <- xform_z_score(box_obj, "column1->d1")
  box_obj <- xform_z_score(box_obj, "column2->d2")
  box_obj <- xform_z_score(box_obj, "column3->d3")
  box_obj <- xform_z_score(box_obj, "column4->d4")
  box_obj <- xform_min_max(box_obj, "d1->dd1")
  box_obj <- xform_min_max(box_obj, "d2->dd2")
  box_obj <- xform_min_max(box_obj, "d3->dd3")
  box_obj <- xform_min_max(box_obj, "d4->dd4")
  box_obj <- xform_z_score(box_obj, "dd1->ddd1")
  box_obj <- xform_z_score(box_obj, "dd2->ddd2")
  box_obj <- xform_z_score(box_obj, "dd3->ddd3")
  box_obj <- xform_z_score(box_obj, "dd4->ddd4")
  fit <- kmeans(box_obj$data[, 14:17], 3)
  p_fit <- pmml(fit, transform = box_obj)
  r_pred <- sprintf("%.0f", cl_predict(fit, data = box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$predictedValue, r_pred)


  box_obj <- xform_wrap(iris)
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
  box_obj <- xform_map(box_obj, xform_info = "[Species->d_Species][string->double]", table = "iris_class_table.csv", default_value = "-1", map_missing_to = "1")
  box_obj <- xform_norm_discrete(box_obj, input_var = "Species")
  fit <- kmeans(box_obj$data[, 14:21], 3)
  p_fit <- pmml(fit, transform = box_obj)
  r_pred <- sprintf("%.0f", cl_predict(fit, data = box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$predictedValue, r_pred)


  box_obj <- xform_wrap(iris)
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
  box_obj <- xform_map(box_obj, xform_info = "[Species->d_Species][string->double]", table = "iris_class_full_name_table.csv", default_value = "-1", map_missing_to = "1")
  box_obj <- xform_norm_discrete(box_obj, input_var = "Species")
  fit <- kmeans(box_obj$data[, 14:21], 3)
  p_fit <- pmml(fit, transform = box_obj)
  r_pred <- sprintf("%.0f", cl_predict(fit, data = box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$predictedValue, r_pred)
})


test_that("GeneralRegressionModel/glmnet PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

  fit <- cv.glmnet(data.matrix(audit[, c(2, 7, 9:10)]), data.matrix(audit[, 13]))
  p_fit <- pmml(fit)
  r_pred <- as.vector(predict(fit, data.matrix(audit[, c(2, 7, 9:10)])))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit[, c(2, 7, 9:10)], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$predictedValue, r_pred)


  fit <- cv.glmnet(data.matrix(iris[1:4]), data.matrix(iris[5]))
  p_fit <- pmml(fit)
  r_pred <- as.vector(predict(fit, data.matrix(data.matrix(iris[1:4]))))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris[1:4], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$predictedValue, r_pred)


  fit <- cv.glmnet(data.matrix(elnino[1:6]), data.matrix(elnino[7]), family = "poisson")
  p_fit <- pmml(fit)
  r_pred <- as.vector(predict(fit, data.matrix(elnino[1:6])))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(elnino[1:6], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$predictedValue, r_pred)


  fit <- cv.glmnet(data.matrix(elnino[1:6]), data.matrix(elnino[7]))
  p_fit <- pmml(fit)
  r_pred <- as.vector(predict(fit, data.matrix(elnino[1:6])))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(elnino[1:6], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$predictedValue, r_pred)


  box_obj <- xform_wrap(elnino)
  rownames(box_obj$field_data)[7] <- "predictedScore"
  box_obj$field_data["predictedScore", "dataType"] <- "numeric"
  names(box_obj$data)[7] <- "predictedScore"
  box_obj <- xform_z_score(box_obj, xform_info = "column1->d1")
  box_obj <- xform_z_score(box_obj, xform_info = "column2->d2")
  box_obj <- xform_z_score(box_obj, xform_info = "column3->d3")
  box_obj <- xform_z_score(box_obj, xform_info = "column4->d4")
  box_obj <- xform_z_score(box_obj, xform_info = "column5->d5")
  box_obj <- xform_z_score(box_obj, xform_info = "column6->d6")
  box_obj <- xform_z_score(box_obj, xform_info = "column7->d7")
  box_obj <- xform_min_max(box_obj, xform_info = "d1->dd1")
  box_obj <- xform_min_max(box_obj, xform_info = "d2->dd2")
  box_obj <- xform_min_max(box_obj, xform_info = "d3->dd3")
  box_obj <- xform_min_max(box_obj, xform_info = "d4->dd4")
  box_obj <- xform_min_max(box_obj, xform_info = "d5->dd5")
  box_obj <- xform_min_max(box_obj, xform_info = "d6->dd6")
  box_obj <- xform_min_max(box_obj, xform_info = "d7->dd7")
  box_obj <- xform_z_score(box_obj, xform_info = "dd1->ddd1")
  box_obj <- xform_z_score(box_obj, xform_info = "dd2->ddd2")
  box_obj <- xform_z_score(box_obj, xform_info = "dd3->ddd3")
  box_obj <- xform_z_score(box_obj, xform_info = "dd4->ddd4")
  box_obj <- xform_z_score(box_obj, xform_info = "dd5->ddd5")
  box_obj <- xform_z_score(box_obj, xform_info = "dd6->ddd6")
  box_obj <- xform_z_score(box_obj, xform_info = "dd7->ddd7")
  x <- data.matrix(box_obj$data[1:6])
  y <- data.matrix(box_obj$data[7])
  fit <- cv.glmnet(x, y)
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- as.numeric(predict(fit, x))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(elnino, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$predictedValue, r_pred)


  box_obj <- xform_wrap(iris)
  box_obj <- xform_map(box_obj,
    xform_info = "[Species->d_Species][string->double]",
    table = "iris_class_full_name_table.csv", default_value = "-1", map_missing_to = "1"
  )
  x <- data.matrix(box_obj$data[, c(1:3, 6)])
  y <- data.matrix(box_obj$data[4])
  fit <- cv.glmnet(x, y)
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- as.numeric(predict(fit, x))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$predictedValue, r_pred)


  box_obj <- xform_wrap(elnino)
  box_obj <- rename_wrap_var(wrap_object = box_obj, xform_info = "temp->predictedScore")
  box_obj <- xform_z_score(box_obj, xform_info = "column1->d1")
  box_obj <- xform_z_score(box_obj, xform_info = "column2->d2")
  box_obj <- xform_z_score(box_obj, xform_info = "column3->d3")
  box_obj <- xform_z_score(box_obj, xform_info = "column4->d4")
  box_obj <- xform_z_score(box_obj, xform_info = "column5->d5")
  box_obj <- xform_z_score(box_obj, xform_info = "column6->d6")
  box_obj <- xform_z_score(box_obj, xform_info = "column7->d7")
  box_obj <- xform_min_max(box_obj, xform_info = "d1->dd1")
  box_obj <- xform_min_max(box_obj, xform_info = "d2->dd2")
  box_obj <- xform_min_max(box_obj, xform_info = "d3->dd3")
  box_obj <- xform_min_max(box_obj, xform_info = "d4->dd4")
  box_obj <- xform_min_max(box_obj, xform_info = "d5->dd5")
  box_obj <- xform_min_max(box_obj, xform_info = "d6->dd6")
  box_obj <- xform_min_max(box_obj, xform_info = "d7->dd7")
  box_obj <- xform_z_score(box_obj, xform_info = "dd1->ddd1")
  box_obj <- xform_z_score(box_obj, xform_info = "dd2->ddd2")
  box_obj <- xform_z_score(box_obj, xform_info = "dd3->ddd3")
  box_obj <- xform_z_score(box_obj, xform_info = "dd4->ddd4")
  box_obj <- xform_z_score(box_obj, xform_info = "dd5->ddd5")
  box_obj <- xform_z_score(box_obj, xform_info = "dd6->ddd6")
  box_obj <- xform_z_score(box_obj, xform_info = "dd7->ddd7")
  x <- data.matrix(box_obj$data[1:6])
  y <- data.matrix(box_obj$data[7])
  fit <- cv.glmnet(x, y)
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- as.numeric(predict(fit, x))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(elnino, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$predictedValue, r_pred)


  x <- data.matrix(iris_p[, c(1:3, 5)])
  y <- data.matrix(iris_p[4])
  box_obj <- xform_wrap(x)
  box_obj <- xform_map(box_obj,
    xform_info = "[class->d_class][string->double]",
    table = "iris_class_index_table.csv", default_value = "-1", map_missing_to = "1"
  )
  box_obj <- xform_norm_discrete(box_obj, input_var = "class")
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_length->dis_pl][double->integer]",
    table = "iris_discretize_pl.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_width->dis_pw][double->integer]",
    table = "iris_discretize_pw.csv", map_missing_to = "0", default_value = "1"
  )
  fit <- cv.glmnet(as.matrix(box_obj$data), y)
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- as.numeric(predict(fit, as.matrix(box_obj$data)))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(as.data.frame(x, stringsAsFactors = TRUE), up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$predictedValue, r_pred)


  x <- data.frame(replicate(20, rnorm(1000)))
  y <- rnorm(1000)
  box_obj <- xform_wrap(x)
  box_obj <- xform_min_max(box_obj, xform_info = "column1->d_X1")
  box_obj <- xform_min_max(box_obj, xform_info = "X2->d_X2")
  box_obj <- xform_min_max(box_obj, xform_info = "X3->myDerived_X3[10,20]")
  box_obj <- xform_z_score(box_obj, xform_info = "column4->d_X4")
  box_obj <- xform_z_score(box_obj, xform_info = "X5->d_X5")

  fit <- cv.glmnet(as.matrix(box_obj$data), y)
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- as.numeric(predict(fit, as.matrix(box_obj$data)))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(as.data.frame(x, stringsAsFactors = TRUE), up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$predictedValue, r_pred)
})


test_that("GeneralRegressionModel/stats PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

  # suppress warning: "glm.fit: fitted probabilities numerically 0 or 1 occurred"
  suppressWarnings(fit <- glm(
    formula = as.factor(Adjusted) ~ Age + Employment + Education + Marital + Occupation + Income + Sex + Deductions + Hours,
    family = binomial(link = logit), audit
  ))
  p_fit <- pmml(fit)
  r_pred <- predict(fit, audit, type = "response")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred)


  fit <- glm(Out ~ ., data = glm_issue3543_data)
  p_fit <- pmml(fit)
  r_pred <- predict(fit, glm_issue3543_data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(glm_issue3543_data, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Out, r_pred)


  suppressWarnings(fit <- glm(
    formula = as.factor(Adjusted) ~ Age + Employment + Education + Marital + Occupation + Income + Sex + Deductions + Hours,
    family = binomial(link = logit), audit
  ))
  p_fit <- pmml(fit)
  r_pred <- predict(fit, audit, type = "response")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred)


  fit <- glm(formula = target ~ A1 + A2 + A3, family = binomial(link = logit), data = credit_class)
  p_fit <- pmml(fit)
  r_pred <- unname(predict(fit, credit_class, type = "response"))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(credit_class, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_pos, r_pred, tolerance = 1e-4)


  fit <- glm(
    formula = Income ~ Age + Employment + Education + Marital + Occupation + Sex + Hours,
    family = Gamma(link = inverse), audit_nor
  )
  p_fit <- pmml(fit)
  r_pred <- predict(fit, audit_nor, type = "response")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_nor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Income, r_pred)


  fit <- glm(
    formula = Adjusted ~ Age + Employment + Education + Marital + Occupation + Income + Sex + Deductions + Hours,
    family = gaussian(link = identity), audit
  )
  p_fit <- pmml(fit)
  r_pred <- predict(fit, audit, type = "response")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred)


  fit <- glm(formula = as.factor(fbs) ~ ., family = binomial(link = logit), heart)
  p_fit <- pmml(fit)
  r_pred <- predict(fit, heart, type = "response")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(heart, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred)


  suppressWarnings(fit <- glm(
    formula = Adjusted ~ Age + Employment + Education + Marital + Occupation + Income + Sex + Deductions + Hours,
    family = poisson(link = log), audit
  ))
  p_fit <- pmml(fit)
  r_pred <- predict(fit, audit, type = "response")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred)


  iris_binom <- iris
  iris_binom$y <- I(iris$Species == "setosa")
  # suppress warning: "glm.fit: algorithm did not converge"
  suppressWarnings(fit <- glm(y ~ ., data = iris_binom, family = binomial))
  p_fit <- pmml(fit)
  r_pred <- predict(fit, iris_binom, type = "response")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_binom, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_y, r_pred)


  box_obj <- xform_wrap(audit)
  box_obj <- xform_z_score(box_obj, "column2->d_Age")
  box_obj <- xform_z_score(box_obj, "column7->d_Income")
  box_obj <- xform_z_score(box_obj, "column9->d_Deductions")
  box_obj <- xform_z_score(box_obj, "column10->d_Hours")
  box_obj <- xform_min_max(box_obj, "d_Age->dd_Age")
  box_obj <- xform_min_max(box_obj, "d_Income->dd_Income")
  box_obj <- xform_min_max(box_obj, "d_Deductions->dd_Deductions")
  box_obj <- xform_min_max(box_obj, "d_Hours->dd_Hours")
  box_obj <- xform_z_score(box_obj, "dd_Age->ddd_Age")
  box_obj <- xform_z_score(box_obj, "dd_Income->ddd_Income")
  box_obj <- xform_z_score(box_obj, "dd_Deductions->ddd_Deductions")
  box_obj <- xform_z_score(box_obj, "dd_Hours->ddd_Hours")
  suppressWarnings(fit <- glm(
    formula = as.factor(Adjusted) ~ ddd_Age + Employment +
      Education + Marital + Occupation + ddd_Income + Sex + ddd_Deductions + ddd_Hours,
    family = binomial(link = logit), box_obj$data
  ))
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- predict(fit, box_obj$data, type = "response")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred)


  audit$Adjusted <- as.factor(audit$Adjusted)
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

  suppressWarnings(fit <- glm(
    formula = Adjusted ~ ddd_Age + ddd_Income + ddd_Deductions +
      ddd_Hours + d_Marital + Employment_Private + Employment_Consultant +
      Employment_SelfEmp + Employment_PSLocal + Employment_PSState +
      Employment_PSFederal + Employment_Volunteer + Sex + Occupation + Education,
    family = binomial(link = logit), box_obj$data
  ))
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- unname(predict(fit, box_obj$data, type = "response"))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred, tolerance = 1e-2)
})


test_that("MiningModel/ada PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

  set.seed(1234)
  fit <- ada(Adjusted ~ Employment + Education + Hours + Income, iter = 3, audit)
  p_fit <- pmml(fit)
  r_pred <- predict(fit, audit, type = "both")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_0, r_pred$probs[, 1], tolerance = 1e-3)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, as.numeric(r_pred$class) - 1)

  set.seed(12345)
  fit <- ada(as.factor(fbs) ~ ., iter = 5, data = heart)
  p_fit <- pmml(fit)
  r_pred <- predict(fit, heart, type = "both")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(heart, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_0, r_pred$probs[, 1])
  expect_equal_nn(z_pred$outputs$Predicted_fbs, as.character(r_pred$class))

  set.seed(1236)
  fit <- ada(target ~ ., iter = 11, data = credit_class)
  p_fit <- pmml(fit)
  r_pred <- predict(fit, credit_class)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(credit_class, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_target, as.character(r_pred))

  set.seed(1834)
  iris_binom_2 <- iris[iris[, 5] != "setosa", ]
  iris_binom_2[, 5] <- as.factor(levels(iris[, 5])[2:3])[as.numeric(iris[, 5]) - 1]
  fit <- ada(Species ~ ., data = iris_binom_2, iter = 20, nu = 0.9, type = "discrete")
  p_fit <- pmml(fit)
  # remove precision issues by varying the training data randomly
  iris_binom_2[, 1:4] <- iris_binom_2[, 1:4] + runif(4, -0.1, 0.1)
  r_pred <- predict(fit, iris_binom_2, type = "both")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_binom_2[, 1:4], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Species, as.character(r_pred$class))
  expect_equal_nn(z_pred$outputs$Probability_versicolor, r_pred$probs[, 1])

  set.seed(534)
  fit <- ada(as.factor(Adjusted) ~ Employment + Education + Hours + Income, iter = 3, audit)
  p_fit <- pmml(fit)
  r_pred <- predict(fit, audit)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, as.character(r_pred))


  box_obj <- xform_wrap(audit)
  box_obj <- xform_z_score(box_obj, "column2->d_Age")
  box_obj <- xform_z_score(box_obj, "column7->d_Income")
  box_obj <- xform_z_score(box_obj, "column9->d_Deductions")
  box_obj <- xform_z_score(box_obj, "column10->d_Hours")
  box_obj <- xform_min_max(box_obj, "d_Age->dd_Age")
  box_obj <- xform_min_max(box_obj, "d_Income->dd_Income")
  box_obj <- xform_min_max(box_obj, "d_Deductions->dd_Deductions")
  box_obj <- xform_min_max(box_obj, "d_Hours->dd_Hours")
  box_obj <- xform_z_score(box_obj, "dd_Age->ddd_Age")
  box_obj <- xform_z_score(box_obj, "dd_Income->ddd_Income")
  box_obj <- xform_z_score(box_obj, "dd_Deductions->ddd_Deductions")
  box_obj <- xform_z_score(box_obj, "dd_Hours->ddd_Hours")
  set.seed(12884)
  fit <- ada(as.factor(Adjusted) ~ ddd_Age + ddd_Income + Sex + ddd_Deductions + ddd_Hours, iter = 3, box_obj$data)
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- predict(fit, box_obj$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, as.character(r_pred))
})


test_that("MiningModel/gbm PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

  set.seed(2112)
  audit_dat <- audit[, -c(1, 4, 6, 9, 10, 11, 12)]
  train_index <- createDataPartition(audit_dat$Adjusted, p = .96, list = FALSE, times = 1)
  audit_dat_train <- audit_dat[train_index, ]
  audit_dat_test <- audit_dat[-train_index, ]


  fit <- gbm(Adjusted ~ ., data = audit_dat_train, n.trees = 3, interaction.depth = 4, distribution = "bernoulli")
  p_fit <- pmml(fit)
  r_pred <- predict(fit, newdata = audit_dat_test, n.trees = 3)
  r_pred_2 <- predict(fit, newdata = audit_dat_test, n.trees = 3, type = "response")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_dat_test, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$BernoulliLinkPrediction, r_pred)
  expect_equal_nn(z_pred$outputs$BernoulliResponsePrediction, r_pred_2)


  fit <- gbm(Adjusted ~ ., data = audit_dat_train, n.trees = 3, interaction.depth = 4, distribution = "gaussian")
  p_fit <- pmml(fit)
  r_pred <- predict(fit, newdata = audit_dat_test, n.trees = 3)
  r_pred_2 <- predict(fit, newdata = audit_dat_test, n.trees = 3, type = "response") - fit$initF
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_dat_test, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$GaussianPrediction, r_pred)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred_2)


  num_rows <- NROW(covtype2)
  covtype2_m <- as.matrix(covtype2)
  y0 <- as.vector(covtype2_m[, "X3"])
  invisible(capture.output(fit <- gbm.fit(covtype2_m[, 1:11], y0,
    distribution = "multinomial", n.trees = 3, interaction.depth = 4
  )))
  p_fit <- pmml(fit)
  r_pred <- predict(fit, newdata = covtype2_m[, 1:11], n.trees = 3)[1:num_rows, , ]
  r_pred_2 <- predict(fit, newdata = covtype2_m[, 1:11], n.trees = 3, type = "response")[1:num_rows, , ]
  pr <- vector()
  for (i in 1:nrow(r_pred_2)) {
    pr[i] <- (colnames(r_pred_2)[which(r_pred_2[i, ] == max(r_pred_2[i, ]))])
  }
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(covtype2, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$link_3, r_pred[, 1])
  expect_equal_nn(z_pred$outputs$link_6, r_pred[, 2])
  expect_equal_nn(z_pred$outputs$Probability_3, r_pred_2[, 1])
  expect_equal_nn(z_pred$outputs$Probability_6, r_pred_2[, 2])
  expect_equal_nn(z_pred$outputs$Predicted_y, pr)


  fit <- gbm(target ~ ., data = credit, n.trees = 3, interaction.depth = 4, distribution = "gaussian")
  p_fit <- pmml(fit)
  r_pred <- predict(fit, newdata = credit, n.trees = 3) - fit$initF
  r_pred_2 <- predict(fit, n.trees = 3, type = "response")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(credit, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_target, r_pred)
  expect_equal_nn(z_pred$outputs$GaussianPrediction, r_pred_2)


  fit <- gbm(target ~ ., data = credit_class, n.trees = 3, distribution = "multinomial", interaction.depth = 4)
  p_fit <- pmml(fit)
  r_pred <- predict(fit, newdata = credit_class, n.trees = 3)[, , 1]
  r_pred_2 <- predict(fit, n.trees = 3, type = "response")[, , 1]
  pr <- vector()
  for (i in 1:nrow(r_pred_2)) {
    pr[i] <- (colnames(r_pred_2)[which(r_pred_2[i, ] == max(r_pred_2[i, ]))])
  }
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(credit_class, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$link_neg, r_pred[, 1], tolerance = 1e-3)
  expect_equal_nn(z_pred$outputs$link_pos, r_pred[, 2], tolerance = 1e-3)
  expect_equal_nn(z_pred$outputs$Probability_neg, r_pred_2[, 1], tolerance = 1e-3)
  expect_equal_nn(z_pred$outputs$Probability_pos, r_pred_2[, 2], tolerance = 1e-3)
  expect_equal_nn(z_pred$outputs$Predicted_target, pr)


  set.seed(12)
  fit <- gbm(target ~ ., data = credit_class_01, n.trees = 3, interaction.depth = 4, distribution = "bernoulli")
  p_fit <- pmml(fit)
  r_pred <- predict(fit, newdata = credit_class_01, n.trees = 3)
  r_pred_2 <- predict(fit, n.trees = 3, type = "response")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(credit_class_01, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$BernoulliLinkPrediction, r_pred, tolerance = 1e-5)
  expect_equal_nn(z_pred$outputs$BernoulliResponsePrediction, r_pred_2, tolerance = 1e-5)


  fit <- gbm(Species ~ ., data = iris, n.trees = 2, interaction.depth = 3, distribution = "multinomial")
  p_fit <- pmml(fit)
  r_pred <- predict(fit, newdata = iris[1:60, ], n.trees = 2)[1:60, , ]
  r_pred_2 <- predict(fit, newdata = iris[1:60, ], n.trees = 2, type = "response")[1:60, , ]
  pr <- vector()
  for (i in 1:nrow(r_pred_2)) {
    pr[i] <- (colnames(r_pred_2)[which(r_pred_2[i, ] == max(r_pred_2[i, ]))])
  }
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris[1:60, ], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$link_setosa, r_pred[, 1])
  expect_equal_nn(z_pred$outputs$link_versicolor, r_pred[, 2])
  expect_equal_nn(z_pred$outputs$link_virginica, r_pred[, 3])
  expect_equal_nn(z_pred$outputs$Probability_setosa, r_pred_2[, 1])
  expect_equal_nn(z_pred$outputs$Probability_versicolor, r_pred_2[, 2])
  expect_equal_nn(z_pred$outputs$Probability_virginica, r_pred_2[, 3])
  expect_equal_nn(z_pred$outputs$Predicted_Species, pr)


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
  fit <- gbm(class ~ ddd1 + ddd2 + ddd3 + ddd4, data = box_obj$data, n.trees = 2, interaction.depth = 3, distribution = "multinomial")
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- predict(fit, newdata = box_obj$data, n.trees = 2)[1:4, , ]
  r_pred_2 <- predict(fit, newdata = box_obj$data, n.trees = 2, type = "response")[1:4, , ]
  pr <- vector()
  for (i in 1:nrow(r_pred_2)) {
    pr[i] <- (colnames(r_pred_2)[which(r_pred_2[i, ] == max(r_pred_2[i, ]))])
  }
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p[1:4, ], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$`link_Iris-setosa`, r_pred[, 1])
  expect_equal_nn(z_pred$outputs$`link_Iris-versicolor`, r_pred[, 2])
  expect_equal_nn(z_pred$outputs$`link_Iris-virginica`, r_pred[, 3])
  expect_equal_nn(z_pred$outputs$`Probability_Iris-setosa`, r_pred_2[, 1])
  expect_equal_nn(z_pred$outputs$`Probability_Iris-versicolor`, r_pred_2[, 2])
  expect_equal_nn(z_pred$outputs$`Probability_Iris-virginica`, r_pred_2[, 3])
  expect_equal_nn(z_pred$outputs$Predicted_class, pr)
})


test_that("MiningModel/randomForest PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

  audit_nor_logical[, "Sex"] <- as.factor(audit_nor_logical[, "Sex"])
  suppressWarnings(fit <- randomForest(Adjusted ~ ., audit_nor_logical[, -1], ntree = 8))
  p_fit <- pmml(fit)
  r_pred <- predict(fit, newdata = audit_nor_logical)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_nor_logical, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred)


  suppressWarnings(fit <- randomForest(Adjusted ~ ., audit_nor, ntree = 4))
  p_fit <- pmml(fit)
  r_pred <- predict(fit, newdata = audit_nor)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_nor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred)


  suppressWarnings(fit <- randomForest(Adjusted ~ ., audit_nor_fake_logical, ntree = 5))
  p_fit <- pmml(fit)
  r_pred <- predict(fit, newdata = audit_nor_fake_logical)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_nor_fake_logical, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred)


  suppressWarnings(fit <- randomForest(predictedClass ~ ., random_data_small, ntree = 7))
  p_fit <- pmml(fit)
  r_pred <- predict(fit, newdata = random_data_small)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(random_data_small, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_predictedClass, r_pred)


  suppressWarnings(fit <- randomForest(temp ~ ., elnino, ntree = 6))
  p_fit <- pmml(fit)
  r_pred <- predict(fit, newdata = elnino)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(elnino, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_temp, r_pred)


  fit <- randomForest(SEPAL_LE ~ ., data = iris_nor, ntree = 9)
  p_fit <- pmml(fit)
  r_pred <- predict(fit, newdata = iris_nor)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_nor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_SEPAL_LE, r_pred)


  iris_nor_logical[, 5] <- as.factor(iris_nor_logical[, 5])
  fit <- randomForest(SEPAL_LE ~ ., iris_nor_logical, ntree = 7)
  p_fit <- pmml(fit)
  r_pred <- predict(fit, newdata = iris_nor_logical)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_nor_logical, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_SEPAL_LE, r_pred)


  box_obj <- xform_wrap(iris)
  box_obj <- xform_z_score(box_obj, xform_info = "column2->d2")
  box_obj <- xform_z_score(box_obj, xform_info = "column3->d3")
  box_obj <- xform_z_score(box_obj, xform_info = "column4->d4")
  box_obj <- xform_min_max(box_obj, xform_info = "d2->dd2")
  box_obj <- xform_min_max(box_obj, xform_info = "d3->dd3")
  box_obj <- xform_min_max(box_obj, xform_info = "d4->dd4")
  box_obj <- xform_z_score(box_obj, xform_info = "dd2->ddd2")
  box_obj <- xform_z_score(box_obj, xform_info = "dd3->ddd3")
  box_obj <- xform_z_score(box_obj, xform_info = "dd4->ddd4")
  set.seed(123)
  fit <- randomForest(Species ~ Petal.Length + ddd2 + ddd3 + ddd4, box_obj$data, ntree = 7)
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- as.character(predict(fit, newdata = box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Species, r_pred)


  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_z_score(box_obj, xform_info = "column2->d2")
  box_obj <- xform_z_score(box_obj, xform_info = "column3->d3")
  box_obj <- xform_z_score(box_obj, xform_info = "column4->d4")
  box_obj <- xform_min_max(box_obj, xform_info = "d2->dd2")
  box_obj <- xform_min_max(box_obj, xform_info = "d3->dd3")
  box_obj <- xform_min_max(box_obj, xform_info = "d4->dd4")
  box_obj <- xform_z_score(box_obj, xform_info = "dd2->ddd2")
  box_obj <- xform_z_score(box_obj, xform_info = "dd3->ddd3")
  box_obj <- xform_z_score(box_obj, xform_info = "dd4->ddd4")
  box_obj <- xform_discretize(box_obj,
    xform_info = "[sepal_width->dis_sw][double->boolean]",
    table = "iris_discretize_bool_sw.csv", map_missing_to = "0", default_value = "1"
  )
  fit <- randomForest(class ~ petal_length + ddd2 + ddd3 + dis_sw, box_obj$data, ntree = 7)
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- as.character(predict(fit, newdata = box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_class, r_pred)


  box_obj <- xform_wrap(audit_factor)
  box_obj <- xform_z_score(box_obj, "column2->d_Age")
  box_obj <- xform_z_score(box_obj, "column7->d_Income")
  box_obj <- xform_z_score(box_obj, "column9->d_Deductions")
  box_obj <- xform_z_score(box_obj, "column10->d_Hours")
  box_obj <- xform_min_max(box_obj, "d_Age->dd_Age")
  box_obj <- xform_min_max(box_obj, "d_Income->dd_Income")
  box_obj <- xform_min_max(box_obj, "d_Deductions->dd_Deductions")
  box_obj <- xform_min_max(box_obj, "d_Hours->dd_Hours")
  box_obj <- xform_z_score(box_obj, "dd_Age->ddd_Age")
  box_obj <- xform_z_score(box_obj, "dd_Income->ddd_Income")
  box_obj <- xform_z_score(box_obj, "dd_Deductions->ddd_Deductions")
  box_obj <- xform_z_score(box_obj, "dd_Hours->ddd_Hours")
  set.seed(14)
  fit <- randomForest(Adjusted ~ ddd_Age + Employment + Education + Marital +
    Occupation + ddd_Income + Sex + ddd_Deductions + ddd_Hours,
  box_obj$data,
  ntree = 7
  )
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- as.character(predict(fit, newdata = box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_factor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred)


  box_obj <- xform_wrap(audit_factor[, c("Age", "Marital", "Sex", "Adjusted")])
  box_obj <- xform_norm_discrete(box_obj, xform_info = "Marital")
  box_obj <- xform_norm_discrete(box_obj, xform_info = "Sex")
  audit_box_features <- box_obj$data[!names(box_obj$data) %in% c("Marital", "Sex")]
  fit <- randomForest(Adjusted ~ ., audit_box_features, ntree = 3)
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- as.character(predict(fit, newdata = box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_factor[, c("Age", "Marital", "Sex", "Adjusted")], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred)


  box_obj <- xform_wrap(audit_factor[, c(
    "Age", "Employment", "Income",
    "Deductions", "Marital", "Sex", "Adjusted"
  )])
  box_obj <- xform_norm_discrete(box_obj, xform_info = "Marital")
  box_obj <- xform_norm_discrete(box_obj, xform_info = "Sex")
  box_obj <- xform_function(box_obj,
    orig_field_name = "Age",
    new_field_name = "Age.log",
    expression = "log(Age)"
  )
  box_obj <- xform_function(box_obj,
    orig_field_name = "Income,Age,Deductions",
    new_field_name = "Inc.Ded.Age",
    expression = "(Income-Deductions)/Age"
  )
  audit_box_features <- box_obj$data[!names(box_obj$data) %in% c("Marital", "Sex")]
  fit <- randomForest(Adjusted ~ ., audit_box_features, ntree = 3)
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- as.character(predict(fit, newdata = box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_factor[, c(
    "Age", "Employment", "Income",
    "Deductions", "Marital", "Sex", "Adjusted"
  )], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred)


  box_obj <- xform_wrap(iris)
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
  set.seed(335)
  fit <- randomForest(Species ~ ddd1 + ddd2 + ddd3 + ddd4, box_obj$data[1:120, ], ntree = 5)
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred_class <- as.character(predict(fit, newdata = box_obj$data[121:150, ]))
  r_pred_prob <- unname(predict(fit, newdata = box_obj$data[121:150, ], type = "prob"))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris[121:150, ], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Species, r_pred_class)
  expect_equal_nn(z_pred$outputs$Probability_setosa, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$Probability_versicolor, r_pred_prob[, 2])
  expect_equal_nn(z_pred$outputs$Probability_virginica, r_pred_prob[, 3])
})


test_that("MiningModel/xgboost PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

  invisible(capture.output(fit <- xgboost(
    data = as.matrix(iris[, 1:4]), label = as.numeric(iris[, 5]) - 1,
    max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "multi:softprob", num_class = 3,
    save_name = xgb_tmp_01_save
  )))
  xgb.dump(fit, xgb_tmp_01_dump)

  p_fit <- pmml(
    model = fit, input_feature_names = colnames(iris[, 1:4]), output_label_name = "Species",
    output_categories = c(1, 2, 3), xgb_dump_file = xgb_tmp_01_dump
  )
  r_pred_prob <- as.data.frame(matrix(predict(fit, newdata = as.matrix(iris[, 1:4])),
                                      nrow = 150, byrow = T), row.names = F, stringsAsFactors = TRUE)
  r_pred_class <- sapply(1:150, function(i) {
    which(r_pred_prob[i, ] == max(r_pred_prob[i, ]))
  })
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris[, 1:4], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred_prob$V1, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Probability_2, r_pred_prob$V2, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Probability_3, r_pred_prob$V3, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Predicted_Species, as.character(r_pred_class), tolerance = 1e-7)


  invisible(capture.output(fit <- xgboost(
    data = as.matrix(audit_factor[, c(2, 7, 9, 10, 12)]),
    label = as.numeric(audit_factor[, 13]) - 1, max_depth = 2, nrounds = 2,
    objective = "binary:logistic", save_name = xgb_tmp_01_save
  )))
  xgb.dump(fit, xgb_tmp_01_dump)
  p_fit <- pmml(fit,
    input_feature_names = colnames(audit_factor[, c(2, 7, 9, 10, 12)]), output_label_name = "Adjusted",
    output_categories = c(0, 1), xgb_dump_file = xgb_tmp_01_dump
  )

  r_pred <- predict(fit, as.matrix(audit_factor[, c(2, 7, 9, 10, 12)]))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_factor[, c(2, 7, 9, 10, 12)], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_0, r_pred, tolerance = 1e-7)


  sparse_mat <- as.matrix(sparse.model.matrix(Adjusted ~ . - 1, data = audit[, c("Marital", "Sex", "Adjusted")]))
  invisible(capture.output(fit <- xgboost(
    data = sparse_mat, label = audit[, c("Adjusted")], max_depth = 2,
    eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic",
    save_name = xgb_tmp_01_save
  )))
  xgb.dump(fit, xgb_tmp_01_dump)
  p_fit <- pmml(fit,
    input_feature_names = colnames(sparse_mat), output_label_name = "Adjusted",
    output_categories = c(0, 1), xgb_dump_file = xgb_tmp_01_dump
  )
  r_pred <- predict(fit, sparse_mat)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(as.data.frame(sparse_mat, stringsAsFactors = TRUE), up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_0, r_pred, tolerance = 1e-7)


  # The next 5 tests check that the naming convention where field name strings are
  # subsets of each other does not cause issues. E.g., V1 is a subset of V11 and V112.
  iris_string_subsets <- iris[1:100, ]
  iris_string_subsets[, 5] <- as.factor(as.character(iris_string_subsets[, 5]))
  colnames(iris_string_subsets) <- c("V11", "V112", "V128", "V22", "V1")
  invisible(capture.output(fit <- xgboost(
    data = as.matrix(iris_string_subsets[, 1:4]),
    label = as.numeric(iris_string_subsets[, 5]) - 1,
    max_depth = 3, eta = 1, nthread = 1, nrounds = 3, objective = "binary:logistic",
    save_name = xgb_tmp_01_save
  )))
  xgb.dump(fit, xgb_tmp_01_dump)
  p_fit <- pmml(fit,
    input_feature_names = colnames(as.matrix(iris_string_subsets[, 1:4])),
    output_label_name = "V1",
    output_categories = c(1, 2),
    xgb_dump_file = xgb_tmp_01_dump
  )


  r_pred <- as.data.frame(matrix(predict(fit, as.matrix(iris_string_subsets[, 1:4])), nrow = 100, byrow = T),
                          row.names = F, stringsAsFactors = TRUE)
  r_pred_prob <- cbind(r_pred, 1 - r_pred)
  r_pred_class <- sapply(1:100, function(i) {
    if (r_pred_prob[i, 1] > 0.5) {
      1
    } else {
      2
    }
  })
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_string_subsets, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred$V1, tolerance = 1e-7)


  iris_string_subsets <- iris
  colnames(iris_string_subsets) <- c("V11", "V112", "V128", "V1281", "V1")
  invisible(capture.output(fit <- xgboost(
    data = as.matrix(iris_string_subsets[, 1:4]), label = as.numeric(iris_string_subsets[, 5]) - 1,
    max_depth = 4, eta = 1, nthread = 1, nrounds = 3, num_class = 3,
    objective = "multi:softprob",
    save_name = xgb_tmp_01_save
  )))
  xgb.dump(fit, xgb_tmp_01_dump)
  p_fit <- pmml(fit,
    input_feature_names = colnames(as.matrix(iris_string_subsets[, 1:4])), output_label_name = "V1",
    output_categories = c(1, 2, 3), xgb_dump_file = xgb_tmp_01_dump
  )
  r_pred_prob <- as.data.frame(matrix(predict(fit, as.matrix(iris_string_subsets[, 1:4])),
                                      nrow = 150, byrow = T), row.names = F, stringsAsFactors = TRUE)
  r_pred_class <- sapply(1:150, function(i) {
    which(r_pred_prob[i, ] == max(r_pred_prob[i, ]))
  })
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_string_subsets, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred_prob$V1, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Probability_2, r_pred_prob$V2, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Probability_3, r_pred_prob$V3, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Predicted_V1, as.character(r_pred_class), tolerance = 1e-7)

  # Use larger number of trees (nrounds) so that some are created with no branches.
  invisible(capture.output(fit <- xgboost(
    data = as.matrix(iris_string_subsets[, 1:4]), label = as.numeric(iris_string_subsets[, 5]) - 1,
    max_depth = 4, eta = 1, nthread = 1, nrounds = 18, num_class = 3,
    objective = "multi:softprob",
    save_name = xgb_tmp_01_save
  )))
  xgb.dump(fit, xgb_tmp_01_dump)
  p_fit <- pmml(fit,
    input_feature_names = colnames(as.matrix(iris_string_subsets[, 1:4])), output_label_name = "V1",
    output_categories = c(1, 2, 3), xgb_dump_file = xgb_tmp_01_dump
  )
  r_pred_prob <- as.data.frame(matrix(predict(fit, as.matrix(iris_string_subsets[, 1:4])),
                                      nrow = 150, byrow = T), row.names = F, stringsAsFactors = TRUE)
  r_pred_class <- sapply(1:150, function(i) {
    which(r_pred_prob[i, ] == max(r_pred_prob[i, ]))
  })
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_string_subsets, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred_prob$V1, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Probability_2, r_pred_prob$V2, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Probability_3, r_pred_prob$V3, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Predicted_V1, as.character(r_pred_class), tolerance = 1e-7)


  # Multinomial model with one tree each
  invisible(capture.output(fit <- xgboost(
    data = as.matrix(iris_string_subsets[, 1:4]), label = as.numeric(iris_string_subsets[, 5]) - 1,
    max_depth = 4, eta = 1, nthread = 1, nrounds = 1, num_class = 3,
    objective = "multi:softprob",
    save_name = xgb_tmp_01_save
  )))
  xgb.dump(fit, xgb_tmp_01_dump)
  p_fit <- pmml(fit,
    input_feature_names = colnames(as.matrix(iris_string_subsets[, 1:4])), output_label_name = "V1",
    output_categories = c(1, 2, 3), xgb_dump_file = xgb_tmp_01_dump
  )

  r_pred_prob <- as.data.frame(matrix(predict(fit, as.matrix(iris_string_subsets[, 1:4])),
                                      nrow = 150, byrow = T), row.names = F, stringsAsFactors = TRUE)
  r_pred_class <- sapply(1:150, function(i) {
    which(r_pred_prob[i, ] == max(r_pred_prob[i, ]))
  })
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_string_subsets, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred_prob$V1, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Probability_2, r_pred_prob$V2, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Probability_3, r_pred_prob$V3, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Predicted_V1, as.character(r_pred_class), tolerance = 1e-7)


  iris_matrix <- as.matrix(iris[, 1:4])
  invisible(capture.output(fit <- xgboost(
    data = iris_matrix, label = as.numeric(iris[, 5]) - 1,
    max_depth = 4, eta = 1, nthread = 1, nrounds = 2, num_class = 3,
    objective = "multi:softmax", save_name = xgb_tmp_01_save
  )))
  xgb.dump(fit, xgb_tmp_01_dump)
  p_fit <- pmml(fit,
    input_feature_names = colnames(iris_matrix), output_label_name = "Species",
    output_categories = c(0, 1, 2),
    xgb_dump_file = xgb_tmp_01_dump
  )
  r_pred <- predict(fit, iris_matrix)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris[, 1:4], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Species, as.character(r_pred))


  box_obj <- xform_wrap(audit_factor[, c("Marital", "Sex", "Adjusted")])
  box_obj <- xform_norm_discrete(box_obj, xform_info = "Marital")
  box_obj <- xform_norm_discrete(box_obj, xform_info = "Sex")
  output_vector <- as.numeric(audit_factor$Adjusted) - 1
  audit_box_filt <- box_obj$data[!names(box_obj$data) %in% c("Marital", "Sex", "Adjusted")]
  set.seed(234)
  invisible(capture.output(fit <- xgboost(
    data = as.matrix(audit_box_filt),
    label = output_vector, max_depth = 2,
    eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic",
    save_name = xgb_tmp_01_save
  )))
  xgb.dump(fit, xgb_tmp_01_dump)
  p_fit <- pmml(fit,
    input_feature_names = colnames(audit_box_filt),
    output_label_name = "Adjusted",
    output_categories = c(0, 1),
    xgb_dump_file = xgb_tmp_01_dump,
    transform = box_obj
  )
  r_pred <- predict(fit, as.matrix(audit_box_filt))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_factor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_0, r_pred, tolerance = 1e-7)


  box_obj <- xform_wrap(audit_factor[, c("Marital", "Sex", "Adjusted")])
  box_obj <- xform_norm_discrete(box_obj, xform_info = "Marital", levelSeparator = "_")
  box_obj <- xform_norm_discrete(box_obj, xform_info = "Sex", levelSeparator = "_")
  output_vector <- as.numeric(audit_factor$Adjusted) - 1
  audit_box_filt <- box_obj$data[!names(box_obj$data) %in% c("Marital", "Sex", "Adjusted")]
  set.seed(234)
  invisible(capture.output(fit <- xgboost(
    data = as.matrix(audit_box_filt),
    label = output_vector, max_depth = 2,
    eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic",
    save_name = xgb_tmp_01_save
  )))
  xgb.dump(fit, xgb_tmp_01_dump)
  p_fit <- pmml(fit,
    input_feature_names = colnames(audit_box_filt),
    output_label_name = "Adjusted",
    output_categories = c(0, 1),
    xgb_dump_file = xgb_tmp_01_dump,
    transform = box_obj
  )
  r_pred <- predict(fit, as.matrix(audit_box_filt))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_factor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_0, r_pred, tolerance = 1e-7)
})


test_that("NaiveBayesModel/e1071 PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

  fit <- naiveBayes(as.factor(Adjusted) ~ Employment + Education + Marital + Occupation + Sex, data = audit_nor)
  p_fit <- pmml(fit, predicted_field = "Adjusted")
  r_pred_class <- predict(fit, newdata = audit_nor)
  r_pred_prob <- predict(fit, newdata = audit_nor, type = "raw")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_nor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, as.character(r_pred_class))
  expect_equal_nn(z_pred$outputs$Probability_0, r_pred_prob[, 1])


  fit <- naiveBayes(BANKCARD ~ GENDER + MARITAL_STATUS + PROFESSION + SAVINGS_ACCOUNT + ONLINE_ACCESS + JOINED_ACCOUNTS,
    data = bank
  )
  p_fit <- pmml(fit, predicted_field = "BANKCARD")
  r_pred_class <- predict(fit, newdata = bank)
  r_pred_prob <- predict(fit, newdata = bank, type = "raw")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(bank, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_BANKCARD, as.character(r_pred_class))
  expect_equal_nn(z_pred$outputs$Probability_NO, r_pred_prob[, 1])


  fit <- naiveBayes(CLASS ~ ., data = iris_nor)
  p_fit <- pmml(fit, predicted_field = "CLASS")
  r_pred_class <- predict(fit, newdata = iris_nor)
  r_pred_prob <- predict(fit, newdata = iris_nor, type = "raw")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_nor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_CLASS, as.character(r_pred_class))
  expect_equal_nn(z_pred$outputs$`Probability_Iris-setosa`, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$`Probability_Iris-virgin`, r_pred_prob[, 3], tolerance = 1e-4)


  fit <- naiveBayes(Marital ~ ., data = audit[, c(2:8, 10)])
  p_fit <- pmml(fit, predicted_field = "Marital")
  r_pred_class <- predict(fit, newdata = audit[, c(2:8, 10)])
  r_pred_prob <- predict(fit, newdata = audit[, c(2:8, 10)], type = "raw")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit[, c(2:8, 10)], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Marital, as.character(r_pred_class))
  expect_equal_nn(z_pred$outputs$Probability_Absent, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$Probability_Divorced, r_pred_prob[, 2])
  expect_equal_nn(z_pred$outputs$Probability_Married, r_pred_prob[, 3])


  fit <- naiveBayes(Marital ~ ., data = audit_r_build_in[, c(2:8, 10)])
  p_fit <- pmml(fit, predicted_field = "Marital")
  r_pred_class <- predict(fit, newdata = audit_r_build_in[, c(2:8, 10)])
  r_pred_prob <- predict(fit, newdata = audit_r_build_in[, c(2:8, 10)], type = "raw")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_r_build_in[, c(2:8, 10)], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Marital, as.character(r_pred_class))
  expect_equal_nn(z_pred$outputs$Probability_Absent, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$Probability_Divorced, r_pred_prob[, 2])
  expect_equal_nn(z_pred$outputs$Probability_Married, r_pred_prob[, 3])


  fit <- naiveBayes(as.factor(amount_of_claims) ~ gender + domicile, data = insurance)
  p_fit <- pmml(fit, predicted_field = "amount_of_claims")
  r_pred_class <- predict(fit, newdata = insurance)
  r_pred_prob <- predict(fit, newdata = insurance, type = "raw")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(insurance, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_amount_of_claims, as.character(r_pred_class))
  expect_equal_nn(z_pred$outputs$Probability_100, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$Probability_500, r_pred_prob[, 2])
  expect_equal_nn(z_pred$outputs$Probability_1000, r_pred_prob[, 3])


  fit <- naiveBayes(as.factor(amount_of_claims) ~ gender + domicile + no_of_claims, data = insurance)
  p_fit <- pmml(fit, predicted_field = "amount_of_claims")
  r_pred_class <- predict(fit, newdata = insurance)
  r_pred_prob <- predict(fit, newdata = insurance, type = "raw")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(insurance, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_amount_of_claims, as.character(r_pred_class))
  expect_equal_nn(z_pred$outputs$Probability_100, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$Probability_500, r_pred_prob[, 2])
  expect_equal_nn(z_pred$outputs$Probability_1000, r_pred_prob[, 3])


  fit <- naiveBayes(class ~ ., data = iris_bin)
  p_fit <- pmml(fit, predicted_field = "class")
  r_pred_class <- predict(fit, newdata = iris_bin)
  r_pred_prob <- predict(fit, newdata = iris_bin, type = "raw")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_bin, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_class, as.character(r_pred_class))
  expect_equal_nn(z_pred$outputs$`Probability_Iris-setosa`, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$`Probability_Iris-versicolor`, r_pred_prob[, 2])
  expect_equal_nn(z_pred$outputs$`Probability_Iris-virginica`, r_pred_prob[, 3])


  fit <- naiveBayes(target ~ ., data = credit_class)
  p_fit <- pmml(fit, predicted_field = "target")
  r_pred_class <- predict(fit, newdata = credit_class)
  r_pred_prob <- predict(fit, newdata = credit_class, type = "raw")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(credit_class, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_target, as.character(r_pred_class))
  expect_equal_nn(z_pred$outputs$Probability_neg, r_pred_prob[, 1], tolerance = 1e-3)
  expect_equal_nn(z_pred$outputs$Probability_pos, r_pred_prob[, 2], tolerance = 1e-3)


  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_length->dis_pl][double->integer]",
    table = "iris_discretize_pl.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_width->dis_pw][double->integer]",
    table = "iris_discretize_pw.csv", map_missing_to = "0", default_value = "1"
  )
  fit <- naiveBayes(class ~ dis_pl + dis_pw + sepal_length + sepal_width, data = box_obj$data)
  p_fit <- pmml(fit, predicted_field = "class", transforms = box_obj)
  r_pred_class <- predict(fit, newdata = box_obj$data)
  r_pred_prob <- predict(fit, newdata = box_obj$data, type = "raw")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_class, as.character(r_pred_class))
  expect_equal_nn(z_pred$outputs$`Probability_Iris-setosa`, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$`Probability_Iris-versicolor`, r_pred_prob[, 2])
  expect_equal_nn(z_pred$outputs$`Probability_Iris-virginica`, r_pred_prob[, 3])


  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_length->dis_pl][double->string]",
    table = "iris_discretize_pl.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_width->dis_pw][double->string]",
    table = "iris_discretize_pw.csv", map_missing_to = "0", default_value = "1"
  )

  fit <- naiveBayes(class ~ dis_pl + dis_pw + sepal_length + sepal_width, data = box_obj$data)
  p_fit <- pmml(fit, predicted_field = "class", transforms = box_obj)
  r_pred_class <- predict(fit, newdata = box_obj$data)
  r_pred_prob <- predict(fit, newdata = box_obj$data, type = "raw")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_class, as.character(r_pred_class))
  expect_equal_nn(z_pred$outputs$`Probability_Iris-setosa`, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$`Probability_Iris-versicolor`, r_pred_prob[, 2])
  expect_equal_nn(z_pred$outputs$`Probability_Iris-virginica`, r_pred_prob[, 3])


  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_length->dis_pl][double->string]",
    table = "iris_discretize_pl.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_width->dis_pw][double->string]",
    table = "iris_discretize_pw.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[sepal_length->dis_sl][double->string]",
    table = "iris_discretize_sl.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[sepal_width->dis_sw][double->string]",
    table = "iris_discretize_sw.csv", map_missing_to = "0", default_value = "1"
  )

  fit <- naiveBayes(class ~ dis_pl + dis_pw + dis_sl + dis_sw, data = box_obj$data)
  p_fit <- pmml(fit, predicted_field = "class", transforms = box_obj)
  r_pred_class <- predict(fit, newdata = box_obj$data)
  r_pred_prob <- predict(fit, newdata = box_obj$data, type = "raw")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_class, as.character(r_pred_class))
  expect_equal_nn(z_pred$outputs$`Probability_Iris-setosa`, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$`Probability_Iris-versicolor`, r_pred_prob[, 2])
  expect_equal_nn(z_pred$outputs$`Probability_Iris-virginica`, r_pred_prob[, 3])


  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_length->dis_pl][double->string]",
    table = "iris_discretize_pl.csv"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_width->dis_pw][double->string]",
    table = "iris_discretize_pw.csv"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[sepal_length->dis_sl][double->string]",
    table = "iris_discretize_sl.csv"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[sepal_width->dis_sw][double->string]",
    table = "iris_discretize_sw.csv"
  )

  fit <- naiveBayes(class ~ dis_pl + dis_pw + dis_sl + dis_sw, data = box_obj$data)
  p_fit <- pmml(fit, predicted_field = "class", transforms = box_obj)
  r_pred_class <- predict(fit, newdata = box_obj$data)
  r_pred_prob <- predict(fit, newdata = box_obj$data, type = "raw")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_class, as.character(r_pred_class))
  expect_equal_nn(z_pred$outputs$`Probability_Iris-setosa`, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$`Probability_Iris-versicolor`, r_pred_prob[, 2])
  expect_equal_nn(z_pred$outputs$`Probability_Iris-virginica`, r_pred_prob[, 3])


  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_length->dis_pl][double->string]",
    table = "iris_discretize_pl.csv"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_width->dis_pw][double->string]",
    table = "iris_discretize_pw.csv"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[sepal_length->dis_sl][double->string]",
    table = "iris_discretize_sl.csv"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[sepal_width->dis_sw][double->string]",
    table = "iris_discretize_sw.csv"
  )
  fit <- naiveBayes(class ~ dis_pl + dis_pw + dis_sl + dis_sw, data = box_obj$data)

  p_fit <- pmml(fit, predicted_field = "class", transforms = box_obj)
  r_pred_class <- predict(fit, newdata = box_obj$data)
  r_pred_prob <- predict(fit, newdata = box_obj$data, type = "raw")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_class, as.character(r_pred_class))
  expect_equal_nn(z_pred$outputs$`Probability_Iris-setosa`, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$`Probability_Iris-versicolor`, r_pred_prob[, 2])
  expect_equal_nn(z_pred$outputs$`Probability_Iris-virginica`, r_pred_prob[, 3])
})


test_that("NearestNeighborModel/neighbr PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

  iris_train <- iris[1:140, ]
  iris_test <- iris[141:150, -c(4, 5)]
  fit <- knn(
    train_set = iris_train, test_set = iris_test, k = 3, categorical_target = "Species",
    continuous_target = "Petal.Width", comparison_measure = "euclidean"
  )
  p_fit <- pmml(fit)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_test, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Species, fit$test_set_scores$categorical_target)
  expect_equal_nn(z_pred$outputs$Predicted_Petal.Width, fit$test_set_scores$continuous_target)


  iris_with_id <- iris
  iris_with_id$ID <- c(1:150)
  iris_train <- iris_with_id[1:130, -c(4, 5)]
  iris_test <- iris_with_id[132:150, -c(4, 5, 6)]
  fit <- knn(
    train_set = iris_train, test_set = iris_test, k = 5, comparison_measure = "euclidean",
    return_ranked_neighbors = 3, id = "ID"
  )
  p_fit <- pmml(fit)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_test, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$neighbor1, as.character(fit$test_set_scores$neighbor1))
  expect_equal_nn(z_pred$outputs$neighbor2, as.character(fit$test_set_scores$neighbor2))
  expect_equal_nn(z_pred$outputs$neighbor3, as.character(fit$test_set_scores$neighbor3))


  iris_train <- iris_with_id[1:130, ]
  iris_test <- iris_with_id[132:150, -c(5, 6)]
  fit <- knn(
    train_set = iris_train, test_set = iris_test, k = 5, categorical_target = "Species",
    comparison_measure = "squared_euclidean", return_ranked_neighbors = 4, id = "ID"
  )
  p_fit <- pmml(fit)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_test, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Species, fit$test_set_scores$categorical_target)
  expect_equal_nn(z_pred$outputs$neighbor1, as.character(fit$test_set_scores$neighbor1))
  expect_equal_nn(z_pred$outputs$neighbor2, as.character(fit$test_set_scores$neighbor2))
  expect_equal_nn(z_pred$outputs$neighbor3, as.character(fit$test_set_scores$neighbor3))


  house_votes_nbr <- house_votes
  feature_names <- names(house_votes_nbr)[!names(house_votes_nbr) %in% c("Class", "ID")]
  for (n in feature_names) {
    levels(house_votes_nbr[, n])[levels(house_votes_nbr[, n]) == "n"] <- 0
    levels(house_votes_nbr[, n])[levels(house_votes_nbr[, n]) == "y"] <- 1
  }
  for (n in feature_names) {
    house_votes_nbr[, n] <- as.numeric(levels(house_votes_nbr[, n]))[house_votes_nbr[, n]]
  }
  house_votes_nbr$ID <- c(1:nrow(house_votes_nbr))


  house_votes_train <- house_votes_nbr[1:100, ]
  house_votes_test <- house_votes_nbr[212:232, !names(house_votes_nbr) %in% c("Class", "ID")]
  fit <- knn(
    train_set = house_votes_train, test_set = house_votes_test, k = 7, categorical_target = "Class",
    comparison_measure = "jaccard", return_ranked_neighbors = 3, id = "ID"
  )
  p_fit <- pmml(fit)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(house_votes_test, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Class, fit$test_set_scores$categorical_target)
  expect_equal_nn(z_pred$outputs$neighbor1, as.character(fit$test_set_scores$neighbor1))
  expect_equal_nn(z_pred$outputs$neighbor2, as.character(fit$test_set_scores$neighbor2))
  expect_equal_nn(z_pred$outputs$neighbor3, as.character(fit$test_set_scores$neighbor3))


  house_votes_train <- house_votes_nbr[1:30, ]
  house_votes_test <- house_votes_nbr[105:232, !names(house_votes_nbr) %in% c("Class", "ID")]
  fit <- knn(
    train_set = house_votes_train, test_set = house_votes_test, k = 4, categorical_target = "Class",
    comparison_measure = "simple_matching", return_ranked_neighbors = 4, id = "ID"
  )
  p_fit <- pmml(fit)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(house_votes_test, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Class, fit$test_set_scores$categorical_target)
  expect_equal_nn(z_pred$outputs$neighbor1, as.character(fit$test_set_scores$neighbor1))
  expect_equal_nn(z_pred$outputs$neighbor2, as.character(fit$test_set_scores$neighbor2))
  expect_equal_nn(z_pred$outputs$neighbor3, as.character(fit$test_set_scores$neighbor3))
  expect_equal_nn(z_pred$outputs$neighbor4, as.character(fit$test_set_scores$neighbor4))


  house_votes_train <- house_votes_nbr[2:90, !names(house_votes_nbr) %in% c("Class")]
  house_votes_test <- house_votes_nbr[195:232, !names(house_votes_nbr) %in% c("Class", "ID")]
  fit <- knn(
    train_set = house_votes_train, test_set = house_votes_test, k = 4, comparison_measure = "tanimoto",
    return_ranked_neighbors = 4, id = "ID"
  )
  p_fit <- pmml(fit)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(house_votes_test, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$neighbor1, as.character(fit$test_set_scores$neighbor1))
  expect_equal_nn(z_pred$outputs$neighbor2, as.character(fit$test_set_scores$neighbor2))
  expect_equal_nn(z_pred$outputs$neighbor3, as.character(fit$test_set_scores$neighbor3))
  expect_equal_nn(z_pred$outputs$neighbor4, as.character(fit$test_set_scores$neighbor4))
})


test_that("NeuralNetwork/nnet PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

  audit_nor_factor <- audit_nor
  audit_nor_factor[, 13] <- as.factor(audit_nor[, 13])
  invisible(capture.output(fit <- nnet(Marital ~ ., data = audit_nor_factor[, c(2, 5, 7, 8, 10, 13)], size = 4)))
  p_fit <- pmml(fit)
  r_pred_class <- predict(fit, audit_nor_factor[, c(2, 5, 7, 8, 10, 13)], type = "class")
  r_pred_prob <- predict(fit, audit_nor_factor[, c(2, 5, 7, 8, 10, 13)], type = "raw")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_nor_factor[, c(2, 5, 7, 8, 10, 13)], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Marital, r_pred_class)
  expect_equal_nn(z_pred$outputs$Probability_Absent, unname(r_pred_prob[, 1]), tolerance = 1e-3)
  expect_equal_nn(z_pred$outputs$Probability_Divorced, unname(r_pred_prob[, 2]), tolerance = 1e-3)
  expect_equal_nn(z_pred$outputs$Probability_Married, unname(r_pred_prob[, 3]), tolerance = 1e-3)
  expect_equal_nn(z_pred$outputs$`Probability_Married-spouse-absent`, unname(r_pred_prob[, 4]), tolerance = 1e-3)
  expect_equal_nn(z_pred$outputs$Probability_Unmarried, unname(r_pred_prob[, 5]), tolerance = 1e-3)
  expect_equal_nn(z_pred$outputs$Probability_Widowed, unname(r_pred_prob[, 6]), tolerance = 1e-3)


  invisible(capture.output(fit <- nnet(CLASS ~ ., data = iris_nor, size = 4)))
  p_fit <- pmml(fit)
  r_pred_class <- predict(fit, iris_nor, type = "class")
  r_pred_prob <- unname(predict(fit, iris_nor, type = "raw"))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_nor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_CLASS, r_pred_class)
  expect_equal_nn(z_pred$outputs$`Probability_Iris-setosa`, r_pred_prob[, 1], tolerance = 1e-5)
  expect_equal_nn(z_pred$outputs$`Probability_Iris-versic`, r_pred_prob[, 2], tolerance = 1e-6)
  expect_equal_nn(z_pred$outputs$`Probability_Iris-virgin`, r_pred_prob[, 3], tolerance = 1e-5)


  invisible(capture.output(fit <- nnet(Adjusted ~ ., data = audit_nor, size = 4)))
  p_fit <- pmml(fit)
  r_pred <- unlist(as.list(predict(fit, audit_nor)))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_nor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred)


  box_obj <- xform_wrap(iris)
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
    xform_info = "[Species->d_Species][string->double]",
    table = "iris_class_table.csv", default_value = "-1", map_missing_to = "1"
  )
  invisible(capture.output(fit <- nnet(Species ~ ddd1 + ddd2 + ddd3 + ddd4, box_obj$data, size = 5)))
  p_fit <- pmml(fit, transform = box_obj)
  r_pred_class <- predict(fit, box_obj$data, type = "class")
  r_pred_prob <- unname(predict(fit, box_obj$data, type = "raw")[, 1:3])
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Species, r_pred_class)
  expect_equal_nn(z_pred$outputs$Probability_setosa, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$Probability_versicolor, r_pred_prob[, 2], tolerance = 1e-6)
  expect_equal_nn(z_pred$outputs$Probability_virginica, r_pred_prob[, 3], tolerance = 1e-5)


  box_obj <- xform_wrap(iris)
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
  invisible(capture.output(fit <- nnet(Species ~ ddd1 + ddd2 + ddd3 + ddd4, box_obj$data, size = 3)))
  p_fit <- pmml(fit, transform = box_obj)
  r_pred_class <- predict(fit, box_obj$data, type = "class")
  r_pred_prob <- unname(predict(fit, box_obj$data, type = "raw")[, 1:3])
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Species, r_pred_class)
  expect_equal_nn(z_pred$outputs$Probability_setosa, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$Probability_versicolor, r_pred_prob[, 2], tolerance = 1e-6)
  expect_equal_nn(z_pred$outputs$Probability_virginica, r_pred_prob[, 3], tolerance = 1e-6)
})


test_that("RegressionModel/nnet PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

  fit <- multinom(as.factor(Adjusted) ~ ., data = audit_nor, trace = F)
  p_fit <- pmml(fit)
  r_pred_class <- predict(fit, audit_nor)
  r_pred_prob <- unname(predict(fit, audit_nor, type = "probs"))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_nor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, as.character(r_pred_class))
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred_prob, tolerance = 1e-4)


  fit <- multinom(CLASS ~ ., data = iris_nor, trace = F)
  p_fit <- pmml(fit)
  r_pred_class <- predict(fit, iris_nor)
  r_pred_prob <- predict(fit, iris_nor, type = "probs")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_nor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_CLASS, as.character(r_pred_class))
  expect_equal_nn(z_pred$outputs$`Probability_Iris-setosa`, r_pred_prob[, 1], tolerance = 1e-4)
  expect_equal_nn(z_pred$outputs$`Probability_Iris-versic`, r_pred_prob[, 2], tolerance = 1e-4)


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
  fit <- multinom(class ~ ddd1 + ddd2 + ddd3 + ddd4, data = box_obj$data, trace = F)
  p_fit <- pmml(fit, transform = box_obj)
  r_pred_class <- predict(fit, data = box_obj$data)
  r_pred_prob <- predict(fit, box_obj$data, type = "probs")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_class, as.character(r_pred_class))
  expect_equal_nn(z_pred$outputs$`Probability_Iris-setosa`, r_pred_prob[, 1], tolerance = 1e-4)
  expect_equal_nn(z_pred$outputs$`Probability_Iris-versic`, r_pred_prob[, 2], tolerance = 1e-4)


  box_obj <- xform_wrap(iris)
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
    xform_info = "[Species->d_Species][string->double]",
    table = "iris_class_table.csv", default_value = "-1", map_missing_to = "1"
  )
  fit <- multinom(Species ~ ddd1 + ddd2 + ddd3 + ddd4, data = box_obj$data, trace = F)
  p_fit <- pmml(fit, transform = box_obj)
  r_pred_class <- predict(fit, data = box_obj$data)
  r_pred_prob <- predict(fit, box_obj$data, type = "probs")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Species, as.character(r_pred_class))
  expect_equal_nn(z_pred$outputs$Probability_setosa, r_pred_prob[, 1], tolerance = 1e-4)
  expect_equal_nn(z_pred$outputs$Probability_versicolor, r_pred_prob[, 2], tolerance = 1e-4)
})


test_that("RegressionModel/stats PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

  fit <- lm(Sepal.Length ~ ., data = iris)
  p_fit <- pmml(fit)
  r_pred <- predict(fit, iris)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Sepal.Length, r_pred)


  fit <- lm(temp ~ ., data = elnino)
  p_fit <- pmml(fit)
  r_pred <- predict(fit, elnino)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(elnino, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_temp, r_pred)


  box_obj <- xform_wrap(audit)
  box_obj <- xform_map(box_obj,
    xform_info = "[Employment,Education,Sex-> d_E]",
    table = "audit_3to1_table.csv", default_value = "X", map_missing_to = "Y"
  )
  fit <- lm(Adjusted ~ d_E + Income + Hours, data = box_obj$data)
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- predict(fit, box_obj$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred)


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
  box_obj <- xform_norm_discrete(box_obj, input_var = "class")
  box_obj <- xform_map(box_obj,
    xform_info = "[class->d_class][string->double]",
    table = "iris_p_class_table.csv", default_value = "-1", map_missing_to = "1"
  )
  fit <- lm(sepal_width ~ ddd1 + ddd2 + ddd3 + d_class + class_Iris_setosa +
    class_Iris_versicolor + class_Iris_virginica, box_obj$data)
  p_fit <- pmml(fit, transform = box_obj)
  r_pred <- predict(fit, data = box_obj$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_sepal_width, r_pred)


  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_length->dis_pl][double->string]",
    table = "iris_discretize_pl.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_width->dis_pw][double->integer]",
    table = "iris_discretize_pw.csv", map_missing_to = "0", default_value = "1"
  )

  fit <- lm(sepal_width ~ dis_pl + dis_pw + class, data = box_obj$data)
  p_fit <- pmml(fit, transform = box_obj)
  r_pred <- predict(fit, data = box_obj$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_sepal_width, r_pred)


  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_length->dis_pl][double->string]",
    table = "iris_discretize_pl.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_width->dis_pw][double->integer]",
    table = "iris_discretize_pw.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_norm_discrete(box_obj, input_var = "class")
  box_obj <- xform_map(box_obj,
    xform_info = "[class->d_class][string->double]",
    table = "iris_p_class_table.csv", default_value = "-1", map_missing_to = "1"
  )
  fit <- lm(sepal_width ~ dis_pl + dis_pw + d_class, data = box_obj$data)
  p_fit <- pmml(fit, transform = box_obj)
  r_pred <- predict(fit, data = box_obj$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_sepal_width, r_pred)
})


test_that("AnomalyDetectionModel/e1071 one-classification PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

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


  # box_obj <- xform_wrap(audit[, c("Income", "Deductions")])
  # fit <- svm(box_obj$data, y = NULL, type = "one-classification")
  # p_fit <- pmml(fit, dataset = box_obj$data, transforms = box_obj, detect_anomaly = FALSE)
  # r_pred <- svm_ad_predict(fit, audit[, c("Income", "Deductions")])
  # up_stat <- upload_model(p_fit)
  # z_pred <- predict_pmml_batch(audit, up_stat$model_name)
  # delete_model(up_stat$model_name)
  # expect_equal_nn(z_pred$outputs$anomalyScore, r_pred$anomaly_score)
  # expect_equal_nn(z_pred$outputs$inlier, r_pred$svm_predict_anomaly)
})


test_that("SupportVectorMachineModel/e1071 PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

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


test_that("SupportVectorMachineModel/kernlab PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

  fit <- ksvm(target ~ ., data = credit, kernel = "rbfdot", model_name = "ksvm")
  p_fit <- pmml(fit, data = credit)
  r_pred <- as.numeric(predict(fit, credit))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(credit, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_target, r_pred, tolerance = 1e-4)


  fit <- ksvm(CLASS ~ ., data = iris_nor, kernel = "rbfdot")
  p_fit <- pmml(fit, data = iris_nor)
  r_pred <- as.character(predict(fit, iris_nor))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_nor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_CLASS, r_pred)


  invisible(capture.output(fit <- ksvm(CLASS ~ ., data = iris_nor, kernel = "vanilladot")))
  p_fit <- pmml(fit, data = iris_nor)
  r_pred <- as.character(predict(fit, iris_nor))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_nor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_CLASS, r_pred)


  fit <- ksvm(Adjusted ~ ., data = audit[1:900, ], kernel = "rbfdot")
  p_fit <- pmml(fit, data = audit[1:900, ])
  r_pred <- as.numeric(predict(fit, audit[1:900, ]))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit[1:900, ], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred)


  fit <- ksvm(as.factor(purchase) ~ ., data = petfood)
  p_fit <- pmml(fit, data = petfood)
  r_pred <- as.character(predict(fit, petfood))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(petfood, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_purchase, r_pred)


  fit <- ksvm(as.factor(Adjusted) ~ ., data = audit[1:900, ], kernel = "rbfdot")
  p_fit <- pmml(fit, data = audit[1:900, ])
  r_pred <- as.character(predict(fit, audit[1:900, ]))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit[1:900, ], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred)


  fit <- ksvm(PRE_1 ~ ., data = job_cat, kernel = "rbfdot")
  p_fit <- pmml(fit, data = job_cat)
  r_pred <- as.character(predict(fit, job_cat))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(job_cat, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_PRE_1, r_pred)


  fit <- ksvm(as.factor(PRE_1) ~ ., data = job_cat_index, kernel = "rbfdot")
  p_fit <- pmml(fit, data = job_cat_index)
  r_pred <- as.character(predict(fit, job_cat_index))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(job_cat_index, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_PRE_1, r_pred)


  box_obj <- xform_wrap(iris)
  box_obj <- xform_z_score(box_obj, "column1->d1")
  box_obj <- xform_z_score(box_obj, "column2->d2")
  box_obj <- xform_z_score(box_obj, "column3->d3")
  box_obj <- xform_z_score(box_obj, "column4->d4")
  box_obj <- xform_min_max(box_obj, "d1->dd1")
  box_obj <- xform_min_max(box_obj, "d2->dd2")
  box_obj <- xform_min_max(box_obj, "d3->dd3")
  box_obj <- xform_min_max(box_obj, "d4->dd4")
  box_obj <- xform_z_score(box_obj, "dd1->ddd1")
  box_obj <- xform_z_score(box_obj, "dd2->ddd2")
  box_obj <- xform_z_score(box_obj, "dd3->ddd3")
  box_obj <- xform_z_score(box_obj, "dd4->ddd4")
  fit <- ksvm(Species ~ ddd1 + ddd2 + ddd3 + ddd4, data = box_obj$data)
  p_fit <- pmml(fit, dataset = box_obj$data, transform = box_obj)
  r_pred <- as.character(predict(fit, newdata = box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Species, r_pred)


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
  box_obj <- xform_norm_discrete(box_obj, xform_info = "class")
  fit <- ksvm(class ~ ddd1 + ddd2 + ddd3 + ddd4, data = box_obj$data)
  p_fit <- pmml(fit, dataset = box_obj$data, transform = box_obj)
  r_pred <- as.character(predict(fit, newdata = box_obj$data))
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
  fit <- ksvm(class ~ ddd1 + ddd2 + ddd3 + ddd4, data = box_obj$data)
  p_fit <- pmml(fit, dataset = box_obj$data, transform = box_obj)
  r_pred <- as.character(predict(fit, newdata = box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_class, r_pred)


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
  fit <- ksvm(Adjusted ~ ddd_Age + ddd_Income + ddd_Deductions +
    ddd_Hours + d_Marital + Employment_Private + Employment_Consultant +
    Employment_SelfEmp + Employment_PSLocal + Employment_PSState +
    Employment_PSFederal + Employment_Volunteer + Sex + Occupation +
    Education, data = box_obj$data)

  p_fit <- pmml(fit, dataset = box_obj$data, transform = box_obj)
  r_pred <- as.character(predict(fit, newdata = box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_factor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred)
})


test_that("TreeModel/rpart PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

  fit <- rpart(as.factor(Adjusted) ~ Employment + Education + Marital + Occupation + Sex, data = audit_nor)
  p_fit <- pmml(fit)
  r_pred_class <- as.character(predict(fit, audit_nor, type = "class"))
  r_pred_prob <- predict(fit, audit_nor, type = "prob")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_nor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred_class)
  expect_equal_nn(z_pred$outputs$Probability_0, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred_prob[, 2])


  fit <- rpart(temp ~ ., data = elnino)
  p_fit <- pmml(fit)
  r_pred <- predict(fit, elnino)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(elnino, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_temp, r_pred)


  fit <- rpart(CLASS ~ ., data = iris_nor)
  p_fit <- pmml(fit)
  r_pred_class <- as.character(predict(fit, iris_nor, type = "class"))
  r_pred_prob <- predict(fit, iris_nor, type = "prob")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_nor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_CLASS, r_pred_class)
  expect_equal_nn(z_pred$outputs$`Probability_Iris-setosa`, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$`Probability_Iris-versic`, r_pred_prob[, 2])
  expect_equal_nn(z_pred$outputs$`Probability_Iris-virgin`, r_pred_prob[, 3])


  fit <- rpart(fbs ~ ., data = heart)
  p_fit <- pmml(fit)
  r_pred <- predict(fit, heart)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(heart, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_fbs, r_pred)


  fit <- rpart(as.factor(fbs) ~ ., data = heart)
  p_fit <- pmml(fit)
  r_pred_class <- as.character(predict(fit, heart, type = "class"))
  r_pred_prob <- predict(fit, heart, type = "prob")
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(heart, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_fbs, r_pred_class)
  expect_equal_nn(z_pred$outputs$Probability_0, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred_prob[, 2])


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
  fit <- rpart(class ~ ddd1 + ddd2 + ddd3 + ddd4, data = box_obj$data)
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred_class <- as.character(predict(fit, box_obj$data, type = "class"))
  r_pred_prob <- predict(fit, box_obj$data, type = "prob")[, 1:3]
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_class, r_pred_class)
  expect_equal_nn(z_pred$outputs$`Probability_Iris-setosa`, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$`Probability_Iris-versicolor`, r_pred_prob[, 2])
  expect_equal_nn(z_pred$outputs$`Probability_Iris-virginica`, r_pred_prob[, 3])
})


test_that("Transformations PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_function(box_obj,
    orig_field_name = "sepal_length",
    new_field_name = "a_derived_field",
    expression = "sqrt(sepal_length^2 + 3)"
  )
  box_obj <- xform_function(box_obj,
    orig_field_name = list("sepal_length, sepal_width"),
    new_field_name = "two_field_formula",
    expression = "sepal_length * sepal_width"
  )
  fit <- lm(petal_width ~ ., data = box_obj$data)
  p_fit <- pmml(fit, transform = box_obj)
  r_pred <- predict(fit, newdata = box_obj$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_petal_width, r_pred)


  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_min_max(box_obj, "1")
  box_obj <- xform_z_score(box_obj, "1", map_missing_to = 999)
  box_obj <- xform_norm_discrete(box_obj, input_var = "class")
  box_obj <- xform_function(box_obj,
    orig_field_name = "sepal_width",
    new_field_name = "a_derived_field",
    expression = "sqrt(sepal_width^2 - 3)"
  )
  fit <- lm(petal_width ~ ., data = box_obj$data)
  p_fit <- pmml(fit, transform = box_obj)
  # suppress warning: "prediction from a rank-deficient fit may be misleading"
  r_pred <- suppressWarnings(predict(fit, newdata = box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_petal_width, r_pred)


  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_discretize(box_obj,
    xform_info = "[petal_width->dis_pw][double->string]",
    table = "iris_discretize_pw.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[sepal_length->dis_sl][double->string]",
    table = "iris_discretize_sl.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
    xform_info = "[sepal_width->dis_sw][double->string]",
    table = "iris_discretize_sw.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_map(box_obj,
    xform_info = "[class->d_class][string->double]",
    table = "iris_p_class_table.csv", default_value = "-1", map_missing_to = "1"
  )
  fit <- lm(petal_length ~ ., data = box_obj$data[, -c(2, 3, 4, 5, 7)])
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- predict(fit, box_obj$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_petal_length, r_pred)


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
  fit <- rpart(Adjusted ~ ., data = box_obj$data[, -1])
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred_class <- as.character(predict(fit, box_obj$data, type = "class"))
  r_pred_prob <- predict(fit, box_obj$data, type = "prob")[, 1:2]
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_factor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred_class)
  expect_equal_nn(z_pred$outputs$Probability_0, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred_prob[, 2])


  factor_40k_box <- xform_wrap(factor_40k)
  factor_40k_box <- xform_norm_discrete(factor_40k_box, xform_info = "CateA")
  factor_40k_box <- xform_norm_discrete(factor_40k_box, xform_info = "CateB")
  fit <- rpart(letter ~ ., data = factor_40k_box$data[, -c(2, 3)])
  p_fit <- pmml(fit, transforms = factor_40k_box)
  r_pred_class <- as.character(predict(fit, factor_40k_box$data, type = "class"))
  r_pred_prob <- predict(fit, factor_40k_box$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(factor_40k, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_letter, r_pred_class)
  expect_equal_nn(z_pred$outputs$Probability_A, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$Probability_B, r_pred_prob[, 2])


  numeric_10k_box <- xform_wrap(numeric_10k)
  numeric_10k_box <- xform_min_max(numeric_10k_box,
    xform_info = "var_10->d_var_10", map_missing_to = "0"
  )
  numeric_10k_box <- xform_min_max(numeric_10k_box,
    xform_info = "var_11->d_var_11", map_missing_to = "0"
  )
  numeric_10k_box <- xform_min_max(numeric_10k_box,
    xform_info = "var_12->d_var_12", map_missing_to = "0"
  )
  numeric_10k_box <- xform_min_max(numeric_10k_box,
    xform_info = "var_13->d_var_13", map_missing_to = "0"
  )
  fit <- lm(var_14 ~ ., data = numeric_10k_box$data)
  p_fit <- pmml(fit, transforms = numeric_10k_box)
  # Suppress warning: "prediction from a rank-deficient fit may be misleading"
  r_pred <- suppressWarnings(as.numeric(predict(fit, numeric_10k_box$data)))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(numeric_10k, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_var_14, r_pred, tolerance = 1e-5)


  numeric_10k_box <- xform_wrap(numeric_10k)
  numeric_10k_box <- xform_z_score(numeric_10k_box, xform_info = "var_0->d_var_0", map_missing_to = "0")
  numeric_10k_box <- xform_z_score(numeric_10k_box, xform_info = "var_1->d_var_1", map_missing_to = "0")
  numeric_10k_box <- xform_z_score(numeric_10k_box, xform_info = "var_2->d_var_2", map_missing_to = "0")
  numeric_10k_box <- xform_z_score(numeric_10k_box, xform_info = "var_3->d_var_3", map_missing_to = "0")
  fit <- lm(var_14 ~ ., data = numeric_10k_box$data)
  p_fit <- pmml(fit, transforms = numeric_10k_box)
  # Suppress warning: "prediction from a rank-deficient fit may be misleading"
  r_pred <- suppressWarnings(as.numeric(predict(fit, numeric_10k_box$data)))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(numeric_10k, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_var_14, r_pred, tolerance = 1e-5)


  factor_10k_box <- xform_wrap(factor_10k)
  factor_10k_box <- xform_norm_discrete(factor_10k_box, input_var = "CateA")
  factor_10k_box <- xform_norm_discrete(factor_10k_box, input_var = "CateB")
  fit <- rpart(letter ~ ., data = factor_10k_box$data[, -c(2, 3)])
  p_fit <- pmml(fit, transforms = factor_10k_box)
  r_pred_class <- as.character(predict(fit, factor_10k_box$data, type = "class"))
  r_pred_prob <- predict(fit, factor_10k_box$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(factor_10k, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_letter, r_pred_class)
  expect_equal_nn(z_pred$outputs$Probability_A, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$Probability_B, r_pred_prob[, 2])
  expect_equal_nn(z_pred$outputs$Probability_C, r_pred_prob[, 3])


  a <- which(factor_10k[, 1] == "A")
  b <- which(factor_10k[, 1] == "B")
  y <- which(factor_10k[, 1] == "Y")
  z <- which(factor_10k[, 1] == "Z")
  factor_10k_smp <- factor_10k[sample(c(a, b, y, z), length(c(a, b, y, z))), ]
  factor_10k_smp[, 1] <- as.character(factor_10k_smp[, 1])
  levels(factor_10k_smp[, 1]) <- c("A", "B", "Y", "Z")
  factor_10k_smp[, 1] <- as.factor(factor_10k_smp[, 1])
  factor_10k_box <- xform_wrap(factor_10k_smp)
  factor_10k_box <- xform_map(factor_10k_box,
    xform_info = "[letter,CateA->d_CateB][string,string->string]",
    table = "map_factor_400.csv", default_value = "-1", map_missing_to = "1"
  )
  fit <- rpart(letter ~ ., data = factor_10k_box$data[, -2])
  p_fit <- pmml(fit, transforms = factor_10k_box)
  r_pred_class <- as.character(predict(fit, factor_10k_box$data, type = "class"))
  r_pred_prob <- predict(fit, factor_10k_box$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(factor_10k_smp, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_letter, r_pred_class)
  expect_equal_nn(z_pred$outputs$Probability_A, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$Probability_B, r_pred_prob[, 2])
  expect_equal_nn(z_pred$outputs$Probability_Y, r_pred_prob[, 3])


  numeric_no_na_10k_box <- xform_wrap(numeric_no_na_10k)
  numeric_no_na_10k_box <- xform_discretize(numeric_no_na_10k_box,
    xform_info = "[var_0->d_var_0][double->integer]",
    table = "numeric_discretize_var.csv",
    map_missing_to = "0", default_value = "1"
  )
  numeric_no_na_10k_box <- xform_discretize(numeric_no_na_10k_box,
    xform_info = "[var_1->d_var_1][double->integer]",
    table = "numeric_discretize_var.csv",
    map_missing_to = "0", default_value = "1"
  )
  numeric_no_na_10k_box <- xform_discretize(numeric_no_na_10k_box,
    xform_info = "[var_2->d_var_2][double->integer]",
    table = "numeric_discretize_var.csv",
    map_missing_to = "0", default_value = "1"
  )
  numeric_no_na_10k_box <- xform_discretize(numeric_no_na_10k_box,
    xform_info = "[var_3->d_var_3][double->integer]",
    table = "numeric_discretize_var.csv",
    map_missing_to = "0", default_value = "1"
  )
  fit <- lm(var_14 ~ ., data = numeric_no_na_10k_box$data[1:600, ])
  p_fit <- pmml(fit, transforms = numeric_no_na_10k_box)
  r_pred <- as.numeric(predict(fit, numeric_no_na_10k_box$data[601:1000, ]))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(numeric_no_na_10k[601:1000, ], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_var_14, r_pred, tolerance = 1e-4)


  numeric_no_na_10k_box <- xform_wrap(numeric_no_na_10k)
  numeric_no_na_10k_box <- xform_min_max(numeric_no_na_10k_box,
    xform_info = "var_0->d_var_0", map_missing_to = "0"
  )
  numeric_no_na_10k_box <- xform_min_max(numeric_no_na_10k_box,
    xform_info = "var_1->d_var_1", map_missing_to = "0"
  )
  numeric_no_na_10k_box <- xform_min_max(numeric_no_na_10k_box,
    xform_info = "var_2->d_var_2", map_missing_to = "0"
  )
  numeric_no_na_10k_box <- xform_min_max(numeric_no_na_10k_box,
    xform_info = "var_3->d_var_3", map_missing_to = "0"
  )
  fit <- lm(var_14 ~ ., data = numeric_no_na_10k_box$data)
  p_fit <- pmml(fit, transforms = numeric_no_na_10k_box)
  # Suppress warning: "prediction from a rank-deficient fit may be misleading
  r_pred <- suppressWarnings(as.numeric(predict(fit, numeric_no_na_10k_box$data)))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(numeric_no_na_10k, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_var_14, r_pred, tolerance = 1e-5)


  numeric_no_na_10k_box <- xform_wrap(numeric_no_na_10k)
  numeric_no_na_10k_box <- xform_z_score(numeric_no_na_10k_box,
    xform_info = "var_0->d_var_0"
  )
  numeric_no_na_10k_box <- xform_z_score(numeric_no_na_10k_box,
    xform_info = "var_1->d_var_1", map_missing_to = "0"
  )
  numeric_no_na_10k_box <- xform_z_score(numeric_no_na_10k_box,
    xform_info = "var_2->d_var_2", map_missing_to = "0"
  )
  numeric_no_na_10k_box <- xform_z_score(numeric_no_na_10k_box,
    xform_info = "var_3->d_var_3", map_missing_to = "0"
  )
  fit <- lm(var_14 ~ ., data = numeric_no_na_10k_box$data)
  p_fit <- pmml(fit, transforms = numeric_no_na_10k_box)
  # Suppress warning: "prediction from a rank-deficient fit may be misleading
  r_pred <- suppressWarnings(as.numeric(predict(fit, numeric_no_na_10k_box$data)))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(numeric_no_na_10k, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_var_14, r_pred, tolerance = 1e-5)


  numeric_10k_box <- xform_wrap(numeric_10k)
  numeric_10k_box <- xform_discretize(numeric_10k_box,
    xform_info = "[var_0->d_var_0][double->integer]",
    table = "numeric_discretize_var.csv", map_missing_to = "0", default_value = "1"
  )
  numeric_10k_box <- xform_discretize(numeric_10k_box,
    xform_info = "[var_1->d_var_1][double->integer]",
    table = "numeric_discretize_var.csv", map_missing_to = "0", default_value = "1"
  )
  numeric_10k_box <- xform_discretize(numeric_10k_box,
    xform_info = "[var_2->d_var_2][double->integer]",
    table = "numeric_discretize_var.csv", map_missing_to = "0", default_value = "1"
  )
  numeric_10k_box <- xform_discretize(numeric_10k_box,
    xform_info = "[var_3->d_var_3][double->integer]",
    table = "numeric_discretize_var.csv", map_missing_to = "0", default_value = "1"
  )
  fit <- lm(var_14 ~ ., data = numeric_10k_box$data[1:500, ])
  p_fit <- pmml(fit, transforms = numeric_10k_box)
  r_pred <- as.numeric(predict(fit, numeric_10k_box$data[501:994, ]))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(numeric_10k[501:994, ], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_var_14, r_pred, tolerance = 1e-4)
})
