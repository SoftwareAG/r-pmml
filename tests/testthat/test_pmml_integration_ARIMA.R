library(zementisr)
library(forecast)

# 
# data(iris)
# data(audit)
# data("WWWusage")
# data("AirPassengers")
# data("USAccDeaths")
# data("JohnsonJohnson")
# data("sunspots")
# audit_factor <- audit
# audit_factor[, 13] <- as.factor(audit_factor[, 13])
# iris_p <- read.csv("iris.csv", stringsAsFactors = TRUE)
# audit <- na.omit(audit)
# elnino <- read.csv("elnino.csv", stringsAsFactors = TRUE)
# heart <- read.csv("heart.csv", stringsAsFactors = TRUE)
# glm_issue3543_data <- read.csv("glm_issue3543_data.csv", stringsAsFactors = TRUE)
# credit_class <- read.csv("credit_class.csv", stringsAsFactors = TRUE)
# covtype2 <- read.csv("covtype2.csv", header = TRUE, stringsAsFactors = TRUE)
# credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
# credit_class_01 <- read.csv("credit_class_01.csv", stringsAsFactors = TRUE)
# audit_nor_logical <- na.omit(read.csv("audit_nor_logical.csv", stringsAsFactors = TRUE))
# audit_nor <- na.omit(read.csv("audit_nor.csv", stringsAsFactors = TRUE))
# audit_nor_fake_logical <- na.omit(read.csv("audit_nor_fake_logical.csv", stringsAsFactors = TRUE))
# random_data_small <- read.csv("random_data_small.csv", stringsAsFactors = TRUE)
# iris_nor <- read.csv("iris_nor.csv", stringsAsFactors = TRUE)
# bank <- na.omit(read.csv("bank.csv", stringsAsFactors = TRUE))
# audit_r_build_in <- na.omit(read.csv("audit_r_build_in.csv", stringsAsFactors = TRUE))
# insurance <- na.omit(read.csv("insurance.csv", stringsAsFactors = TRUE))
# iris_bin <- read.csv("iris_bin.csv", stringsAsFactors = TRUE)
# house_votes <- na.omit(read.csv("house_votes_84.csv", stringsAsFactors = TRUE))
# iris_mini_dot <- read.csv("iris_mini_dot.csv", stringsAsFactors = TRUE)
# petfood <- read.csv("petfood.csv", stringsAsFactors = TRUE)
# job_cat <- read.csv("job_cat.csv", stringsAsFactors = TRUE)
# job_cat_index <- read.csv("job_cat_index.csv", stringsAsFactors = TRUE)
# iris_nor_logical <- read.csv("iris_nor_logical.csv", stringsAsFactors = TRUE)
# factor_40k <- read.csv("factor_40k.csv", stringsAsFactors = TRUE)
# numeric_10k <- na.omit(read.csv("numeric_10k.csv", stringsAsFactors = TRUE))
# factor_10k <- read.csv("factor_10k.csv", stringsAsFactors = TRUE)
# numeric_no_na_10k <- read.csv("numeric_no_na_10k.csv", stringsAsFactors = TRUE)


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
  
  z_pred_out <- z_pred_out[NROW(z_pred_out), ] # only use the last row
  
  # rearrange z_pred columns into same order as that of r_pred;
  # using names(r_pred) accounts for names with different CPI
  z_pred_out <- z_pred_out[names(r_pred)]
  
  z_pred_transf <- data.frame(matrix(NA,
                                     nrow = NCOL(z_pred_out[[1]]),
                                     ncol = NCOL(r_pred)
  ), stringsAsFactors = TRUE)
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
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
  
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
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
  
  fit <- Arima(JohnsonJohnson, order = c(0, 1, 0), seasonal = c(0, 1, 0))
  p_fit <- pmml(fit, model_name = "arima_010010", exact_least_squares = FALSE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
  
  fit <- Arima(JohnsonJohnson, order = c(0, 1, 0), seasonal = c(0, 1, 2))
  p_fit <- pmml(fit, model_name = "arima_010012", exact_least_squares = FALSE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
  
  fit <- Arima(AirPassengers, order = c(0, 1, 1), seasonal = c(0, 1, 1))
  p_fit <- pmml(fit, model_name = "arima_011011", exact_least_squares = FALSE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
  
  fit <- Arima(JohnsonJohnson, order = c(1, 1, 1), seasonal = c(1, 1, 1))
  p_fit <- pmml(fit, model_name = "arima_111111", exact_least_squares = FALSE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
  
  fit <- Arima(JohnsonJohnson, order = c(0, 0, 1), seasonal = c(0, 1, 0))
  p_fit <- pmml(fit, model_name = "arima_001010", exact_least_squares = FALSE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
  
  fit <- Arima(sunspots, order = c(1, 0, 0), seasonal = c(1, 0, 0))
  p_fit <- pmml(fit, model_name = "arima_100100", exact_least_squares = FALSE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
  
  # expect the following test to fail
  fit <- Arima(JohnsonJohnson, order = c(1, 1, 0), seasonal = c(0, 0, 1))
  p_fit <- pmml(fit, model_name = "arima_110001", exact_least_squares = FALSE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_failure(expect_equal_nn(as.numeric(
    z_pred$outputs$Predicted_ts_value[20, ]
  ), r_pred))
  
  # tests with seasonal ELS
  fit <- Arima(JohnsonJohnson, order = c(1, 1, 0), seasonal = c(0, 0, 1))
  p_fit <- pmml(fit, model_name = "arima_110001", exact_least_squares = TRUE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
  
  fit <- Arima(JohnsonJohnson, order = c(4, 1, 4), seasonal = c(1, 1, 1))
  p_fit <- pmml(fit, model_name = "arima_414111", exact_least_squares = TRUE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
  
  fit <- Arima(USAccDeaths, order = c(2, 1, 1), seasonal = c(2, 1, 4))
  p_fit <- pmml(fit, model_name = "arima_211214", exact_least_squares = TRUE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
  
  fit <- Arima(USAccDeaths, order = c(2, 2, 3), seasonal = c(0, 1, 1))
  p_fit <- pmml(fit, model_name = "arima_223011", exact_least_squares = TRUE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
  
  fit <- Arima(USAccDeaths, order = c(2, 0, 0), seasonal = c(2, 0, 0))
  p_fit <- pmml(fit, model_name = "arima_200200", exact_least_squares = TRUE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
  
  fit <- Arima(WWWusage, order = c(3, 0, 3), seasonal = c(3, 0, 3))
  p_fit <- pmml(fit, model_name = "arima_303303", exact_least_squares = TRUE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
  
  fit <- Arima(JohnsonJohnson, order = c(0, 0, 4), seasonal = c(0, 2, 4))
  p_fit <- pmml(fit, model_name = "arima_004024", exact_least_squares = TRUE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
  
  # seasonal model with (0,0,0) non-seasonal component
  fit <- Arima(JohnsonJohnson, order = c(0, 0, 0), seasonal = c(0, 2, 4))
  p_fit <- pmml(fit, model_name = "arima_000024", exact_least_squares = FALSE)
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
  
  # state space representation
})
