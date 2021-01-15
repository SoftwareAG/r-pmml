

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
                                     ncol = NCOL(r_pred)),
                              stringsAsFactors = TRUE)
  
  colnames(z_pred_transf) <- names(r_pred)
  
  for (x in names(r_pred)) {
    z_pred_transf[x] <- as.numeric(t(z_pred_out[[x]]))
  }
  
  expect_equal_nn(z_pred_transf, r_pred)
}

expect_equal_df_3 <- function(z_pred_out, r_pred) {
  # updated version of expect_equal_df_2.
  # expect_equal for data frames with point and CPI where Zementis output is a JSON string.
  
  # rearrange z_pred columns into same order as that of r_pred;
  # using names(r_pred) accounts for names with different CPI
  z_pred_out <- z_pred_out[names(r_pred)]
  
  # get order of column names for each field returned by zementisr
  z_order <- order(as.numeric(names(z_pred_out$Predicted_ts_value))) 
  
  z_pred_transf <- data.frame(matrix(NA,
                                     nrow = NCOL(z_pred_out[[1]]),
                                     ncol = NCOL(r_pred)),
                              stringsAsFactors = TRUE)
  
  colnames(z_pred_transf) <- names(r_pred)
  
  for (x in names(r_pred)) {
    
    z_row <- z_pred_out[[x]]
    z_row <- z_row[, z_order]
    z_pred_transf[x] <- as.numeric(t(z_row))

    # z_pred_transf[x] <- as.numeric(t(z_pred_out[[x]]))
  }
  
  expect_equal_nn(z_pred_transf, r_pred)
}


expect_equal_nn <- function(...) {
  # expect_equal without name checking
  expect_equal(..., check.names = FALSE)
}

expect_equal_sorted <- function(z_pred_val, r_pred) {
  # expect_equal where z is sorted by numeric names first
  z_order <- order(as.numeric(names(z_pred_val)))
  z_pred_val <- z_pred_val[z_order]
  expect_equal_nn(as.numeric(z_pred_val), r_pred)
}


single_col_h_df <- function(h) {
  # For ARIMA models, create a data frame with a single column of h (number of steps ahead) values.
  dframe <- data.frame("h" = c(1:h), stringsAsFactors = TRUE)
  return(dframe)
}

h_20 <- single_col_h_df(20)
h_20_one_line <- data.frame("h" = c(20))



test_that("TimeSeriesModel/forecast PMML output matches R for non-seasonal and ts_type='arima'", {
  skip_on_cran()
  skip_on_ci()
  # skip("skip non-seasonal ts_type='arima'")
  
  library(zementisr)
  library(forecast)
  
  
  # non-seasonal tests - with CLS and CPI
  fit <- auto.arima(sunspots) # creates non-seasonal model
  p_fit <- pmml(fit, model_name = "arima_auto_01", ts_type = "arima")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_2(z_pred$outputs, r_pred)
  
  fit <- Arima(AirPassengers, order = c(2, 1, 2))
  p_fit <- pmml(fit, model_name = "arima_212", ts_type = "arima")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_2(z_pred$outputs, r_pred)
  
  fit <- Arima(AirPassengers, order = c(1, 1, 1))
  p_fit <- pmml(fit, model_name = "arima_111", ts_type = "arima")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_2(z_pred$outputs, r_pred)
  
  fit <- Arima(WWWusage, order = c(2, 0, 2))
  p_fit <- pmml(fit, model_name = "arima_202", ts_type = "arima")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_2(z_pred$outputs, r_pred)
  
  fit <- Arima(USAccDeaths, order = c(1, 2, 0))
  p_fit <- pmml(fit, model_name = "arima_120", ts_type = "arima")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_2(z_pred$outputs, r_pred)
  
  # non-seasonal tests with d=2 - expect mismatch
  fit <- Arima(AirPassengers, order = c(2, 2, 2))
  p_fit <- pmml(fit, model_name = "arima_222", ts_type = "arima")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_failure(expect_equal_df_2(z_pred$outputs, r_pred))
  
  fit <- Arima(USAccDeaths, order = c(1, 2, 3))
  p_fit <- pmml(fit, model_name = "arima_123", ts_type = "arima")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_failure(expect_equal_df_2(z_pred$outputs, r_pred))
})


test_that("TimeSeriesModel/forecast PMML output matches R for seasonal,ts_type='arima'", {
  skip_on_cran()
  skip_on_ci()
  # skip("skip seasonal CLS")

  library(zementisr)
  library(forecast)
  
  
  # seasonal with CLS
  fit <- auto.arima(JohnsonJohnson) # creates seasonal model
  p_fit <- pmml(fit, model_name = "arima_auto_02", ts_type = "arima")
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_sorted(z_pred$outputs$Predicted_ts_value, r_pred)
  
  fit <- Arima(JohnsonJohnson, order = c(1, 1, 0), seasonal = c(0, 1, 1))
  p_fit <- pmml(fit, model_name = "arima_110011", ts_type = "arima")
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_sorted(z_pred$outputs$Predicted_ts_value, r_pred)
  
  fit <- Arima(JohnsonJohnson, order = c(0, 1, 0), seasonal = c(0, 1, 0))
  p_fit <- pmml(fit, model_name = "arima_010010", ts_type = "arima")
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_sorted(z_pred$outputs$Predicted_ts_value, r_pred)
  
  fit <- Arima(JohnsonJohnson, order = c(0, 1, 0), seasonal = c(0, 1, 2))
  p_fit <- pmml(fit, model_name = "arima_010012", ts_type = "arima")
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_sorted(z_pred$outputs$Predicted_ts_value, r_pred)
  
  fit <- Arima(AirPassengers, order = c(0, 1, 1), seasonal = c(0, 1, 1))
  p_fit <- pmml(fit, model_name = "arima_011011", ts_type = "arima")
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_sorted(z_pred$outputs$Predicted_ts_value, r_pred)
  
  fit <- Arima(JohnsonJohnson, order = c(1, 1, 1), seasonal = c(1, 1, 1))
  p_fit <- pmml(fit, model_name = "arima_111111", ts_type = "arima")
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_sorted(z_pred$outputs$Predicted_ts_value, r_pred)
  
  fit <- Arima(JohnsonJohnson, order = c(0, 0, 1), seasonal = c(0, 1, 0))
  p_fit <- pmml(fit, model_name = "arima_001010", ts_type = "arima")
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_sorted(z_pred$outputs$Predicted_ts_value, r_pred)
  
  fit <- Arima(sunspots, order = c(1, 0, 0), seasonal = c(1, 0, 0))
  p_fit <- pmml(fit, model_name = "arima_100100", ts_type = "arima")
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_sorted(z_pred$outputs$Predicted_ts_value, r_pred)
  
  # expect the following test to fail
  fit <- Arima(JohnsonJohnson, order = c(1, 1, 0), seasonal = c(0, 0, 1))
  p_fit <- pmml(fit, model_name = "arima_110001", ts_type = "arima")
  r_pred <- as.numeric(forecast(fit, h = 20)$mean)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_failure(expect_equal_sorted(z_pred$outputs$Predicted_ts_value, r_pred))
})


test_that("TimeSeriesModel/forecast PMML output matches R for statespace representation", {
  skip_on_cran()
  skip_on_ci()
  # skip("skip statespace")
  
  library(zementisr)
  library(forecast)
  
  
  fit <- auto.arima(JohnsonJohnson) # creates seasonal model
  p_fit <- pmml(fit, model_name = "arima_auto_02_ss",ts_type = "statespace")
  r_pred <- forecast_with_cpi(fit,20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_3(z_pred$outputs, r_pred)
  
  fit <- Arima(AirPassengers, order = c(2, 1, 2))
  p_fit <- pmml(fit, model_name = "arima_212_ss", ts_type = "statespace")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_3(z_pred$outputs, r_pred)
  
  fit <- Arima(WWWusage, order = c(2, 0, 2))
  p_fit <- pmml(fit, model_name = "arima_202_ss", ts_type = "statespace")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_3(z_pred$outputs, r_pred)
  
  fit <- Arima(AirPassengers, order = c(2, 2, 2))
  p_fit <- pmml(fit, model_name = "arima_222_ss", ts_type = "statespace")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_3(z_pred$outputs, r_pred)
  
  fit <- Arima(USAccDeaths, order = c(1, 2, 3))
  p_fit <- pmml(fit, model_name = "arima_123_ss", ts_type = "statespace")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_3(z_pred$outputs, r_pred)

  fit <- Arima(JohnsonJohnson, order = c(1, 1, 0), seasonal = c(0, 1, 1))
  p_fit <- pmml(fit, model_name = "arima_110011_ss", ts_type = "statespace")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_3(z_pred$outputs, r_pred)

  fit <- Arima(AirPassengers, order = c(0, 1, 1), seasonal = c(0, 1, 1))
  p_fit <- pmml(fit, model_name = "arima_011011_ss", ts_type = "statespace")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_3(z_pred$outputs, r_pred)

  fit <- Arima(JohnsonJohnson, order = c(1, 1, 1), seasonal = c(1, 1, 1))
  p_fit <- pmml(fit, model_name = "arima_111111_ss", ts_type = "statespace")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_3(z_pred$outputs, r_pred)

  fit <- Arima(JohnsonJohnson, order = c(0, 0, 1), seasonal = c(0, 1, 0))
  p_fit <- pmml(fit, model_name = "arima_001010_ss", ts_type = "statespace")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_3(z_pred$outputs, r_pred)

  fit <- Arima(sunspots, order = c(1, 0, 0), seasonal = c(1, 0, 0))
  p_fit <- pmml(fit, model_name = "arima_100100_ss", ts_type = "statespace")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_3(z_pred$outputs, r_pred)
  
  fit <- Arima(JohnsonJohnson, order = c(1, 1, 0), seasonal = c(0, 0, 1))
  p_fit <- pmml(fit, model_name = "arima_110001_ss", ts_type = "statespace")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_3(z_pred$outputs, r_pred)
  
  # tests with seasonal ELS
  fit <- Arima(JohnsonJohnson, order = c(1, 1, 0), seasonal = c(0, 0, 1))
  p_fit <- pmml(fit, model_name = "arima_110001_ss", ts_type = "statespace")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_3(z_pred$outputs, r_pred)
  
  fit <- Arima(JohnsonJohnson, order = c(4, 1, 4), seasonal = c(1, 1, 1))
  p_fit <- pmml(fit, model_name = "arima_414111_ss", ts_type = "statespace")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_3(z_pred$outputs, r_pred)
  
  fit <- Arima(USAccDeaths, order = c(2, 1, 1), seasonal = c(2, 1, 4))
  p_fit <- pmml(fit, model_name = "arima_211214_ss", ts_type = "statespace")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_3(z_pred$outputs, r_pred)
  
  fit <- Arima(USAccDeaths, order = c(2, 2, 3), seasonal = c(0, 1, 1))
  p_fit <- pmml(fit, model_name = "arima_223011_ss", ts_type = "statespace")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_3(z_pred$outputs, r_pred)
  
  fit <- Arima(USAccDeaths, order = c(2, 0, 0), seasonal = c(2, 0, 0))
  p_fit <- pmml(fit, model_name = "arima_200200_ss", ts_type = "statespace")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_3(z_pred$outputs, r_pred)
  
  fit <- Arima(WWWusage, order = c(3, 0, 3), seasonal = c(3, 0, 3))
  p_fit <- pmml(fit, model_name = "arima_303303_ss", ts_type = "statespace")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_3(z_pred$outputs, r_pred)
  
  fit <- Arima(JohnsonJohnson, order = c(0, 0, 4), seasonal = c(0, 2, 4))
  p_fit <- pmml(fit, model_name = "arima_004024_ss", ts_type = "statespace")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_3(z_pred$outputs, r_pred)
  
  # seasonal model with (0,0,0) non-seasonal component
  fit <- Arima(JohnsonJohnson, order = c(0, 0, 0), seasonal = c(0, 2, 4))
  p_fit <- pmml(fit, model_name = "arima_000024_ss", ts_type = "statespace")
  r_pred <- forecast_with_cpi(fit, 20)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(h_20_one_line, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_df_3(z_pred$outputs, r_pred)
  
})


# this test should be used when drift is supported:
# fit <- Arima(WWWusage, order = c(3, 1, 1), include.drift = TRUE)
# p_fit <- pmml(fit, model_name = "arima_311")
# r_pred <- forecast_with_cpi(fit, 20)
# up_stat <- upload_model(p_fit)
# z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
# delete_model(up_stat$model_name)
# expect_equal_df(z_pred$outputs, r_pred)

# ExactLeastSquares no longer supported
# test_that("TimeSeriesModel/forecast PMML output matches R for seasonal,ts_type='arima', and exact_least_squares = TRUE", {
#   skip_on_cran()
#   skip_on_ci()
#   skip("skip seasonal ELS")
#   
#   fit <- Arima(JohnsonJohnson, order = c(1, 1, 0), seasonal = c(0, 0, 1))
#   p_fit <- pmml(fit, model_name = "arima_110001", exact_least_squares = TRUE)
#   r_pred <- as.numeric(forecast(fit, h = 20)$mean)
#   up_stat <- upload_model(p_fit)
#   z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
#   delete_model(up_stat$model_name)
#   expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
#   
#   fit <- Arima(JohnsonJohnson, order = c(4, 1, 4), seasonal = c(1, 1, 1))
#   p_fit <- pmml(fit, model_name = "arima_414111", exact_least_squares = TRUE)
#   r_pred <- as.numeric(forecast(fit, h = 20)$mean)
#   up_stat <- upload_model(p_fit)
#   z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
#   delete_model(up_stat$model_name)
#   expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
#   
#   fit <- Arima(USAccDeaths, order = c(2, 1, 1), seasonal = c(2, 1, 4))
#   p_fit <- pmml(fit, model_name = "arima_211214", exact_least_squares = TRUE)
#   r_pred <- as.numeric(forecast(fit, h = 20)$mean)
#   up_stat <- upload_model(p_fit)
#   z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
#   delete_model(up_stat$model_name)
#   expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
#   
#   fit <- Arima(USAccDeaths, order = c(2, 2, 3), seasonal = c(0, 1, 1))
#   p_fit <- pmml(fit, model_name = "arima_223011", exact_least_squares = TRUE)
#   r_pred <- as.numeric(forecast(fit, h = 20)$mean)
#   up_stat <- upload_model(p_fit)
#   z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
#   delete_model(up_stat$model_name)
#   expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
#   
#   fit <- Arima(USAccDeaths, order = c(2, 0, 0), seasonal = c(2, 0, 0))
#   p_fit <- pmml(fit, model_name = "arima_200200", exact_least_squares = TRUE)
#   r_pred <- as.numeric(forecast(fit, h = 20)$mean)
#   up_stat <- upload_model(p_fit)
#   z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
#   delete_model(up_stat$model_name)
#   expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
#   
#   fit <- Arima(WWWusage, order = c(3, 0, 3), seasonal = c(3, 0, 3))
#   p_fit <- pmml(fit, model_name = "arima_303303", exact_least_squares = TRUE)
#   r_pred <- as.numeric(forecast(fit, h = 20)$mean)
#   up_stat <- upload_model(p_fit)
#   z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
#   delete_model(up_stat$model_name)
#   expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
#   
#   fit <- Arima(JohnsonJohnson, order = c(0, 0, 4), seasonal = c(0, 2, 4))
#   p_fit <- pmml(fit, model_name = "arima_004024", exact_least_squares = TRUE)
#   r_pred <- as.numeric(forecast(fit, h = 20)$mean)
#   up_stat <- upload_model(p_fit)
#   z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
#   delete_model(up_stat$model_name)
#   expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
#   
#   # seasonal model with (0,0,0) non-seasonal component
#   fit <- Arima(JohnsonJohnson, order = c(0, 0, 0), seasonal = c(0, 2, 4))
#   p_fit <- pmml(fit, model_name = "arima_000024", exact_least_squares = FALSE)
#   r_pred <- as.numeric(forecast(fit, h = 20)$mean)
#   up_stat <- upload_model(p_fit)
#   z_pred <- predict_pmml_batch(h_20, up_stat$model_name)
#   delete_model(up_stat$model_name)
#   expect_equal_nn(as.numeric(z_pred$outputs$Predicted_ts_value[20, ]), r_pred)
# })
