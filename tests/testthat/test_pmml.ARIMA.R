context("test pmml.ARIMA converter")

library(forecast)
data("WWWusage")
data("AirPassengers")

test_that("forecast ARIMA object works", {
  fit <- Arima(AirPassengers,order=c(0,0,0))
  p_fit <- pmml(fit)
  
  fit2 <- Arima(AirPassengers,order=c(3,1,0))
  p_fit2 <- pmml(fit2)  
})

test_that("forecast auto.arima object works 1", {
  mod2 <- auto.arima(AirPassengers)
  p_mod2 <- pmml(mod2)
})

test_that("forecast auto.arima object works 2", {
  mod3 <- auto.arima(WWWusage)
  p_mod3 <- pmml(mod3)
})

test_that("NonseasonalComponent node contains required elements 1", {
  s <- ts(data=c(11357.92, 10605.95, 16998.57, 6563.75, 6607.69, 9839.0))
  fit_5 <- Arima(s, order=c(3,1,1))
  p_fit_5 <- pmml(fit_5)
  
  # AR component
  expect_equal(toString(p_fit_5[[3]][[3]][[1]][[1]][[1]][[1]]),
               '-0.196933688666896 0.0882676656284808 0.9429079310464')

  # MA component - MACoefficients
  expect_equal(toString(p_fit_5[[3]][[3]][[1]][[2]][[1]][[1]]),
               "<Array type=\"real\" n=\"1\">0.999467612244043</Array>")
  
  # MA component - Residuals
  expect_equal(toString(p_fit_5[[3]][[3]][[1]][[2]][[2]]),
               "<Residuals>\n <Array type=\"real\" n=\"1\">-846.776313143145</Array>\n</Residuals>")
})

test_that("NonseasonalComponent node contains required elements 2", {
  s <- ts(data=c(1.02, 2.9, 3.11, 4, 5, 4.4, 5.3))
  fit_6 <- Arima(s, order=c(3,0,3))
  p_fit_6 <- pmml(fit_6)
  
  # # AR component
  # expect_equal(toString(p_fit_6[[3]][[3]][[1]][[1]][[1]][[1]]),
  #              '-0.196933688666896 0.0882676656284808 0.9429079310464')
})