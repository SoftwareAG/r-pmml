context("test pmml.ARIMA converter")

library(forecast)
data("WWWusage")
data("AirPassengers")
data("JohnsonJohnson")


test_that("DataDictionary node contains expected elements", {
  fit_2 <- auto.arima(WWWusage)
  p_fit_2 <- pmml(fit_2)
  expect_equal(toString(p_fit_2[[2]]),"<DataDictionary numberOfFields=\"2\">\n <DataField name=\"ts_value\" optype=\"continuous\" dataType=\"double\"/>\n <DataField name=\"h\" optype=\"continuous\" dataType=\"double\"/>\n</DataDictionary>")
})

test_that("MiningSchema node contains expected elements", {
  fit_3 <- auto.arima(WWWusage)
  p_fit_3 <- pmml(fit_3)
  expect_equal(toString(p_fit_3[[3]][[1]]),"<MiningSchema>\n <MiningField name=\"ts_value\" usageType=\"predicted\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"h\" usageType=\"supplementary\" invalidValueTreatment=\"returnInvalid\"/>\n</MiningSchema>")
})


test_that("Output node contains expected elements", {
  fit_4 <- auto.arima(WWWusage)
  p_fit_4 <- pmml(fit_4)
  expect_equal(toString(p_fit_4[[3]][[2]][[1]]),
               "<OutputField name=\"Predicted_ts_value\" optype=\"continuous\" dataType=\"double\" feature=\"predictedValue\"/>")
})

test_that("NonseasonalComponent node contains required elements 1", {
  s <- ts(data=c(11357.92, 10605.95, 16998.57, 6563.75, 6607.69, 9839.0))
  fit_5 <- Arima(s, order=c(3,1,1))
  p_fit_5 <- pmml(fit_5)
  
  # NonseasonalComponent attributes
  expect_equal(xmlGetAttr(p_fit_5[[3]][[4]][[1]],name="p"),3)
  expect_equal(xmlGetAttr(p_fit_5[[3]][[4]][[1]],name="d"),1)
  expect_equal(xmlGetAttr(p_fit_5[[3]][[4]][[1]],name="q"),1)
  
  # AR component
  expect_equal(toString(p_fit_5[[3]][[4]][[1]][[1]][[1]][[1]]),
               '-0.196933688666896 0.0882676656284808 0.9429079310464')

  # MA component - MACoefficients; sign of coefficients in PMML is opposite to R
  expect_equal(toString(p_fit_5[[3]][[4]][[1]][[2]][[1]][[1]]),
               "<Array type=\"real\" n=\"1\">0.999467612244043</Array>")
  
  # MA component - Residuals
  expect_equal(toString(p_fit_5[[3]][[4]][[1]][[2]][[2]]),
               "<Residuals>\n <Array type=\"real\" n=\"1\">-846.776313143145</Array>\n</Residuals>")
})

test_that("non-seasonal ARIMA node contains correct attributes", {
  s <- ts(data=c(11357.92, 10605.95, 16998.57, 6563.75, 6607.69, 9839.0))
  fit_6 <- Arima(s, order=c(0,0,1))
  p_fit_6 <- pmml(fit_6)
  
  expect_equal(xmlGetAttr(p_fit_6[[3]][[4]],name="RMSE"),"3472.72443700833")
  expect_equal(xmlGetAttr(p_fit_6[[3]][[4]],name="transformation"),"none")
  expect_equal(xmlGetAttr(p_fit_6[[3]][[4]],name="constantTerm"),"10327.6226360507")
  expect_equal(xmlGetAttr(p_fit_6[[3]][[4]],name="predictionMethod"),"conditionalLeastSquares")
  
})


test_that("seasonal ARIMA model contains correct elements 1", {
  fit_7 <- Arima(JohnsonJohnson,order=c(0,0,2),seasonal=c(0,0,1))
  p_fit_7 <- pmml(fit_7)
  
  expect_equal(xmlGetAttr(p_fit_7[[3]][[4]][[1]],name="p"),0)
  expect_equal(xmlGetAttr(p_fit_7[[3]][[4]][[1]],name="d"),0)
  expect_equal(xmlGetAttr(p_fit_7[[3]][[4]][[1]],name="q"),2)
  
  expect_equal(xmlGetAttr(p_fit_7[[3]][[4]][[2]],name="P"),0)
  expect_equal(xmlGetAttr(p_fit_7[[3]][[4]][[2]],name="D"),0)
  expect_equal(xmlGetAttr(p_fit_7[[3]][[4]][[2]],name="Q"),1)
  expect_equal(xmlGetAttr(p_fit_7[[3]][[4]][[2]],name="period"),4)
  
  expect_equal(toString(p_fit_7[[3]][[4]][[2]][[1]][[1]][[1]]),'<Array type="real" n="1">0.999999926590528</Array>')
  # Seasonal residuals array should have 6 elements.
  expect_equal(toString(p_fit_7[[3]][[4]][[2]][[1]][[2]][[1]]),
               '<Array type="real" n="6">0.840294788225463 1.79540974711022 3.43600813764863 0.595257922098859 1.560371580367 1.33444221515274</Array>')
})

test_that("seasonal ARIMA model contains correct elements 2", {
  fit_8 <- Arima(AirPassengers,order=c(1,1,1),seasonal=c(1,1,1))
  p_fit_8 <- pmml(fit_8)
  
  
  expect_equal(xmlGetAttr(p_fit_8[[3]][[4]][[1]],name="p"),1)
  expect_equal(xmlGetAttr(p_fit_8[[3]][[4]][[1]],name="d"),1)
  expect_equal(xmlGetAttr(p_fit_8[[3]][[4]][[1]],name="q"),1)
  
  expect_equal(xmlGetAttr(p_fit_8[[3]][[4]][[2]],name="P"),1)
  expect_equal(xmlGetAttr(p_fit_8[[3]][[4]][[2]],name="D"),1)
  expect_equal(xmlGetAttr(p_fit_8[[3]][[4]][[2]],name="Q"),1)
  expect_equal(xmlGetAttr(p_fit_8[[3]][[4]][[2]],name="period"),12)
  
  expect_equal(toString(p_fit_8[[3]][[4]][[2]][[1]][[1]]),'<Array type="real" n="1">-0.926970851026725</Array>')
  # Seasonal residuals array should have 13 elements.
  expect_equal(toString(p_fit_8[[3]][[4]][[2]][[2]][[2]][[1]]),
               '<Array type="real" n="13">16.6406514721613 -2.15049966299974 -8.78471974960152 -34.7165557860653 42.1922537998818 2.72016048914321 5.57048733936438 14.8646481189408 -23.3767800799817 -7.00132283870703 5.56068703039644 -24.4368048030507 -7.86142188676456</Array>')
  
})


test_that("seasonal ARIMA model contains correct elements 3", {
  fit_9 <- Arima(AirPassengers,order=c(1,2,3),seasonal=c(1,2,1))
  p_fit_9 <- pmml(fit_9)
  
  expect_equal(xmlGetAttr(p_fit_9[[3]][[4]],name="constantTerm"),"0")
  
  expect_equal(xmlGetAttr(p_fit_9[[3]][[4]][[1]],name="p"),1)
  expect_equal(xmlGetAttr(p_fit_9[[3]][[4]][[1]],name="d"),2)
  expect_equal(xmlGetAttr(p_fit_9[[3]][[4]][[1]],name="q"),3)
  
  expect_equal(xmlGetAttr(p_fit_9[[3]][[4]][[2]],name="P"),1)
  expect_equal(xmlGetAttr(p_fit_9[[3]][[4]][[2]],name="D"),2)
  expect_equal(xmlGetAttr(p_fit_9[[3]][[4]][[2]],name="Q"),1)
  expect_equal(xmlGetAttr(p_fit_9[[3]][[4]][[2]],name="period"),12)
  
  # NonseasonalComponent coefficients
  expect_equal(toString(p_fit_9[[3]][[4]][[1]][[1]][[1]]),'<Array type="real" n="1">-0.918811953411307</Array>')
  expect_equal(toString(p_fit_9[[3]][[4]][[1]][[2]][[1]][[1]]),'<Array type="real" n="3">-0.488398025152718 -0.981238492527093 0.47123365713457</Array>')
  expect_equal(toString(p_fit_9[[3]][[4]][[1]][[2]][[2]][[1]]),'<Array type="real" n="3">10.6024485251517 -16.6110095847448 -3.45077032800267</Array>')
  
  # SeasonalComponent coefficients
  expect_equal(toString(p_fit_9[[3]][[4]][[2]][[1]][[1]]),'<Array type="real" n="1">-0.33060697133757</Array>')
  expect_equal(toString(p_fit_9[[3]][[4]][[2]][[2]][[1]][[1]]),'<Array type="real" n="1">-0.964840603841212</Array>')
  # Seasonal residuals array should have 15 elements.
  expect_equal(toString(p_fit_9[[3]][[4]][[2]][[2]][[2]][[1]]),'<Array type=\"real\" n=\"15\">-0.231414430272416 7.85449813502021 14.0341170808424 0.488050253167314 -3.47775177064476 -34.7531345572846 36.9803974323111 -0.721770286617552 3.21345471099022 5.83445945385706 -21.7707201166725 -1.21862197305704 10.6024485251517 -16.6110095847448 -3.45077032800267</Array>')
  
  
})


test_that("seasonal ARIMA model contains correct elements 4", {
  fit_9a <- Arima(AirPassengers,order=c(1,1,0),seasonal=c(0,1,1))
  p_fit_9a <- pmml(fit_9a)
  

  
  
})



test_that("Seasonal ARIMA without non-seasonal component does not contain NonseasonalComponent", {
  fit_10 <- Arima(AirPassengers,order=c(0,0,0),seasonal=c(1,2,1))
  p_fit_10 <- pmml(fit_10)
  expect_equal(substr(toString(p_fit_10[[3]][[4]][[1]]),1,18),"<SeasonalComponent")
})

test_that("ARIMA with both intercept and drift terms throws error", {
  fit_11 <- Arima(AirPassengers,order=c(1,0,1), include.drift = TRUE)
  expect_error(pmml(fit_11),"ARIMA models with both mean and drift terms not supported.")
  
  fit_12 <- Arima(AirPassengers,order=c(2,0,2), include.drift = TRUE)
  expect_error(pmml(fit_12),"ARIMA models with both mean and drift terms not supported.")
  
})


# TODO: check that constant is not 0 when d=1 and include.drift is TRUE


# TODO: compare coefficients in PMML directly with R coef

