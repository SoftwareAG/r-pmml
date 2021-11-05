library(ada)
data(audit)

test_that("pmml.ada error when object is not ada", {
  a <- "foo"
  expect_error(pmml.ada(a), "Not a legitimate ada object")
})

test_that("Elements and attributes are correct", {
  fit <- ada(Adjusted ~ Employment + Education + Hours + Income, iter = 3, audit)
  fit_pmml <- pmml(fit)

  # Test miningModel attributes
  expect_equal(xmlGetAttr(fit_pmml[[3]], name = "modelName"), "AdaBoost_Model")
  expect_equal(xmlGetAttr(fit_pmml[[3]], name = "functionName"), "regression")
  expect_equal(xmlGetAttr(fit_pmml[[3]], name = "algorithmName"), "ADA_BOOST")

  # Test Output node
  expect_equal(toString(fit_pmml[[3]][[2]][[1]]), "<OutputField name=\"rawValue\" feature=\"predictedValue\" dataType=\"double\" optype=\"continuous\"/>")

  expect_equal(xmlGetAttr(fit_pmml[[3]][[2]][[2]], name = "name"), "boostValue")
  expect_equal(xmlGetAttr(fit_pmml[[3]][[2]][[2]], name = "feature"), "transformedValue")
  expect_equal(xmlGetAttr(fit_pmml[[3]][[2]][[2]], name = "dataType"), "double")
  expect_equal(xmlGetAttr(fit_pmml[[3]][[2]][[2]], name = "optype"), "continuous")

  expect_equal(toString(fit_pmml[[3]][[2]][[3]]), "<OutputField name=\"Predicted_Adjusted\" feature=\"transformedValue\" dataType=\"double\">\n <Apply function=\"if\">\n  <Apply function=\"lessThan\">\n   <FieldRef field=\"boostValue\"/>\n   <Constant>0.0</Constant>\n  </Apply>\n  <Constant>0</Constant>\n  <Constant>1</Constant>\n </Apply>\n</OutputField>")
  expect_equal(toString(fit_pmml[[3]][[2]][[4]]), "<OutputField name=\"Probability_0\" feature=\"transformedValue\" dataType=\"double\">\n <Apply function=\"/\">\n  <Constant>1.0</Constant>\n  <Apply function=\"+\">\n   <Constant>1.0</Constant>\n   <Apply function=\"exp\">\n    <Apply function=\"*\">\n     <Constant>2.0</Constant>\n     <FieldRef field=\"boostValue\"/>\n    </Apply>\n   </Apply>\n  </Apply>\n </Apply>\n</OutputField>")
  expect_equal(toString(fit_pmml[[3]][[2]][[5]]), "<OutputField name=\"Probability_1\" feature=\"transformedValue\" dataType=\"double\">\n <Apply function=\"-\">\n  <Constant>1.0</Constant>\n  <FieldRef field=\"Probability_0\"/>\n </Apply>\n</OutputField>")
  expect_error(toString(fit_pmml[[3]][[2]][[6]]))

  # Test Segmentation node attribute
  expect_equal(xmlGetAttr(fit_pmml[[3]][[4]], name = "multipleModelMethod"), "weightedAverage")
})
