library(arules)

test_that("error when object is not arules", {
  expect_error(pmml.rules("foo"), "Not a legitimate arules rules object")
})


test_that("pmml.rules produces correct PMML", {
  # Example from arules::rules documentation
  data("Adult")

  invisible(capture.output(fit <- apriori(Adult, parameter = list(support = 0.3))))
  
  fit_pmml <- pmml(fit)
  
  expect_equal(xmlGetAttr(fit_pmml[[1]], name = "description"), "Association Rules Model")
  
  expect_equal(toString(fit_pmml[[2]]), "<DataDictionary numberOfFields=\"2\">\n <DataField name=\"transaction\" optype=\"categorical\" dataType=\"string\"/>\n <DataField name=\"item\" optype=\"categorical\" dataType=\"string\"/>\n</DataDictionary>") 
  expect_equal(toString(fit_pmml[[3]][[1]]), "<MiningSchema>\n <MiningField name=\"transaction\" usageType=\"group\"/>\n <MiningField name=\"item\" usageType=\"active\"/>\n</MiningSchema>")
  
  expect_equal(xmlGetAttr(fit_pmml[[3]], name = "functionName"), "associationRules")
  expect_equal(xmlGetAttr(fit_pmml[[3]], name = "numberOfTransactions"), "48842")
  expect_equal(xmlGetAttr(fit_pmml[[3]], name = "numberOfItems"), "115")
  expect_equal(xmlGetAttr(fit_pmml[[3]], name = "minimumSupport"), "0.3")
  expect_equal(xmlGetAttr(fit_pmml[[3]], name = "minimumConfidence"), "0.8")
  expect_equal(xmlGetAttr(fit_pmml[[3]], name = "numberOfItemsets"), "209")
  
  
})