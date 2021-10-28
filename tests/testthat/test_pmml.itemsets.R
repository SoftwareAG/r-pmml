library(arules)

test_that("error when object is not itemsets", {
  expect_error(pmml.itemsets("foo"), "Not a legitimate arules itemsets rules object")
})


test_that("pmml.itemsets produces correct PMML", {
  # Example from arules::itemsets
  data("Adult")
  
  # invisible(capture.output(fit <- apriori(Adult, parameter = list(support = 0.3))))
  
  invisible(capture.output(fit <- new("itemsets", items = encode(list(
                       c("age=Young", "relationship=Unmarried"), c("age=Old")),
                       itemLabels = itemLabels(Adult))
  )))
  
  quality(fit) <- data.frame(support = interestMeasure(fit, measure = c("support"), transactions = Adult))
  
  fit_pmml <- pmml(fit)
  
  expect_equal(xmlGetAttr(fit_pmml[[1]], name = "description"), "Frequent Itemsets Model")
  
  expect_equal(toString(fit_pmml[[2]]), "<DataDictionary numberOfFields=\"2\">\n <DataField name=\"transaction\" optype=\"categorical\" dataType=\"string\"/>\n <DataField name=\"item\" optype=\"categorical\" dataType=\"string\"/>\n</DataDictionary>") 
  expect_equal(toString(fit_pmml[[3]][[1]]), "<MiningSchema>\n <MiningField name=\"transaction\" usageType=\"group\"/>\n <MiningField name=\"item\" usageType=\"active\"/>\n</MiningSchema>")
  
  expect_equal(xmlGetAttr(fit_pmml[[3]], name = "functionName"), "associationRules")
  expect_equal(xmlGetAttr(fit_pmml[[3]], name = "numberOfItems"), "115")
  expect_equal(xmlGetAttr(fit_pmml[[3]], name = "minimumSupport"), "0.010503255394947")
  expect_equal(xmlGetAttr(fit_pmml[[3]], name = "minimumConfidence"), "0")
  expect_equal(xmlGetAttr(fit_pmml[[3]], name = "numberOfItemsets"), "2")
  expect_equal(xmlGetAttr(fit_pmml[[3]], name = "numberOfRules"), "0")
})