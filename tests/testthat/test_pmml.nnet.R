data(iris)

# teardown({detach("package:nnet", unload=TRUE)})

test_that("error when object is not nnet", {
  expect_error(pmml.nnet("foo"), "Not a legitimate nnet object")
})

test_that("No error for formula input", {
  skip_if_not_installed("nnet")
  library(nnet)
  
  fit_3 <- nnet(Species ~ ., data = iris, size = 4, trace = FALSE)
  expect_error(pmml.nnet(fit_3), NA)
})

test_that("No error when number of output neurons is 1", {
  skip_if_not_installed("nnet")
  library(nnet)
  
  fit_4 <- nnet(Sepal.Width ~ Petal.Length + Petal.Width,
    data = iris,
    size = 3, trace = FALSE
  )
  expect_error(pmml.nnet(fit_4), NA)
})


test_that("No error for matrix input", {
  skip_if_not_installed("nnet")
  library(nnet)
  
  ir <- rbind(iris3[, , 1], iris3[, , 2], iris3[, , 3])
  targets <- class.ind(c(rep("s", 50), rep("c", 50), rep("v", 50)))
  set.seed(1)
  samp <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
  fit <- nnet(ir[samp, ], targets[samp, ],
    size = 2, rang = 0.1,
    decay = 5e-4, maxit = 5, trace = FALSE
  )

  expect_error(pmml(fit), NA)
})

test_that("No error for data.frame input", {
  skip_if_not_installed("nnet")
  library(nnet)
  
  ir <- as.data.frame(rbind(iris3[, , 1], iris3[, , 2], iris3[, , 3]))
  targets <- as.data.frame(class.ind(c(rep("s", 50), rep("c", 50), rep("v", 50))))
  set.seed(2)
  samp <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
  fit_2 <- nnet(ir[samp, ], targets[samp, ],
    size = 2, rang = 0.1,
    decay = 5e-4, maxit = 6, trace = FALSE
  )

  expect_error(pmml(fit_2), NA)
})

test_that("PMML is exported correctly when input to nnet() is not a formula", {
  skip_if_not_installed("nnet")
  library(nnet)
  
  data(audit)
  skip("skip until export issue is resolved")

  x <- dat[1:200, c(7, 9)]
  y <- dat$Adjusted[1:200]
  fit <- nnet(x = x, y = y, size = 3)
  pmml_fit <- pmml(fit, trace = FALSE)
})

test_that("PMML is exported correctly when training data has factors", {
  skip_if_not_installed("nnet")
  library(nnet)
  
  data(audit)
  fit <- nnet(Adjusted ~ ., data = audit, size = 3, trace = FALSE)
  pmml_fit <- pmml(fit)

  expect_equal(toString(pmml_fit[[2]][[4]]), "<DataField name=\"Employment\" optype=\"categorical\" dataType=\"string\">\n <Value value=\"Consultant\"/>\n <Value value=\"Private\"/>\n <Value value=\"PSFederal\"/>\n <Value value=\"PSLocal\"/>\n <Value value=\"PSState\"/>\n <Value value=\"SelfEmp\"/>\n <Value value=\"Volunteer\"/>\n</DataField>")
  expect_equal(toString(pmml_fit[[3]][[1]]), "<MiningSchema>\n <MiningField name=\"Adjusted\" usageType=\"predicted\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"ID\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Age\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Employment\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Education\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Marital\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Occupation\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Income\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Sex\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Deductions\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Hours\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Accounts\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Adjustment\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n</MiningSchema>")
})

test_that("PMML with 1 output neuron for classification is exported correctly", {
  skip_if_not_installed("nnet")
  library(nnet)
  
  data(audit)
  audit_factor <- audit
  audit_factor$Adjusted <- as.factor(audit_factor$Adjusted)
  fit <- nnet(Adjusted ~ ., data = audit_factor, size = 3, trace = FALSE)
  pmml_fit <- pmml(fit)

  # expect Output element to contain two probability OutputFields
  expect_equal(toString(pmml_fit[[3]][[2]]), "<Output>\n <OutputField name=\"Predicted_Adjusted\" optype=\"categorical\" dataType=\"string\" feature=\"predictedValue\"/>\n <OutputField name=\"Probability_0\" optype=\"continuous\" dataType=\"double\" feature=\"probability\" value=\"0\"/>\n <OutputField name=\"Probability_1\" optype=\"continuous\" dataType=\"double\" feature=\"probability\" value=\"1\"/>\n</Output>")
  # expect that the special-case neural layer is created
  expect_equal(toString(pmml_fit[[3]][[6]]), "<NeuralLayer numberOfNeurons=\"2\" activationFunction=\"threshold\" threshold=\"0.5\">\n <Neuron id=\"82\" bias=\"1.0\">\n  <Con from=\"81\" weight=\"-1.0\"/>\n </Neuron>\n <Neuron id=\"83\" bias=\"0.0\">\n  <Con from=\"81\" weight=\"1.0\"/>\n </Neuron>\n</NeuralLayer>")
})
