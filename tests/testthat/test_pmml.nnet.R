library(nnet)
data(iris)

# teardown({detach("package:nnet", unload=TRUE)})

test_that("error when object is not nnet", {
  expect_error(pmml.nnet("foo"), "Not a legitimate nnet object")
})

test_that("No error for formula input", {
  fit_3 <- nnet(Species ~ ., data = iris, size = 4, trace = FALSE)
  expect_error(pmml.nnet(fit_3), NA)
})

test_that("No error when number of output neurons is 1", {
  fit_4 <- nnet(Sepal.Width ~ Petal.Length + Petal.Width,
    data = iris,
    size = 3, trace = FALSE
  )
  expect_error(pmml.nnet(fit_4), NA)
})


test_that("No error for matrix input", {
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
  data(audit)
  skip("skip until export issue is resolved")

  x <- dat[1:200,c(7,9)] # dat[1:200,c(2,3,4,7)]
  y <- dat$Adjusted[1:200] #as.data.frame(dat$Adjusted[1:200]) # dat$Adjusted
  fit <- nnet(x = x, y = y, size = 3)
  pmml_fit <- pmml(fit)
})

test_that("PMML is exported correctly when training data has factors", {
  data(audit)
  fit <- nnet(Adjusted ~ ., data = audit, size = 3)
  pmml_fit <- pmml(fit)
  
  expect_equal(toString(pmml_fit[[2]][[4]]), "<DataField name=\"Employment\" optype=\"categorical\" dataType=\"string\">\n <Value value=\"Consultant\"/>\n <Value value=\"Private\"/>\n <Value value=\"PSFederal\"/>\n <Value value=\"PSLocal\"/>\n <Value value=\"PSState\"/>\n <Value value=\"SelfEmp\"/>\n <Value value=\"Volunteer\"/>\n</DataField>")
  expect_equal(toString(pmml_fit[[3]][[1]]), "<MiningSchema>\n <MiningField name=\"Adjusted\" usageType=\"predicted\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"ID\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Age\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Employment\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Education\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Marital\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Occupation\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Income\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Sex\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Deductions\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Hours\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Accounts\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n <MiningField name=\"Adjustment\" usageType=\"active\" invalidValueTreatment=\"returnInvalid\"/>\n</MiningSchema>")
})


