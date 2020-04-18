library(randomForestSRC)
data(veteran)

test_that("error when object is not iForest", {
  expect_error(pmml.iForest("foo"), "Not a legitimate iForest object")
})


test_that("no error occurs in doc example", {
  
  skip_on_cran() # CRAN only runs test against current version of randomForestSRC
  
  veteran_mod <- rfsrc(Surv(time, status) ~ ., data = veteran,
                       ntree = 5, forest = TRUE, membership = TRUE)
  expect_error(pmml(veteran_mod), NA) # expect no error
})
