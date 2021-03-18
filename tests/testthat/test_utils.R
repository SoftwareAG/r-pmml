rn <- .pmmlRootNode()

test_that("version attribute in PMML root node has value 4.4.1", {
  expect_equal(xmlGetAttr(rn, name = "version"), "4.4.1")
})

test_that("xsi:schemaLocation in PMML root node points to pmml-4-4", {
  expect_equal(
    xmlGetAttr(rn, name = "xsi:schemaLocation"),
    "http://www.dmg.org/PMML-4_4 http://www.dmg.org/pmml/v4-4/pmml-4-4.xsd"
  )
})


test_that(".pmmlHeader() adds modelVersion attribute when model_version is not NULL", {
  header <- .pmmlHeader(description = "Test model", copyright = NULL, 
                        model_version = "someVersion", app_name = "App Name")
  expect_equal(xmlGetAttr(header, name = "modelVersion"), "someVersion")
  
  header2 <- .pmmlHeader(description = "Test model", copyright = NULL, 
                        model_version = c("someVersion"), app_name = "App Name")
  expect_equal(xmlGetAttr(header2, name = "modelVersion"), "someVersion")
})

test_that(".pmmlHeader() does not add modelVersion attribute when model_version is NULL", {
  header <- .pmmlHeader(description = "Test model", copyright = NULL, 
                        model_version = NULL, app_name = "App Name")
  # XML::xmlNode does not add an attribute if its value is NULL
  expect_equal(xmlGetAttr(header, name = "modelVersion"), NULL)
})

test_that(".pmmlHeader() errors if model_version is not a character vector of length 1", {
  expect_error(.pmmlHeader(description = "Test model", copyright = NULL, 
                        model_version = 123, app_name = "App Name"),
               'model_version must be of type "character" and of length 1.')
  
  expect_error(.pmmlHeader(description = "Test model", copyright = NULL, 
                           model_version = c(1,2,3), app_name = "App Name"),
               'model_version must be of type "character" and of length 1.')  
  
  expect_error(.pmmlHeader(description = "Test model", copyright = NULL, 
                           model_version = c("adf",1), app_name = "App Name"),
               'model_version must be of type "character" and of length 1.')

  expect_error(.pmmlHeader(description = "Test model", copyright = NULL, 
                           model_version = c("adf","asdf"), app_name = "App Name"),
               'model_version must be of type "character" and of length 1.')
  
})
