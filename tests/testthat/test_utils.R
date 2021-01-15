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
