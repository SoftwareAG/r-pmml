tmp_file <- tempfile()
teardown(unlink(tmp_file))

test_that("save_pmml() does not output marker for Zementis Server", {
  iris_lm <- lm(Sepal.Length ~ ., data = iris)
  iris_lm_pmml <- pmml(iris_lm)
  save_pmml(iris_lm_pmml, tmp_file)
  loaded_pmml <- xmlParse(file = tmp_file)
  expect_equal(
    capture.output(loaded_pmml)[2],
    "<PMML xmlns=\"http://www.dmg.org/PMML-4_4\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" version=\"4.4.1\" xsi:schemaLocation=\"http://www.dmg.org/PMML-4_4 http://www.dmg.org/pmml/v4-4/pmml-4-4.xsd\">"
  )
})
