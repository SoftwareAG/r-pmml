data(audit)

test_that("xform_map produces correct mapping for a data point", {
  audit_box <- xform_wrap(audit)
  t <- list()
  m <- data.frame(
    c("Sex", "string", "Male", "Female"), c("Employment", "string", "PSLocal", "PSState"),
    c("d_sex", "integer", 1, 0)
  )
  t[[1]] <- m
  audit_box <- xform_map(audit_box, xform_info = t, default_value = c(3), map_missing_to = 2)

  expect_equal(audit_box$field_data["d_sex", "transform"], "MapValues")
  expect_equal(audit_box$field_data["d_sex", "default"], 3)
  expect_equal(audit_box$field_data["d_sex", "missingValue"], 2)
  expect_true(audit_box$data$d_sex[[1]] == 3)
})

test_that("PMML from xform_map contains correct local transformation", {
  audit_box <- xform_wrap(audit)
  t <- list()
  m <- data.frame(
    c("Sex", "string", "Male", "Female"), c("Employment", "string", "PSLocal", "PSState"),
    c("d_sex", "integer", 1, 0)
  )
  t[[1]] <- m
  audit_box <- xform_map(audit_box, xform_info = t, default_value = c(3), map_missing_to = 2)

  # linear regression
  fit <- lm(Adjusted ~ ., data = audit_box$data)
  fit_pmml <- pmml(fit, transforms = audit_box)

  expect_equal(toString(fit_pmml[[3]][[3]]), "<LocalTransformations>\n <DerivedField name=\"d_sex\" dataType=\"string\" optype=\"categorical\">\n  <MapValues mapMissingTo=\"2\" defaultValue=\"3\" outputColumn=\"output\">\n   <FieldColumnPair field=\"Sex\" column=\"input1\"/>\n   <FieldColumnPair field=\"Employment\" column=\"input2\"/>\n   <InlineTable>\n    <row>\n     <input1>Male</input1>\n     <input2>PSLocal</input2>\n     <output>1</output>\n    </row>\n    <row>\n     <input1>Female</input1>\n     <input2>PSState</input2>\n     <output>0</output>\n    </row>\n   </InlineTable>\n  </MapValues>\n </DerivedField>\n</LocalTransformations>")
})



test_that("xform_map uses file with xform_info correctly", {
  audit_box <- xform_wrap(audit)

  audit_box <- xform_map(audit_box,
    xform_info = "[Sex -> d_sex][string->integer]",
    table = "map_audit.csv", map_missing_to = "0"
  )

  expect_equal(audit_box$data$d_sex[1:5], c(2, 1, 1, 1, 1))

  f_map <- cbind(c("Sex", "string", "Male", "Female"), c("d_sex", "numeric", "1", "2"))
  expect_equal(audit_box$field_data$fieldsMap[[14]], f_map, check.attributes = FALSE)
})
