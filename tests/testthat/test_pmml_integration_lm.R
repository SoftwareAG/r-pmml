
iris_p <- read.csv("iris.csv", stringsAsFactors = TRUE)
data(audit)
audit <- na.omit(audit)
elnino <- read.csv("elnino.csv", stringsAsFactors = TRUE)

expect_equal_nn <- function(...) {
  # expect_equal without name checking
  expect_equal(..., check.names = FALSE)
}



test_that("RegressionModel/stats PMML output matches R", {
  skip_on_cran()
  skip_on_ci()
  
  library(zementisr)
  
  fit <- lm(Sepal.Length ~ ., data = iris)
  p_fit <- pmml(fit, model_version = "version_001")
  r_pred <- predict(fit, iris)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Sepal.Length, r_pred)
  
  
  fit <- lm(temp ~ ., data = elnino)
  p_fit <- pmml(fit, model_version = NULL)
  r_pred <- predict(fit, elnino)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(elnino, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_temp, r_pred)
  
  
  box_obj <- xform_wrap(audit)
  box_obj <- xform_map(box_obj,
                       xform_info = "[Employment,Education,Sex-> d_E]",
                       table = "audit_3to1_table.csv", default_value = "X", map_missing_to = "Y"
  )
  fit <- lm(Adjusted ~ d_E + Income + Hours, data = box_obj$data)
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- predict(fit, box_obj$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred)
  
  
  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_z_score(box_obj, xform_info = "column1->d1")
  box_obj <- xform_z_score(box_obj, xform_info = "column2->d2")
  box_obj <- xform_z_score(box_obj, xform_info = "column3->d3")
  box_obj <- xform_z_score(box_obj, xform_info = "column4->d4")
  box_obj <- xform_min_max(box_obj, xform_info = "d1->dd1")
  box_obj <- xform_min_max(box_obj, xform_info = "d2->dd2")
  box_obj <- xform_min_max(box_obj, xform_info = "d3->dd3")
  box_obj <- xform_min_max(box_obj, xform_info = "d4->dd4")
  box_obj <- xform_z_score(box_obj, xform_info = "dd1->ddd1")
  box_obj <- xform_z_score(box_obj, xform_info = "dd2->ddd2")
  box_obj <- xform_z_score(box_obj, xform_info = "dd3->ddd3")
  box_obj <- xform_z_score(box_obj, xform_info = "dd4->ddd4")
  box_obj <- xform_norm_discrete(box_obj, input_var = "class")
  box_obj <- xform_map(box_obj,
                       xform_info = "[class->d_class][string->double]",
                       table = "iris_p_class_table.csv", default_value = "-1", map_missing_to = "1"
  )
  fit <- lm(sepal_width ~ ddd1 + ddd2 + ddd3 + d_class + class_Iris_setosa +
              class_Iris_versicolor + class_Iris_virginica, box_obj$data)
  p_fit <- pmml(fit, transform = box_obj)
  r_pred <- predict(fit, data = box_obj$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_sepal_width, r_pred)
  
  
  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_discretize(box_obj,
                              xform_info = "[petal_length->dis_pl][double->string]",
                              table = "iris_discretize_pl.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
                              xform_info = "[petal_width->dis_pw][double->integer]",
                              table = "iris_discretize_pw.csv", map_missing_to = "0", default_value = "1"
  )
  
  fit <- lm(sepal_width ~ dis_pl + dis_pw + class, data = box_obj$data)
  p_fit <- pmml(fit, transform = box_obj)
  r_pred <- predict(fit, data = box_obj$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_sepal_width, r_pred)
  
  
  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_discretize(box_obj,
                              xform_info = "[petal_length->dis_pl][double->string]",
                              table = "iris_discretize_pl.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
                              xform_info = "[petal_width->dis_pw][double->integer]",
                              table = "iris_discretize_pw.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_norm_discrete(box_obj, input_var = "class")
  box_obj <- xform_map(box_obj,
                       xform_info = "[class->d_class][string->double]",
                       table = "iris_p_class_table.csv", default_value = "-1", map_missing_to = "1"
  )
  fit <- lm(sepal_width ~ dis_pl + dis_pw + d_class, data = box_obj$data)
  p_fit <- pmml(fit, transform = box_obj)
  r_pred <- predict(fit, data = box_obj$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_sepal_width, r_pred)
})


test_that("lm model with integer feature - output matches R", {
  # Integer feature is exported with dataType="double" and optype="continuous"
  dat <- iris
  dat$SL_int <- as.integer(iris$Sepal.Length)
  
  fit <- lm(Sepal.Width ~ SL_int + Petal.Length, data = dat)
  p_fit <- pmml(fit, model_version = "model_with_int")
  r_pred <- predict(fit, dat)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(dat, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Sepal.Width, r_pred)

  # save_pmml(p_fit, "../../../temp/iris_lm_int_feature.pmml")
  # pred_df <- dat
  # pred_df$Predicted_Petal.Length <- r_pred
  # write.csv(pred_df, "../../../temp/iris_int_feature.csv", row.names = FALSE)
})

test_that("lm model with logical feature - output matches R", {
  # Logical feature is exported with dataType="boolean" and optype="categorical
  skip("skip")
  dat <- iris
  dat$SL_logi <- unlist(lapply(dat$Sepal.Length, function(x) if (x < 6) {TRUE} else {FALSE}))
  
  fit <- lm(Sepal.Width ~ SL_logi + Petal.Length, data = dat)
  p_fit <- pmml(fit, model_version = "model_with_logi")
  r_pred <- as.numeric(predict(fit, dat))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(dat, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Sepal.Width, r_pred)
  
  # save_pmml(p_fit, "../../../temp/iris_lm_logi_feature.pmml")
  # pred_df <- dat
  # pred_df$Predicted_Sepal.Width <- r_pred
  # write.csv(pred_df, "../../../temp/iris_logi_feature.csv", row.names = FALSE)
})


test_that("lm model with factor feature - output matches R", {
  # Logical feature is exported with dataType="boolean" and optype="categorical"
  dat <- iris
  dat$SL_factor <- unlist(lapply(dat$Sepal.Length, function(x) if (x < 6) {TRUE} else {FALSE}))
  dat$SL_factor <- as.factor(dat$SL_factor)
  
  fit <- lm(Sepal.Width ~ SL_factor + Petal.Length, data = dat)
  p_fit <- pmml(fit, model_version = "model_with_factor")
  r_pred <- as.numeric(predict(fit, dat))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(dat, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Sepal.Width, r_pred)
  
  # save_pmml(p_fit, "../../../temp/iris_lm_factor_feature.pmml")
  # pred_df <- dat
  # pred_df$Predicted_Sepal.Width <- r_pred
  # write.csv(pred_df, "../../../temp/iris_factor_feature.csv", row.names = FALSE)
})
