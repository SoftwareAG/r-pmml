# Additional tests with transformations

library(rpart)
# library(zementisr)

data(iris)
data(audit)
audit_factor <- audit
audit_factor[, 13] <- as.factor(audit_factor[, 13])
iris_p <- read.csv("iris.csv", stringsAsFactors = TRUE)
factor_40k <- read.csv("factor_40k.csv", stringsAsFactors = TRUE)
numeric_10k <- na.omit(read.csv("numeric_10k.csv", stringsAsFactors = TRUE))
factor_10k <- read.csv("factor_10k.csv", stringsAsFactors = TRUE)
numeric_no_na_10k <- read.csv("numeric_no_na_10k.csv", stringsAsFactors = TRUE)

expect_equal_nn <- function(...) {
  # expect_equal without name checking
  expect_equal(..., check.names = FALSE)
}

test_that("Transformations PMML output matches R", {
  skip_on_cran()
  skip_on_ci()
  
  library(zementisr)
  
  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_function(box_obj,
                            orig_field_name = "sepal_length",
                            new_field_name = "a_derived_field",
                            expression = "sqrt(sepal_length^2 + 3)"
  )
  box_obj <- xform_function(box_obj,
                            orig_field_name = list("sepal_length, sepal_width"),
                            new_field_name = "two_field_formula",
                            expression = "sepal_length * sepal_width"
  )
  fit <- lm(petal_width ~ ., data = box_obj$data)
  p_fit <- pmml(fit, transform = box_obj)
  r_pred <- predict(fit, newdata = box_obj$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_petal_width, r_pred)
  
  
  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_min_max(box_obj, "1")
  box_obj <- xform_z_score(box_obj, "1", map_missing_to = 999)
  box_obj <- xform_norm_discrete(box_obj, input_var = "class")
  box_obj <- xform_function(box_obj,
                            orig_field_name = "sepal_width",
                            new_field_name = "a_derived_field",
                            expression = "sqrt(sepal_width^2 - 3)"
  )
  fit <- lm(petal_width ~ ., data = box_obj$data)
  p_fit <- pmml(fit, transform = box_obj)
  # suppress warning: "prediction from a rank-deficient fit may be misleading"
  r_pred <- suppressWarnings(predict(fit, newdata = box_obj$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_petal_width, r_pred)
  
  
  box_obj <- xform_wrap(iris_p)
  box_obj <- xform_discretize(box_obj,
                              xform_info = "[petal_width->dis_pw][double->string]",
                              table = "iris_discretize_pw.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
                              xform_info = "[sepal_length->dis_sl][double->string]",
                              table = "iris_discretize_sl.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_discretize(box_obj,
                              xform_info = "[sepal_width->dis_sw][double->string]",
                              table = "iris_discretize_sw.csv", map_missing_to = "0", default_value = "1"
  )
  box_obj <- xform_map(box_obj,
                       xform_info = "[class->d_class][string->double]",
                       table = "iris_p_class_table.csv", default_value = "-1", map_missing_to = "1"
  )
  fit <- lm(petal_length ~ ., data = box_obj$data[, -c(2, 3, 4, 5, 7)])
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred <- predict(fit, box_obj$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_p, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_petal_length, r_pred)
  
  
  box_obj <- xform_wrap(audit_factor)
  box_obj <- xform_z_score(box_obj, xform_info = "column2->d_Age")
  box_obj <- xform_z_score(box_obj, xform_info = "column7->d_Income")
  box_obj <- xform_z_score(box_obj, xform_info = "column9->d_Deductions")
  box_obj <- xform_z_score(box_obj, xform_info = "column10->d_Hours")
  box_obj <- xform_min_max(box_obj, xform_info = "d_Age->dd_Age")
  box_obj <- xform_min_max(box_obj, xform_info = "d_Income->dd_Income")
  box_obj <- xform_min_max(box_obj, xform_info = "d_Deductions->dd_Deductions")
  box_obj <- xform_min_max(box_obj, xform_info = "d_Hours->dd_Hours")
  box_obj <- xform_z_score(box_obj, xform_info = "dd_Age->ddd_Age")
  box_obj <- xform_z_score(box_obj, xform_info = "dd_Income->ddd_Income")
  box_obj <- xform_z_score(box_obj, xform_info = "dd_Deductions->ddd_Deductions")
  box_obj <- xform_z_score(box_obj, xform_info = "dd_Hours->ddd_Hours")
  box_obj <- xform_norm_discrete(box_obj, input_var = "Employment")
  box_obj <- xform_map(box_obj,
                       xform_info = "[Marital-> d_Marital][string->double]",
                       table = "audit_marital_table.csv", default_value = "-1", map_missing_to = "1"
  )
  fit <- rpart(Adjusted ~ ., data = box_obj$data[, -1])
  p_fit <- pmml(fit, transforms = box_obj)
  r_pred_class <- as.character(predict(fit, box_obj$data, type = "class"))
  r_pred_prob <- predict(fit, box_obj$data, type = "prob")[, 1:2]
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_factor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred_class)
  expect_equal_nn(z_pred$outputs$Probability_0, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred_prob[, 2])
  
  
  factor_40k_box <- xform_wrap(factor_40k)
  factor_40k_box <- xform_norm_discrete(factor_40k_box, xform_info = "CateA")
  factor_40k_box <- xform_norm_discrete(factor_40k_box, xform_info = "CateB")
  fit <- rpart(letter ~ ., data = factor_40k_box$data[, -c(2, 3)])
  p_fit <- pmml(fit, transforms = factor_40k_box)
  r_pred_class <- as.character(predict(fit, factor_40k_box$data, type = "class"))
  r_pred_prob <- predict(fit, factor_40k_box$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(factor_40k, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_letter, r_pred_class)
  expect_equal_nn(z_pred$outputs$Probability_A, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$Probability_B, r_pred_prob[, 2])
  
  
  numeric_10k_box <- xform_wrap(numeric_10k)
  numeric_10k_box <- xform_min_max(numeric_10k_box,
                                   xform_info = "var_10->d_var_10", map_missing_to = "0"
  )
  numeric_10k_box <- xform_min_max(numeric_10k_box,
                                   xform_info = "var_11->d_var_11", map_missing_to = "0"
  )
  numeric_10k_box <- xform_min_max(numeric_10k_box,
                                   xform_info = "var_12->d_var_12", map_missing_to = "0"
  )
  numeric_10k_box <- xform_min_max(numeric_10k_box,
                                   xform_info = "var_13->d_var_13", map_missing_to = "0"
  )
  fit <- lm(var_14 ~ ., data = numeric_10k_box$data)
  p_fit <- pmml(fit, transforms = numeric_10k_box)
  # Suppress warning: "prediction from a rank-deficient fit may be misleading"
  r_pred <- suppressWarnings(as.numeric(predict(fit, numeric_10k_box$data)))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(numeric_10k, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_var_14, r_pred, tolerance = 1e-5)
  
  
  numeric_10k_box <- xform_wrap(numeric_10k)
  numeric_10k_box <- xform_z_score(numeric_10k_box, xform_info = "var_0->d_var_0", map_missing_to = "0")
  numeric_10k_box <- xform_z_score(numeric_10k_box, xform_info = "var_1->d_var_1", map_missing_to = "0")
  numeric_10k_box <- xform_z_score(numeric_10k_box, xform_info = "var_2->d_var_2", map_missing_to = "0")
  numeric_10k_box <- xform_z_score(numeric_10k_box, xform_info = "var_3->d_var_3", map_missing_to = "0")
  fit <- lm(var_14 ~ ., data = numeric_10k_box$data)
  p_fit <- pmml(fit, transforms = numeric_10k_box)
  # Suppress warning: "prediction from a rank-deficient fit may be misleading"
  r_pred <- suppressWarnings(as.numeric(predict(fit, numeric_10k_box$data)))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(numeric_10k, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_var_14, r_pred, tolerance = 1e-5)
  
  
  factor_10k_box <- xform_wrap(factor_10k)
  factor_10k_box <- xform_norm_discrete(factor_10k_box, input_var = "CateA")
  factor_10k_box <- xform_norm_discrete(factor_10k_box, input_var = "CateB")
  fit <- rpart(letter ~ ., data = factor_10k_box$data[, -c(2, 3)])
  p_fit <- pmml(fit, transforms = factor_10k_box)
  r_pred_class <- as.character(predict(fit, factor_10k_box$data, type = "class"))
  r_pred_prob <- predict(fit, factor_10k_box$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(factor_10k, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_letter, r_pred_class)
  expect_equal_nn(z_pred$outputs$Probability_A, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$Probability_B, r_pred_prob[, 2])
  expect_equal_nn(z_pred$outputs$Probability_C, r_pred_prob[, 3])
  
  
  a <- which(factor_10k[, 1] == "A")
  b <- which(factor_10k[, 1] == "B")
  y <- which(factor_10k[, 1] == "Y")
  z <- which(factor_10k[, 1] == "Z")
  factor_10k_smp <- factor_10k[sample(c(a, b, y, z), length(c(a, b, y, z))), ]
  factor_10k_smp[, 1] <- as.character(factor_10k_smp[, 1])
  levels(factor_10k_smp[, 1]) <- c("A", "B", "Y", "Z")
  factor_10k_smp[, 1] <- as.factor(factor_10k_smp[, 1])
  factor_10k_box <- xform_wrap(factor_10k_smp)
  factor_10k_box <- xform_map(factor_10k_box,
                              xform_info = "[letter,CateA->d_CateB][string,string->string]",
                              table = "map_factor_400.csv", default_value = "-1", map_missing_to = "1"
  )
  fit <- rpart(letter ~ ., data = factor_10k_box$data[, -2])
  p_fit <- pmml(fit, transforms = factor_10k_box)
  r_pred_class <- as.character(predict(fit, factor_10k_box$data, type = "class"))
  r_pred_prob <- predict(fit, factor_10k_box$data)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(factor_10k_smp, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_letter, r_pred_class)
  expect_equal_nn(z_pred$outputs$Probability_A, r_pred_prob[, 1])
  expect_equal_nn(z_pred$outputs$Probability_B, r_pred_prob[, 2])
  expect_equal_nn(z_pred$outputs$Probability_Y, r_pred_prob[, 3])
  
  
  numeric_no_na_10k_box <- xform_wrap(numeric_no_na_10k)
  numeric_no_na_10k_box <- xform_discretize(numeric_no_na_10k_box,
                                            xform_info = "[var_0->d_var_0][double->integer]",
                                            table = "numeric_discretize_var.csv",
                                            map_missing_to = "0", default_value = "1"
  )
  numeric_no_na_10k_box <- xform_discretize(numeric_no_na_10k_box,
                                            xform_info = "[var_1->d_var_1][double->integer]",
                                            table = "numeric_discretize_var.csv",
                                            map_missing_to = "0", default_value = "1"
  )
  numeric_no_na_10k_box <- xform_discretize(numeric_no_na_10k_box,
                                            xform_info = "[var_2->d_var_2][double->integer]",
                                            table = "numeric_discretize_var.csv",
                                            map_missing_to = "0", default_value = "1"
  )
  numeric_no_na_10k_box <- xform_discretize(numeric_no_na_10k_box,
                                            xform_info = "[var_3->d_var_3][double->integer]",
                                            table = "numeric_discretize_var.csv",
                                            map_missing_to = "0", default_value = "1"
  )
  fit <- lm(var_14 ~ ., data = numeric_no_na_10k_box$data[1:600, ])
  p_fit <- pmml(fit, transforms = numeric_no_na_10k_box)
  r_pred <- as.numeric(predict(fit, numeric_no_na_10k_box$data[601:1000, ]))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(numeric_no_na_10k[601:1000, ], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_var_14, r_pred, tolerance = 1e-4)
  
  
  numeric_no_na_10k_box <- xform_wrap(numeric_no_na_10k)
  numeric_no_na_10k_box <- xform_min_max(numeric_no_na_10k_box,
                                         xform_info = "var_0->d_var_0", map_missing_to = "0"
  )
  numeric_no_na_10k_box <- xform_min_max(numeric_no_na_10k_box,
                                         xform_info = "var_1->d_var_1", map_missing_to = "0"
  )
  numeric_no_na_10k_box <- xform_min_max(numeric_no_na_10k_box,
                                         xform_info = "var_2->d_var_2", map_missing_to = "0"
  )
  numeric_no_na_10k_box <- xform_min_max(numeric_no_na_10k_box,
                                         xform_info = "var_3->d_var_3", map_missing_to = "0"
  )
  fit <- lm(var_14 ~ ., data = numeric_no_na_10k_box$data)
  p_fit <- pmml(fit, transforms = numeric_no_na_10k_box)
  # Suppress warning: "prediction from a rank-deficient fit may be misleading
  r_pred <- suppressWarnings(as.numeric(predict(fit, numeric_no_na_10k_box$data)))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(numeric_no_na_10k, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_var_14, r_pred, tolerance = 1e-5)
  
  
  numeric_no_na_10k_box <- xform_wrap(numeric_no_na_10k)
  numeric_no_na_10k_box <- xform_z_score(numeric_no_na_10k_box,
                                         xform_info = "var_0->d_var_0"
  )
  numeric_no_na_10k_box <- xform_z_score(numeric_no_na_10k_box,
                                         xform_info = "var_1->d_var_1", map_missing_to = "0"
  )
  numeric_no_na_10k_box <- xform_z_score(numeric_no_na_10k_box,
                                         xform_info = "var_2->d_var_2", map_missing_to = "0"
  )
  numeric_no_na_10k_box <- xform_z_score(numeric_no_na_10k_box,
                                         xform_info = "var_3->d_var_3", map_missing_to = "0"
  )
  fit <- lm(var_14 ~ ., data = numeric_no_na_10k_box$data)
  p_fit <- pmml(fit, transforms = numeric_no_na_10k_box)
  # Suppress warning: "prediction from a rank-deficient fit may be misleading
  r_pred <- suppressWarnings(as.numeric(predict(fit, numeric_no_na_10k_box$data)))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(numeric_no_na_10k, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_var_14, r_pred, tolerance = 1e-5)
  
  
  numeric_10k_box <- xform_wrap(numeric_10k)
  numeric_10k_box <- xform_discretize(numeric_10k_box,
                                      xform_info = "[var_0->d_var_0][double->integer]",
                                      table = "numeric_discretize_var.csv", map_missing_to = "0", default_value = "1"
  )
  numeric_10k_box <- xform_discretize(numeric_10k_box,
                                      xform_info = "[var_1->d_var_1][double->integer]",
                                      table = "numeric_discretize_var.csv", map_missing_to = "0", default_value = "1"
  )
  numeric_10k_box <- xform_discretize(numeric_10k_box,
                                      xform_info = "[var_2->d_var_2][double->integer]",
                                      table = "numeric_discretize_var.csv", map_missing_to = "0", default_value = "1"
  )
  numeric_10k_box <- xform_discretize(numeric_10k_box,
                                      xform_info = "[var_3->d_var_3][double->integer]",
                                      table = "numeric_discretize_var.csv", map_missing_to = "0", default_value = "1"
  )
  fit <- lm(var_14 ~ ., data = numeric_10k_box$data[1:500, ])
  p_fit <- pmml(fit, transforms = numeric_10k_box)
  r_pred <- as.numeric(predict(fit, numeric_10k_box$data[501:994, ]))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(numeric_10k[501:994, ], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_var_14, r_pred, tolerance = 1e-4)
})

test_that("Transformations PMML matches R 2", {
  skip_on_cran()
  skip_on_ci()

  library(randomForest)
  library(zementisr)
  iris_box_1 <- xform_wrap(iris)

  iris_box_1 <- xform_function(wrap_object = iris_box_1,
                               orig_field_name = "Sepal.Width",
                               new_field_name = "Sepal.Width.Transformed",
                               new_field_data_type = "numeric",
                               expression = "Sepal.Width + 3.5"
  )

  iris_box_1 <- xform_function(wrap_object = iris_box_1,
                               orig_field_name = "Sepal.Length",
                               new_field_name = "Sepal.Length.Transformed_num",
                               new_field_data_type = "numeric",
                               expression = "Sepal.Length * 0.1"
  )

  set.seed(321)
  fit <- randomForest(Petal.Length ~ Sepal.Length.Transformed_num + Sepal.Width.Transformed, data = iris_box_1$data, ntree = 1)
  p_fit <- pmml(fit, transforms = iris_box_1)
  r_pred <- as.numeric(predict(fit, newdata = iris_box_1$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Petal.Length, r_pred, tolerance = 1e-4)

})

test_that("Using box data with no transforms matches", {
  skip_on_cran()
  skip_on_ci()
  
  library(zementisr)
  
  iris_box_1 <- xform_wrap(iris)
  fit <- lm(Petal.Length ~ Species, data = iris_box_1$data)
  
  p_fit <- pmml(fit, transforms = iris_box_1)
  r_pred <- as.numeric(predict(fit, iris_box_1$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Petal.Length, r_pred, tolerance = 1e-4)
  
  # save_pmml(p_fit, "../../../temp/iris_lm_empty_transf.pmml")
  # pred_df <- iris
  # pred_df$Predicted_Petal.Length <- r_pred
  # write.csv(pred_df, "../../../temp/iris_empty_transf.csv", row.names = FALSE)
})




test_that("Transformation preserves numeric when input is unchanged", {
  skip_on_cran()
  skip_on_ci()
  
  library(zementisr)
  
  iris_box_1 <- xform_wrap(iris)
  iris_box_1 <- xform_function(wrap_object = iris_box_1,
                               orig_field_name = "Sepal.Length",
                               new_field_name = "Sepal.Length_transf",
                               new_field_data_type = "numeric",
                               expression = "Sepal.Length"
  )
  
  fit <- lm(Petal.Length ~ Sepal.Length_transf, data = iris_box_1$data)
  
  p_fit <- pmml(fit, transforms = iris_box_1)
  r_pred <- as.numeric(predict(fit, iris_box_1$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Petal.Length, r_pred, tolerance = 1e-4)
  
  # save_pmml(p_fit, "../../../temp/iris_lm_numeric.pmml")
  # pred_df <- iris
  # pred_df$Predicted_Petal.Length <- r_pred
  # write.csv(pred_df, "../../../temp/iris_lm_numeric.csv", row.names = FALSE)
})

test_that("Transformation preserves factor when input is unchanged", {
  skip_on_cran()
  skip_on_ci()
  
  library(zementisr)
  
  iris_box_1 <- xform_wrap(iris)
  iris_box_1 <- xform_function(wrap_object = iris_box_1,
                               orig_field_name = "Species",
                               new_field_name = "Species_transf",
                               new_field_data_type = "factor",
                               expression = "Species"
  )
  
  fit <- lm(Petal.Length ~ Species_transf, data = iris_box_1$data)
  
  p_fit <- pmml(fit, transforms = iris_box_1)
  r_pred <- as.numeric(predict(fit, iris_box_1$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Petal.Length, r_pred, tolerance = 1e-4)

  # save_pmml(p_fit, "../../../temp/iris_lm_factor.pmml")
  # pred_df <- iris
  # pred_df$Predicted_Petal.Length <- r_pred
  # write.csv(pred_df, "../../../temp/iris_lm_factor.csv", row.names = FALSE)
})

test_that("Transformation preserves factor names when input is numeric", {
  # Skip - this test fails because R removes trailing zeroes when converting
  # numeric to factor.
  skip("skip")
  skip_on_cran()
  skip_on_ci()
  
  library(zementisr)
  iris_box_5 <- xform_wrap(iris)
  iris_box_5 <- xform_function(wrap_object = iris_box_5,
                               orig_field_name = "Sepal.Length",
                               new_field_name = "Sepal.Length_transf",
                               new_field_data_type = "factor",
                               expression = "Sepal.Length")
  
  fit <- lm(Petal.Length ~ Sepal.Length_transf, data = iris_box_5$data)
  
  p_fit <- pmml(fit, transforms = iris_box_5)
  r_pred <- as.numeric(predict(fit, iris_box_5$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Petal.Length, r_pred, tolerance = 1e-4)
  
  # save_pmml(p_fit, "../../../temp/iris_lm_num_to_factor.pmml")
  # pred_df <- iris
  # pred_df$Predicted_Petal.Length <- r_pred
  # write.csv(pred_df, "../../../temp/iris_lm_num_to_factor.csv", row.names = FALSE)
  
})





test_that("PMML matches R with multiple xform_function transformations - 1", {
  skip_on_cran()
  skip_on_ci()
  
  library(zementisr)

  iris_box_1 <- xform_wrap(iris)

  iris_box_1 <- xform_function(wrap_object = iris_box_1,
                               orig_field_name = "Sepal.Length",
                               new_field_name = "Sepal.Length.T2",
                               new_field_data_type = "factor",
                               expression = "if(Sepal.Length < 5){'less'} else {'more'}"
  )

  iris_box_1 <- xform_function(wrap_object = iris_box_1,
                               orig_field_name = "Sepal.Width",
                               new_field_name = "Sepal.Width.Transformed",
                               new_field_data_type = "numeric",
                               expression = "Sepal.Width + 3.5"
  )

  fit <- lm(Petal.Length ~ Species + Sepal.Length.T2 + Sepal.Width.Transformed, data = iris_box_1$data)
  
  p_fit <- pmml(fit, transforms = iris_box_1)
  r_pred <- as.numeric(predict(fit, iris_box_1$data))
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Petal.Length, r_pred, tolerance = 1e-4)
  
  # save_pmml(p_fit, "../../../temp/iris_lm.pmml")
  # pred_df <- iris
  # pred_df$Predicted_Petal.Length <- r_pred
  # write.csv(pred_df, "../../../temp/iris_lm.csv", row.names = FALSE)
  
})