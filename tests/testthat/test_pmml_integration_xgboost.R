data(iris)
data(audit)
audit_factor <- audit
audit_factor[, 13] <- as.factor(audit_factor[, 13])


expect_equal_nn <- function(...) {
  # expect_equal without name checking
  expect_equal(..., check.names = FALSE)
}

# Temp files for xgboost
xgb_tmp_01_save <- tempfile()
xgb_tmp_01_dump <- tempfile()


teardown(unlink(c(xgb_tmp_01_save, xgb_tmp_01_dump), recursive = TRUE))


test_that("MiningModel/xgboost PMML output matches R", {
  skip_on_cran()
  skip_on_ci()

  skip_if_not_installed("xgboost")
  library(xgboost)
  library(zementisr)

  invisible(capture.output(fit <- xgboost(
    data = as.matrix(iris[, 1:4]), label = as.numeric(iris[, 5]) - 1,
    max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "multi:softprob", num_class = 3,
    save_name = xgb_tmp_01_save
  )))
  xgb.dump(fit, xgb_tmp_01_dump)

  p_fit <- pmml(
    model = fit, input_feature_names = colnames(iris[, 1:4]), output_label_name = "Species",
    output_categories = c(1, 2, 3), xgb_dump_file = xgb_tmp_01_dump
  )
  r_pred_prob <- as.data.frame(matrix(predict(fit, newdata = as.matrix(iris[, 1:4])),
    nrow = 150, byrow = T
  ), row.names = F, stringsAsFactors = TRUE)
  r_pred_class <- sapply(1:150, function(i) {
    which(r_pred_prob[i, ] == max(r_pred_prob[i, ]))
  })
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris[, 1:4], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred_prob$V1, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Probability_2, r_pred_prob$V2, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Probability_3, r_pred_prob$V3, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Predicted_Species, as.character(r_pred_class), tolerance = 1e-7)


  invisible(capture.output(fit <- xgboost(
    data = as.matrix(audit_factor[, c(2, 7, 9, 10, 12)]),
    label = as.numeric(audit_factor[, 13]) - 1, max_depth = 2, nrounds = 2,
    objective = "binary:logistic",
    eval_metric = "error",
    save_name = xgb_tmp_01_save
  )))
  xgb.dump(fit, xgb_tmp_01_dump)
  p_fit <- pmml(fit,
    input_feature_names = colnames(audit_factor[, c(2, 7, 9, 10, 12)]), output_label_name = "Adjusted",
    output_categories = c(0, 1), xgb_dump_file = xgb_tmp_01_dump
  )

  r_pred_prob <- predict(fit, as.matrix(audit_factor[, c(2, 7, 9, 10, 12)]))
  r_pred_class <- sapply(r_pred_prob, function(x) {
    if (x > .5) {
      "1"
    } else {
      "0"
    }
  })
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_factor[, c(2, 7, 9, 10, 12)], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred_prob, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred_class)


  sparse_mat <- as.matrix(sparse.model.matrix(Adjusted ~ . - 1, data = audit[, c("Marital", "Sex", "Adjusted")]))
  invisible(capture.output(fit <- xgboost(
    data = sparse_mat, label = audit[, c("Adjusted")], max_depth = 2,
    eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic",
    eval_metric = "error",
    save_name = xgb_tmp_01_save
  )))
  xgb.dump(fit, xgb_tmp_01_dump)
  p_fit <- pmml(fit,
    input_feature_names = colnames(sparse_mat), output_label_name = "Adjusted",
    output_categories = c(0, 1), xgb_dump_file = xgb_tmp_01_dump
  )
  r_pred_prob <- predict(fit, sparse_mat)
  r_pred_class <- sapply(r_pred_prob, function(x) {
    if (x > .5) {
      "1"
    } else {
      "0"
    }
  })
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(as.data.frame(sparse_mat, stringsAsFactors = TRUE), up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred_prob, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred_class)


  # The next 5 tests check that the naming convention where field name strings are
  # subsets of each other does not cause issues. E.g., V1 is a subset of V11 and V112.
  iris_string_subsets <- iris[1:100, ]
  iris_string_subsets[, 5] <- as.factor(as.character(iris_string_subsets[, 5]))
  colnames(iris_string_subsets) <- c("V11", "V112", "V128", "V22", "V1")
  invisible(capture.output(fit <- xgboost(
    data = as.matrix(iris_string_subsets[, 1:4]),
    label = as.numeric(iris_string_subsets[, 5]) - 1,
    max_depth = 3, eta = 1, nthread = 1, nrounds = 3, objective = "binary:logistic",
    eval_metric = "auc",
    save_name = xgb_tmp_01_save
  )))
  xgb.dump(fit, xgb_tmp_01_dump)
  p_fit <- pmml(fit,
    input_feature_names = colnames(as.matrix(iris_string_subsets[, 1:4])),
    output_label_name = "V1",
    output_categories = c(1, 2),
    xgb_dump_file = xgb_tmp_01_dump
  )

  r_pred <- predict(fit, as.matrix(iris_string_subsets[, 1:4]))
  r_pred_class <- sapply(r_pred, function(x) {
    if (x > 0.5) {
      "2"
    } else {
      "1"
    }
  })
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_string_subsets, up_stat$model_name)
  delete_model(up_stat$model_name)
  # Probability_2 is prob of the label having the 2nd value. E.g., 1 in {0,1} or 2 in {1,2}.
  expect_equal_nn(z_pred$outputs$Probability_2, r_pred, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Predicted_V1, r_pred_class)


  iris_string_subsets <- iris
  colnames(iris_string_subsets) <- c("V11", "V112", "V128", "V1281", "V1")
  invisible(capture.output(fit <- xgboost(
    data = as.matrix(iris_string_subsets[, 1:4]), label = as.numeric(iris_string_subsets[, 5]) - 1,
    max_depth = 4, eta = 1, nthread = 1, nrounds = 3, num_class = 3,
    objective = "multi:softprob",
    save_name = xgb_tmp_01_save
  )))
  xgb.dump(fit, xgb_tmp_01_dump)
  p_fit <- pmml(fit,
    input_feature_names = colnames(as.matrix(iris_string_subsets[, 1:4])), output_label_name = "V1",
    output_categories = c(1, 2, 3), xgb_dump_file = xgb_tmp_01_dump
  )
  r_pred_prob <- as.data.frame(matrix(predict(fit, as.matrix(iris_string_subsets[, 1:4])),
    nrow = 150, byrow = T
  ), row.names = F, stringsAsFactors = TRUE)
  r_pred_class <- sapply(1:150, function(i) {
    which(r_pred_prob[i, ] == max(r_pred_prob[i, ]))
  })
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_string_subsets, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred_prob$V1, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Probability_2, r_pred_prob$V2, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Probability_3, r_pred_prob$V3, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Predicted_V1, as.character(r_pred_class), tolerance = 1e-7)

  # Use larger number of trees (nrounds) so that some are created with no branches.
  invisible(capture.output(fit <- xgboost(
    data = as.matrix(iris_string_subsets[, 1:4]), label = as.numeric(iris_string_subsets[, 5]) - 1,
    max_depth = 4, eta = 1, nthread = 1, nrounds = 18, num_class = 3,
    objective = "multi:softprob",
    save_name = xgb_tmp_01_save
  )))
  xgb.dump(fit, xgb_tmp_01_dump)
  p_fit <- pmml(fit,
    input_feature_names = colnames(as.matrix(iris_string_subsets[, 1:4])), output_label_name = "V1",
    output_categories = c(1, 2, 3), xgb_dump_file = xgb_tmp_01_dump
  )
  r_pred_prob <- as.data.frame(matrix(predict(fit, as.matrix(iris_string_subsets[, 1:4])),
    nrow = 150, byrow = T
  ), row.names = F, stringsAsFactors = TRUE)
  r_pred_class <- sapply(1:150, function(i) {
    which(r_pred_prob[i, ] == max(r_pred_prob[i, ]))
  })
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_string_subsets, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred_prob$V1, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Probability_2, r_pred_prob$V2, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Probability_3, r_pred_prob$V3, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Predicted_V1, as.character(r_pred_class), tolerance = 1e-7)


  # Multinomial model with one tree each
  invisible(capture.output(fit <- xgboost(
    data = as.matrix(iris_string_subsets[, 1:4]), label = as.numeric(iris_string_subsets[, 5]) - 1,
    max_depth = 4, eta = 1, nthread = 1, nrounds = 1, num_class = 3,
    objective = "multi:softprob",
    save_name = xgb_tmp_01_save
  )))
  xgb.dump(fit, xgb_tmp_01_dump)
  p_fit <- pmml(fit,
    input_feature_names = colnames(as.matrix(iris_string_subsets[, 1:4])), output_label_name = "V1",
    output_categories = c(1, 2, 3), xgb_dump_file = xgb_tmp_01_dump
  )

  r_pred_prob <- as.data.frame(matrix(predict(fit, as.matrix(iris_string_subsets[, 1:4])),
    nrow = 150, byrow = T
  ), row.names = F, stringsAsFactors = TRUE)
  r_pred_class <- sapply(1:150, function(i) {
    which(r_pred_prob[i, ] == max(r_pred_prob[i, ]))
  })
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris_string_subsets, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred_prob$V1, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Probability_2, r_pred_prob$V2, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Probability_3, r_pred_prob$V3, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Predicted_V1, as.character(r_pred_class), tolerance = 1e-7)


  iris_matrix <- as.matrix(iris[, 1:4])
  invisible(capture.output(fit <- xgboost(
    data = iris_matrix, label = as.numeric(iris[, 5]) - 1,
    max_depth = 4, eta = 1, nthread = 1, nrounds = 2, num_class = 3,
    objective = "multi:softmax", save_name = xgb_tmp_01_save
  )))
  xgb.dump(fit, xgb_tmp_01_dump)
  p_fit <- pmml(fit,
    input_feature_names = colnames(iris_matrix), output_label_name = "Species",
    output_categories = c(0, 1, 2),
    xgb_dump_file = xgb_tmp_01_dump
  )
  r_pred <- predict(fit, iris_matrix)
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(iris[, 1:4], up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Predicted_Species, as.character(r_pred))

  # with tranformations
  box_obj <- xform_wrap(audit_factor[, c("Marital", "Sex", "Adjusted")])
  box_obj <- xform_norm_discrete(box_obj, xform_info = "Marital")
  box_obj <- xform_norm_discrete(box_obj, xform_info = "Sex")
  output_vector <- as.numeric(audit_factor$Adjusted) - 1
  audit_box_filt <- box_obj$data[!names(box_obj$data) %in% c("Marital", "Sex", "Adjusted")]
  set.seed(234)
  invisible(capture.output(fit <- xgboost(
    data = as.matrix(audit_box_filt),
    label = output_vector, max_depth = 2,
    eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic",
    eval_metric = "error",
    save_name = xgb_tmp_01_save
  )))
  xgb.dump(fit, xgb_tmp_01_dump)
  p_fit <- pmml(fit,
    input_feature_names = colnames(audit_box_filt),
    output_label_name = "Adjusted",
    output_categories = c(0, 1),
    xgb_dump_file = xgb_tmp_01_dump,
    transform = box_obj
  )
  r_pred_prob <- predict(fit, as.matrix(audit_box_filt))
  r_pred_class <- sapply(r_pred_prob, function(x) {
    if (x > .5) {
      "1"
    } else {
      "0"
    }
  })
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_factor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred_prob, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred_class)


  box_obj <- xform_wrap(audit_factor[, c("Marital", "Sex", "Adjusted")])
  box_obj <- xform_norm_discrete(box_obj, xform_info = "Marital", levelSeparator = "_")
  box_obj <- xform_norm_discrete(box_obj, xform_info = "Sex", levelSeparator = "_")
  output_vector <- as.numeric(audit_factor$Adjusted) - 1
  audit_box_filt <- box_obj$data[!names(box_obj$data) %in% c("Marital", "Sex", "Adjusted")]
  set.seed(234)
  invisible(capture.output(fit <- xgboost(
    data = as.matrix(audit_box_filt),
    label = output_vector, max_depth = 2,
    eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic",
    eval_metric = "auc",
    save_name = xgb_tmp_01_save
  )))
  xgb.dump(fit, xgb_tmp_01_dump)
  p_fit <- pmml(fit,
    input_feature_names = colnames(audit_box_filt),
    output_label_name = "Adjusted",
    output_categories = c(0, 1),
    xgb_dump_file = xgb_tmp_01_dump,
    transform = box_obj
  )
  r_pred <- predict(fit, as.matrix(audit_box_filt))
  r_pred_class <- sapply(r_pred_prob, function(x) {
    if (x > .5) {
      "1"
    } else {
      "0"
    }
  })
  up_stat <- upload_model(p_fit)
  z_pred <- predict_pmml_batch(audit_factor, up_stat$model_name)
  delete_model(up_stat$model_name)
  expect_equal_nn(z_pred$outputs$Probability_1, r_pred, tolerance = 1e-7)
  expect_equal_nn(z_pred$outputs$Predicted_Adjusted, r_pred_class)
})
