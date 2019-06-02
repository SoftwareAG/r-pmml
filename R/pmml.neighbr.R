# PMML: Predictive Model Markup Language
#
# Copyright (c) 2009-2016, Zementis, Inc.
# Copyright (c) 2016-2019, Software AG, Darmstadt, Germany and/or Software AG
# USA Inc., Reston, VA, USA, and/or its subsidiaries and/or its affiliates
# and/or their licensors.
#
# This file is part of the PMML package for R.
#
# The PMML package is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# The PMML package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. Please see the
# GNU General Public License for details (http://www.gnu.org/licenses/).
# #############################################################################

#' Generate PMML for a neighbr object from the \bold{neighbr} package.
#'
#' @param model A neighbr object.
#' @param missing_value_replacement Value to be used as the 'missingValueReplacement' attribute for all MiningFields.
#'
#' @inheritParams pmml
#'
#' @return PMML representation of the neighbr object.
#'
#' @details The model is represented in the PMML NearestNeighborModel format.
#'
#' The current version of this converter does not support transformations (\code{transforms}
#' must be left as \code{NULL}), sets \code{categoricalScoringMethod} to "majorityVote", sets
#' \code{continuousScoringMethod} to "average", and \code{isTransoformed} to "false".
#'
#' @examples
#' \dontrun{
#'
#' # Continuous features with continuous target, categorical target,
#' # and neighbor ranking:
#'
#' library(neighbr)
#' data(iris)
#'
#' # Add an ID column to the data for neighbor ranking:
#' iris$ID <- c(1:150)
#'
#' # Train set contains all predicted variables, features, and ID column:
#' train_set <- iris[1:140, ]
#'
#' # Omit predicted variables or ID column from test set:
#' test_set <- iris[141:150, -c(4, 5, 6)]
#'
#' fit <- knn(
#'   train_set = train_set, test_set = test_set,
#'   k = 3,
#'   categorical_target = "Species",
#'   continuous_target = "Petal.Width",
#'   comparison_measure = "squared_euclidean",
#'   return_ranked_neighbors = 3,
#'   id = "ID"
#' )
#'
#' pmml(fit)
#'
#'
#' # Logical features with categorical target and neighbor ranking:
#'
#' library(neighbr)
#' data("houseVotes84")
#'
#' # Remove any rows with N/A elements:
#' dat <- houseVotes84[complete.cases(houseVotes84), ]
#'
#' # Change all {yes,no} factors to {0,1}:
#' feature_names <- names(dat)[!names(dat) %in% c("Class", "ID")]
#' for (n in feature_names) {
#'   levels(dat[, n])[levels(dat[, n]) == "n"] <- 0
#'   levels(dat[, n])[levels(dat[, n]) == "y"] <- 1
#' }
#'
#' # Change factors to numeric:
#' for (n in feature_names) {
#'   dat[, n] <- as.numeric(levels(dat[, n]))[dat[, n]]
#' }
#'
#' # Add an ID column for neighbor ranking:
#' dat$ID <- c(1:nrow(dat))
#'
#' # Train set contains features, predicted variable, and ID:
#' train_set <- dat[1:225, ]
#'
#' # Test set contains features only:
#' test_set <- dat[226:232, !names(dat) %in% c("Class", "ID")]
#'
#' fit <- knn(
#'   train_set = train_set, test_set = test_set,
#'   k = 5,
#'   categorical_target = "Class",
#'   comparison_measure = "jaccard",
#'   return_ranked_neighbors = 3,
#'   id = "ID"
#' )
#'
#' pmml(fit)
#' }
#'
#' @seealso \code{\link[pmml]{pmml}},
#' \href{http://dmg.org/pmml/v4-3/KNN.html}{PMML KNN specification}
#'
#' @export pmml.neighbr
#' @export
pmml.neighbr <- function(model,
                         model_name = "kNN_model",
                         app_name = "SoftwareAG PMML Generator",
                         description = "K Nearest Neighbors Model",
                         copyright = NULL,
                         transforms = NULL,
                         missing_value_replacement = NULL,
                         ...) {
  if (!inherits(model, "neighbr")) stop("Not a legitimate neighbr object")

  if (!is.null(transforms)) stop("transforms currently not supported for knn models")

  field <- NULL

  targets <- c()
  classes <- rep("numeric", length(model$features))
  names(classes) <- c(model$features)
  field$levels <- NULL
  extra_model_attrs <- c()
  if (model$function_name == "classification") {
    targets <- model$categorical_target
    classes <- c(classes, "factor")
    names(classes) <- c(model$features, model$categorical_target)
    field$levels[[model$categorical_target]] <- model$categorical_levels
    extra_model_attrs <- c(categoricalScoringMethod = "majorityVote")
  } else if (model$function_name == "regression") {
    targets <- model$continuous_target
    classes <- c(classes, "numeric")
    names(classes) <- c(model$features, model$continuous_target)
    extra_model_attrs <- c(continuousScoringMethod = model$continuous_scoring_method)
  } else if (model$function_name == "mixed") {
    targets <- c(model$categorical_target, model$continuous_target)
    classes <- c(classes, "factor", "numeric")
    names(classes) <- c(model$features, model$categorical_target, model$continuous_target)
    field$levels[[model$categorical_target]] <- model$categorical_levels
    extra_model_attrs <- c(categoricalScoringMethod = "majorityVote", continuousScoringMethod = model$continuous_scoring_method)
  } else if (model$function_name == "clustering") {
    targets <- NULL
    extra_model_attrs <- NULL
  } else {
    stop(paste0("not a valid function_name: ", model$function_name))
  }

  if (!is.null(model$id)) {
    extra_model_attrs <- c(extra_model_attrs, instanceIdVariable = model$id, threshold = "0.001")
  }

  field$name <- c(model$features, targets)
  field$class <- classes

  # PMML
  pmml <- .pmmlRootNode("4.3")

  # PMML -> Header
  pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, app_name))

  # PMML -> DataDictionary
  pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field, transformed = transforms))

  # PMML -> NearestNeighborModel
  pmml <- append.XMLNode(pmml, .theModel(model_name, extra_model_attrs, model, field, targets, transforms, missing_value_replacement))

  return(pmml)
}


.theModel <- function(model_name, extra_model_attrs, model, field, targets, transforms, missing_value_replacement) {
  # NearestNeighborModel
  the.model <- xmlNode("NearestNeighborModel",
    attrs = c(
      modelName = model_name,
      extra_model_attrs,
      numberOfNeighbors = model$k,
      functionName = model$function_name
    )
  )

  # NearestNeighborModel ->  MiningSchema
  the.model <- append.XMLNode(the.model, .pmmlMiningSchemaKNN(field, targets, transforms, missing_value_replacement = missing_value_replacement))

  # NearestNeighborModel -> Output
  the.model <- append.XMLNode(the.model, .pmmlOutputKNN(model, field, targets))

  # NearestNeighborModel -> TrainingInstances
  the.model <- append.XMLNode(the.model, .trainingInstances(field, model))

  # NearestNeighborModel -> ComparisonMeasure
  the.model <- append.XMLNode(the.model, .comparisonMeasure(model))

  # NearestNeighborModel -> KNNInputs
  the.model <- append.XMLNode(the.model, .make_knn_inputs(model$features))

  return(the.model)
}

.pmmlOutputKNN <- function(model, field, targets) {
  # Adds support multiple targets and IDs
  output <- xmlNode("Output")
  for (targ in targets) {
    if (field$class[[targ]] == "factor") {
      output <- append.XMLNode(output, xmlNode("OutputField",
        attrs = c(
          name = paste0("Predicted_", targ), feature = "predictedValue",
          dataType = "string", optype = "categorical"
        )
      ))
    } else if (field$class[[targ]] == "numeric") {
      output <- append.XMLNode(output, xmlNode("OutputField",
        attrs = c(
          name = paste0("Predicted_", targ), feature = "predictedValue",
          dataType = "double", optype = "continuous"
        )
      ))
    }
  }
  if (!is.null(model$id)) {
    for (n in (1:model$return_ranked_neighbors)) {
      id_node <- xmlNode("OutputField", attrs = c(
        name = paste0("neighbor", n), feature = "entityId", dataType = "string",
        optype = "categorical", rank = n
      ))
      output <- append.XMLNode(output, id_node)
    }
  }
  return(output)
}

.pmmlMiningSchemaKNN <- function(field, targets = NULL, transformed = NULL, missing_value_replacement = NULL) {
  namelist <- .origFieldList(field, transformed)
  mining.schema <- xmlNode("MiningSchema")

  unknownVal <- NULL
  invalidVal <- NULL


  for (j in 1:length(namelist)) {
    if (!is.na(namelist[[j]])) {
      usage <- ifelse(namelist[[j]] %in% targets, "predicted", "active")
      if ((!is.null(targets)) && (!(namelist[[j]] %in% targets))) {
        if (!is.null(missing_value_replacement)) {
          unknownVal <- missing_value_replacement
          invalidVal <- "asMissing"
        }
      } else if (is.null(targets) && !is.null(missing_value_replacement)) {
        unknownVal <- missing_value_replacement
        invalidVal <- "asMissing"
      }
      if (namelist[[j]] == "Temp" || namelist[[j]] == "DiscretePlaceHolder") {
        if (length(field$levels[[namelist[[j]]]]) == 1) {
          mf <- xmlNode("MiningField", attrs = c(
            name = namelist[[j]],
            usageType = usage, missingValueReplacement = field$levels[[namelist[[j]]]]
          ))
        }
      } else {
        mf <- xmlNode("MiningField", attrs = c(
          name = namelist[[j]], usageType = usage,
          missingValueReplacement = unknownVal, invalidValueTreatment = invalidVal
        ))
      }

      mining.schema <- append.XMLNode(mining.schema, mf)
    }
  }


  return(mining.schema)
}

.trainingInstances <- function(field, model) {
  field_count <- length(c(field$class, model$id))
  training_instances <- xmlNode("TrainingInstances",
    attrs = c(
      recordCount = model$num_train_rows,
      fieldCount = field_count, # Assumes that id is NULL if not present, not "none".
      isTransformed = "false"
    )
  )

  instance_fields <- xmlNode("InstanceFields")

  field_names_with_id <- c(field$name, model$id)

  for (f in field_names_with_id) {
    instance_fields <- append.XMLNode(instance_fields, xmlNode("InstanceField", attrs = c(field = f, column = f)))
  }

  training_instances <- append.XMLNode(training_instances, instance_fields)

  # InlineTable
  inline_table <- xmlNode("InlineTable")
  make_inline_table_row <- function(the_row) {
    # Turn data frame row into a <row> node.
    row_names <- names(the_row)
    row_node <- xmlNode("row")
    for (f in row_names) {
      field_node <- xmlNode(f, text = the_row[[f]])
      row_node <- append.XMLNode(row_node, field_node)
    }
    return(row_node)
  }

  for (g in 1:nrow(model$train_set)) {
    the_row <- model$train_set[g, ]
    inline_table <- append.XMLNode(inline_table, make_inline_table_row(the_row))
  }

  training_instances <- append.XMLNode(training_instances, inline_table)

  return(training_instances)
}

.comparisonMeasure <- function(model) {
  allowed_distance_measures <- c("euclidean", "squared_euclidean")
  names(allowed_distance_measures) <- c("euclidean", "squaredEuclidean")
  allowed_similarity_measures <- c("simple_matching", "jaccard", "tanimoto")
  names(allowed_similarity_measures) <- c("simpleMatching", "jaccard", "tanimoto")

  if (model$comparison_measure %in% allowed_distance_measures) {
    comparison_measure <- xmlNode("ComparisonMeasure", attrs = c(kind = "distance"))
    measure_name <- xmlNode(names(allowed_distance_measures)[match(model$comparison_measure, allowed_distance_measures)])
  } else if (model$comparison_measure %in% allowed_similarity_measures) {
    comparison_measure <- xmlNode("ComparisonMeasure", attrs = c(kind = "similarity"))
    measure_name <- xmlNode(names(allowed_similarity_measures)[match(model$comparison_measure, allowed_similarity_measures)])
  } else {
    stop("invalid comparison_measure")
  }

  comparison_measure <- append.XMLNode(comparison_measure, measure_name)
  return(comparison_measure)
}

.make_knn_inputs <- function(features) {
  knn_inputs <- xmlNode("KNNInputs")
  for (f in features) {
    knn_inputs <- append.XMLNode(knn_inputs, xmlNode("KNNInput", attrs = c(field = f, compareFunction = "absDiff")))
  }
  return(knn_inputs)
}
