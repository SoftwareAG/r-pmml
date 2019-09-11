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

#' Generate the PMML representation for a naiveBayes object from the package
#' \pkg{e1071}.
#'
#'
#' @param model A naiveBayes object.
#' @param missing_value_replacement Value to be used as the 'missingValueReplacement'
#' attribute for all MiningFields.
#' @param predicted_field Required parameter; the name of the predicted field.
#'
#' @inheritParams pmml
#'
#' @return PMML representation of the naiveBayes object.
#'
#' @author Tridivesh Jena
#'
#' @details
#' The PMML representation of the NaiveBayes model implements the definition as
#' specified by the Data Mining Group: intermediate probability values which
#' are less than the threshold value are replaced by the threshold value. This
#' is different from the prediction function of the \pkg{e1071} in which only
#' probability values of 0 and standard deviations of continuous variables of
#' with the value 0 are replaced by the threshold value. The two values will
#' therefore not match exactly for cases involving very small probabilities.
#'
#' @references \itemize{
#' \item \href{https://CRAN.R-project.org/package=e1071}{e1071: Misc Functions of the Department of Statistics, Probability Theory Group (Formerly: E1071), TU Wien (on CRAN)}
#'
#' \item A. Guazzelli, T. Jena, W. Lin, M. Zeller (2013). Extending the Naive
#' Bayes Model Element in PMML: Adding Support for Continuous Input Variables.
#' In \emph{Proceedings of the 19th ACM SIGKDD Conference on Knowledge
#' Discovery and Data Mining}.
#'
#' }
#' @examples
#' library(e1071)
#'
#' data(houseVotes84)
#' house <- na.omit(houseVotes84)
#'
#' model <- naiveBayes(Class ~ V1 + V2 + V3, data = house, threshold = 0.003)
#'
#' model_pmml <- pmml(model, dataset = house, predicted_field = "Class")
#' @export pmml.naiveBayes
#' @export
pmml.naiveBayes <- function(model,
                            model_name = "naiveBayes_Model",
                            app_name = "SoftwareAG PMML Generator",
                            description = "NaiveBayes Model",
                            copyright = NULL,
                            transforms = NULL,
                            missing_value_replacement = NULL,
                            predicted_field,
                            ...) {
  if (!inherits(model, "naiveBayes")) stop("Not a legitimate naiveBayes object")
  requireNamespace("e1071", quietly = TRUE)

  # Field names and attributes are not given, and must be inferred from the tables.
  field <- NULL

  # Target field name not given in R model object. Use predicted_field input
  # parameter to get name. Cannot assume in general that the target is the first
  # column. Find it by going through columns and checking if the levels of the
  # variable are the same as the target levels, which are given in the R model output.

  target <- predicted_field
  colname <- 1

  if (predicted_field == "Y") {
    stop("predicted variable name not given.")
  }

  if (!is.null(transforms)) {
    if (!(predicted_field %in% names(transforms$data))) {
      stop("predicted_field not in list of original fields.")
    }
  }

  if (is.null(model$levels)) {
    stop("Unable to determine levels of target variable.
          If numeric, please ensure it is read as a factor by using as.factor(variable_name)")
  }

  field$name <- c(target, names(model$tables))
  number.of.fields <- length(field$name)
  field$class[[target]] <- "factor"
  field$levels[[target]] <- model$levels

  for (i in 1:length(names(model$tables)))
  {
    name <- names(model$tables)[i]
    if (length(grep("^as.factor\\(", name))) {
      name <- sub("^as.factor\\((.*)\\)", "\\1", name)
    }

    if (!is.null(colnames(model$tables[[i]]))) {
      field$class[[name]] <- "factor"
      field$levels[[name]] <- colnames(model$tables[[i]])
    } else {
      # continuous variable
      field$class[[name]] <- "numeric"
    }
  }

  field$origName <- NA
  for (i in 1:number.of.fields)
  {
    if (length(grep("^as.factor\\(", field$name[i]))) {
      field$origName[i] <- field$name[i]
      field$name[i] <- sub("^as.factor\\((.*)\\)", "\\1", field$name[i])
    } else {
      field$origName[i] <- NA
    }
  }

  # PMML

  pmml <- .pmmlRootNode()

  # PMML -> Header

  pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, app_name))

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field, NULL, NULL, transforms))

  # PMML -> NaiveBayesModel

  thresh <- NULL
  if (!is.null(as.list(model$call)$threshold)) {
    thresh <- as.list(model$call)$threshold
  } else {
    thresh <- 0.001
  }
  the.model <- xmlNode("NaiveBayesModel",
    attrs = c(
      modelName = model_name,
      functionName = "classification",
      threshold = thresh
    )
  )

  #--------------------------------------------------
  # PMML ->  MiningSchema

  the.model <- append.XMLNode(the.model, .pmmlMiningSchema(field, target, transforms, missing_value_replacement = missing_value_replacement))

  #-----------------------------------------
  #  PMML -> OUTPUT
  the.model <- append.XMLNode(the.model, .pmmlOutput(field, target))

  #------------------------------------------
  # PMML -> LocalTransformations

  if (!is.null(transforms)) {
    the.model <- append.XMLNode(the.model, .pmmlLocalTransformations(field, transforms))
  }

  #---------------------------------------
  prob <- NULL
  BayesInputsNode <- xmlNode("BayesInputs")
  for (i in 2:number.of.fields)
  {
    if (field$class[[field$name[i]]] == "factor") {
      BayesInputNode <- xmlNode("BayesInput", attrs = c(fieldName = field$name[i]))
      for (j in 1:length(field$levels[[field$name[i]]]))
      {
        PairCountsNode <- xmlNode("PairCounts", attrs = c(value = field$levels[[field$name[i]]][j]))
        TargetValueCountsNode <- xmlNode("TargetValueCounts")
        for (k in 1:length(field$levels[[target]]))
        {
          valuek <- field$levels[[target]][k]
          # PMML needs counts, while R gives probability. Convert probability to count by
          # multiplying by the total number of cases for that category.
          if (is.null(field$origName[i]) || is.na(field$origName[i])) {
            prob <- model$tables[[field$name[i]]][k, j]
          } else {
            prob <- model$tables[[field$origName[i]]][k, j]
          }
          # If prob is null, field name doesnt exist in model tables => it is the temp field.
          if (is.null(prob)) {
            prob <- 1.0
          }
          countk <- prob * model$apriori[[k]]
          TargetValueCountNode <- xmlNode("TargetValueCount", attrs = c(value = valuek, count = countk))
          TargetValueCountsNode <- append.XMLNode(TargetValueCountsNode, TargetValueCountNode)
        }
        PairCountsNode <- append.XMLNode(PairCountsNode, TargetValueCountsNode)
        BayesInputNode <- append.XMLNode(BayesInputNode, PairCountsNode)
      }
      BayesInputsNode <- append.XMLNode(BayesInputsNode, BayesInputNode)
    } else {
      BayesInputNode <- xmlNode("BayesInput", attrs = c(fieldName = field$name[i]))

      TargetValueStatsNode <- xmlNode("TargetValueStats")
      for (j in 1:length(field$levels[[target]]))
      {
        TargetValueStatNode <- xmlNode("TargetValueStat", attrs = c(value = field$levels[[target]][j]))
        avg <- model$tables[[field$name[i]]][j, ][1]
        std <- model$tables[[field$name[i]]][j, ][2]
        var <- std * std
        GaussianNode <- xmlNode("GaussianDistribution", attrs = c(mean = avg, variance = var))
        TargetValueStatNode <- append.xmlNode(TargetValueStatNode, GaussianNode)
        TargetValueStatsNode <- append.xmlNode(TargetValueStatsNode, TargetValueStatNode)
      }
      BayesInputNode <- append.xmlNode(BayesInputNode, TargetValueStatsNode)
      BayesInputsNode <- append.XMLNode(BayesInputsNode, BayesInputNode)
    }
  }

  the.model <- append.XMLNode(the.model, BayesInputsNode)
  BayesOutputNode <- xmlNode("BayesOutput", attrs = c(fieldName = target))
  TargetValueCountsNode <- xmlNode("TargetValueCounts")
  for (i in 1:length(field$levels[[target]]))
  {
    valuei <- field$levels[[target]][i]
    counti <- model$apriori[[i]]
    TargetValueCountNode <- xmlNode("TargetValueCount", attrs = c(value = valuei, count = counti))
    TargetValueCountsNode <- append.XMLNode(TargetValueCountsNode, TargetValueCountNode)
  }
  BayesOutputNode <- append.XMLNode(BayesOutputNode, TargetValueCountsNode)
  the.model <- append.XMLNode(the.model, BayesOutputNode)

  pmml <- append.XMLNode(pmml, the.model)

  return(pmml)
}
