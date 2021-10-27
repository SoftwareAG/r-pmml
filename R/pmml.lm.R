# PMML: Predictive Model Markup Language
#
# Copyright (c) 2009-2016, Zementis, Inc.
# Copyright (c) 2016-2021, Software AG, Darmstadt, Germany and/or Software AG
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

#' Generate the PMML representation for an lm object from the package \pkg{stats}.
#'
#' @param model An lm object.
#' @param missing_value_replacement Value to be used as the 'missingValueReplacement'
#' attribute for all MiningFields.
#' @param weights The weights used for building the model.
#'
#' @inheritParams pmml
#'
#' @return PMML representation of the \code{lm} object.
#'
#' @details
#' The resulting PMML representation will not encode interaction
#' terms. Currently, only numeric regression is supported.
#'
#' @author Rajarshi Guha
#'
#' @references
#' \href{http://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html}{R project: Fitting Linear Models}
#'
#' @examples
#'
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fit_pmml <- pmml(fit)
#' @export pmml.lm
#' @export
pmml.lm <- function(model,
                    model_name = "lm_Model",
                    app_name = "SoftwareAG PMML Generator",
                    description = "Linear Regression Model",
                    copyright = NULL,
                    model_version = NULL,
                    transforms = NULL,
                    missing_value_replacement = NULL,
                    weights = NULL,
                    ...) {
  if (!inherits(model, "lm")) stop("Not a legitimate lm object")

  # For a regression, all variables will have been used except those
  # with a NA coefficient indicating singularities. We mark
  # singularities as inactive shortly.

  terms <- attributes(model$terms)

  field <- NULL

  field$name <- names(terms$dataClasses)
  # Check for a "(weights)" data class and remove it. This
  # arises in the glm models when a Weight variable is used.
  weights <- which(field$name == "(weights)")
  if (length(weights)) field$name <- field$name[-weights]
  orig.names <- field$name

  field$class <- terms$dataClasses
  if (length(weights)) field$class <- field$class[-weights]
  orig.class <- field$class

  number.of.fields <- length(field$name)

  target <- field$name[1]

  inactive <- names(which(is.na(coef(model))))
  active <- names(which(!is.na(coef(model))))

  field$name <- enc2utf8(field$name)

  tmp <- sapply(sapply(field$name, grep, inactive), length)
  inactive.vars <- names(tmp[tmp > 0])
  tmp <- sapply(sapply(field$name, grep, active), length)
  active.vars <- names(tmp[tmp > 0])

  inactive <- setdiff(inactive.vars, active.vars)

  for (i in 1:number.of.fields)
  {
    if (field$class[[field$name[i]]] == "factor") {
      if (is.null(model$data)) {
        field$levels[[field$name[i]]] <- model$xlevels[[field$name[i]]]
      } else {
        field$levels[[field$name[i]]] <- levels(model$data[[field$name[i]]])
      }
    }
  }

  coeff <- coefficients(model)

  # For singularities the coefficient is NA. The DMG specification says:
  # <xs:attribute name="coefficient" type="REAL-NUMBER" use="required" />
  # Replace NAs with 0. The effect should be the same.

  coeff[is.na(coeff)] <- 0

  coeffnames <- names(coeff)
  if (grepl(":", toString(coeffnames))) {
    stop("Possible interaction terms detected. Please note that interaction terms for regression models are not presently supported.")
  }

  if (coeffnames[[1]] == "(Intercept)") {
    intercept <- as.numeric(coeff[[1]])
  } else {
    intercept <- 0
  }


  # PMML

  pmml <- .pmmlRootNode()

  # PMML -> Header

  pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, model_version, app_name))

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field, weights = weights, transformed = transforms))

  # PMML -> RegressionModel

  if (model$call[[1]] == "lm") {
    model.type <- "lm"
  } else if (model$call[[1]] == "glm") {
    model.type <- model$family$family
  } else {
    model.type <- "unknown"
  }

  if (model.type == "binomial") {
    the.model <- xmlNode("RegressionModel",
      attrs = c(
        modelName = model_name,
        functionName = "classification",
        algorithmName = "glm",
        normalizationMethod = "softmax"
      )
    )
  }
  else if (model.type == "poisson") {
    the.model <- xmlNode("RegressionModel",
      attrs = c(
        modelName = model_name,
        functionName = "regression",
        algorithmName = "glm",
        normalizationMethod = "exp"
      )
    )
  }
  else if (model.type == "gaussian") {
    the.model <- xmlNode("RegressionModel",
      attrs = c(
        modelName = model_name,
        functionName = "regression",
        algorithmName = "glm"
      )
    )
  }
  else if (model.type == "lm") {
    # The original code for linear regression models.
    the.model <- xmlNode("RegressionModel",
      attrs = c(
        modelName = model_name,
        functionName = "regression",
        algorithmName = "least squares"
      )
    )
  }
  else {
    stop("pmml.lm: Not a supported family object: ", model.type)
  }

  # PMML -> RegressionModel -> MiningSchema

  the.model <- append.XMLNode(the.model, .pmmlMiningSchema(field, target, transformed = transforms, missing_value_replacement = missing_value_replacement))

  # PMML -> TreeModel -> Output

  the.model <- append.XMLNode(the.model, .pmmlOutput(field, target))

  #----------------------------------------------------
  # PMML -> TreeModel -> LocalTransforms

  # test of xform functions
  if (!is.null(transforms)) {
    the.model <- append.XMLNode(the.model, .pmmlLocalTransformations(field, transforms, NULL))
  }

  # PMML -> RegressionModel -> RegressionTable

  regTable2 <- NULL

  if (model.type == "binomial") {
    values <- sort(unique(model$data[[target]]))
    alternative.value <- as.character(values[1])
    target.value <- as.character(values[2])
    regTable <- xmlNode("RegressionTable",
      attrs = c(
        targetCategory = target.value,
        intercept = intercept
      )
    )
    regTable2 <- xmlNode("RegressionTable",
      attrs = c(
        targetCategory = alternative.value,
        intercept = "0.0"
      )
    )
  }
  else {
    regTable <- xmlNode("RegressionTable",
      attrs = c(intercept = intercept)
    )
  }

  for (i in 1:length(orig.names))
  {
    name <- orig.names[[i]]
    if (name == target) next
    klass <- orig.class[[name]]
    if (klass == "numeric") {
      predictorNode <- xmlNode("NumericPredictor",
        attrs = c(
          name = name,
          exponent = "1",
          coefficient = as.numeric(coeff[which(coeffnames == name)])
        )
      )
      regTable <- append.XMLNode(regTable, predictorNode)
    }
  }

  for (i in 1:length(orig.names))
  {
    name <- orig.names[[i]]
    if (name == target) next
    klass <- orig.class[[name]]
    if (klass == "factor") {
      levs <- model$xlevels[[name]]

      for (l in levs)
      {
        tmp <- paste(name, l, sep = "")

        coefficient <- ifelse(!length(which(coeffnames == tmp)), 0.00,
          as.numeric(coeff[which(coeffnames == tmp)])
        )
        predictorNode <- xmlNode("CategoricalPredictor",
          attrs = c(
            name = name,
            value = .markupSpecials(l), coefficient = coefficient
          )
        )
        regTable <- append.XMLNode(regTable, predictorNode)
      }
    }
  }

  the.model <- append.XMLNode(the.model, regTable)
  if (!is.null(regTable2)) the.model <- append.XMLNode(the.model, regTable2)

  pmml <- append.XMLNode(pmml, the.model)

  return(pmml)
}
