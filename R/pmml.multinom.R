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

#' Generate the PMML representation for a multinom object from package
#' \pkg{nnet}.
#'
#'
#' Generate the multinomial logistic model in the PMML
#' RegressionModel format.  The function implements the use of numerical,
#' categorical and multiplicative terms involving both numerical and
#' categorical variables.
#'
#' @param model A multinom object.
#' @param missing_value_replacement Value to be used as the 'missingValueReplacement'
#' attribute for all MiningFields.
#'
#' @inheritParams pmml
#'
#' @return PMML representation of the \code{multinom} object.
#'
#' @author Tridivesh Jena
#'
#' @references
#' \href{https://CRAN.R-project.org/package=nnet}{nnet: Feed-forward
#' Neural Networks and Multinomial Log-Linear Models (on CRAN)}
#'
#' @examples
#' library(nnet)
#' fit <- multinom(Species ~ ., data = iris)
#' fit_pmml <- pmml(fit)
#' @export pmml.multinom
#' @export
pmml.multinom <- function(model,
                          model_name = "multinom_Model",
                          app_name = "SoftwareAG PMML Generator",
                          description = "Multinomial Logistic Model",
                          copyright = NULL,
                          model_version = NULL,
                          transforms = NULL,
                          missing_value_replacement = NULL,
                          ...) {
  if (!inherits(model, "multinom")) stop("Not a legitimate multinom object")
  requireNamespace("nnet", quietly = TRUE)

  #------------------------------------------------
  # Unlike models built with matrices, models built with formulae are missing the
  # categorical variable levels and the dataType of the target variable from the
  # model description. Add this information by hand below.
  field <- NULL
  if (is.null(model$lev) && length(grep("matrix", attributes(model$terms)$dataClasses[1][1])) == 1) {
    # Since this is a multinomial model, assume target is categorical.
    attributes(model$terms)$dataClasses[1] <- "factor"

    # The target levels are already stored as 'lab'.
    model$lev <- model$lab
  }

  terms <- attributes(model$terms)
  field <- NULL
  field$name <- names(terms$dataClasses)
  orig.names <- field$name
  number.of.fields <- length(field$name)
  field$class <- terms$dataClasses
  orig.class <- field$class

  number.of.fields <- length(field$name)

  target <- field$name[1]
  numfac <- 0
  for (i in 1:number.of.fields)
  {
    if (field$class[[field$name[i]]] == "factor") {
      numfac <- numfac + 1
      if (field$name[i] == target) {
        field$levels[[field$name[i]]] <- model$lev
      } else {
        field$levels[[field$name[i]]] <- model$xlevels[[field$name[i]]]
      }
    }
    if (length(grep("^as.factor\\(", field$name[i]))) {
      field$name[i] <- sub("^as.factor\\((.*)\\)", "\\1", field$name[i])
      names(field$class)[i] <- sub("^as.factor\\((.*)\\)", "\\1", names(field$class)[i])
      names(field$levels)[numfac] <- sub("^as.factor\\((.*)\\)", "\\1", names(field$levels)[numfac])
    }
  }
  target <- field$name[1]
  # PMML

  pmml <- .pmmlRootNode()

  # PMML -> Header

  pmml <- append.XMLNode(pmml, .pmmlHeader(
    description, copyright,
    model_version, app_name
  ))

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field, NULL, NULL, transforms))

  # PMML -> RegressionModel

  the.model <- xmlNode("RegressionModel",
    attrs = c(
      modelName = model_name,
      functionName = "classification",
      algorithmName = as.character(model$call[[1]]),
      normalizationMethod = "softmax"
    )
  )

  # PMML -> RegressionModel -> MiningSchema

  the.model <- append.XMLNode(the.model, .pmmlMiningSchema(field, target, transforms, missing_value_replacement = missing_value_replacement))

  the.model <- append.XMLNode(the.model, .pmmlOutput(field, target))

  #----------------------------------------------------
  # PMML -> TreeModel -> LocalTransformations -> DerivedField -> NormContiuous

  LTNode <- xmlNode("LocalTransformations")

  #--------------------------------------------------------------------------
  # PMML -> RegressionModel -> RegressionTable

  coeff <- coefficients(model)

  # Handle special binary case.
  if (length(model$lev) == 2) {
    l <- length(coefficients(model))
    coeff <- array(dim = c(1, l))
    colnames(coeff) <- names(coefficients(model))
    rownames(coeff) <- model$lev[2]
    for (i in 1:l) {
      coeff[1, i] <- coefficients(model)[i]
    }
  }
  coeffnames <- colnames(coeff)
  targetnames <- rownames(coeff)

  # Construct LocalTransformations to define a multiplicative term, if used.
  LTAdd <- FALSE
  # Ignore the 1st column (intercept).
  for (i in 2:ncol(coeff))
  {
    name <- coeffnames[i]
    if (name == target) next
    if (length(grep(".+:.+", name)) == 1) {
      for (j in 1:length(strsplit(name, ":")[[1]]))
      {
        spltname <- strsplit(name, ":")[[1]][j]
        if (length(grep("^as.factor\\(", spltname)) == 1) {
          LTAdd <- TRUE
          facLesName <- gsub("^as.factor\\(", "", spltname)
          dFieldName <- paste(strsplit(facLesName, ")")[[1]][1], strsplit(facLesName, ")")[[1]][2])
          dvf <- xmlNode("DerivedField", attrs = c(
            name = dFieldName, optype = "continuous",
            dataType = "double"
          ))
          dvNode <- xmlNode("NormDiscrete", attrs = c(
            field = strsplit(oname, ")")[[1]][1],
            value = strsplit(oname, ")")[[1]][2]
          ))
          dvf <- append.XMLNode(dvf, dvNode)
          LTNode <- append.xmlNode(LTNode, dvf)
        }
        for (k in 2:length(orig.names))
        {
          if ((field$class[[field$name[k]]] == "factor") && length(grep(field$name[k], spltname) == 1)) {
            LTAdd <- TRUE
            fldValName <- gsub(field$name[k], "", spltname)
            dvf <- xmlNode("DerivedField", attrs = c(
              name = spltname, optype = "continuous",
              dataType = "double"
            ))
            dvNode <- xmlNode("NormDiscrete", attrs = c(field = field$name[k], value = fldValName))
            dvf <- append.XMLNode(dvf, dvNode)
            LTNode <- append.XMLNode(LTNode, dvf)
          }
        }
      }
    }
  }

  append <- FALSE
  if (LTAdd && !is.null(transforms)) {
    LTNode <- .pmmlLocalTransformations(field, transforms, LTNode)
    the.model <- append.XMLNode(the.model, LTNode)
  }
  if (LTAdd && is.null(transforms)) {
    the.model <- append.XMLNode(the.model, LTNode)
  }
  if (!LTAdd && !is.null(transforms)) {
    the.model <- append.XMLNode(the.model, .pmmlLocalTransformations(field, transforms, LTNode))
  }

  for (k in 1:nrow(coeff))
  {
    reg.table <- xmlNode("RegressionTable",
      attrs = c(
        intercept = as.numeric(coeff[k, 1]),
        targetCategory = targetnames[k]
      )
    )
    # Ignore the 1st column (intercept).
    for (j in 2:ncol(coeff))
    {
      name <- coeffnames[j]
      if (name == target) next
      if (length(grep(".+:.+", name)) == 1) {
      } else if (length(grep("^as.factor\\(", name)) == 1) {
      } else {
        # all numeric factors
        for (i in 2:length(orig.names))
        {
          if ((field$class[[field$name[i]]] == "numeric") && field$name[i] == name) {
            predictor.node <- xmlNode("NumericPredictor",
              attrs = c(
                name = name, exponent = "1",
                coefficient = as.numeric(coeff[k, which(coeffnames == name)])
              )
            )
            reg.table <- append.XMLNode(reg.table, predictor.node)
          }
        }
      }
    }
    # now find all categorical terms
    # ignore the 1st column (intercept)
    for (j in 2:ncol(coeff))
    {
      name <- coeffnames[j]
      if (name == target) next
      if (length(grep(".+:.+", name)) == 1) {
      } else if (length(grep("^as.factor\\(", name)) == 1) {
      } else {
        for (i in 2:length(orig.names))
        {
          if ((field$class[[field$name[i]]] == "factor") && length(grep(field$name[i], name) == 1)) {
            predictor.node <- xmlNode("CategoricalPredictor", attrs = c(
              name = field$name[i],
              value = gsub(field$name[i], "", name),
              coefficient = as.numeric(coeff[k, which(coeffnames == name)])
            ))
            reg.table <- append.XMLNode(reg.table, predictor.node)
          }
        }
      }
    }
    # Now all the numerical terms forced into categorical.
    # Ignore the 1st column (intercept).
    for (j in 2:ncol(coeff))
    {
      name <- coeffnames[j]
      if (name == target) next
      if (length(grep(".+:.+", name)) == 1) {
      } else if (length(grep("^as.factor\\(", name)) == 1) {
        # Sometimes numeric variables forces into categorical variables have the
        # category appended to field names: i.e. as.factor(field) with possible values
        # 1,2,...  becomes as.factor(field)1  as.factor(field)2, etc.
        oname <- gsub("^as.factor\\(", "", name)
        predictor.node <- xmlNode("CategoricalPredictor",
          attrs = c(
            name = strsplit(oname, ")")[[1]][1],
            value = strsplit(oname, ")")[[1]][2],
            coefficient = as.numeric(coeff[k, which(coeffnames == name)])
          )
        )

        reg.table <- append.XMLNode(reg.table, predictor.node)
      }
    }
    # interactive terms
    for (j in 2:ncol(coeff))
    {
      name <- coeffnames[j]
      if (name == target) next
      if (length(grep(".+:.+", name)) == 1) {
        # Assume ':' in the middle of an expression means interactive variables.
        cval <- as.numeric(coeff[k, which(coeffnames == name)])
        predictorNode <- xmlNode("PredictorTerm", attrs = c(coefficient = cval))
        for (i in 1:length(strsplit(name, ":")[[1]]))
        {
          spltname <- strsplit(name, ":")[[1]][i]

          if (length(grep("^as.factor\\(", spltname)) == 1) {
            facLessName <- gsub("^as.factor\\(", "", spltname)
            dField <- paste(strsplit(facLessName, ")")[[1]][1], strsplit(facLessName, ")")[[1]][2])
            fNode <- xmlNode("FieldRef", attrs = c(field = dField))
            predictorNode <- append.XMLNode(predictorNode, fNode)
          }
          for (l in 2:length(orig.names))
          {
            if ((field$class[[field$name[l]]] == "factor") && length(grep(field$name[l], spltname) == 1)) {
              fNode <- xmlNode("FieldRef", attrs = c(field = spltname))
              predictorNode <- append.XMLNode(predictorNode, fNode)
            } else if ((field$class[[field$name[l]]] == "numeric") && length(grep(field$name[l], spltname) == 1)) {
              fNode <- xmlNode("FieldRef", attrs = c(field = spltname))
              predictorNode <- append.XMLNode(predictorNode, fNode)
            }
          }
        }
        reg.table <- append.XMLNode(reg.table, predictorNode)
      }
    }

    the.model <- append.XMLNode(the.model, reg.table)
  }


  # Add a regression table for the base case.
  # Lab is null for binary case.
  basename <- setdiff(model$lev, targetnames)
  the.model <- append.XMLNode(
    the.model,
    xmlNode("RegressionTable",
      attrs = c(
        intercept = "0.0",
        targetCategory = basename
      )
    )
  )

  pmml <- append.XMLNode(pmml, the.model)

  return(pmml)
}
