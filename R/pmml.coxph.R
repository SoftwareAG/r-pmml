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

#' Generate the PMML representation for a coxph object from the package
#' \pkg{survival}.
#'
#' @param model A coxph object.
#' @param missing_value_replacement Value to be used as the 'missingValueReplacement'
#' attribute for all MiningFields.
#'
#' @inheritParams pmml
#'
#' @details
#' A coxph object is the result of fitting a proportional hazards regression
#' model, using the \code{coxph} function from the package \pkg{survival}. Although
#' the \pkg{survival} package supports special terms "cluster", "tt" and
#' "strata", only the special term "strata" is supported by the \pkg{pmml}
#' package. Note that special term "strata" cannot be a multiplicative variable
#' and only numeric risk regression is supported.
#'
#' @author Graham Williams
#'
#' @references
#' \href{https://CRAN.R-project.org/package=survival}{coxph: Survival
#' Analysis}
#'
#' @export pmml.coxph
#' @export
pmml.coxph <- function(model,
                       model_name = "CoxPH_Survival_Regression_Model",
                       app_name = "SoftwareAG PMML Generator",
                       description = "CoxPH Survival Regression Model",
                       copyright = NULL,
                       model_version = NULL,
                       transforms = NULL,
                       missing_value_replacement = NULL,
                       ...) {
  if (!inherits(model, "coxph")) stop("Not a legitimate coxph object")

  # Detect if special terms exist which are not supported.
  vars <- names(attributes(model$terms)$dataClasses)
  coefs <- names(coefficients(model))
  for (i in 1:length(vars))
  {
    if (grepl("cluster\\(", vars[i]) || grepl("tt\\(", vars[i])) {
      stop("Special model equation terms 'cluster' and 'tt' not yet supported in PMML")
    }
  }
  for (i in 1:length(coefs))
  {
    if (grepl(":strata\\(", coefs[i])) {
      stop("Multiplicative strata variables not yet supported in PMML")
    }
  }

  # For a regression, all variables will have been used except those
  # with a NA coefficient indicating singularities. We mark
  # singularities as inactive shortly.

  terms <- attributes(model$terms)

  field <- NULL
  field2 <- NULL
  numFields <- length(terms$dataClasses)
  numFields2 <- 0
  for (i in 1:numFields)
  {
    if (!grepl("strata", names(terms$dataClasses)[i]) && !grepl("Surv", names(terms$dataClasses)[i])) {
      field$name[i] <- names(terms$dataClasses)[i]
      field$class[i] <- terms$dataClasses[i]
      names(field$class)[i] <- field$name[i]

      field2$name[i] <- names(terms$dataClasses)[i]
      field2$class[i] <- terms$dataClasses[i]
      names(field2$class)[i] <- field2$name[i]
      numFields2 <- numFields2 + 1
    }
  }

  field$name[1] <- "survival"
  field$class[1] <- "numeric"
  names(field$class)[1] <- field$name[1]
  field2$name[1] <- "survival"
  field2$class[1] <- "numeric"
  names(field2$class)[1] <- field2$name[1]
  numFields2 <- numFields2 + 1

  # Include startTime, endTime, status and strata variable if present
  isStrata <- FALSE
  starttimeVar <- FALSE
  endtimeVar <- ""
  statusVar <- ""
  strataVar <- ""
  model.type <- "coxph"

  survObject <- names(terms$dataClasses)[1]
  survObject0 <- gsub("Surv\\(", "", survObject)
  survObject1 <- gsub("\\)", "", survObject0)
  survList <- strsplit(survObject1, ",")
  if (length(survList[[1]]) == 2) {
    endtimeVar <- gsub(" ", "", survList[[1]][1])
    statusVar <- gsub(" ", "", survList[[1]][2])
  } else {
    starttimeVar <- gsub(" ", "", survList[[1]][1])
    endtimeVar <- gsub(" ", "", survList[[1]][2])
    statusVar <- gsub(" ", "", survList[[1]][3])
  }

  for (i in 1:length(attributes(model$terms)$term.labels))
  {
    termVar <- attributes(model$terms)$term.labels[i]
    if (grepl("strata", termVar) != 0) {
      isStrata <- TRUE
      strataVar0 <- gsub("strata\\(", "", termVar)
      strataVar <- gsub("\\)", "", strataVar0)
      if (length(grep("^as.factor\\(", strataVar))) {
        strataVar <- sub("^as.factor\\((.*)\\)", "\\1", strataVar)
      }
    }
  }

  if ((endtimeVar %in% field$name) || (starttimeVar %in% field$name) ||
    (statusVar %in% field$name) || (strataVar %in% field$name)) {
    stop("Predictor fields cannot be status, time or strata variables")
  }

  orig.names <- field$name
  orig.class <- field$class

  number.of.fields <- length(field$name)

  target <- field$name[1]

  inactive <- names(which(is.na(coef(model))))
  active <- names(which(!is.na(coef(model))))

  tmp <- sapply(sapply(field$name, grep, inactive), length)
  inactive.vars <- names(tmp[tmp > 0])
  tmp <- sapply(sapply(field$name, grep, active), length)
  active.vars <- names(tmp[tmp > 0])

  inactive <- setdiff(inactive.vars, active.vars)

  numfac <- 0
  for (i in 1:number.of.fields)
  {
    if (field$class[[field$name[i]]] == "factor") {
      numfac <- numfac + 1

      if (is.null(model$data)) {
        field$levels[[field$name[i]]] <- model$xlevels[[field$name[i]]]
      } else {
        field$levels[[field$name[i]]] <- levels(model$data[[field$name[i]]])
      }

      if (length(grep("^as.factor\\(", field$name[i]))) {
        field$name[i] <- sub("^as.factor\\((.*)\\)", "\\1", field$name[i])
        names(field$class)[i] <- sub("^as.factor\\((.*)\\)", "\\1", names(field$class)[i])
        names(field$levels)[numfac] <- sub("^as.factor\\((.*)\\)", "\\1", names(field$levels)[numfac])
      }
    }
  }

  # PMML

  pmml <- .pmmlRootNode()

  # PMML -> Header

  pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, model_version, app_name))

  # PMML -> RegressionModel

  # Different start node depending on existence of startTimeVariable
  # or baselineStrataVariable attributes.
  if (isStrata) {
    if (starttimeVar == FALSE) {
      the.model <- xmlNode("GeneralRegressionModel",
        attrs = c(
          modelType = "CoxRegression",
          modelName = model_name,
          functionName = "regression",
          algorithmName = "coxph",
          endTimeVariable = endtimeVar,
          statusVariable = statusVar,
          baselineStrataVariable = strataVar
        )
      )
      field2$name[numFields2 + 1] <- endtimeVar
      field2$class[numFields2 + 1] <- "numeric"
      names(field2$class)[numFields2 + 1] <- field2$name[numFields2 + 1]
      field2$name[numFields2 + 2] <- statusVar
      field2$class[numFields2 + 2] <- "numeric"
      names(field2$class)[numFields2 + 2] <- field2$name[numFields2 + 2]
      field2$name[numFields2 + 3] <- strataVar
      field2$class[numFields2 + 3] <- "factor"
      names(field2$class)[numFields2 + 3] <- field2$name[numFields2 + 3]
    } else {
      the.model <- xmlNode("GeneralRegressionModel",
        attrs = c(
          modelType = "CoxRegression",
          modelName = model_name,
          functionName = "regression",
          algorithmName = "coxph",
          endTimeVariable = endtimeVar,
          startTimeVariable = starttimeVar,
          statusVariable = statusVar,
          baselineStrataVariable = strataVar
        )
      )
      field2$name[numFields2 + 1] <- endtimeVar
      field2$class[numFields2 + 1] <- "numeric"
      names(field2$class)[numFields2 + 1] <- field2$name[numFields2 + 1]
      field2$name[numFields2 + 2] <- starttimeVar
      field2$class[numFields2 + 2] <- "numeric"
      names(field2$class)[numFields2 + 2] <- field2$name[numFields2 + 2]
      field2$name[numFields2 + 3] <- statusVar
      field2$class[numFields2 + 3] <- "numeric"
      names(field2$class)[numFields2 + 3] <- field2$name[numFields2 + 3]
      field2$name[numFields2 + 4] <- strataVar
      field2$class[numFields2 + 4] <- "factor"
      names(field2$class)[numFields2 + 4] <- field2$name[numFields2 + 4]
    }
  } else {
    if (starttimeVar == FALSE) {
      the.model <- xmlNode("GeneralRegressionModel",
        attrs = c(
          modelType = "CoxRegression",
          modelName = model_name,
          functionName = "regression",
          algorithmName = "coxph",
          endTimeVariable = endtimeVar,
          statusVariable = statusVar
        )
      )
      field2$name[numFields2 + 1] <- endtimeVar
      field2$class[numFields2 + 1] <- "numeric"
      names(field2$class)[numFields2 + 1] <- field2$name[numFields2 + 1]
      field2$name[numFields2 + 2] <- statusVar
      field2$class[numFields2 + 2] <- "numeric"
      names(field2$class)[numFields2 + 2] <- field2$name[numFields2 + 2]
    } else {
      the.model <- xmlNode("GeneralRegressionModel",
        attrs = c(
          modelType = "CoxRegression",
          modelName = model_name,
          functionName = "regression",
          algorithmName = "coxph",
          startTimeVariable = starttimeVar,
          endTimeVariable = endtimeVar,
          statusVariable = statusVar
        )
      )
      field2$name[numFields2 + 1] <- starttimeVar
      field2$class[numFields2 + 1] <- "numeric"
      names(field2$class)[numFields2 + 1] <- field2$name[numFields2 + 1]
      field2$name[numFields2 + 2] <- endtimeVar
      field2$class[numFields2 + 2] <- "numeric"
      names(field2$class)[numFields2 + 2] <- field2$name[numFields2 + 2]
      field2$name[numFields2 + 3] <- statusVar
      field2$class[numFields2 + 3] <- "numeric"
      names(field2$class)[numFields2 + 3] <- field2$name[numFields2 + 3]
    }
  }

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field2, transformed = transforms))

  # PMML -> RegressionModel -> MiningSchema

  the.model <- append.XMLNode(the.model, .pmmlMiningSchema(field2, target, transforms, missing_value_replacement = missing_value_replacement))

  # Add output fields to output both hazard and survival.

  output <- xmlNode("Output")

  out1 <- xmlNode("OutputField", attrs = c(name = "Predicted_hazard", feature = "predictedValue"))
  output <- append.XMLNode(output, out1)

  out2 <- xmlNode("OutputField", attrs = c(name = "SurvivalProbability", feature = "transformedValue"))
  applyExp <- xmlNode("Apply", attrs = c("function" = "exp"))
  const <- xmlNode("Constant", "-1.0")
  applyMult <- xmlNode("Apply", attrs = c("function" = "*"))
  fieldref <- xmlNode("FieldRef", attrs = c(field = "Predicted_hazard"))

  applyMult <- append.XMLNode(applyMult, const)
  applyMult <- append.XMLNode(applyMult, fieldref)
  applyExp <- append.XMLNode(applyExp, applyMult)
  out2 <- append.xmlNode(out2, applyExp)

  output <- append.XMLNode(output, out2)
  the.model <- append.XMLNode(the.model, output)

  # PMML -> TreeModel -> LocalTransforms

  if (!is.null(transforms)) {
    the.model <- append.XMLNode(the.model, .pmmlLocalTransformations(field, transforms))
  }

  plNode <- xmlNode("ParameterList")
  num <- 0
  for (i in 1:length(names(coefficients(model)))) {
    pname <- paste("p", num)
    pname <- gsub(" ", "", pname)
    num <- num + 1
    pnode <- xmlNode("Parameter", attrs = c(
      name = pname, label = names(coefficients(model))[i],
      referencePoint = model$means[[i]]
    ))
    plNode <- append.XMLNode(plNode, pnode)
  }

  the.model <- append.XMLNode(the.model, plNode)

  flNode <- xmlNode("FactorList")
  for (i in 2:number.of.fields)
  {
    if (field$class[i] == "factor") {
      pdNode <- xmlNode("Predictor", attrs = c(name = field$name[i]))
      flNode <- append.XMLNode(flNode, pdNode)
    }
  }

  the.model <- append.XMLNode(the.model, flNode)

  cvNode <- xmlNode("CovariateList")
  for (i in 2:number.of.fields)
  {
    if (field$class[i] == "numeric") {
      pdNode <- xmlNode("Predictor", attrs = c(name = field$name[i]))
      cvNode <- append.XMLNode(cvNode, pdNode)
    }
  }

  the.model <- append.XMLNode(the.model, cvNode)

  ppm <- xmlNode("PPMatrix")
  for (j in 1:length(model$coefficients)) {
    if (length(grep(".+:.+", names(coefficients(model))[j])) == 1) {
      for (k in 1:length(strsplit(names(coefficients(model))[j], ":")[[1]])) {
        for (f in 2:number.of.fields) {
          if (field$class[[field$name[f]]] == "factor") {
            if (length(grep(field$name[f], strsplit(names(coefficients(model))[j], ":")[[1]][k])) == 1) {
              modfield <- gsub(field$name[f], "", strsplit(names(coefficients(model))[j], ":")[[1]][k])
              ppcell <- xmlNode("PPCell", attrs = c(
                value = modfield, predictorName = field$name[f],
                parameterName = gsub(" ", "", paste("p", j - 1))
              ))
              ppm <- append.XMLNode(ppm, ppcell)
            }
          } else {
            if (length(grep(field$name[f], strsplit(names(coefficients(model))[j], ":")[[1]][k])) == 1) {
              ppcell <- xmlNode("PPCell", attrs = c(
                value = "1", predictorName = field$name[f],
                parameterName = gsub(" ", "", paste("p", j - 1))
              ))
              ppm <- append.XMLNode(ppm, ppcell)
            }
          }
        }
      }
    } else {
      # categorical terms
      for (f in 2:number.of.fields) {
        if (field$class[[field$name[f]]] == "factor") {
          if (length(grep(field$name[f], names(coefficients(model))[j])) == 1) {
            if (length(grep("^as.factor\\(", names(coefficients(model))[j])) == 1) {
              modfield <- sub("^as.factor\\((.*)\\)", "\\1", names(coefficients(model))[j])
              modfield <- gsub(field$name[f], "", modfield)
            } else {
              modfield <- gsub(field$name[f], "", names(coefficients(model))[j])
            }
            ppcell <- xmlNode("PPCell", attrs = c(
              value = modfield, predictorName = field$name[f],
              parameterName = gsub(" ", "", paste("p", j - 1))
            ))
            ppm <- append.XMLNode(ppm, ppcell)
          }
        } else {
          # numeric terms
          if (length(grep(field$name[f], names(coefficients(model))[j])) == 1) {
            ppcell <- xmlNode("PPCell", attrs = c(
              value = "1", predictorName = field$name[f],
              parameterName = gsub(" ", "", paste("p", j - 1))
            ))
            ppm <- append.XMLNode(ppm, ppcell)
          }
        }
      }
    }
  }
  the.model <- append.XMLNode(the.model, ppm)

  pmNode <- xmlNode("ParamMatrix")
  for (i in 1:length(model$coefficients))
  {
    if (!is.na(coefficients(model)[i])) {
      pcNode <- xmlNode("PCell", attrs = c(
        parameterName = gsub(" ", "", paste("p", i - 1)), df = "1",
        beta = as.numeric(coefficients(model)[i])
      ))
      pmNode <- append.XMLNode(pmNode, pcNode)
    } else {
      stop("Model coefficients did not converge and resulted in singular values")
    }
  }
  the.model <- append.XMLNode(the.model, pmNode)

  sfit <- survival::survfit(model)
  H <- -log(sfit$surv)

  strata <- sfit$strata
  if (!is.null(strata)) {
    strata <- factor(rep(names(strata), strata), levels = names(strata))
  }

  if (is.null(strata)) {
    CumHazard <- data.frame(
      hazard = H, time = sfit$time,
      stringsAsFactors = TRUE
    )
  } else {
    CumHazard <- data.frame(
      hazard = H, time = sfit$time,
      strata = strata, stringsAsFactors = TRUE
    )
  }

  numTime <- length(CumHazard$time)
  levl <- NULL
  baselineNode <- NULL
  maxTim <- 0

  if (isStrata) {
    baseTable <- xmlNode("BaseCumHazardTables")
    for (i in 1:length(levels(CumHazard$strata)))
    {
      name <- levels(CumHazard$strata)[i]
      for (j in 1:length(CumHazard$hazard))
      {
        if (CumHazard$strata[j] == name) {
          levl <- gsub(strataVar, "", name)
          levl <- gsub("=", "", levl)
          maxTim <- CumHazard$time[j]
        }
      }
      stratumNode <- xmlNode("BaselineStratum", attrs = c(value = levl, maxTime = maxTim))
      for (j in 1:length(CumHazard$hazard))
      {
        if (CumHazard$strata[j] == name) {
          if (CumHazard$hazard[j] != Inf) {
            baseCell <- xmlNode("BaselineCell", attrs = c(time = CumHazard$time[j], cumHazard = CumHazard$hazard[j]))
            stratumNode <- append.XMLNode(stratumNode, baseCell)
          } else {
            baseCell <- xmlNode("BaselineCell", attrs = c(time = CumHazard$time[j], cumHazard = 1.0E+10))
            stratumNode <- append.XMLNode(stratumNode, baseCell)
            warnNode <- xmlCommentNode("Cumulative Hazards approach Infinity after this time")
            stratumNode <- append.XMLNode(stratumNode, warnNode)
            break
          }
        }
      }
      baseTable <- append.XMLNode(baseTable, stratumNode)
    }
  } else {
    baseTable <- xmlNode("BaseCumHazardTables", attrs = c(maxTime = CumHazard$time[numTime]))
    for (i in 1:length(CumHazard$time))
    {
      if (CumHazard$hazard[i] != Inf) {
        baseCell <- xmlNode("BaselineCell", attrs = c(
          time = CumHazard$time[i],
          cumHazard = CumHazard$hazard[i]
        ))
        baseTable <- append.XMLNode(baseTable, baseCell)
      } else {
        baseCell <- xmlNode("BaselineCell", attrs = c(time = CumHazard$time[i], cumHazard = 1.0E+10))
        baseTable <- append.XMLNode(baseTable, baseCell)
        warnNode <- xmlCommentNode("Cumulative Hazards approach Infinity after this time")
        baseTable <- append.XMLNode(baseTable, warnNode)
        break
      }
    }
  }
  the.model <- append.XMLNode(the.model, baseTable)

  pmml <- append.XMLNode(pmml, the.model)

  return(pmml)
}
