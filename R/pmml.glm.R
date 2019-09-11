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

#' Generate the PMML representation for a glm object from the package \pkg{stats}.
#'
#'
#' @param model A glm object.
#' @param missing_value_replacement Value to be used as the 'missingValueReplacement'
#' attribute for all MiningFields.
#' @param weights The weights used for building the model.
#'
#' @inheritParams pmml
#'
#' @return PMML representation of the glm object.
#'
#' @details
#' The function exports the glm model in the PMML GeneralRegressionModel
#' format.
#'
#' Note on glm models for 2-class problems: a dataset where the target
#' categorical variable has more than 2 classes may be turned into a 2-class
#' problem by creating a new target variable that is TRUE for a particular
#' class and FALSE for all other classes. While the R formula function allows
#' such a transformation to be passed directly to it, this may cause issues
#' when the model is converted to PMML. Therefore, it is advised to create a
#' new 2-class separately, and then pass that variable to glm(). This is shown
#' in an example below.
#'
#'
#' @references
#' \href{http://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html}{R project: Fitting Generalized Linear Models}
#'
#' @examples
#' data(iris)
#' mod <- glm(Sepal.Length ~ ., data = iris, family = "gaussian")
#' mod_pmml <- pmml(mod)
#' rm(mod, mod_pmml)
#'
#' data(audit)
#' mod <- glm(Adjusted ~ Age + Employment + Education + Income, data = audit, family = binomial(logit))
#' mod_pmml <- pmml(mod)
#' rm(mod, mod_pmml)
#'
#' # Create a new 2-class target from a 3-class variable:
#' data(iris)
#' dat <- iris[, 1:4]
#' # Add a new 2-class target "Species_setosa" before passing it to glm():
#' dat$Species_setosa <- iris$Species == "setosa"
#' mod <- glm(Species_setosa ~ ., data = dat, family = binomial(logit))
#' mod_pmml <- pmml(mod)
#' rm(dat, mod, mod_pmml)
#' @export pmml.glm
#' @export
pmml.glm <- function(model,
                     model_name = "General_Regression_Model",
                     app_name = "SoftwareAG PMML Generator",
                     description = "Generalized Linear Regression Model",
                     copyright = NULL,
                     transforms = NULL,
                     missing_value_replacement = NULL,
                     weights = NULL,
                     ...) {
  if (!inherits(model, "glm")) stop("Not a legitimate glm object")

  # For a regression, all variables will have been used except those with a NA coefficient
  # indicating singularities. We mark singularities as inactive shortly.

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
  if (length(grep("^as.factor\\(", field$name[1]))) {
    field$name[1] <- sub("^as.factor\\((.*)\\)", "\\1", field$name[1])
    target <- field$name[1]
    names(field$class)[1] <- target
    levels(model$data[[target]]) <- levels(model$model[[1]])
  }

  inactive <- names(which(is.na(coef(model))))
  active <- names(which(!is.na(coef(model))))

  field$name <- enc2utf8(field$name)

  tmp <- sapply(sapply(field$name, grep, inactive), length)
  inactive.vars <- names(tmp[tmp > 0])
  tmp <- sapply(sapply(field$name, grep, active), length)
  active.vars <- names(tmp[tmp > 0])

  ylevels <- FALSE
  numfac <- 0
  for (i in 1:number.of.fields)
  {
    if (field$class[[field$name[i]]] == "factor") {
      numfac <- numfac + 1
      if (field$name[i] == target) {
        if (length(levels(model$data[[field$name[i]]])) != 2) {
          stop("binomial family with more than two target categories is not
                currently supported by PMML")
        }
        ylevels <- TRUE
        field$levels[[field$name[i]]] <- levels(model$data[[field$name[i]]])
      } else {
        field$levels[[field$name[i]]] <- model$xlevels[[field$name[i]]]

        if (length(grep("^as.factor\\(", field$name[i]))) {
          field$name[i] <- sub("^as.factor\\((.*)\\)", "\\1", field$name[i])
          names(field$class)[i] <- sub("^as.factor\\((.*)\\)", "\\1", names(field$class)[i])
          names(field$levels)[numfac] <- sub("^as.factor\\((.*)\\)", "\\1", names(field$levels)[numfac])
        }
      }
    } else {
      if (length(grep("^as.numeric\\(", field$name[i]))) {
        field$name[i] <- sub("^as.numeric\\((.*)\\)", "\\1", field$name[i])
        names(field$class)[i] <- sub("^as.numeric\\((.*)\\)", "\\1", names(field$class)[i])
        names(field$levels)[numfac] <- sub("^as.numeric\\((.*)\\)", "\\1", names(field$levels)[numfac])
      }
    }
  }

  # PMML

  pmml <- .pmmlRootNode()

  # PMML -> Header

  pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, app_name))

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field, weights = weights, transformed = transforms))

  # Determine the distribution and link function to add. Quasi distributions cannot be
  # listed. Certain link functions are not supported and an error must be thrown. Certain
  # link functions can be recast as a power link and must be constructed separately.
  add <- FALSE
  addl <- FALSE
  addl2 <- FALSE
  if (model$call[[1]] == "glm") {
    model.type <- model$family$family
    model.link <- model$family$link
  }
  else {
    model.type <- "unknown"
  }

  # Only binary categorical cases can be handled. For multinomial cases, glm assumes the first
  # category as one and all the rest together as one. The output then is the probability of the
  # first category NOT be true. This case is not implemented.
  categ <- FALSE
  if (ylevels) {
    if (model.type == "binomial") {
      categ <- TRUE
      add <- TRUE
    }
  }

  if (model.type == "binomial") {
    add <- TRUE
  }
  if (model.type == "Gamma") {
    model.type <- "gamma"
    add <- TRUE
  }
  if (model.type == "inverse.gaussian") {
    model.type <- "igauss"
    add <- TRUE
  }
  if (model.type == "gaussian") {
    model.type <- "normal"
    add <- TRUE
  }
  if (model.type == "poisson") {
    add <- TRUE
  }

  if (model.link == "cloglog") {
    addl <- TRUE
  } else
  if (model.link == "identity") {
    addl <- TRUE
  } else
  if (model.link == "log") {
    addl <- TRUE
  } else
  if (model.link == "logit") {
    addl <- TRUE
  } else
  if (model.link == "probit") {
    addl <- TRUE
  } else
  if (model.link == "inverse") {
    addl <- TRUE
    addl2 <- TRUE
    d <- "-1"
  } else
  if (model.link == "sqrt") {
    addl <- TRUE
    addl2 <- TRUE
    d <- "0.5"
  } else {
    stop("link function currently not supported by PMML")
  }

  if (categ) {
    the.model <- xmlNode("GeneralRegressionModel",
      attrs = c(
        modelName = model_name,
        modelType = "generalizedLinear",
        functionName = "classification",
        algorithmName = "glm",
        distribution = model.type,
        linkFunction = model.link
      )
    )
  } else if (add && addl && addl2) {
    the.model <- xmlNode("GeneralRegressionModel",
      attrs = c(
        modelName = model_name,
        modelType = "generalizedLinear",
        functionName = "regression",
        algorithmName = "glm",
        distribution = model.type,
        linkFunction = "power",
        linkParameter = d
      )
    )
  } else if (add && addl && !addl2) {
    the.model <- xmlNode("GeneralRegressionModel",
      attrs = c(
        modelName = model_name,
        modelType = "generalizedLinear",
        functionName = "regression",
        algorithmName = "glm",
        distribution = model.type,
        linkFunction = model.link
      )
    )
  } else if (!add && addl && addl2) {
    the.model <- xmlNode("GeneralRegressionModel",
      attrs = c(
        modelName = model_name,
        modelType = "generalizedLinear",
        functionName = "regression",
        algorithmName = "glm",
        linkFunction = "power",
        linkParameter = d
      )
    )
  } else if (!add && addl && !addl2) {
    the.model <- xmlNode("GeneralRegressionModel",
      attrs = c(
        modelName = model_name,
        modelType = "generalizedLinear",
        functionName = "regression",
        algorithmName = "glm",
        linkFunction = model.link
      )
    )
  } else {
    stop("model type not supported")
  }



  # PMML -> RegressionModel -> MiningSchema

  the.model <- append.XMLNode(the.model, .pmmlMiningSchema(field, target, transforms, missing_value_replacement = missing_value_replacement))

  if (categ) {
    outn <- xmlNode("Output")
    pname <- gsub(" ", "", paste("Probability_", levels(model$data[[field$name[1]]])[2]))
    outpn <- xmlNode("OutputField", attrs = c(
      name = pname, targetField = target, feature = "probability",
      value = levels(model$data[[field$name[1]]])[2],
      optype = "continuous", dataType = "double"
    ))
    outpn2 <- xmlNode("OutputField", attrs = c(
      name = gsub(" ", "", paste("Predicted_", target)), feature = "predictedValue",
      optype = "categorical", dataType = "string"
    ))
    outn <- append.XMLNode(outn, outpn)
    outn <- append.XMLNode(outn, outpn2)
  } else {
    outn <- xmlNode("Output")
    out <- xmlNode("OutputField", attrs = c(
      name = gsub(" ", "", paste("Predicted_", target)), feature = "predictedValue",
      optype = "continuous", dataType = "double"
    ))
    outn <- append.XMLNode(outn, out)
  }


  the.model <- append.XMLNode(the.model, outn)

  #---------------------------------------------------
  # PMML -> TreeModel -> LocalTransforms

  if (!is.null(transforms)) {
    the.model <- append.XMLNode(the.model, .pmmlLocalTransformations(field, transforms))
  }

  plNode <- xmlNode("ParameterList")
  num <- 0
  for (i in 1:length(names(coefficients(model))))
  {
    pname <- paste("p", num)
    pname <- gsub(" ", "", pname)
    num <- num + 1
    pnode <- xmlNode("Parameter", attrs = c(name = pname, label = names(coefficients(model))[i]))
    plNode <- append.XMLNode(plNode, pnode)
  }

  the.model <- append.XMLNode(the.model, plNode)

  flNode <- xmlNode("FactorList")
  # Start from 2, as 1 is the predicted field.
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
  for (j in 1:length(model$coefficients))
  {
    special <- FALSE
    coefname <- names(coefficients(model))[j]
    if (grepl("^`.*`$", coefname)) {
      coefname <- gsub("^`", "", coefname)
      coefname <- gsub("`$", "", coefname)
    }

    if (grepl("Intercept", coefname)) {
    } else if (grepl(".+:.+", coefname)) {
      # interactive terms
      fnms <- strsplit(coefname, ":")[[1]]
      for (k in 1:length(fnms))
      {
        if (grepl("`", fnms[k])) {
          special <- TRUE
          fnm <- strsplit(fnms, "`")[[1]]
          fnm <- fnm[fnm != ""]
        } else {
          fnm <- fnms[k]
        }

        for (f in 2:number.of.fields)
        {
          if (field$class[[field$name[f]]] == "factor") {
            match <- FALSE
            if (special) {
              match <- (field$name[f] %in% fnm)
            } else {
              match <- grepl(field$name[f], fnm)
            }

            if (!special && match) {
              modfield <- gsub(field$name[f], "", fnm)
              ppcell <- xmlNode("PPCell", attrs = c(
                value = modfield, predictorName = field$name[f],
                parameterName = gsub(" ", "", paste("p", j - 1))
              ))
              ppm <- append.XMLNode(ppm, ppcell)
            } else if (special && match) {
              ppcell <- xmlNode("PPCell", attrs = c(
                value = fnm[2], predictorName = field$name[f],
                parameterName = gsub(" ", "", paste("p", j - 1))
              ))
              ppm <- append.XMLNode(ppm, ppcell)
            }
          } else {
            if (field$name[f] == fnm[1]) {
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
      if (grepl("`", coefname)) {
        special <- TRUE
        fnm <- strsplit(coefname, "`")[[1]]
        fnm <- fnm[fnm != ""]
      } else {
        fnm <- coefname
      }

      for (f in 2:number.of.fields)
      {
        if (field$class[[field$name[f]]] == "factor") {
          match <- FALSE
          if (special) {
            match <- (field$name[f] %in% fnm)
          } else {
            match <- grepl(field$name[f], fnm)
          }

          if (match && special) {
            ppcell <- xmlNode("PPCell", attrs = c(
              value = fnm[2], predictorName = field$name[f],
              parameterName = gsub(" ", "", paste("p", j - 1))
            ))
            ppm <- append.XMLNode(ppm, ppcell)
          } else if (match && !special) {
            modfield <- gsub(field$name[f], "", fnm)
            if (grepl("^as.factor\\(", fnm)) {
              modfield <- sub("^as.factor\\((.*)\\)", "\\1", fnm)
              modfield <- gsub(field$name[f], "", modfield)
            } else {
              modfield <- gsub(field$name[f], "", fnm)
            }
            ppcell <- xmlNode("PPCell", attrs = c(
              value = modfield, predictorName = field$name[f],
              parameterName = gsub(" ", "", paste("p", j - 1))
            ))
            ppm <- append.XMLNode(ppm, ppcell)
          }
        } else {
          # numerical terms
          if (field$name[f] == fnm[1]) {
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
      if (categ) {
        pcNode <- xmlNode("PCell", attrs = c(
          targetCategory = field$levels[[target]][2],
          parameterName = gsub(" ", "", paste("p", i - 1)), df = "1",
          beta = as.numeric(coefficients(model)[i])
        ))
        pmNode <- append.XMLNode(pmNode, pcNode)
      } else {
        pcNode <- xmlNode("PCell", attrs = c(
          parameterName = gsub(" ", "", paste("p", i - 1)), df = "1",
          beta = as.numeric(coefficients(model)[i])
        ))
        pmNode <- append.XMLNode(pmNode, pcNode)
      }
    } else {
      #     stop("Model coefficients did not converge")
    }
  }
  the.model <- append.XMLNode(the.model, pmNode)

  # Add to the top level structure.

  pmml <- append.XMLNode(pmml, the.model)

  return(pmml)
}
