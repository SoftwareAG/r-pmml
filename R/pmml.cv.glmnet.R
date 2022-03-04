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

#' Generate the PMML representation for a cv.glmnet object from the package
#' \pkg{glmnet}.
#'
#' @param model A cv.glmnet object.
#' @param missing_value_replacement Value to be used as the 'missingValueReplacement'
#' attribute for all MiningFields.
#' @param dataset Data used to train the cv.glmnet model.
#' @param s 'lambda' parameter at which to output the model. If not given, the
#' lambda.1se parameter from the model is used instead.
#'
#' @inheritParams pmml
#'
#' @return PMML representation of the cv.glmnet object.
#'
#' @details
#' The \code{glmnet} package expects the input and predicted values in a matrix
#' format - not as arrays or data frames. As of now, it will also accept
#' numerical values only. As such, any string variables must be converted to
#' numerical ones. One possible way to do so is to use data transformation
#' functions from this package. However, the result is a data frame. In all
#' cases, lists, arrays and data frames can be converted to a matrix format
#' using the data.matrix function from the base package. Given a data frame df,
#' a matrix m can thus be created by using \code{m <- data.matrix(df)}.
#'
#' The PMML language requires variable names which will be read in as the
#' column names of the input matrix. If the matrix does not have variable
#' names, they will be given the default values of "X1", "X2", ...
#'
#' Currently, only \code{gaussian} and \code{poisson} family types are
#' supported.
#'
#' @author Tridivesh Jena
#'
#' @references
#' \href{https://CRAN.R-project.org/package=glmnet}{glmnet: Lasso and
#' elastic-net regularized generalized linear models (on CRAN)}
#'
#' @examples
#' \dontrun{
#' library(glmnet)
#'
#' # Create a simple predictor (x) and response(y) matrices:
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rnorm(100)
#'
#' # Build a simple gaussian model:
#' model1 <- cv.glmnet(x, y)
#'
#' # Output the model in PMML format:
#' model1_pmml <- pmml(model1)
#'
#' # Shift y between 0 and 1 to create a poisson response:
#' y <- y - min(y)
#'
#' # Give the predictor variables names (default values are V1,V2,...):
#' name <- NULL
#' for (i in 1:20) {
#'   name <- c(name, paste("variable", i, sep = ""))
#' }
#' colnames(x) <- name
#'
#' # Create a simple poisson model:
#' model2 <- cv.glmnet(x, y, family = "poisson")
#'
#' # Output the regression model in PMML format at the lambda
#' # parameter = 0.006:
#' model2_pmml <- pmml(model2, s = 0.006)
#' }
#' @export pmml.cv.glmnet
#' @export
pmml.cv.glmnet <- function(model,
                           model_name = "Elasticnet_Model",
                           app_name = "SoftwareAG PMML Generator",
                           description = "Generalized Linear Regression Model",
                           copyright = NULL,
                           model_version = NULL,
                           transforms = NULL,
                           missing_value_replacement = NULL,
                           dataset = NULL,
                           s = NULL,
                           ...) {
  if (!inherits(model, "cv.glmnet")) stop("Not a legitimate cross-validated glmnet object")

  fitmodel <- model$glmnet.fit
  lambda <- model$lambda
  if (!is.null(s)) {
    minlambda <- s
  } else {
    minlambda <- model$lambda.1se
  }

  precision <- 0.000000001
  exact <- TRUE
  index <- 1
  index1 <- 1
  index2 <- 1
  if (lambda[1] < minlambda) {
    index <- 1
  } else if (lambda[length(lambda)] > minlambda) {
    index <- length(lambda)
  } else {
    for (i in 1:length(lambda))
    {
      if (abs(lambda[i] - minlambda) < precision) {
        index <- i
        break
      }
      if (lambda[i] <= minlambda) {
        index1 <- i
        index0 <- i - 1
        exact <- FALSE
        break
      }
    }
  }

  if (model$name == "Poisson Deviance") {
    type <- "poisson"
  } else if (model$name == "Mean-Squared Error") {
    # Type could be gaussian or mgaussian. Only mgaussian has dfmat.
    if (!is.null(model$glmnet.fit$dfmat)) {
      stop("Only poisson and gaussian family types supported.")
    } else {
      type <- "gaussian"
    }
  } else {
    stop("Only poisson and gaussian family types supported.")
  }

  beta <- fitmodel$beta
  varnames <- attributes(beta)$Dimnames[[1]]

  if (exact) {
    intercept <- fitmodel$a0[index]
    coeffs <- beta[, index]
  } else {
    v0 <- fitmodel$a0[index0]
    v1 <- fitmodel$a0[index1]
    l <- minlambda
    l0 <- lambda[index0]
    l1 <- lambda[index1]
    intercept <- (v1 * (l - l0) - v0 * (l - l1)) / (l1 - l0)
    v0 <- beta[, index0]
    v1 <- beta[, index1]
    l <- minlambda
    l0 <- lambda[index0]
    l1 <- lambda[index1]
    coeffs <- (v1 * (l - l0) - v0 * (l - l1)) / (l1 - l0)
  }

  class <- NULL
  field <- NULL
  field$name <- c("predictedScore", varnames)

  for (i in 1:length(field$name))
  {
    if (is.null(dataset)) {
      class <- c(class, "numeric")
    } else {
      class <- c(class, class(dataset[, i]))
    }
  }

  field$class <- class
  names(field$class) <- field$name

  number.of.fields <- length(field$name)
  target <- "predictedScore"

  field$name <- enc2utf8(field$name)

  # Not checked; not used in present implementation of regression models.
  # Kept for future use if multinomial models are implemented.
  # if (FALSE) {
  #   ylevels <- FALSE
  #   numfac <- 0
  #   for (i in 1:number.of.fields)
  #   {
  #     if (field$class[[field$name[i]]] == "factor") {
  #       numfac <- numfac + 1
  #       if (field$name[i] == target) {
  #         if (length(levels(model$data[[field$name[i]]])) != 2) {
  #           stop("binomial family with more than two target categories is not
  #               currently supported by PMML")
  #         }
  #         ylevels <- TRUE
  #         field$levels[[field$name[i]]] <- levels(model$data[[field$name[i]]])
  #       } else {
  #         field$levels[[field$name[i]]] <- model$xlevels[[field$name[i]]]
  #
  #         if (length(grep("^as.factor\\(", field$name[i]))) {
  #           field$name[i] <- sub("^as.factor\\((.*)\\)", "\\1", field$name[i])
  #           names(field$class)[i] <- sub("^as.factor\\((.*)\\)", "\\1", names(field$class)[i])
  #           names(field$levels)[numfac] <- sub("^as.factor\\((.*)\\)", "\\1", names(field$levels)[numfac])
  #         }
  #       }
  #     }
  #   }
  # }

  # PMML

  pmml <- .pmmlRootNode()

  # PMML -> Header

  pmml <- append.XMLNode(pmml, .pmmlHeader(
    description, copyright, model_version,
    app_name
  ))

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field, transformed = transforms))

  # Following not used or checked. Not used for present implementation of regression models.
  # Kept for possible future use if multinomial general regression models are implemented.

  # if (FALSE) {
  #   # Determine the distribution and link function to add. Quasi distributions cannot be
  #   # listed. Certain link functions are not supported and an error must be thrown. Certain
  #   # link function can be recast as a power link and must be constructed separately.
  #   add <- FALSE
  #   addl <- FALSE
  #   addl2 <- FALSE
  #   if (model$call[[1]] == "glm") {
  #     model.type <- model$family$family
  #     model.link <- model$family$link
  #   }
  #   else {
  #     model.type <- "unknown"
  #   }
  #
  #   # Only binary categorical cases can be handled. For multinomial cases, glm assumes the first
  #   # category as one and all the rest together as one. The output then is the probability of the
  #   # first category NOT be true. This case is not implemented.
  #   categ <- FALSE
  #   if (ylevels) {
  #     if (model.type == "binomial") {
  #       categ <- TRUE
  #       add <- TRUE
  #     }
  #   }
  #
  #   if (model.type == "binomial") {
  #     add <- TRUE
  #   }
  #   if (model.type == "Gamma") {
  #     model.type <- "gamma"
  #     add <- TRUE
  #   }
  #   if (model.type == "inverse.gaussian") {
  #     model.type <- "igauss"
  #     add <- TRUE
  #   }
  #   if (model.type == "gaussian") {
  #     model.type <- "normal"
  #     add <- TRUE
  #   }
  #   if (model.type == "poisson") {
  #     add <- TRUE
  #   }
  #
  #   if (model.link == "cloglog") {
  #     addl <- TRUE
  #   } else
  #   if (model.link == "identity") {
  #     addl <- TRUE
  #   } else
  #   if (model.link == "log") {
  #     addl <- TRUE
  #   } else
  #   if (model.link == "logit") {
  #     addl <- TRUE
  #   } else
  #   if (model.link == "probit") {
  #     addl <- TRUE
  #   } else
  #   if (model.link == "inverse") {
  #     addl <- TRUE
  #     addl2 <- TRUE
  #     d <- "-1"
  #   } else
  #   if (model.link == "sqrt") {
  #     addl <- TRUE
  #     addl2 <- TRUE
  #     d <- "0.5"
  #   } else {
  #     stop("link function currently not supported by PMML")
  #   }
  #
  #   if (categ) {
  #     the.model <- xmlNode("GeneralRegressionModel",
  #       attrs = c(
  #         modelName = model_name,
  #         modelType = "generalizedLinear",
  #         functionName = "classification",
  #         algorithmName = "glm",
  #         distribution = model.type,
  #         linkFunction = model.link
  #       )
  #     )
  #   } else if (add && addl && addl2) {
  #     the.model <- xmlNode("GeneralRegressionModel",
  #       attrs = c(
  #         modelName = model_name,
  #         modelType = "generalizedLinear",
  #         functionName = "regression",
  #         algorithmName = "glm",
  #         distribution = model.type,
  #         linkFunction = "power",
  #         linkParameter = d
  #       )
  #     )
  #   } else if (add && addl && !addl2) {
  #     the.model <- xmlNode("GeneralRegressionModel",
  #       attrs = c(
  #         modelName = model_name,
  #         modelType = "generalizedLinear",
  #         functionName = "regression",
  #         algorithmName = "glm",
  #         distribution = model.type,
  #         linkFunction = model.link
  #       )
  #     )
  #   } else if (!add && addl && addl2) {
  #     the.model <- xmlNode("GeneralRegressionModel",
  #       attrs = c(
  #         modelName = model_name,
  #         modelType = "generalizedLinear",
  #         functionName = "regression",
  #         algorithmName = "glm",
  #         linkFunction = "power",
  #         linkParameter = d
  #       )
  #     )
  #   } else if (!add && addl && !addl2) {
  #     the.model <- xmlNode("GeneralRegressionModel",
  #       attrs = c(
  #         modelName = model_name,
  #         modelType = "generalizedLinear",
  #         functionName = "regression",
  #         algorithmName = "glm",
  #         linkFunction = model.link
  #       )
  #     )
  #   } else {
  #     stop("model type not supported")
  #   }
  # }

  the.model <- xmlNode("GeneralRegressionModel",
    attrs = c(
      modelName = model_name,
      modelType = "generalLinear",
      algorithmName = "glmnet",
      functionName = "regression"
    )
  )


  extensionNode <- xmlNode("Extension", attrs = c(name = "lambda", value = minlambda))
  the.model <- append.XMLNode(the.model, extensionNode)

  # PMML -> RegressionModel -> MiningSchema

  the.model <- append.XMLNode(the.model, .pmmlMiningSchema(field, target, transforms, missing_value_replacement = missing_value_replacement))

  # Assume that functionName="regression" always.
  outn <- xmlNode("Output")
  outpn <- xmlNode("OutputField", attrs = c(
    name = "predictedValue", feature = "predictedValue",
    dataType = "double", optype = "continuous"
  ))
  outn <- append.XMLNode(outn, outpn)
  if (type == "poisson") {
    outpn <- xmlNode("OutputField", attrs = c(
      name = "predictedMean", feature = "transformedValue",
      dataType = "double"
    ))
    applyNode <- xmlNode("Apply", attrs = c("function" = "exp"))
    fldNode <- xmlNode("FieldRef", attrs = c(field = "predictedValue"))
    applyNode <- append.XMLNode(applyNode, fldNode)
    outpn <- append.XMLNode(outpn, applyNode)
    outn <- append.XMLNode(outn, outpn)
  }



  the.model <- append.XMLNode(the.model, outn)

  #--------------------------------------------
  # PMML -> Model -> LocalTransforms

  if (!is.null(transforms)) {
    the.model <- append.XMLNode(the.model, .pmmlLocalTransformations(field, transforms, NULL))
  }

  plNode <- xmlNode("ParameterList")
  pnode <- xmlNode("Parameter", attrs = c(name = "p0", label = "Intercept"))
  plNode <- append.XMLNode(plNode, pnode)

  for (i in 1:length(coeffs))
  {
    pnode <- xmlNode("Parameter", attrs = c(name = paste("p", i, sep = ""), label = varnames[i]))
    plNode <- append.XMLNode(plNode, pnode)
  }

  the.model <- append.XMLNode(the.model, plNode)

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
  for (i in 1:length(coeffs)) {
    ppcell <- xmlNode("PPCell", attrs = c(
      value = "1", predictorName = varnames[i],
      parameterName = paste("p", i, sep = "")
    ))
    ppm <- append.XMLNode(ppm, ppcell)
  }

  the.model <- append.XMLNode(the.model, ppm)

  pmNode <- xmlNode("ParamMatrix")
  pcNode <- xmlNode("PCell", attrs = c(parameterName = "p0", df = "1", beta = intercept[[1]]))
  pmNode <- append.XMLNode(pmNode, pcNode)
  for (i in 1:length(coeffs))
  {
    if ((!is.na(coeffs[i])) && (coeffs[i] != 0)) {
      pcNode <- xmlNode("PCell", attrs = c(
        parameterName = paste("p", i, sep = ""), df = "1",
        beta = coeffs[[i]]
      ))
      pmNode <- append.XMLNode(pmNode, pcNode)
    }
  }

  the.model <- append.XMLNode(the.model, pmNode)

  pmml <- append.XMLNode(pmml, the.model)

  return(pmml)
}
