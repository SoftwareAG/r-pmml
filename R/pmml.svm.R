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

#' Generate the PMML representation of an svm object from the \pkg{e1071} package.
#'
#' @param model An svm object from package \pkg{e1071}.
#' @param missing_value_replacement Value to be used as the 'missingValueReplacement' attribute for all MiningFields.
#' @param dataset Required for one-classification only; data used to train the one-class SVM model.
#'
#' @inheritParams pmml
#'
#' @return PMML representation of the svm object.
#'
#' @details The model is represented in the PMML SupportVectorMachineModel format.
#'
#' Note that the sign of the coefficient of each support vector flips between the R object
#' and the exported PMML file for classification and regression models. This is due to the minor
#' difference in the training/scoring formula between the LIBSVM algorithm and the DMG specification.
#' Hence the output value of each support vector machine has a sign flip between the DMG definition and
#' the svm prediction function.
#'
#' In a classification model, even though the output of the support vector machine has
#' a sign flip, it does not affect the final predicted category. This is because in the
#' DMG definition, the winning category is defined as the left side of threshold 0 while
#' the LIBSVM defines the winning category as the right side of threshold 0.
#'
#' For a regression model, the exported PMML code has two OutputField elements. The OutputField
#' \code{predictedValue} shows the support vector machine output per DMG definition. The OutputField
#' \code{svm_predict_function} gives the value corresponding to the R predict function for the svm
#' model. This output should be used when making model predictions.
#'
#' For a one-classification svm (OCSVM) model, the PMML has three OutputField elements.
#'
#' The OutputField \code{anomalyScore} is the signed distance to the separating boundary; 
#' \code{anomalyScore} corresponds to the decision.values attribute of the output of the 
#' svm predict function in R.
#'
#' The OutputField \code{anomaly} is a boolean value that is TRUE when an anomaly is detected. 
#' This field conforms to the DMG definition of an anomaly detection model. This value is the 
#' opposite of the prediction by the e1071::svm object in R.
#'
#' The OutputField \code{svm_predict_function} is a boolean value that is TRUE when an inlier is 
#' detected, and conforms to the e1071 definition of one-class SVMs. This field is FALSE when 
#' an anomaly is detected; that is, the R svm model predicts whether an input belongs to the 
#' class. When comparing the predictions from R and PMML, this field should be used, since it 
#' will match R's output.
#'
#' For example, say that for an input of observations, the R OCSVM model predicts a positive 
#' decision value of 0.4 and label of TRUE. According to the R object, this means that the 
#' observation is an inlier. The PMML export of this model will give the following for the 
#' same input: \code{anomalyScore = 0.4, anomaly = "false", svm_predict_function="true"}. 
#' According to the PMML, the observation is not an anomaly. Note that there is no sign flip 
#' for \code{anomalyScore} between R and PMML for OCSVM models.
#'
#'
#' To export a OCSVM model, an additional argument, \code{dataset}, is required by the function.
#' This argument expects a dataframe with data that was used to train the model. This is
#' necessary because for one-class svm, the R svm object does not contain information about
#' the data types of the features used to train the model. The exporter does not yet support
#' the formula interface for one-classification models, so the default S3 method must be used
#' to train the SVM. The data used to train the one-class SVM must be numeric and not of
#' integer class.
#'
#' @references
#' * R project CRAN package: \emph{\bold{e1071}: Misc Functions of the Department of Statistics,
#' Probability Theory Group (Formerly: E1071), TU Wien} \url{https://CRAN.R-project.org/package=e1071}
#'
#' * Chang, Chih-Chung and Lin, Chih-Jen, \emph{LIBSVM: a library for Support Vector Machines}
#'   \url{http://www.csie.ntu.edu.tw/~cjlin/libsvm}
#'
#' @examples
#'
#' \dontrun{
#' library(e1071)
#' data(iris)
#'
#' # Classification with a polynomial kernel
#' fit <- svm(Species ~ ., data = iris, kernel = "polynomial")
#' fit_pmml <- pmml(fit)
#'
#' # Regression
#' fit <- svm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
#' fit_pmml <- pmml(fit)
#'
#' # Anomaly detection with one-classification
#' fit <- svm(iris[, 1:4], y = NULL, type = "one-classification")
#' fit_pmml <- pmml(fit, dataset = iris[, 1:4])
#' }
#'
#' @seealso \code{\link[pmml]{pmml}},
#' \href{http://dmg.org/pmml/v4-3/SupportVectorMachine.html}{PMML SVM specification}
#'
#' @export pmml.svm
#' @export
pmml.svm <- function(model,
                     model_name = "LIBSVM_Model",
                     app_name = "SoftwareAG PMML Generator",
                     description = "Support Vector Machine Model",
                     copyright = NULL,
                     transforms = NULL,
                     missing_value_replacement = NULL,
                     dataset = NULL,
                     ...) {
  if (!inherits(model, "svm")) stop("Not a legitimate svm object")

  #---------------------------------------------------
  # Check if svm model is being used for novelty detection.
  if (model$type == 2) {

    # Dataset must not be null for one-classfication models.
    if (is.null(dataset)) {
      stop("dataset must not be null for one-classification.")
    }
    dataset <- dataset[1:10, ]
    feature.info <- sapply(dataset, class)

    if ((!(all(sapply(dataset, is.numeric)))) | (any(sapply(dataset, is.integer)))) {
      stop("Features must be of numeric, and not integer.")
    }

    if (!is.null(model$call$formula)) {
      stop("Formula interface not supported for one-class svm. Please use the default S3 method to train.")
    }


    field <- NULL
    field$name <- names(feature.info)
    field$class <- feature.info
    functionName <- "regression"

    #----------------------------------------------------------
    # PMML

    pmml <- .pmmlRootNode("4.4")

    #----------------------------------------------------------
    # PMML -> Header

    pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, app_name))

    #-----------------------------------------------------------
    # PMML -> DataDictionary

    pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field, transformed = transforms))

    #------------------------------------------------------------
    # PMML -> AnomalyDetectionModel
    xmlADModel <- xmlNode("AnomalyDetectionModel",
      attrs = c(
        modelName = model_name,
        functionName = "regression",
        algorithmType = "ocsvm"
      )
    )

    #---------------------------------------------------------------
    # PMML -> AnomalyDetectionModel -> MiningSchema

    xmlMiningSchema <- .pmmlMiningSchema(
      field = field,
      transformed = transforms,
      missing_value_replacement = missing_value_replacement
    )

    xmlADModel <- append.XMLNode(xmlADModel, xmlMiningSchema)

    #-----------------------------------------------------------------
    # PMML -> AnomalyDetectionModel -> Output

    xmlADOutput <- xmlNode("Output")
    xmlOF_anomalyScore <- xmlNode("OutputField",
      attrs = c(
        name = "anomalyScore",
        feature = "predictedValue",
        dataType = "double",
        optype = "continuous"
      )
    )

    ## old anomaly field that uses "threshold" attribute.
    # xmlOF_anomaly <- xmlNode("OutputField",
    #   attrs = c(
    #     name = "anomaly", feature = "anomaly",
    #     dataType = "boolean", optype = "categorical", threshold = "0"
    #   )
    # )

    # updated anomaly field that replaces "threshold" attribute with Apply function.
    xmlOF_anomaly <- xmlNode("OutputField",
                             attrs = c(
                               name = "anomaly", feature = "decision",
                               dataType = "boolean", optype = "categorical"
                             ))

    xmlOF_anomaly_apply <- xmlNode("Apply", attrs = c("function" = "lessThan"))
    xmlOF_anomaly_fieldref <- xmlNode("FieldRef", attrs = c(field = "anomalyScore"))
    xmlOF_anomaly_constant <- xmlNode("Constant", 0, attrs = c(dataType = "double"))
    xmlOF_anomaly_apply <- append.XMLNode(xmlOF_anomaly_apply, xmlOF_anomaly_fieldref, xmlOF_anomaly_constant)
    xmlOF_anomaly <- append.XMLNode(xmlOF_anomaly, xmlOF_anomaly_apply)

    # Additional output field with a transformed decision.values value.
    # This value corresponds to R's prediction for the one-class svm model.
    xmlOF_svm_predict_anomaly <- xmlNode("OutputField",
      attrs = c(
        name = "svm_predict_anomaly",
        feature = "transformedValue",
        dataType = "boolean",
        optype = "categorical"
      )
    )

    xmlOF_svm_predict_anomaly_apply <- xmlNode("Apply", attrs = c("function" = "greaterOrEqual"))
    xmlOF_svm_predict_anomaly_fieldref <- xmlNode("FieldRef", attrs = c(field = "anomalyScore"))
    xmlOF_svm_predict_anomaly_constant <- xmlNode("Constant", 0, attrs = c(dataType = "double"))
    xmlOF_svm_predict_anomaly_apply <- append.XMLNode(xmlOF_svm_predict_anomaly_apply,
                                                      xmlOF_svm_predict_anomaly_fieldref,
                                                      xmlOF_svm_predict_anomaly_constant)
    xmlOF_svm_predict_anomaly <- append.XMLNode(xmlOF_svm_predict_anomaly, xmlOF_svm_predict_anomaly_apply)
    
    
    
    # # pre-4.4 negation of anomaly field to correspond to R
    # xmlApply <- xmlNode("Apply", attrs = c("function" = "not"))
    # xmlFR <- xmlNode("FieldRef", attrs = c(field = "anomaly"))
    # xmlApply <- append.XMLNode(xmlApply, xmlFR)
    # xmlOF_svm_predict_anomaly <- append.XMLNode(xmlOF_svm_predict_anomaly, xmlApply)

    xmlADOutput <- append.XMLNode(xmlADOutput, xmlOF_anomalyScore)
    xmlADOutput <- append.XMLNode(xmlADOutput, xmlOF_anomaly)
    xmlADOutput <- append.XMLNode(xmlADOutput, xmlOF_svm_predict_anomaly)

    xmlADModel <- append.XMLNode(xmlADModel, xmlADOutput)

    #------------------------------------------------------------
    # PMML -> AnomalyDetectionModel -> SupportVectorMachineModel
    xmlModel <- xmlNode("SupportVectorMachineModel",
      attrs = c(
        modelName = "ocsvm",
        functionName = "regression", # Required
        threshold = "0"
      )
    )

    #---------------------------------------------------------------
    # SupportVectorMachineModel -> MiningSchema

    xmlModel <- append.XMLNode(xmlModel, xmlMiningSchema)

    #-----------------------------------------------------------------
    # PMML -> SupportVectorMachineModel -> Output

    xmlOutput <- xmlNode("Output")
    xmlOF_predicted <- xmlNode("OutputField",
      attrs = c(
        name = "predicted",
        feature = "predictedValue",
        dataType = "double",
        optype = "continuous"
      )
    )

    xmlOutput <- append.XMLNode(xmlOutput, xmlOF_predicted)
    xmlModel <- append.XMLNode(xmlModel, xmlOutput)

    xmlModel <- .makeOtherNodes(model, field, transforms, functionName, xmlModel)
    #-------------------------------------------------------------------

    xmlADModel <- append.XMLNode(xmlADModel, xmlModel)
    pmml <- append.XMLNode(pmml, xmlADModel)
  } else {
    # Case when the object is not anomaly detection

    field <- NULL
    field$name <- as.character(attr(model$terms, "variables"))[-1]
    field$class <- attr(model$terms, "dataClasses")

    target <- as.character(attr(model$terms, "variables"))[-1][1]

    functionName <- "classification"
    if (field$class[[1]] == "numeric") {
      functionName <- "regression"
    }

    #----------------------------------------------------------
    # PMML

    pmml <- .pmmlRootNode()

    #----------------------------------------------------------
    # PMML -> Header

    pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, app_name))

    #-----------------------------------------------------------
    # PMML -> DataDictionary

    pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field, transformed = transforms, target = target))

    #------------------------------------------------------------
    # PMML -> SupportVectorMachineModel

    if (model$nclasses > 2) {
      xmlModel <- xmlNode("SupportVectorMachineModel",
        attrs = c(
          modelName = model_name,
          functionName = functionName, # Required
          algorithmName = "LIBSVM", classificationMethod = "OneAgainstOne"
        )
      )
    } else {
      xmlModel <- xmlNode("SupportVectorMachineModel",
        attrs = c(
          modelName = model_name,
          functionName = functionName, # Required
          algorithmName = "LIBSVM"
        )
      )
    }


    #---------------------------------------------------------------
    # PMML -> SupportVectorMachineModel -> MiningSchema

    xmlModel <- append.XMLNode(xmlModel, .pmmlMiningSchema(field, target, transformed = transforms, missing_value_replacement = missing_value_replacement))

    #-----------------------------------------------------------------
    # PMML -> SupportVectorMachineModel -> Output

    xmlOutput <- NULL

    if (functionName == "regression") {
      xmlOutput <- xmlNode("Output")
      xmlOF_predicted <- xmlNode("OutputField",
        attrs = c(
          name = "predictedValue",
          feature = "predictedValue",
          dataType = "double",
          optype = "continuous"
        )
      )
      xmlOutput <- append.XMLNode(xmlOutput, xmlOF_predicted)

      xmlOF <- xmlNode("OutputField",
        attrs = c(
          name = "svm_predict_function",
          feature = "transformedValue",
          dataType = "double",
          optype = "continuous"
        )
      )

      ym <- model$y.scale$`scaled:center`[[1]]
      ys <- model$y.scale$`scaled:scale`[[1]]

      xmlApply <- xmlNode("Apply", attrs = c("function" = "*"))
      xmlFR <- xmlNode("FieldRef", attrs = c(field = "predictedValue"))
      xmlConst <- xmlNode("Constant", -1 * ys)

      xmlApply <- append.XMLNode(xmlApply, xmlFR)
      xmlApply <- append.XMLNode(xmlApply, xmlConst)

      xmlApply_sum <- xmlNode("Apply", attrs = c("function" = "+"))
      xmlConst_sum <- xmlNode("Constant", ym)
      xmlApply_sum <- append.XMLNode(xmlApply_sum, xmlApply)
      xmlApply_sum <- append.XMLNode(xmlApply_sum, xmlConst_sum)

      xmlOF <- append.XMLNode(xmlOF, xmlApply_sum)

      xmlOutput <- append.XMLNode(xmlOutput, xmlOF)
    } else {
      xmlOutput <- .pmmlOutput(field, target)
    }

    xmlModel <- append.XMLNode(xmlModel, xmlOutput)

    xmlModel <- .makeOtherNodes(model, field, transforms, functionName, xmlModel)

    pmml <- append.XMLNode(pmml, xmlModel)
  }

  return(pmml)
}

.makeOtherNodes <- function(model, field, transforms, functionName, xmlModel) {
  # Function to create LocalTransformations, Kernel, Vector Instances, and Suppor Vector Machines.
  # This function is very similar for one-class SVM and and other SVMs; vfStart is the only variable that changes.
  #
  # vfStart: starting value for a for loop. For Anomaly Detection (type 2), set vfStart to 1.
  #          Otherwise, set to 2.

  if (model$type == 2) {
    vfStart <- 1
  } else {
    vfStart <- 2
  }


  #------------------------------------------------------------------
  # PMML -> SupportVectorMachineModel -> LocalTransformations

  #------------------------------------------------------------------
  # PMML -> SupportVectorMachineModel -> LocalTransformations

  xmlLT <- NULL

  if (!is.null(transforms)) {
    if (model$type == 2) {
      xmlLT <- .pmmlLocalTransformationsAD(field, transforms)
      # xmlLT <- .pmmlLocalTransformations(field, transforms)
    } else {
      xmlLT <- .pmmlLocalTransformations(field, transforms)
    }
  } else {
    xmlLT <- xmlNode("LocalTransformations")
  }



  if (is.null(model$x.scale) == FALSE) {
    # NormContinuous transformform.

    num.inputs <- length(model$x.scale[[1]])
    inputNames <- names(model$x.scale[[1]])

    inputDFNames <- array(NA, dim = num.inputs)
    for (i in 1:num.inputs) {
      dfName <- paste("algorithm_derived_nc_", inputNames[i], sep = "")
      inputDFNames[i] <- dfName
      xmlDF <- xmlNode("DerivedField", attrs = c(name = dfName, dataType = "double", optype = "continuous"))
      xmlNC <- xmlNode("NormContinuous", attrs = c(field = inputNames[i]))

      m <- model$x.scale$`scaled:center`[[i]]
      s <- model$x.scale$`scaled:scale`[[i]]

      xmlLN1 <- xmlNode("LinearNorm", attrs = c(orig = 0, norm = -m / s))
      xmlLN2 <- xmlNode("LinearNorm", attrs = c(orig = m, norm = 0))

      if (m < 0) {
        xmlNC <- append.XMLNode(xmlNC, xmlLN2)
        xmlNC <- append.XMLNode(xmlNC, xmlLN1)
      } else {
        xmlNC <- append.XMLNode(xmlNC, xmlLN1)
        xmlNC <- append.XMLNode(xmlNC, xmlLN2)
      }

      xmlDF <- append.XMLNode(xmlDF, xmlNC)
      xmlLT <- append.XMLNode(xmlLT, xmlDF)
    }
  }

  allVectorAttrName <- attr(model$SV, "dimnames")[2]
  num.vector.attr <- length(allVectorAttrName[[1]])

  # vfNames : the array stores the names of all vector fields.
  vfNames <- array(NA, dim = length(allVectorAttrName[[1]]))
  vfIndex <- 1

  for (i in vfStart:length(field$name)) {
    inputName <- NULL
    inputName <- field$name[[i]]

    if (field$class[[i]] == "numeric") {
      if (is.null(model$x.scale)) {
        vfNames[vfIndex] <- inputName
      } else {
        vfNames[vfIndex] <- paste("algorithm_derived_nc_", inputName, sep = "")
      }

      vfIndex <- vfIndex + 1
      next
    }

    for (j in 1:num.vector.attr) {
      vectorAttr <- allVectorAttrName[[1]][j]

      if (grepl(inputName, vectorAttr) == TRUE) {
        ndValue <- NULL
        ndValue <- gsub(inputName, "", vectorAttr)

        if (grepl("_", ndValue) == TRUE) {
          next
        }

        dfName <- NULL
        dfName <- paste("algorithm_derived_nd_", inputName, "_", ndValue, sep = "")

        vfNames[vfIndex] <- dfName
        vfIndex <- vfIndex + 1

        xmlDF <- xmlNode("DerivedField", attrs = c(name = dfName, dataType = "double", optype = "continuous"))
        if (grepl("\\.", ndValue) == TRUE) {
          xmlND <- xmlNode("NormDiscrete", attrs = c(field = inputName, value = ndValue))
          xmlWarning <- newXMLCommentNode(" R Warning: The character '-' in the original data might have been replaced by the '.' character. Check the desired scoring data for consistency")
          xmlND <- append.XMLNode(xmlND, xmlWarning)
        } else {
          xmlND <- xmlNode("NormDiscrete", attrs = c(field = inputName, value = ndValue))
        }

        xmlDF <- append.XMLNode(xmlDF, xmlND)
        xmlLT <- append.XMLNode(xmlLT, xmlDF)
      }
    }
  }

  xmlModel <- append.XMLNode(xmlModel, xmlLT)

  #------------------------------------------------------------------
  # Kernel
  xmlKernel <- NULL
  if (model$kernel == 0) {
    xmlKernel <- xmlNode("LinearKernelType", attrs = c(description = "Linear kernel type"))
  } else if (model$kernel == 1) {
    xmlKernel <- xmlNode("PolynomialKernelType", attrs = c(
      gamma = model$gamma, coef0 = model$coef0, degree = model$degree,
      description = "Polynomial kernel type"
    ))
  } else if (model$kernel == 3) {
    xmlKernel <- xmlNode("SigmoidKernelType", attrs = c(
      gamma = model$gamma, coef0 = model$coef0,
      description = "Sigmoid kernel type"
    ))
  } else {
    xmlKernel <- xmlNode("RadialBasisKernelType", attrs = c(gamma = model$gamma, description = "Radial basis kernel type"))
  }

  xmlModel <- append.XMLNode(xmlModel, xmlKernel)
  #------------------------------------------------------------------
  # Vector Instances

  vectorSize <- length(model$SV[1, ])

  xmlVD <- xmlNode("VectorDictionary", attrs = c(numberOfVectors = model$tot.nSV))

  xmlVF <- xmlNode("VectorFields", attrs = c(numberOfFields = vectorSize))

  for (i in 1:num.vector.attr) {
    xmlFR <- xmlNode("FieldRef", attrs = c(field = vfNames[i]))
    xmlVF <- append.XMLNode(xmlVF, xmlFR)
  }

  xmlVD <- append.XMLNode(xmlVD, xmlVF)
  for (i in 1:model$tot.nSV) {
    xmlVI <- xmlNode("VectorInstance", attrs = c(id = i))
    xmlRealSA <- xmlNode("REAL-SparseArray", attrs = c(n = num.vector.attr))

    indices <- NULL
    realEntries <- NULL

    for (j in 1:num.vector.attr) {
      indices <- paste(indices, j)
      realEntries <- paste(realEntries, model$SV[i, ][[j]])
    }

    xmlIndices <- xmlNode("Indices", indices)
    xmlRealEntries <- xmlNode("REAL-Entries", realEntries)

    xmlRealSA <- append.XMLNode(xmlRealSA, xmlIndices)
    xmlRealSA <- append.XMLNode(xmlRealSA, xmlRealEntries)
    xmlVI <- append.XMLNode(xmlVI, xmlRealSA)
    xmlVD <- append.XMLNode(xmlVD, xmlVI)
  }

  xmlModel <- append.XMLNode(xmlModel, xmlVD)

  #------------------------------------------------------------------
  # Support Vector Machines

  startVectorIndex <- array(NA, dim = model$nclasses)
  startVectorIndex[1] <- 1

  for (i in 2:model$nclasses) {
    startVectorIndex[i] <- startVectorIndex[i - 1] + model$nSV[i - 1]
  }

  svmCount <- 0

  if (functionName == "classification") {
    trgtAltTrgts <- attributes(model$decision.values)$dimnames[[2]]
    for (i in 1:(model$nclasses - 1)) {
      for (j in (i + 1):model$nclasses) {
        svmCount <- svmCount + 1
        trgtAltTrgt <- strsplit(trgtAltTrgts[svmCount], "/")[[1]]
        trgt <- trgtAltTrgt[1]
        altTrgt <- trgtAltTrgt[2]

        xmlSVM <- xmlNode("SupportVectorMachine",
          attrs = c(targetCategory = trgt, alternateTargetCategory = altTrgt)
        )

        si <- startVectorIndex[i]
        sj <- startVectorIndex[j]
        ci <- model$nSV[i]
        cj <- model$nSV[j]

        coef1Array <- model$coefs[, j - 1]
        coef2Array <- model$coefs[, i]

        xmlSVs <- xmlNode("SupportVectors", attrs = c(numberOfAttributes = num.vector.attr, numberOfSupportVectors = ci + cj))

        if (model$type == 2) {
          xmlCFs <- xmlNode("Coefficients", attrs = c(absoluteValue = -model$rho[svmCount], numberOfCoefficients = ci + cj))
        } else {
          xmlCFs <- xmlNode("Coefficients", attrs = c(absoluteValue = model$rho[svmCount], numberOfCoefficients = ci + cj))
        }

        for (k in 0:(ci - 1)) {
          xmlSV <- xmlNode("SupportVector", attrs = c(vectorId = si + k))

          if (model$type == 2) {
            xmlCF <- xmlNode("Coefficient", attrs = c(value = coef1Array[si + k]))
          } else {
            xmlCF <- xmlNode("Coefficient", attrs = c(value = -1 * coef1Array[si + k]))
          }

          xmlSVs <- append.XMLNode(xmlSVs, xmlSV)
          xmlCFs <- append.XMLNode(xmlCFs, xmlCF)
        }

        for (k in 0:(cj - 1)) {
          xmlSV <- xmlNode("SupportVector", attrs = c(vectorId = sj + k))

          if (model$type == 2) {
            xmlCF <- xmlNode("Coefficient", attrs = c(value = coef2Array[sj + k]))
          } else {
            xmlCF <- xmlNode("Coefficient", attrs = c(value = -1 * coef2Array[sj + k]))
          }

          xmlSVs <- append.XMLNode(xmlSVs, xmlSV)
          xmlCFs <- append.XMLNode(xmlCFs, xmlCF)
        }

        xmlSVM <- append.XMLNode(xmlSVM, xmlSVs)
        xmlSVM <- append.XMLNode(xmlSVM, xmlCFs)

        xmlModel <- append.XMLNode(xmlModel, xmlSVM)
      }
    }
  } else {
    xmlSVM <- xmlNode("SupportVectorMachine")

    xmlSVs <- xmlNode("SupportVectors",
      attrs = c(numberOfAttributes = num.vector.attr, numberOfSupportVectors = model$tot.nSV)
    )

    if (model$type == 2) {
      xmlCFs <- xmlNode("Coefficients", attrs = c(absoluteValue = -1 * model$rho[1], numberOfCoefficients = model$tot.nSV))
    } else {
      xmlCFs <- xmlNode("Coefficients", attrs = c(absoluteValue = model$rho[1], numberOfCoefficients = model$tot.nSV))
    }
    for (i in 1:model$tot.nSV) {
      xmlSV <- xmlNode("SupportVector", attrs = c(vectorId = i))

      if (model$type == 2) {
        xmlCF <- xmlNode("Coefficient", attrs = c(value = model$coefs[i]))
      } else {
        xmlCF <- xmlNode("Coefficient", attrs = c(value = -1 * model$coefs[i]))
      }

      xmlSVs <- append.XMLNode(xmlSVs, xmlSV)
      xmlCFs <- append.XMLNode(xmlCFs, xmlCF)
    }

    xmlSVM <- append.XMLNode(xmlSVM, xmlSVs)
    xmlSVM <- append.XMLNode(xmlSVM, xmlCFs)

    xmlModel <- append.XMLNode(xmlModel, xmlSVM)
  }


  return(xmlModel)
}
