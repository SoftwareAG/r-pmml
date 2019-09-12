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

#' Generate PMML for an iForest object from the \bold{isofor} package.
#'
#' @param model An iForest object from package \bold{isofor}.
#' @param missing_value_replacement Value to be used as the 'missingValueReplacement'
#' attribute for all MiningFields.
#' @param anomaly_threshold Double between 0 and 1. Predicted values greater than this are classified as anomalies.
#' @param anomalyThreshold Deprecated.
#' @param parent_invalid_value_treatment Invalid value treatment at the top MiningField level.
#' @param child_invalid_value_treatment Invalid value treatment at the model segment MiningField level.
#' @param ... Further arguments passed to or from other methods.
#'
#' @inheritParams pmml
#'
#' @return PMML representation of the \code{iForest} object.
#'
#' @author Tridivesh Jena
#'
#' @details This function converts the iForest model object to the PMML format. The
#' PMML outputs the anomaly score as well as a boolean value indicating whether the
#' input is an anomaly or not. This is done by simply comparing the anomaly score with
#' \code{anomaly_threshold}, a parameter in the \code{pmml} function.
#' The iForest function automatically adds an extra level to all categorical variables,
#' labelled "."; this is kept in the PMML representation even though the use of this extra
#' factor in the predict function is unclear.
#'
#' @examples
#' \dontrun{
#'
#' # Build iForest model using iris dataset. Create an isolation
#' # forest with 10 trees. Sample 30 data points at a time from
#' # the iris dataset to fit the trees.
#' library(isofor)
#' data(iris)
#' mod <- iForest(iris, nt = 10, phi = 30)
#'
#' # Convert to PMML:
#' mod_pmml <- pmml(mod)
#' }
#'
#' @seealso \code{\link[pmml]{pmml}}
#'
#' @references
#' \href{https://github.com/Zelazny7/isofor}{isofor package on GitHub}
#'
#' @export pmml.iForest
#' @export
pmml.iForest <- function(model,
                         model_name = "isolationForest_Model",
                         app_name = "SoftwareAG PMML Generator",
                         description = "Isolation Forest Model",
                         copyright = NULL,
                         transforms = NULL,
                         missing_value_replacement = NULL,
                         anomaly_threshold = 0.6,
                         anomalyThreshold,
                         parent_invalid_value_treatment = "returnInvalid",
                         child_invalid_value_treatment = "asIs",
                         ...) {
  if (!inherits(model, "iForest")) {
    stop("Not a legitimate iForest object")
  }

  # Deprecated argument.
  if (!missing(anomalyThreshold)) {
    warning("argument anomalyThreshold is deprecated; please use anomaly_threshold instead.",
      call. = FALSE
    )
    anomaly_threshold <- anomalyThreshold
  }

  field <- list()
  field$name <- model$vNames
  field$levels <- model$vLevels
  target <- "anomalyScore"
  number.of.fields <- length(field$name)

  for (i in 1:number.of.fields)
  {
    if (is.null(field$levels[[field$name[i]]])) {
      field$class[[field$name[i]]] <- "numeric"
    }
    else {
      field$class[[field$name[i]]] <- "factor"
    }
  }

  # PMML

  pmml <- .pmmlRootNode()

  # PMML -> Header

  pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, app_name))

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field, transformed = transforms))

  # 4.4 node with sampleDataSize attribute
  anomalyModel <- xmlNode("AnomalyDetectionModel", attrs = c(
    functionName = "regression",
    sampleDataSize = model$phi,
    algorithmType = "iforest", modelName = model_name
  ))

  anomalyModel <- append.XMLNode(anomalyModel, .pmmlMiningSchema(field, target, transforms, missing_value_replacement,
    invalidValueTreatment = parent_invalid_value_treatment
  ))

  anomalyModel <- append.XMLNode(anomalyModel, .pmmlAnomalyOutput(field, target, anomaly_threshold))

  mmodel <- xmlNode("MiningModel", attrs = c(
    modelName = model_name, algorithmName = "randomForest",
    functionName = "regression"
  ))

  mmodel <- append.XMLNode(mmodel, .pmmlMiningSchema(field, target, transforms, missing_value_replacement,
    invalidValueTreatment = parent_invalid_value_treatment
  ))

  mmodel <- append.XMLNode(mmodel, .pmmlAnomalyMiningOutput("avg_path_length"))

  # If interaction terms do exist, define a product in LocalTransformations and use
  # it as a model variable. This step is rare as randomForest seems to avoid multiplicative
  # terms.
  ltNode <- xmlNode("LocalTransformations")
  interact <- FALSE
  for (fld in 1:number.of.fields)
  {
    if (length(grep(":", field$name[fld])) == 1) {
      interact <- TRUE
      drvnode <- xmlNode("DerivedField", attrs = c(
        name = field$name[fld], optype = "continuous",
        dataType = "double"
      ))
      applyNode <- xmlNode("Apply", attrs = c("function" = "*"))
      for (fac in 1:length(strsplit(field$name[fld], ":")[[1]]))
      {
        fldNode <- xmlNode("FieldRef", attrs = c(field = strsplit(field$name[fld], ":")[[1]][fac]))
        if (length(grep("as\\.factor\\(", fldNode)) == 1) {
          fldNode <- gsub("as.factor\\((\\w*)\\)", "\\1", fldNode, perl = TRUE)
        }
        applyNode <- append.XMLNode(applyNode, fldNode)
      }
      drvnode <- append.XMLNode(drvnode, applyNode)
    }
    if (interact) {
      ltNode <- append.XMLNode(ltNode, drvnode)
    }
  }
  if (interact && is.null(transforms)) {
    mmodel <- append.XMLNode(mmodel, ltNode)
  }

  if (interact && !is.null(transforms)) {
    ltNode <- .pmmlLocalTransformations(field, transforms, ltNode)
    mmodel <- append.XMLNode(mmodel, ltNode)
  }
  if (!interact && !is.null(transforms)) {
    mmodel <- append.XMLNode(mmodel, .pmmlLocalTransformations(field, transforms, ltNode))
  }

  segmentation <- xmlNode("Segmentation", attrs = c(multipleModelMethod = "average"))

  numTrees <- model$nTrees
  segments <- lapply(1:numTrees, function(x) {
    .makeASegment(x, model, model_name, field, target, missing_value_replacement, child_invalid_value_treatment)
  })
  segmentation <- append.XMLNode(segmentation, segments)

  mmodel <- append.XMLNode(mmodel, segmentation)
  anomalyModel <- append.XMLNode(anomalyModel, mmodel)
  pmml <- append.XMLNode(pmml, anomalyModel)

  return(pmml)
}

.getParentRow <- function(currNodeRow, treeFrame) {
  left <- which(treeFrame[, "Left"] == currNodeRow)
  right <- which(treeFrame[, "Right"] == currNodeRow)
  if (any(left)) {
    return(left)
  } else {
    return(right)
  }
}

.getTreeDepth <- function(currRow, t) {
  depth <- 1
  while (.getParentRow(currRow, t) != 1) {
    depth <- depth + 1
    currRow <- .getParentRow(currRow, t)
  }
  return(depth)
}

.makeASegment <- function(b, model, model_name, field, target, missing_value_replacement = NULL, child_invalid_value_treatment) {
  message(paste("Now converting tree ", b, " to PMML"))

  t <- model$forest[[b]]
  t <- cbind(t, rep(0, nrow(t)))
  colnames(t)[ncol(t)] <- "TreeDepth"
  for (i in 1:nrow(t)) {
    if (t[i, "Type"] == -1) {
      t[i, "TreeDepth"] <- .getTreeDepth(i, t)
    }
  }

  # PMML -> TreeModel -> Node
  tree <- cbind(
    t[, "Left"], t[, "Right"], t[, "SplitAtt"], t[, "SplitValue"],
    t[, "Size"], t[, "AttType"], t[, "TreeDepth"]
  )

  nodeList <- list()
  # rowId <- which(tree[,1] == max(tree[,1]))
  nodeList <- lapply(1:nrow(tree), function(x) {
    .makeANode(x, model, tree, field)
  })

  nodeF <- .makeATree(nodeList, tree, nrow(tree))
  rm(nodeList)

  # PMML -> TreeModel
  tree.model <- xmlNode("TreeModel", attrs = c(
    modelName = model_name,
    functionName = "regression", algorithmName = "isofor",
    splitCharacteristic = "binarySplit"
  ))

  # PMML -> TreeModel -> MiningSchema
  tree.model <- append.XMLNode(tree.model, .pmmlMiningSchema(field, target,
    missing_value_replacement = missing_value_replacement,
    invalidValueTreatment = child_invalid_value_treatment
  ))

  # Add to the top level structure.
  segment <- xmlNode("Segment", attrs = c(id = b))
  tru <- xmlNode("True")
  segment <- append.XMLNode(segment, tru)

  # Add to the top level structure.
  tree.model <- append.XMLNode(tree.model, nodeF)
  segment <- append.XMLNode(segment, tree.model)
  return(segment)
}

.makeATree <- function(nodeLi, tre, rId) {
  while (rId != 0) {
    if (tre[rId, 1] != 0) {
      nodeR <- nodeLi[[tre[rId, 2]]]
      nodeL <- nodeLi[[tre[rId, 1]]]
      nodeT <- nodeLi[[rId]]

      nodeT <- append.XMLNode(nodeT, nodeL)

      nodeT <- append.XMLNode(nodeT, nodeR)

      nodeLi[[rId]] <- nodeT

      rm(nodeR)
      rm(nodeL)
      rm(nodeT)
    }
    rId <- rId - 1
  }

  return(nodeLi[[1]])
}


.makeANode <- function(n, mod, tinf, fieldInfo) {
  if (n == 1) {
    return(append.XMLNode(xmlNode("Node", attrs = c(id = 1)), xmlNode("True")))
  }
  else {
    side <- 2
    if (n / 2 == floor(n / 2)) {
      side <- 1
    }
    score <- NULL
    if (tinf[n, 6] == 0) {
      score <- unname(tinf[n, 7])
      if (tinf[n, 5] > 1) {
        size <- tinf[n, 5]
        if (size == 2) {
          scale <- 1
        } else if (size < 2) {
          scale <- 0
        } else {
          scale <- 2.0 * (log(size - 1) + 0.5772156649) - 2.0 * (size - 1) / size
        }
        score <- score + scale
      }
    }

    if (is.null(score)) {
      rfNode <- xmlNode("Node", attrs = c(id = n))
    } else {
      rfNode <- xmlNode("Node", attrs = c(id = n, score = score))
    }
    # After the node, add the split info in pmml
    rowid <- which(tinf[, side] == n)
    fname <- fieldInfo$name[tinf[rowid, 3]]
    # isofor source code accounts for numeric and factor only; ignore logical.
    logical <- FALSE
    fieldClass <- fieldInfo$class[[fname]]

    # Split if var is numeric
    if (fieldClass == "numeric") {
      if (side == 1) {
        splitNode <- xmlNode("SimplePredicate", attrs = c(
          field = fname, operator = "lessThan",
          value = format(unname(tinf[rowid, 4]), digits = 18)
        ))
      } else {
        splitNode <- xmlNode("SimplePredicate", attrs = c(
          field = fname, operator = "greaterOrEqual",
          value = format(unname(tinf[rowid, 4]), digits = 18)
        ))
      }
    } else if (logical)
    # This is always false in this implementation.
    {
      bool <- ifelse(tinf[rowid, 4] <= 0.5, FALSE, TRUE)
      splitNode <- xmlNode("SimplePredicate", attrs = c(
        field = fname, operator = "equal",
        value = bool
      ))
    } else {
      if (tinf[rowid, 4] < 0) {
        stop(paste("Unable to determine categorical split."))
      } else {
        # Split if var is categorical.
        binary <- .sdecimal2binary(tinf[rowid, 4])
        ssp <- xmlNode("SimpleSetPredicate", attrs = c(field = fname, booleanOperator = "isIn"))
        num1 <- 0
        scat <- NULL
        holder <- array(0, dim = c(1, length(mod$vLevels[[fname]])))
        for (k in 1:length(binary))
        {
          holder[k] <- binary[k]
        }

        # For each category allowed, if value is 1 (from the binary conversion) then go left.
        options(useFancyQuotes = FALSE)
        for (k in 1:length(holder))
        {
          catname <- mod$vLevels[[fname]][k]
          if (side == 1) {
            if (holder[k] == 1) {
              num1 <- num1 + 1
              scat <- paste(scat, " ", dQuote(catname))
            }
          } else {
            if (holder[k] == 0) {
              num1 <- num1 + 1
              scat <- paste(scat, " ", dQuote(catname))
            }
          }
        }

        # Strip intermediate, leading and trailing spaces.
        scat <- gsub("^[ ]*", "", scat)
        ap <- xmlNode("Array", attrs = c(n = num1, type = "string"), scat)
        ssp <- append.XMLNode(ssp, ap)
        splitNode <- ssp
      }
    }
    rfNode <- append.XMLNode(rfNode, splitNode)
  }
  return(rfNode)
}


# 4.4 function with decision field
.pmmlAnomalyOutput <- function(field, target, anomaly_threshold) {
  output <- xmlNode("Output")
  output1 <- xmlNode("OutputField", attrs = c(
    name = "anomalyScore", optype = "continuous",
    dataType = "double", feature = "predictedValue"
  ))
  output_anomaly <- xmlNode("OutputField", attrs = c(
    name = "anomaly", optype = "categorical", dataType = "boolean",
    feature = "decision"
  ))

  output_anomaly_comp <- xmlNode("Apply", attrs = c("function" = "greaterOrEqual"))
  output_anomaly_c <- xmlNode("FieldRef", attrs = c(field = "anomalyScore"))
  output_anomaly_d <- xmlNode("Constant", attrs = c(dataType = "double"), anomaly_threshold)
  output_anomaly_comp <- append.XMLNode(output_anomaly_comp, output_anomaly_c, output_anomaly_d)
  output_anomaly <- append.XMLNode(output_anomaly, output_anomaly_comp)

  output <- append.XMLNode(output, output1, output_anomaly)
  return(output)
}

.pmmlAnomalyMiningOutput <- function(targetName) {
  output <- xmlNode("Output")
  output1 <- xmlNode("OutputField", attrs = c(
    name = paste0("Predicted_", targetName), optype = "continuous",
    dataType = "double", feature = "predictedValue"
  ))
  output <- append.XMLNode(output, output1)
  return(output)
}
