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

#' Generate the PMML representation for a randomForest object from the package
#' \pkg{randomForest}.
#'
#' @param model A randomForest object.
#' @param missing_value_replacement Value to be used as the 'missingValueReplacement'
#' attribute for all MiningFields.
#' @param parent_invalid_value_treatment Invalid value treatment at the top
#' MiningField level.
#' @param child_invalid_value_treatment Invalid value treatment at the model
#' segment MiningField level.
#'
#' @inheritParams pmml
#'
#' @return PMML representation of the randomForest object.
#'
#' @details
#' This function outputs a Random Forest in PMML format.
#'
#' @author Tridivesh Jena
#'
#' @references
#' \href{https://CRAN.R-project.org/package=randomForest}{randomForest:
#' Breiman and Cutler's random forests for classification and regression}
#'
#' @examples
#' # Build a randomForest model
#' library(randomForest)
#' iris.rf <- randomForest(Species ~ ., data = iris, ntree = 20)
#'
#' # Convert to pmml
#' pmml(iris.rf)
#'
#' rm(iris.rf)
#' @export
pmml.randomForest <- function(model,
                              model_name = "randomForest_Model",
                              app_name = "SoftwareAG PMML Generator",
                              description = "Random Forest Tree Model",
                              copyright = NULL,
                              transforms = NULL,
                              missing_value_replacement = NULL,
                              parent_invalid_value_treatment = "returnInvalid",
                              child_invalid_value_treatment = "asIs",
                              ...) {
  if (!inherits(model, "randomForest")) {
    stop("Not a legitimate randomForest object")
  }

  requireNamespace("randomForest", quietly = TRUE)

  field <- NULL
  tr.vars <- attr(model$terms, "dataClasses")
  var.names0 <- gsub("as\\.factor\\(", "", names(tr.vars))
  var.names <- gsub("\\)", "", var.names0)
  field$name <- var.names
  number.of.fields <- length(field$name)
  target <- var.names[1]

  field$class <- attr(model$terms, "dataClasses")
  names(field$class) <- var.names

  cat <- list()
  for (i in 1:number.of.fields)
  {
    if (field$class[[field$name[i]]] == "factor") {
      if (field$name[i] == target) {
        field$levels[[field$name[i]]] <- model$classes
      }
      else {
        cat <- c(cat, model$forest$xlevels[field$name[i]])
      }
    }
  }

  # PMML

  pmml <- .pmmlRootNode()

  # PMML -> Header

  pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, app_name))

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field, transformed = transforms))

  mmodel <- xmlNode("MiningModel", attrs = c(modelName = model_name, algorithmName = "randomForest", functionName = model$type))
  mmodel <- append.XMLNode(mmodel, .pmmlMiningSchema(field, target, transforms, missing_value_replacement,
    invalidValueTreatment = parent_invalid_value_treatment
  ))

  mmodel <- append.XMLNode(mmodel, .pmmlOutput(field, target))

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

  if (model$type == "regression") {
    segmentation <- xmlNode("Segmentation", attrs = c(multipleModelMethod = "average"))
  }
  if (model$type == "classification") {
    segmentation <- xmlNode("Segmentation", attrs = c(multipleModelMethod = "majorityVote"))
  }

  numTrees <- model$ntree
  segments <- lapply(1:numTrees, function(x) {
    .makeSegment(x, model, model_name, field, target, missing_value_replacement, child_invalid_value_treatment)
  })
  segmentation2 <- append.XMLNode(segmentation, segments)
  rm(segmentation)
  rm(segments)

  mmodel2 <- append.XMLNode(mmodel, segmentation2)
  rm(mmodel)
  rm(segmentation2)

  pmml2 <- append.XMLNode(pmml, mmodel2)
  rm(pmml)

  return(pmml2)
}

.makeSegment <- function(b, model, model_name, field, target, missing_value_replacement = NULL, child_invalid_value_treatment) {
  message(paste("Now converting tree ", b, " to PMML"))
  # PMML -> TreeModel -> Node

  # Tree structure information here as produced by the getTree function of the
  # randomForest package.
  if (model$type == "regression") {
    tree <- cbind(
      model$forest$leftDaughter[, b],
      model$forest$rightDaughter[, b],
      model$forest$bestvar[, b],
      model$forest$xbestsplit[, b],
      model$forest$nodepred[, b]
    )[1:model$forest$ndbigtree[b], ]
  } else {
    tree <- cbind(
      model$forest$treemap[, , b],
      model$forest$bestvar[, b],
      model$forest$xbestsplit[, b],
      model$forest$nodepred[, b]
    )[1:model$forest$ndbigtree[b], ]
  }
  nodeList <- list()
  rowId <- which(tree[, 2] == max(tree[, 2]))
  nodeList <- lapply(1:tree[rowId, 2], function(x) {
    .makeNode(x, model, tree, field)
  })
  nodeF <- .makeTree(nodeList, tree, rowId)

  rm(nodeList)

  # PMML -> TreeModel
  if (model$type == "regression") {
    tree.model <- xmlNode("TreeModel",
      attrs = c(
        modelName = model_name,
        functionName = "regression",
        algorithmName = "randomForest",
        splitCharacteristic = "binarySplit"
      )
    )
  }
  if (model$type == "classification") {
    tree.model <- xmlNode("TreeModel",
      attrs = c(
        modelName = model_name,
        functionName = "classification",
        algorithmName = "randomForest",
        splitCharacteristic = "binarySplit"
      )
    )
  }

  # PMML -> TreeModel -> MiningSchema
  tree.model <- append.XMLNode(tree.model, .pmmlMiningSchema(field, target,
    missing_value_replacement = missing_value_replacement,
    invalidValueTreatment = child_invalid_value_treatment
  ))

  segment <- xmlNode("Segment", attrs = c(id = b))
  tru <- xmlNode("True")
  segment <- append.XMLNode(segment, tru)

  tree.model <- append.XMLNode(tree.model, nodeF)
  segment <- append.XMLNode(segment, tree.model)
  return(segment)
}

.makeTree <- function(nodeLi, tre, rId) {
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


.makeNode <- function(n, mod, tinf, fieldInfo) {
  if (n == 1) {
    return(append.XMLNode(xmlNode("Node", attrs = c(id = 1)), xmlNode("True")))
  }
  else {
    side <- 2
    if (n / 2 == floor(n / 2)) {
      side <- 1
    }
    score <- NULL
    if (tinf[n, 1] == 0) {
      if (mod$type == "regression") {
        score <- tinf[n, 5]
      } else {
        score <- mod$classes[tinf[n, 5]]
      }
    }

    if (is.null(score)) {
      rfNode <- xmlNode("Node", attrs = c(id = n))
    } else {
      rfNode <- xmlNode("Node", attrs = c(id = n, score = score))
    }

    # After the node, add the split info in pmml
    rowid <- which(tinf[, side] == n)
    fname <- names(mod$forest$xlevels[tinf[rowid, 3]])
    logical <- FALSE
    numeric <- FALSE
    fieldClass <- fieldInfo$class[fname]
    if (fieldClass == "numeric") {
      numeric <- TRUE
    }
    if (fieldClass == "logical") {
      logical <- TRUE
    }

    # Split if var is numeric.
    if (numeric) {
      if (side == 1) {
        splitNode <- xmlNode("SimplePredicate", attrs = c(
          field = fname, operator = "lessOrEqual",
          value = format(tinf[rowid, 4], digits = 18)
        ))
      } else {
        splitNode <- xmlNode("SimplePredicate", attrs = c(
          field = fname, operator = "greaterThan",
          value = format(tinf[rowid, 4], digits = 18)
        ))
      }
    } else if (logical) {
      bool <- ifelse(tinf[rowid, 4] <= 0.5, FALSE, TRUE)
      splitNode <- xmlNode("SimplePredicate", attrs = c(
        field = fname, operator = "equal",
        value = bool
      ))
    } else {
      if (tinf[rowid, 4] < 0) {
        stop(paste(
          "Unable to determine categorical split. Possible cause is number of categories of variable",
          fname, "is higher than the randomForest function can accomodate."
        ))
      } else {
        # Split if var is categorical.
        binary <- .sdecimal2binary(tinf[rowid, 4])
        ssp <- xmlNode("SimpleSetPredicate", attrs = c(field = fname, booleanOperator = "isIn"))
        num1 <- 0
        scat <- NULL
        holder <- array(0, dim = c(1, mod$forest$ncat[fname][[1]]))
        for (k in 1:length(binary))
        {
          holder[k] <- binary[k]
        }

        # For each category allowed, if value is 1 (from the binary conversion) then go left.
        options(useFancyQuotes = FALSE)
        for (k in 1:mod$forest$ncat[fname][[1]])
        {
          if (side == 1) {
            if (holder[k] == 1) {
              num1 <- num1 + 1
              catname <- mod$forest$xlevels[fname][[1]][k]
              scat <- paste(scat, " ", dQuote(catname))
            }
          } else {
            if (holder[k] == 0) {
              num1 <- num1 + 1
              catname <- mod$forest$xlevels[fname][[1]][k]
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



.getRFTreeNodes2 <- function(recursiveObject, model, side, tinf, rowfrom, rownext, fieldInfo) {
  if (!((model$type == "regression") || (model$type == "classification"))) {
    print("Model type not supported")
  }
  treeSkip <- FALSE

  # Keep going over nodes; if leaf node, add score, else split and keep going.
  if ((rowfrom == 1) && (rownext == 1)) {
    # Handle trees with 1 node only.
    if (is.null(dim(tinf))) {
      rfNode <- xmlNode("Node", attrs = c(id = "1", score = tinf[5]))
      nodeB <- xmlNode("True")
      rfNode <- append.XMLNode(rfNode, nodeB)
      recursiveObject$internalNode <- rfNode
      return(recursiveObject)
    } else {
      # Add top node at first loop only.
      rfNode <- xmlNode("Node", attrs = c(id = "1"))
      nodeB <- xmlNode("True")
      rfNode <- append.XMLNode(rfNode, nodeB)
    }
  } else {
    fname <- attributes(model$forest$xlevels[tinf[rowfrom, 3]])[[1]]
    # Treat left and right leafs separately as their information is stored in separate column in tree.
    if (side == -1) {
      if (tinf[rownext, 1] == 0) {
        # The score for classification must be translated from a number to the category name.
        if (model$type == "regression") {
          # The score for regresion can just be read off.
          rfNode <- xmlNode("Node", attrs = c(id = tinf[rowfrom, 1], score = tinf[rownext, 5]))
        } else {
          rfNode <- xmlNode("Node", attrs = c(id = tinf[rowfrom, 1], score = model$classes[tinf[rownext, 5]]))
        }
      } else {
        rfNode <- xmlNode("Node", attrs = c(id = tinf[rowfrom, 1]))
      }
      # After the node, add the split info in pmml
      # left side, regression model, terminal node.
      logical <- FALSE
      numeric <- FALSE
      if (is.numeric(model$forest$xlevels[[tinf[rowfrom, 3]]][1])) {
        name <- names(model$forest$xlevels[tinf[rowfrom, 3]])
        if (fieldInfo$class[name] == "logical") {
          logical <- TRUE
        } else {
          numeric <- TRUE
        }
      } else {
        numeric <- FALSE
      }
      # Split if var is numeric.
      if (numeric) {
        splitNode <- xmlNode("SimplePredicate", attrs = c(
          field = fname, operator = "lessOrEqual",
          value = tinf[rowfrom, 4]
        ))
      } else if (logical) {
        bool <- ifelse(tinf[rowfrom, 4] <= 0.5, FALSE, TRUE)
        splitNode <- xmlNode("SimplePredicate", attrs = c(
          field = fname, operator = "equal",
          value = bool
        ))
      } else {
        if (tinf[rowfrom, 4] >= 0) {
          # Split if var is categorical.
          binary <- .sdecimal2binary(tinf[rowfrom, 4])
          ssp <- xmlNode("SimpleSetPredicate", attrs = c(field = fname, booleanOperator = "isIn"))
          num1 <- 0
          scat <- NULL
          holder <- array(0, dim = c(1, model$forest$ncat[fname][[1]]))
          for (k in 1:length(binary))
          {
            holder[k] <- binary[k]
          }

          # For each category allowed, if value is 1 (from the binary conversion) then go left.
          options(useFancyQuotes = FALSE)
          for (k in 1:model$forest$ncat[fname][[1]])
          {
            if (holder[k] == 1) {
              num1 <- num1 + 1
              catname <- model$forest$xlevels[fname][[1]][k]
              scat <- paste(scat, " ", dQuote(catname))
            }
          }

          # Strip intermediate, leading and trailing spaces.
          scat <- gsub("^[ ]*", "", scat)
          ap <- xmlNode("Array", attrs = c(n = num1, type = "string"), scat)
          ssp <- append.XMLNode(ssp, ap)
          splitNode <- ssp
        } else {
          treeSkip <- TRUE
          sknode <- xmlNode("skip")
          recursiveObject$internalNode <- "skip"
          return(recursiveObject)
        }
      }
      if (treeSkip == FALSE) {
        rfNode <- append.XMLNode(rfNode, splitNode)
      }
    } else {

      # Right side, regression, terminal node.
      # Repeat for right side.
      if (tinf[rownext, 1] == 0) {
        if (model$type == "regression") {
          # The only difference is where to read off the node info from the tree structure.
          rfNode <- xmlNode("Node", attrs = c(id = tinf[rowfrom, 2], score = tinf[rownext, 5]))
        } else {
          rfNode <- xmlNode("Node", attrs = c(id = tinf[rowfrom, 2], score = model$classes[tinf[rownext, 5]]))
        }
      }
      else {
        rfNode <- xmlNode("Node", attrs = c(id = tinf[rowfrom, 2]))
      }

      logical <- FALSE
      numeric <- FALSE
      if (is.numeric(model$forest$xlevels[[tinf[rowfrom, 3]]][1])) {
        name <- names(model$forest$xlevels[tinf[rowfrom, 3]])
        if (fieldInfo$class[name] == "logical") {
          logical <- TRUE
        } else {
          numeric <- TRUE
        }
      } else {
        numeric <- FALSE
      }

      if (numeric) {
        splitNode <- xmlNode("SimplePredicate", attrs = c(
          field = fname, operator = "greaterThan",
          value = tinf[rowfrom, 4]
        ))
      } else if (logical) {
        bool <- ifelse(tinf[rowfrom, 4] <= 0.5, TRUE, FALSE)
        splitNode <- xmlNode("SimplePredicate", attrs = c(
          field = fname, operator = "equal",
          value = bool
        ))
      } else {
        if (tinf[rowfrom, 4] >= 0) {
          # Split if var is categorical.
          binary <- .sdecimal2binary(tinf[rowfrom, 4])
          ssp <- xmlNode("SimpleSetPredicate", attrs = c(field = fname, booleanOperator = "isIn"))
          num1 <- 0
          scat <- NULL
          holder <- array(0, dim = c(1, model$forest$ncat[fname][[1]]))
          options(useFancyQuotes = FALSE)
          for (k in 1:length(binary))
          {
            holder[k] <- binary[k]
          }
          for (k in 1:model$forest$ncat[fname][[1]])
          {
            if (holder[k] == 0) {
              num1 <- num1 + 1
              catname <- as.character(unlist(model$forest$xlevels[fname]))[k]
              scat <- paste(scat, " ", dQuote(catname))
            }
          }

          scat <- gsub("^[ ]*", "", scat)
          ap <- xmlNode("Array", attrs = c(n = num1, type = "string"), scat)
          ssp <- append.XMLNode(ssp, ap)
          splitNode <- ssp
        } else {
          treeSkip <- TRUE
          sknode <- xmlNode("skip")
          recursiveObject$internalNode <- "skip"
          return(recursiveObject)
        }
      }
      if (treeSkip == FALSE) {
        rfNode <- append.XMLNode(rfNode, splitNode)
      }
    }
  }

  if (tinf[rownext, 5] == -1) {
    terminalFlag <- TRUE
  } else {
    terminalFlag <- FALSE
  }

  if (terminalFlag == TRUE) {
    # Only the predicted value for this node is the output.
  }
  if (terminalFlag == FALSE) {
    recursiveObject$internalNode <- NULL
    recursiveObject <- .getRFTreeNodes2(recursiveObject, model, -1, tinf, rownext, tinf[rownext, 1], fieldInfo)
    if (!is.null(recursiveObject$internalNode) && (recursiveObject$internalNode == "skip")[[1]]) {
      return(recursiveObject)
    } else {
      rfNode <- append.XMLNode(rfNode, recursiveObject$internalNode)
    }



    recursiveObject$internalNode <- NULL
    recursiveObject <- .getRFTreeNodes2(recursiveObject, model, 1, tinf, rownext, tinf[rownext, 2], fieldInfo)
    if (!is.null(recursiveObject$internalNode) && (recursiveObject$internalNode == "skip")[[1]]) {
      return(recursiveObject)
    } else {
      rfNode <- append.XMLNode(rfNode, recursiveObject$internalNode)
    }
  }

  if (!is.null(recursiveObject$internalNode) && (recursiveObject$internalNode == "skip")[[1]]) {
    return(recursiveObject)
  } else {
    recursiveObject$internalNode <- rfNode
    return(recursiveObject)
  }
}
