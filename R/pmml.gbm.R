# PMML: Predictive Model Markup Language
#
# Copyright (c) 2009-2016, Zementis, Inc.
# Copyright (c) 2016-2020, Software AG, Darmstadt, Germany and/or Software AG
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

#' Generate the PMML representation for a gbm object from the package \pkg{gbm}.
#'
#'
#' @param model A \code{gbm} object.
#' @param missing_value_replacement Value to be used as the 'missingValueReplacement'
#' attribute for all MiningFields.
#'
#' @inheritParams pmml
#'
#' @return PMML representation of the gbm object.
#'
#' @details
#' The 'gbm' function uses various distribution types to fit a model; currently
#' only the "bernoulli", "poisson" and "multinomial" distribution types are
#' supported.
#'
#' For all cases, the model output includes the gbm prediction type
#' "link" and "response".
#'
#' @author Tridivesh Jena
#'
#' @references
#' \href{https://CRAN.R-project.org/package=gbm}{gbm: Generalized Boosted
#' Regression Models (on CRAN)}
#'
#' @examples
#' library(gbm)
#' data(audit)
#'
#' mod <- gbm(Adjusted ~ .,
#'   data = audit[, -c(1, 4, 6, 9, 10, 11, 12)],
#'   n.trees = 3, interaction.depth = 4
#' )
#'
#' mod_pmml <- pmml(mod)
#'
#' # Classification example:
#' mod2 <- gbm(Species ~ .,
#'   data = iris, n.trees = 2,
#'   interaction.depth = 3, distribution = "multinomial"
#' )
#'
#' # The PMML will include a regression model to read the gbm object outputs
#' # and convert to a "response" prediction type.
#' mod2_pmml <- pmml(mod2)
#' @export pmml.gbm
#' @export
pmml.gbm <- function(model,
                     model_name = "GBM_Model",
                     app_name = "SoftwareAG PMML Generator",
                     description = "Generalized Boosted Tree Model",
                     copyright = NULL,
                     transforms = NULL,
                     missing_value_replacement = NULL,
                     ...) {
  if (!inherits(model, "gbm")) {
    stop("Not a legitimate gbm object")
  }

  requireNamespace("gbm", quietly = TRUE)


  dist <- model$distribution$name

  field <- NULL
  var.names0 <- c(model$response.name, model$var.names)
  var.names0 <- gsub("as\\.factor\\(", "", var.names0)
  var.names <- gsub("\\)", "", var.names0)
  field$name <- var.names
  number.of.fields <- length(field$name)
  target <- model$response.name

  field$class <- c(0, model$var.type)
  field$class[field$class > 0] <- "factor"
  if (model$num.classes > 1) field$class[1] <- "factor"
  field$class[field$class == 0] <- "numeric"
  names(field$class) <- var.names

  levelvector <- which(vapply(model$var.levels, function(x) {
    is.null(attr(x, "names"))
  }, FUN.VALUE = 0) == 1)
  j <- 1
  for (i in 1:number.of.fields)
  {
    if (field$name[i] == model$response.name) {
      field$levels[[field$name[i]]] <- model$classes
    } else {
      if (field$class[[field$name[i]]] == "factor") {
        field$levels[[field$name[i]]] <- model$var.levels[[levelvector[j]]]
        j <- j + 1
      }
    }
  }

  numtrees <- model$n.trees
  numcategories <- model$num.classes

  # PMML

  pmml <- .pmmlRootNode()

  # PMML -> Header

  pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, app_name))

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field, transformed = transforms))

  if (dist == "multinomial") {
    mmodel <- xmlNode("MiningModel", attrs = c(modelName = model_name, functionName = "classification"))
  } else {
    mmodel <- xmlNode("MiningModel", attrs = c(modelName = model_name, functionName = "regression"))
  }
  mmodel <- append.XMLNode(mmodel, .pmmlMiningSchema(field, target, transforms, missing_value_replacement))

  output <- .pmmlOutput(field, target)
  if (dist == "bernoulli") {
    responseNode <- xmlNode("OutputField", attrs = c(
      name = "BernoulliResponsePrediction",
      feature = "transformedValue", dataType = "double"
    ))
    divideNode <- xmlNode("Apply", attrs = c("function" = "/"))
    multiplyNode <- xmlNode("Apply", attrs = c("function" = "*"))
    plusNode <- xmlNode("Apply", attrs = c("function" = "+"))
    expNode <- xmlNode("Apply", attrs = c("function" = "exp"))
    minusNode <- xmlNode("Apply", attrs = c("function" = "-"))
    fieldNode <- xmlNode("FieldRef", attrs = c(field = paste("Predicted_", target, sep = "")))
    initNode <- xmlNode("Constant", model$initF)
    one1Node <- xmlNode("Constant", 1)
    one2Node <- xmlNode("Constant", 1)
    minus1Node <- xmlNode("Constant", -1)

    multiplyNode <- append.XMLNode(multiplyNode, minus1Node)
    multiplyNode <- append.XMLNode(multiplyNode, xmlNode("FieldRef", attrs = c(field = "BernoulliLinkPrediction")))
    expNode <- append.XMLNode(expNode, multiplyNode)
    plusNode <- append.XMLNode(plusNode, one1Node)
    plusNode <- append.XMLNode(plusNode, expNode)
    divideNode <- append.XMLNode(divideNode, one2Node)
    divideNode <- append.XMLNode(divideNode, plusNode)
    responseNode <- append.XMLNode(responseNode, divideNode)

    linkNode <- xmlNode("OutputField", attrs = c(
      name = "BernoulliLinkPrediction",
      feature = "transformedValue", dataType = "double"
    ))
    addNode <- xmlNode("Apply", attrs = c("function" = "+"))
    addNode <- append.XMLNode(addNode, fieldNode)
    addNode <- append.XMLNode(addNode, initNode)
    linkNode <- append.XMLNode(linkNode, addNode)

    output <- append.XMLNode(output, linkNode)
    output <- append.XMLNode(output, responseNode)
  }
  if (dist == "multinomial") {
    for (j in seq_along(field$levels[[target]]))
    {
      output.fields <- xmlNode("OutputField", attrs = c(
        name = paste("link_",
          field$levels[[target]][j],
          sep = ""
        ),
        optype = "continuous",
        dataType = "double",
        feature = "transformedValue",
        segmentId = numtrees * j
      ))
      fieldRefNode <- xmlNode("FieldRef", attrs = c(field = paste("UpdatedPredictedValue",
        j, numtrees,
        sep = ""
      )))

      output.fields <- append.XMLNode(output.fields, fieldRefNode)
      output <- append.XMLNode(output, output.fields)
    }
  }
  if (dist == "gaussian") {
    responseNode <- xmlNode("OutputField", attrs = c(
      name = "GaussianPrediction",
      feature = "transformedValue", dataType = "double"
    ))
    applyNode <- xmlNode("Apply", attrs = c("function" = "+"))
    fieldNode <- xmlNode("FieldRef", attrs = c(field = paste("Predicted_", target, sep = "")))
    initNode <- xmlNode("Constant", model$initF)
    applyNode <- append.XMLNode(applyNode, fieldNode)
    applyNode <- append.XMLNode(applyNode, initNode)
    responseNode <- append.XMLNode(responseNode, applyNode)
    output <- append.XMLNode(output, responseNode)
  }
  mmodel <- append.XMLNode(mmodel, output)

  mmodel <- .makeTransforms(mmodel, field, number.of.fields, transforms, "LocalTransformations")

  if (dist == "multinomial") {
    segmentation <- xmlNode("Segmentation", attrs = c(multipleModelMethod = "modelChain"))
    for (i in 1:numcategories)
    {
      segments <- lapply(1:numtrees, function(x) {
        .makeSegments(x, model, model_name, field, target, missing_value_replacement, i)
      })
      segmentation <- append.XMLNode(segmentation, segments)
    }
  } else {
    segmentation <- xmlNode("Segmentation", attrs = c(multipleModelMethod = "sum"))
    segments <- lapply(1:numtrees, function(x) {
      .makeSegments(x, model, model_name, field, target, missing_value_replacement, 1)
    })
    segmentation <- append.XMLNode(segmentation, segments)
  }
  if (dist == "multinomial") {
    regModel <- .makeRegressionModel(field, target, numcategories, numtrees, model_name)
    segmentation <- append.XMLNode(segmentation, regModel)
  }
  mmodel <- append.XMLNode(mmodel, segmentation)

  pmml <- append.XMLNode(pmml, mmodel)

  return(pmml)
}

.makeTransforms <- function(pmodel, field, number.of.fields, transforms, name) {
  # If interaction terms do exist, define a product in LocalTransformations and use
  # it as a model variable.
  ltNode <- xmlNode(name)
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
    pmodel <- append.XMLNode(pmodel, ltNode)
  }

  if (interact && !is.null(transforms)) {
    ltNode <- .pmmlLocalTransformations(field, transforms, ltNode)
    pmodel <- append.XMLNode(pmodel, ltNode)
  }
  if (!interact && !is.null(transforms)) {
    pmodel <- append.XMLNode(pmodel, .pmmlLocalTransformations(field, transforms, ltNode))
  }

  return(pmodel)
}

.makeRegressionModel <- function(field, target, numcat, nt, model_name) {
  segment <- xmlNode("Segment", attrs = c(id = numcat * nt + 1))
  tru <- xmlNode("True")
  segment <- append.XMLNode(segment, tru)

  regModel <- xmlNode("RegressionModel",
    attrs = c(modelName = model_name, functionName = "classification", normalizationMethod = "softmax")
  )
  miningNode <- xmlNode("MiningSchema")
  for (i in 1:numcat)
  {
    mfNode <- xmlNode("MiningField", attrs = c(name = paste("UpdatedPredictedValue", i, nt, sep = "")))
    miningNode <- append.XMLNode(miningNode, mfNode)
  }
  regModel <- append.XMLNode(regModel, miningNode)

  output <- .pmmlOutput(field, target)
  regModel <- append.XMLNode(regModel, output)

  for (i in 1:numcat)
  {
    tableNode <- xmlNode("RegressionTable", attrs = c(
      intercept = 0.0,
      targetCategory = field$levels[[target]][i]
    ))
    predictorNode <- xmlNode("NumericPredictor", attrs = c(
      name = paste("UpdatedPredictedValue", i, nt, sep = ""),
      coefficient = 1.0
    ))
    tableNode <- append.XMLNode(tableNode, predictorNode)
    regModel <- append.XMLNode(regModel, tableNode)
  }
  segment <- append.XMLNode(segment, regModel)

  return(segment)
}

.makeSegments <- function(b, model, model_name, field, target, missing_value_replacement = NULL, ncat) {
  message(paste("Now converting tree ", b, " to PMML, for category#", ncat))

  nodeList <- list()
  if (model$distribution$name == "multinomial") {
    ntrees <- model$n.trees
    Ncat <- model$num.classes
    tree <- cbind(
      model$trees[[ncat + Ncat * (b - 1)]][[3]] + 1,
      model$trees[[ncat + Ncat * (b - 1)]][[4]] + 1,
      model$trees[[ncat + Ncat * (b - 1)]][[1]] + 2,
      model$trees[[ncat + Ncat * (b - 1)]][[2]],
      model$trees[[ncat + Ncat * (b - 1)]][[8]],
      model$trees[[ncat + Ncat * (b - 1)]][[5]] + 1
    )

    rowId <- max(c(
      model$trees[[ncat + Ncat * (b - 1)]][[3]] + 1, model$trees[[ncat + Ncat * (b - 1)]][[4]] + 1,
      model$trees[[ncat + Ncat * (b - 1)]][[5]] + 1
    ))
  } else {
    tree <- cbind(
      model$trees[[b]][[3]] + 1,
      model$trees[[b]][[4]] + 1,
      model$trees[[b]][[1]] + 2,
      model$trees[[b]][[2]], # CHANGED
      model$trees[[b]][[8]],
      model$trees[[b]][[5]] + 1
    )
    rowId <- max(c(
      model$trees[[b]][[3]] + 1, model$trees[[b]][[4]] + 1,
      model$trees[[b]][[5]] + 1
    ))
  }

  nodeList <- lapply(1:rowId, function(x) {
    .makeNodes(x, model, tree, field)
  })
  nodeFi <- .makeTrees(nodeList, tree, rowId)

  tree.model <- xmlNode("TreeModel",
    attrs = c(
      modelName = model_name,
      functionName = "regression",
      algorithmName = "gbm",
      splitCharacteristic = "multiSplit"
    )
  )

  # PMML -> TreeModel -> MiningSchema
  MSNode <- .pmmlMiningSchema(field, target, missing_value_replacement = missing_value_replacement)

  outputNode <- xmlNode("Output")
  if (model$distribution$name == "multinomial") {
    if (b == 1) {
      tree.model <- append.XMLNode(tree.model, MSNode)

      outputfield1 <- xmlNode("OutputField",
        attrs = c(
          name = paste("UpdatedPredictedValue", ncat, b, sep = ""), optype = "continuous",
          dataType = "double", feature = "predictedValue"
        )
      )
      outputNode <- append.XMLNode(outputNode, outputfield1)
    }
    if (b != 1) {
      miningNode <- xmlNode("MiningField", attrs = c(
        name = paste("UpdatedPredictedValue", ncat, b - 1, sep = ""),
        optype = "continuous"
      ))
      MSNode <- append.XMLNode(MSNode, miningNode)
      tree.model <- append.XMLNode(tree.model, MSNode)

      outputfield1 <- xmlNode("OutputField",
        attrs = c(
          name = paste("TreePredictedValue", ncat, b, sep = ""), optype = "continuous",
          dataType = "double", feature = "predictedValue"
        )
      )
      outputNode <- append.XMLNode(outputNode, outputfield1)

      outputField2 <- xmlNode("OutputField",
        attrs = c(
          name = paste("UpdatedPredictedValue", ncat, b, sep = ""), optype = "continuous",
          dataType = "double", feature = "transformedValue"
        )
      )
      applyNode <- xmlNode("Apply", attrs = c("function" = "+"))
      previousFieldNode <- xmlNode("FieldRef", attrs = c(field = paste("UpdatedPredictedValue", ncat, b - 1, sep = "")))
      newFieldNode <- xmlNode("FieldRef", attrs = c(field = paste("TreePredictedValue", ncat, b, sep = "")))

      applyNode <- append.XMLNode(applyNode, previousFieldNode)
      applyNode <- append.XMLNode(applyNode, newFieldNode)
      outputField2 <- append.XMLNode(outputField2, applyNode)
      outputNode <- append.XMLNode(outputNode, outputField2)
    }
    tree.model <- append.XMLNode(tree.model, outputNode)
    segment <- xmlNode("Segment", attrs = c(id = (ncat - 1) * ntrees + b))
    tru <- xmlNode("True")
    segment <- append.XMLNode(segment, tru)
  } else {
    tree.model <- append.XMLNode(tree.model, MSNode)

    segment <- xmlNode("Segment", attrs = c(id = b))
    tru <- xmlNode("True")
    segment <- append.XMLNode(segment, tru)
  }

  tree.model <- append.XMLNode(tree.model, nodeFi)
  segment <- append.XMLNode(segment, tree.model)
  return(segment)
}

.makeTrees <- function(nodeLi, tre, rId) {
  while (rId != 0) {
    if (tre[rId, 1] != 0) {
      nodeR <- nodeLi[[tre[rId, 2]]]
      nodeL <- nodeLi[[tre[rId, 1]]]
      nodeM <- nodeLi[[tre[rId, 6]]]
      nodeT <- nodeLi[[rId]]

      nodeT <- append.XMLNode(nodeT, nodeL)
      nodeT <- append.XMLNode(nodeT, nodeR)
      nodeT <- append.XMLNode(nodeT, nodeM)
      nodeLi[[rId]] <- nodeT
    }
    rId <- rId - 1
  }

  return(nodeLi[[1]])
}

.makeNodes <- function(n, mod, tinf, fieldInfo) {
  if (n == 1) {
    return(append.XMLNode(xmlNode("Node", attrs = c(id = 1)), xmlNode("True")))
  }
  else {
    side <- 6
    if (n %in% tinf[, 1]) {
      side <- 1
    }
    if (n %in% tinf[, 2]) {
      side <- 2
    }
    score <- NULL

    score <- tinf[n, 5]
    if (is.null(score)) {
      rfNode <- xmlNode("Node", attrs = c(id = n))
    } else {
      rfNode <- xmlNode("Node", attrs = c(id = n, score = score))
    }
    # After the node, add the split info in pmml
    rowid <- which(tinf[, side] == n)
    fname <- fieldInfo$name[tinf[rowid, 3]]
    fname0 <- names(attr(mod$Terms, "dataClasses"))[tinf[rowid, 3]]
    logical <- FALSE
    numeric <- FALSE
    fieldClass <- fieldInfo$class[[fname]]
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
          field = fname, operator = "lessThan",
          value = tinf[rowid, 4]
        ))
      } else if (side == 2) {
        splitNode <- xmlNode("SimplePredicate", attrs = c(
          field = fname, operator = "greaterOrEqual",
          value = tinf[rowid, 4]
        ))
      } else {
        splitNode <- xmlNode("SimplePredicate", attrs = c(field = fname, operator = "isMissing"))
      }
    } else if (logical) {
      if (side == 6) {
        splitNode <- xmlNode("SimplePredicate", attrs = c(field = fname, operator = "isMissing"))
      } else {
        bool <- ifelse(tinf[rowid, 4] <= 0.5, FALSE, TRUE)
        splitNode <- xmlNode("SimplePredicate", attrs = c(
          field = fname, operator = "equal",
          value = bool
        ))
      }
    } else {
      if (side == 6) {
        splitNode <- xmlNode("SimplePredicate", attrs = c(field = fname, operator = "isMissing"))
      } else {
        splitid <- mod$c.splits[[as.integer(tinf[rowid, 4]) + 1]]
        binary <- 0.5 - splitid / 2.0
        # Split if var is categorical.
        ssp <- xmlNode("SimpleSetPredicate", attrs = c(field = fname, booleanOperator = "isIn"))
        num1 <- 0
        scat <- NULL
        holder <- array(0, dim = length(fieldInfo$levels[fname][[1]]))
        holder0 <- array(0, dim = c(1, mod$forest$ncat[fname][[1]]))
        for (k in 1:length(binary))
        {
          holder[k] <- binary[k]
        }

        # For each category allowed, if value is 1 (from the binary conversion) then go left.
        options(useFancyQuotes = FALSE)
        for (k in 1:length(fieldInfo$levels[[fname]]))
        {
          if (side == 1) {
            if (holder[k] == 1) {
              num1 <- num1 + 1
              catname <- fieldInfo$levels[[fname]][k]
              catname0 <- mod$forest$xlevels[fname][[1]][k]
              scat <- paste(scat, " ", dQuote(catname))
            }
          } else {
            if (holder[k] == 0) {
              num1 <- num1 + 1
              catname <- fieldInfo$levels[[fname]][k]
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
