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

#' Generate the PMML representation for a nnet object from package \pkg{nnet}.
#'
#' @param model A nnet object.
#' @param missing_value_replacement Value to be used as the 'missingValueReplacement'
#' attribute for all MiningFields.
#'
#' @inheritParams pmml
#'
#' @return PMML representation of the nnet object.
#'
#' @details
#' This function supports both regression and classification neural
#' network models. The model is represented in the PMML NeuralNetwork format.
#'
#' @author Tridivesh Jena
#'
#' @references
#' \href{https://CRAN.R-project.org/package=nnet}{nnet: Feed-forward
#' Neural Networks and Multinomial Log-Linear Models (on CRAN)}
#'
#' @examples
#' library(nnet)
#' fit <- nnet(Species ~ ., data = iris, size = 4)
#' pmml(fit)
#'
#' rm(fit)
#' 
#' @export pmml.nnet
#' @export
pmml.nnet <- function(model,
                      model_name = "NeuralNet_model",
                      app_name = "SoftwareAG PMML Generator",
                      description = "Neural Network Model",
                      copyright = NULL,
                      transforms = NULL,
                      missing_value_replacement = NULL,
                      ...) {
  if (!inherits(model, "nnet")) stop("Not a legitimate nnet object")

  number.of.neural.layers <- length(model$n) - 1

  # Models built with formulae have 3 main pieces of information missing
  # from the model description of models built with matrices: the categorical
  # varable levels, the name and the dataTypes of the input variables. Add
  # this by hand below, set the model to the appropriate class
  # and it will be as if model$call[[1]] is always nnet.formula.

  field <- NULL
  numerical <- NULL
  if (model$call[[1]] != "nnet.formula") {
    if (is.null(attributes(model$fitted.values)$dimnames[[2]][1])) {
      numerical <- TRUE
    }
    else {
      numerical <- FALSE
      tmp <- c()
      for (i in 1:length(attributes(model$fitted.values)$dimnames[[2]]))
      {
        tmp <- c(tmp, attributes(model$fitted.values)$dimnames[[2]][i])
      }
      levels <- list(tmp)
      names(levels) <- "lev"
      model <- c(model, levels)

      trms <- list("terms", name = "Variable Information")
      names(trms) <- "terms"

      number.of.inputs <- model$n[1]
      allnames <- c("y")
      input.names <- c()
      if (numerical) {
        classes <- c("numeric")
      }
      else {
        classes <- c("factor")
      }
      for (i in 1:number.of.inputs)
      {
        tmp <- paste("x", i, sep = "")
        allnames <- c(allnames, tmp)
        input.names <- c(input.names, tmp)
        classes <- c(classes, "numeric")
      }

      trms <- list(name = "variable information")
      names(trms) <- "terms"

      attr(trms$terms, "term.labels") <- input.names
      attr(trms$terms, "dataClasses") <- classes
      names(attributes(trms$terms)$dataClasses) <- allnames

      model$call[[1]] <- "nnet.formula"
      model <- c(model, trms)
      attr(model, "class") <- c("nnet.formula", "nnet")
    }
  }

  if (model$call[[1]] == "nnet.formula") {
    terms <- attributes(model$terms)
    field$name <- names(terms$dataClasses)
    field$class <- terms$dataClasses
    target <- field$name[1]
    number.of.fields <- length(terms$term.labels) + 1
    number.of.inputs <- length(terms$term.labels)
  }
  else {
    number.of.fields <- model$n[1] + 1
    number.of.inputs <- model$n[1]
    target <- "y"
    field$name[1] <- target
    field$class[[field$name[1]]] <- "numeric"
    for (i in 1:number.of.inputs)
    {
      tmp <- paste("x", i, sep = "")
      field$name[i + 1] <- tmp
      field$class[[field$name[i + 1]]] <- "numeric"
    }
  }

  if (length(grep("^as.factor\\(", field$name[1]))) {
    field$name[1] <- sub("^as.factor\\((.*)\\)", "\\1", field$name[1])
    names(field$class)[1] <- sub("^as.factor\\((.*)\\)", "\\1", names(field$class)[1])
    names(field$levels)[1] <- sub("^as.factor\\((.*)\\)", "\\1", names(field$levels)[1])
  }

  target <- field$name[1]

  number.of.fields <- length(field$name)

  # According to the nnet documentation:
  #
  # If the response in formula is a factor, an appropriate
  # classification network is constructed; this has one output and
  # entropy fit if the number of levels is two, and a number of
  # outputs equal to the number of classes and a softmax output stage
  # for more levels.  If the response is not a factor, it is passed on
  # unchanged to nnet.default.
  #
  # However, we actually export a network with two output neurons for binary
  # classification with a softmax output stage.

  normalization.method <- "none"
  skipLayers <- FALSE
  linearOutputUnits <- FALSE

  if (length(model$call$skip) && model$call$skip) {
    skipLayers <- TRUE
  }
  if (model$nunits > model$nsunits) {
    linearOutputUnits <- TRUE
  }
  if (model$softmax) {
    normalization.method <- "softmax"
  }
  if (model$censored) {
    stop("PMML does not support the censored variant of softmax!")
  }

  if (field$class[[field$name[1]]] == "factor") {
    field$levels[[field$name[1]]] <- model$lev
  }

  for (i in seq_len(number.of.inputs)) {
    if (field$class[[field$name[i + 1]]] == "factor") {
      field$levels[[field$name[i + 1]]] <- model$xlevels[[field$name[i + 1]]]
    }
  }

  # PMML

  pmml <- .pmmlRootNode()

  # PMML -> Header

  pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, app_name))

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field, transformed = transforms))
  # 091206 REMOVE pmml <- append.XMLNode(pmml, pmml.nnet.DataDictionary(field))

  # PMML -> NeuralNetwork

  if (model$n[length(model$n)] == 1 && field$class[[field$name[1]]] == "factor") {
    temp <- number.of.neural.layers + 1
  }
  else {
    temp <- number.of.neural.layers
  }

  if (field$class[[field$name[1]]] == "factor") {
    the.model <- xmlNode("NeuralNetwork",
      attrs = c(
        modelName = model_name,
        functionName = "classification",
        numberOfLayers = temp,
        activationFunction = "logistic"
      )
    )
  }
  else {
    the.model <- xmlNode("NeuralNetwork",
      attrs = c(
        modelName = model_name,
        functionName = "regression",
        numberOfLayers = temp,
        activationFunction = "logistic"
      )
    )
  }

  # PMML -> NeuralNetwork -> MiningSchema

  the.model <- append.XMLNode(the.model, .pmmlMiningSchema(field, target, transformed = transforms, missing_value_replacement = missing_value_replacement))

  #  PMML -> NeuralNetwork -> Output

  the.model <- append.XMLNode(the.model, .pmmlOutput(field, target))

  # PMML -> NeuralNetwork -> LocalTransforms

  if (!is.null(transforms)) {
    the.model <- append.XMLNode(the.model, .pmmlLocalTransformations(field, transforms, NULL))
  }

  # PMML -> NeuralNetwork -> NeuralInputs

  neuralInputs <- xmlNode("NeuralInputs",
    attrs = c(numberOfInputs = as.numeric(model$n[1]))
  )
  input_count <- 1
  factor_count <- 1

  for (i in seq_len(number.of.inputs))
  {
    if (terms$dataClasses[[terms$term.labels[i]]] == "factor") {
      number.of.values <- length(model$xlevels[[factor_count]])
      usedValues <- model$xlevels[[factor_count]]
      factor_count <- factor_count + 1

      for (j in 1:number.of.values)
      {
        if (j > 1) # Skips first category during dummyfication.
        {
          neuralInputNode <- xmlNode("NeuralInput",
            attrs = c(id = as.numeric(input_count))
          )
          input_count <- input_count + 1

          fieldName <- paste("derivedNI_", terms$term.labels[i], sep = "")
          fieldName <- paste(fieldName, usedValues[j], sep = "")

          derivedFieldNode <- xmlNode("DerivedField",
            attrs = c(
              name = fieldName,
              optype = "continuous",
              dataType = "double"
            )
          )

          normDiscreteNode <- xmlNode("NormDiscrete",
            attrs = c(
              field = terms$term.labels[i],
              value = usedValues[j]
            )
          )

          derivedFieldNode <- append.XMLNode(derivedFieldNode, normDiscreteNode)

          neuralInputNode <- append.XMLNode(neuralInputNode, derivedFieldNode)

          neuralInputs <- append.XMLNode(neuralInputs, neuralInputNode)
        }
      }
    }
    else {
      neuralInputNode <- xmlNode("NeuralInput",
        attrs = c(id = as.numeric(input_count))
      )
      input_count <- input_count + 1

      name <- terms$term.labels[i]
      fieldName <- paste("derivedNI_", name, sep = "")

      derivedFieldNode <- xmlNode("DerivedField",
        attrs = c(
          name = fieldName,
          optype = "continuous",
          dataType = "double"
        )
      )

      fieldRefNode <- xmlNode("FieldRef",
        attrs = c(field = terms$term.labels[i])
      )

      derivedFieldNode <- append.XMLNode(derivedFieldNode, fieldRefNode)

      neuralInputNode <- append.XMLNode(neuralInputNode, derivedFieldNode)

      neuralInputs <- append.XMLNode(neuralInputs, neuralInputNode)
    }
  }

  the.model <- append.XMLNode(the.model, neuralInputs)

  number.of.inputs <- model$n[1]

  # PMML -> NeuralNetwork -> NeuralLayers

  wtsID <- 1
  neuronID <- number.of.inputs
  previous.number.of.neurons <- number.of.inputs
  for (i in 1:number.of.neural.layers)
  {
    number.of.neurons <- model$n[i + 1]

    if (i == number.of.neural.layers) # Output layer.
    {
      if (number.of.neurons == 1 && field$class[[field$name[1]]] == "factor") {
        neuralLayerNode <- xmlNode("NeuralLayer",
          attrs = c(numberOfNeurons = as.numeric(number.of.neurons))
        )
      }
      else if (model$softmax) {
        neuralLayerNode <- xmlNode("NeuralLayer",
          attrs = c(
            numberOfNeurons = as.numeric(number.of.neurons),
            activationFunction = "identity",
            normalizationMethod = "softmax"
          )
        )
      }
      else if (linearOutputUnits) {
        neuralLayerNode <- xmlNode("NeuralLayer",
          attrs = c(
            numberOfNeurons = as.numeric(number.of.neurons),
            activationFunction = "identity"
          )
        )
      }
      else {
        neuralLayerNode <- xmlNode("NeuralLayer",
          attrs = c(numberOfNeurons = as.numeric(number.of.neurons))
        )
      }
    }
    else # Hidden layer.
    {
      neuralLayerNode <- xmlNode("NeuralLayer",
        attrs = c(numberOfNeurons = as.numeric(number.of.neurons))
      )
    }

    for (j in 1:number.of.neurons)
    {
      neuronID <- neuronID + 1

      neuronNode <- xmlNode("Neuron",
        attrs = c(
          id = as.numeric(neuronID),
          bias = model$wts[wtsID]
        )
      )
      wtsID <- wtsID + 1

      if (i == number.of.neural.layers && j == 1) # Output layer.
      {
        first.outputNeuronID <- neuronID
      }
      if (i == number.of.neural.layers && skipLayers) {
        previous.number.of.neurons <- previous.number.of.neurons + number.of.inputs
      }
      for (k in 1:previous.number.of.neurons)
      {
        number.of.connections <- model$n[i + 1]

        connectionNode <- xmlNode("Con",
          attrs = c(
            from = model$conn[wtsID],
            weight = model$wts[wtsID]
          )
        )
        wtsID <- wtsID + 1

        neuronNode <- append.XMLNode(neuronNode, connectionNode)
      }
      neuralLayerNode <- append.XMLNode(neuralLayerNode, neuronNode)
    }

    previous.number.of.neurons <- number.of.neurons

    the.model <- append.XMLNode(the.model, neuralLayerNode)
  }

  # Special case for NN with 1 output neuron implementing classification
  # Code creates an extra neural layer with a connection set to 1 and bias
  # to 0 so that the threshold function can be applied.
  # The previous layer is assumed to have an output from 0 to 1 and so the
  # threshold is set to 0.5.

  if (number.of.neurons == 1 && field$class[[field$name[1]]] == "factor") {
    neuralLayerNode <- xmlNode("NeuralLayer",
      attrs = c(
        numberOfNeurons = "2",
        activationFunction = "threshold", threshold = "0.5"
      )
    )

    neuronID <- neuronID + 1
    first.outputNeuronID <- neuronID

    neuronNode <- xmlNode("Neuron",
      attrs = c(
        id = as.numeric(neuronID),
        bias = "1.0"
      )
    )

    connectionNode <- xmlNode("Con",
      attrs = c(
        from = neuronID - 1,
        weight = "-1.0"
      )
    )

    neuronNode <- append.XMLNode(neuronNode, connectionNode)

    neuralLayerNode <- append.XMLNode(neuralLayerNode, neuronNode)

    neuronID <- neuronID + 1

    neuronNode <- xmlNode("Neuron",
      attrs = c(
        id = as.numeric(neuronID),
        bias = "0.0"
      )
    )

    connectionNode <- xmlNode("Con",
      attrs = c(
        from = neuronID - 2,
        weight = "1.0"
      )
    )

    neuronNode <- append.XMLNode(neuronNode, connectionNode)

    neuralLayerNode <- append.XMLNode(neuralLayerNode, neuronNode)

    the.model <- append.XMLNode(the.model, neuralLayerNode)

    number.of.neurons <- number.of.neurons + 1

    previous.number.of.neurons <- number.of.neurons
  }

  # PMML -> NeuralNetwork -> NeuralOutputs

  neuralOutputs <- xmlNode("NeuralOutputs",
    attrs = c(numberOfOutputs = previous.number.of.neurons)
  )

  for (i in 1:number.of.neurons)
  {
    neuralOutputNode <- xmlNode("NeuralOutput",
      attrs = c(outputNeuron = first.outputNeuronID)
    )

    first.outputNeuronID <- first.outputNeuronID + 1

    if (field$class[[field$name[1]]] == "factor") {
      targetName <- target
      temp <- grep("as.factor", field$name[1], value = TRUE, fixed = TRUE)
      if (length(temp) > 0) {
        target <- field$name[1]
        tempName <- strsplit(field$name[1], "")
        endPos <- (length(tempName[[1]]) - 1)
        targetName <- substring(target, 11, endPos)
      }

      fieldName <- paste("derivedNO_", targetName, sep = "")

      derivedFieldNode <- xmlNode("DerivedField",
        attrs = c(
          name = fieldName,
          optype = "continuous",
          dataType = "double"
        )
      )

      normDiscreteNode <- xmlNode("NormDiscrete",
        attrs = c(
          field = targetName,
          value = model$lev[i]
        )
      )

      derivedFieldNode <- append.XMLNode(derivedFieldNode, normDiscreteNode)
    }
    else # Regression.
    {
      name <- field$name[1]
      fieldName <- paste("derivedNO_", name, sep = "")

      derivedFieldNode <- xmlNode("DerivedField",
        attrs = c(
          name = fieldName,
          optype = "continuous",
          dataType = "double"
        )
      )

      fieldRefNode <- xmlNode("FieldRef",
        attrs = c(field = field$name[1])
      )

      derivedFieldNode <- append.XMLNode(derivedFieldNode, fieldRefNode)
    }

    neuralOutputNode <- append.XMLNode(neuralOutputNode, derivedFieldNode)

    neuralOutputs <- append.XMLNode(neuralOutputs, neuralOutputNode)
  }

  the.model <- append.XMLNode(the.model, neuralOutputs)

  pmml <- append.XMLNode(pmml, the.model)

  return(pmml)
}
