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

#' Add Output nodes to a PMML object.
#'
#' @param xml_model The PMML model to which the OutputField elements are to be added
#' @param outputNodes The Output nodes to be added. These may be created using the
#' 		'make_output_nodes' helper function
#' @param at Given an Output element, the 1 based index after which the given Output
#' 		child element should be inserted at
#' @param xformText Post-processing information to be included in the OutputField element.
#' 		This expression will be processed by the function_to_pmml function
#' @param nodeName The name of the element to be added
#' @param attributes The attributes to be added
#' @param whichOutput The index of the Output element
#' @param namespace The namespace of the PMML model
#'
#' @details
#'   This function is meant to add any post-processing information to an existing model via
#' the OutputField element. One can also use this to tell the PMML model to output other values
#' not automatically added to the model output.
#'   The first method is to use the 'make_output_nodes' helper function to make a list of output
#' elements to be added. 'whichOutput' lets the function know which of the Output elements we want to
#' work with; there may be more than one in a multiple model file. One can then add those elements there,
#' at the desired index given by the 'at' parameter; the elements are inserted after the OutputField
#' element at the 'at' index. In other words, find the 'whichOutput' Output element, add the 'outputNodes'
#' child elements (which should be OutputField nodes) at the 'at' position in the child nodes.
#'   This function can also be used with the 'nodeName' and 'attributes' to add the list of attributes to
#' an OutputField element with name 'nodeName' element using the 'xml_model', 'outputNodes' and 'at' parameters.
#'   Finally, one can use this to add the transformation expression given by the 'xformText' parameter
#' to the node with name 'nodeName'. The string given via 'xformText' is converted to an XML expression similarly
#' to the function_to_pmml function. In other words, find the OutputField node with tha name 'nodeName' and add
#' the list of attributes given with 'attributes' and also, add the child transformations given in the 'xformText'
#' parameter.
#'
#' @return Output node with the OutputField elements inserted.
#'
#' @author Tridivesh Jena
#'
#' @examples
#' # Load the standard iris dataset
#' data(iris)
#'
#' # Create a linear model and convert it to PMML
#' mod <- lm(Sepal.Length ~ ., iris)
#' pmod <- pmml(mod)
#'
#' # Create additional output nodes
#' onodes0 <- make_output_nodes(
#'   name = list("OutputField", "OutputField"),
#'   attributes = list(list(
#'     name = "dbl",
#'     optype = "continuous"
#'   ), NULL),
#'   expression = list("ln(x)", "ln(x/(1-x))")
#' )
#' onodes2 <- make_output_nodes(
#'   name = list("OutputField", "OutputField"),
#'   attributes = list(
#'     list(
#'       name = "F1",
#'       dataType = "double", optype = "continuous"
#'     ),
#'     list(name = "F2")
#'   )
#' )
#'
#' # Create new pmml objects with the output nodes appended
#' add_output_field(
#'   xml_model = pmod, outputNodes = onodes2, at = "End",
#'   xformText = NULL, nodeName = NULL, attributes = NULL,
#'   whichOutput = 1
#' )
#' pmod2 <- add_output_field(
#'   xml_model = pmod, outputNodes = onodes0, at = "End",
#'   xformText = NULL, nodeName = NULL,
#'   attributes = NULL, whichOutput = 1
#' )
#'
#' # Create nodes with attributes and transformations
#' add_output_field(xml_model = pmod2, outputNodes = onodes2, at = 2)
#' add_output_field(
#'   xml_model = pmod2, xformText = list("exp(x) && !x"),
#'   nodeName = "Predicted_Sepal.Length"
#' )
#'
#' att <- list(datype = "dbl", optpe = "dsc")
#' add_output_field(
#'   xml_model = pmod2, nodeName = "Predicted_Sepal.Length",
#'   attributes = att
#' )
#' @importFrom XML getNodeSet addChildren addAttributes xmlTreeParse toString.XMLNode
#'
#' @export
add_output_field <- function(xml_model = NULL, outputNodes = NULL, at = "End", xformText = NULL, nodeName = NULL,
                             attributes = NULL, whichOutput = 1, namespace = "4_3") {

  # Flush to avoid malloc error.
  flush.console()
  namespace <- .getNamespace(namespace)

  modelstring <- toString.XMLNode(xml_model)
  modelInternalDocument <- xmlTreeParse(modelstring, asText = TRUE, useInternalNodes = TRUE)

  if (!is.null(outputNodes)) {
    warning("OutputField given, childNode and attributes ignored.")
    outNode <- getNodeSet(modelInternalDocument, paste0("/p:PMML/descendant::p:Output[", whichOutput, "]"), c(p = namespace))
    if (length(outNode) == 0) {
      warning(paste0("Could not find ", whichOutput, " Output nodes"))
    }
    if (at == "End") {
      addChildren(outNode[[1]], kids = list(outputNodes), append = TRUE, supressNamespaceWarning = TRUE)
    } else {
      # Inserted after 'at' OutputField element.
      addChildren(outNode[[1]], kids = list(outputNodes), at = at, append = TRUE, supressNamespaceWarning = TRUE)
    }
  } else
  if (!is.null(attributes)) {
    warning("attributes given, outputNode and childNode will be ignored")
    outputNode <- getNodeSet(
      modelInternalDocument, paste0("/p:PMML/descendant::p:Output/p:OutputField[@name='", nodeName, "']"),
      c(p = namespace)
    )
    for (i in 1:length(attributes)) {
      if (length(outputNode) == 1) {
        addAttributes(outputNode[[1]], .attrs = attributes[i])
      } else {
        addAttributes(outputNode[[1]], .attrs = attributes[[i]])
      }
    }
  } else
  if (!is.null(xformText)) {
    warning("childNode will be added. outputNode and attributes will be ignored.")
    outputNode <- getNodeSet(
      modelInternalDocument, paste0("/p:PMML/descendant::p:Output/p:OutputField[@name='", nodeName, "']"),
      c(p = namespace)
    )
    for (i in 1:length(nodeName)) {
      addChildren(outputNode[[i]], kids = list(.pmmlU(xformText[[i]])), append = TRUE, supressNamespaceWarning = TRUE)
    }
  }

  return(modelInternalDocument)
}
