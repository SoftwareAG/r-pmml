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

#' Add attribute values to an existing DataField element in a given PMML file
#'
#'
#' @param xml_model The PMML model in a XML node format. If the model is a text
#' file, it should be converted to an XML node, for example, using the
#' file_to_xml_node function.
#' @param attributes The attributes to be added to the data fields. The user
#' should make sure that the attributes being added are allowed in the PMML
#' schema.
#' @param field The field to which the attributes are to be added. This is used
#' when the attributes are a vector of name-value pairs, intended for this one
#' field.
#' @param namespace The namespace of the PMML model. This is frequently also the PMML version of the model.
#' @param \dots Further arguments passed to or from other methods.
#'
#' @return An object of class \code{XMLNode} as that defined by the \pkg{XML} package.
#' This represents the top level, or root node, of the XML document and is of
#' type PMML. It can be written to file with \code{saveXML}.
#'
#' @details
#' The PMML format allows a DataField element to have various attributes,
#' which, although useful, may not always be present in a PMML model. This
#' function allows one to take an existing PMML file and add these attributes
#' to the DataFields.
#'
#' The attribute information can be provided as a dataframe or a vector. Each
#' row of the data frame corresponds to an attribute name and each column
#' corresponding to a variable name. This way one can add as many attributes to
#' as many variables as one wants in one step. A more convinient method to add
#' multiple attributes to one field might be to give the attribute name and
#' values as a vector. This function may be used multiple times to add new
#' attribute values step-by-step. However this function overwrites any
#' pre-existing attribute values, so it must be used with care. This behavior
#' is by design as this feature is meant to help an user add new defined
#' attribute values at different times. For example, one may use this to modify
#' the display name of a field at different times.
#'
#' @author Tridivesh Jena
#'
#' @keywords interface
#'
#' @examples
#' # Make a sample model:
#' model0 <- lm(Sepal.Length ~ ., data = iris[, -5])
#' model <- pmml(model0)
#'
#' # The resulting model has mining fields with no information besides
#' # fieldName, dataType and optype. This object is already an xml
#' # node (not an external text file), so there is no need to convert
#' # it to an xml node object.
#'
#' # Create data frame with attribute information:
#'
#' attributes <- data.frame(c("FlowerWidth", 1), c("FlowerLength", 0),
#'   stringAsFactors = FALSE
#' )
#' rownames(attributes) <- c("displayName", "isCyclic")
#' colnames(attributes) <- c("Sepal.Width", "Petal.Length")
#'
#' # Although not needed in this first try, necessary to easily add
#' # new values later. Removes values as factors so that new values
#' # added later are not evaluated as factor values and thus rejected
#' # as invalid.
#' attributes[] <- lapply(attributes, as.character)
#'
#' add_data_field_attriubutes(model, attributes, namespace = "4_3")
#'
#' # Alternative method to add attributes to a single field,
#' # "Sepal.Width":
#' add_data_field_attriubutes(
#'   model, c(displayName = "FlowerWidth", isCyclic = 1),
#'   "Sepal.Width"
#' )
#'
#'
#' mi <- make_intervals(
#'   list("openClosed", "closedClosed", "closedOpen"),
#'   list(NULL, 1, 2), list(1, 2, NULL)
#' )
#' mv <- make_values(
#'   list("A", "B", "C"), list(NULL, NULL, NULL),
#'   list("valid", NULL, "invalid")
#' )
#' add_data_field_children(model, field = "Sepal.Length", interval = mi, values = mv)
#' @importFrom XML getNodeSet addChildren addAttributes xmlTreeParse toString.XMLNode
#'
#' @export
add_data_field_attriubutes <- function(xml_model = NULL, attributes = NULL, field = NULL, namespace = "4_3", ...) {

  # Flush to avoid malloc error.
  flush.console()
  namespace <- .getNamespace(namespace)

  if (!is.data.frame(attributes) && !is.vector(attributes)) {
    print("Please give attribute information in data frame format or as a vector.")
  }

  modelstring <- toString.XMLNode(xml_model)
  modelInternalDocument <- xmlTreeParse(modelstring, asText = TRUE, useInternalNodes = TRUE)

  dd <- getNodeSet(modelInternalDocument, "/p:PMML/p:DataDictionary", c(p = namespace))[[1]]
  formulaFields <- getNodeSet(modelInternalDocument, "/p:PMML/p:DataDictionary/p:DataField/@name", c(p = namespace))
  formulaString <- lapply(formulaFields, FUN = toString)

  if (is.vector(attributes)) {
    attributes <- data.frame(attributes)
    colnames(attributes) <- field
    fieldNames <- field
  } else {
    fieldNames <- colnames(attributes)
  }

  # Remove levels from data frame (strings stored as factors) so that new values can be added.
  attributes[] <- lapply(attributes, as.character)

  if (!all(is.element(fieldNames, formulaFields))) {
    print("WARNING: the following field additions will be ignored")
    cat(fieldNames[which(!(is.element(fieldNames, formulaFields)))], "not in model\n")
    attributes[, c(which(!(is.element(fieldNames, formulaFields))))] <- NA
  }

  for (i in 1:ncol(attributes))
  {
    if (!all(is.na(attributes[, i]))) {
      df <- getNodeSet(dd, paste0("p:DataField[@name='", colnames(attributes)[i], "']"), c(p = namespace))[[1]]
      s <- structure(names = as.character(rownames(attributes)[which(!is.na(attributes[, i]))]), attributes[which(!is.na(attributes[, i])), i])
      addAttributes(df, .attrs = s, append = TRUE)
    }
  }

  return(modelInternalDocument)
}
