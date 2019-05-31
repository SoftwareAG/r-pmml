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

#' Add attribute values to an existing MiningField element in a given PMML
#' file.
#'
#'
#' @param xml_model The PMML model in a XML node format. If the model is a text
#' file, it should be converted to an XML node, for example, using the
#' file_to_xml_node function.
#' @param attributes The attributes to be added to the mining fields. The user
#' should make sure that the attributes being added are allowed in the PMML
#' schema.
#' @param namespace The namespace of the PMML model. This is frequently also
#' the PMML version of the model.
#' @param \dots Further arguments passed to or from other methods.
#'
#' @return An object of class \code{XMLNode} as that defined by the \pkg{XML} package.
#' This represents the top level, or root node, of the XML document and is of
#' type PMML. It can be written to file with \code{saveXML}.
#'
#' @details
#' The PMML format allows a MiningField element to have attributes 'usageType',
#' 'missingValueReplacement' and 'invalidValueTreatment' which although useful,
#' may not always be present in a PMML model. This function allows one to take
#' an existing PMML file and add these attributes to the MiningFields.
#'
#' The attribute information should be provided as a dataframe; each row
#' corresponding to an attribute name and each column corresponding to a
#' variable name. This way one can add as many attributes to as many variables
#' as one wants in one step. On the other extreme, a one-by-one data frame may
#' be used to add one new attribute to one variable. This function may be used
#' multiple times to add new attribute values step-by-step. This function
#' overwrites any pre-existing attribute values, so it must be used with care.
#' However, this is by design as this feature is meant to help an user defined
#' new attribute values at different times. For example, one may use this to
#' impute missing values in a model at different times.
#'
#'
#' @author Tridivesh Jena
#'
#' @keywords interface
#'
#' @examples
#' # Make a sample model
#' model0 <- lm(Sepal.Length ~ ., data = iris[, -5])
#' model <- pmml(model0)
#'
#' # The resulting model has mining fields with no information
#' # besides fieldName, dataType and optype. This object is
#' # already an xml node (not an external text file), so there
#' # is no need to convert it to an xml node object.
#'
#' # Create data frame with attribute information:
#' attributes <- data.frame(
#'   c("active", 1.1, "asIs"),
#'   c("active", 2.2, "asIs"),
#'   c("active", NA, "asMissing")
#' )
#' rownames(attributes) <- c(
#'   "usageType", "missingValueReplacement",
#'   "invalidValueTreatment"
#' )
#' colnames(attributes) <- c(
#'   "Sepal.Width", "Petal.Length",
#'   "Petal.Width"
#' )
#'
#' # Although not needed in this first try, necessary to easily
#' # add new values later:
#' for (k in 1:ncol(attributes)) {
#'   attributes[[k]] <- as.character(attributes[[k]])
#' }
#'
#' add_mining_field_attributes(model, attributes, namespace = "4_3")
#' @importFrom XML getNodeSet addChildren addAttributes xmlTreeParse toString.XMLNode
#'
#' @export
add_mining_field_attributes <- function(xml_model = NULL, attributes = NULL, namespace = "4_3", ...) {
  # Flush to avoid malloc error.
  flush.console()

  namespace <- .getNamespace(namespace)

  if (!is.data.frame(attributes)) {
    print("Please give attribute information in data frame format.")
  }

  modelstring <- toString.XMLNode(xml_model)
  modelInternalDocument <- xmlTreeParse(modelstring, asText = TRUE, useInternalNodes = TRUE)

  ms <- getNodeSet(modelInternalDocument, "/p:PMML/child::*/p:MiningSchema", c(p = namespace))[[1]]
  formulaFields <- getNodeSet(modelInternalDocument, "/p:PMML/child::*/p:MiningSchema/p:MiningField/@name", c(p = namespace))
  formulaString <- lapply(formulaFields, FUN = toString)

  # Remove levels from data frame (strings stored as factors) so that new values can be added.
  for (k in 1:ncol(attributes)) {
    attributes[[k]] <- as.character(attributes[[k]])
  }

  fieldNames <- colnames(attributes)
  internalAttr <- attributes
  if (FALSE %in% is.element(fieldNames, formulaFields)) {
    print(cat(fieldNames[which(!(is.element(fieldNames, formulaFields)))], "not in model", sep = ""))
    for (k in 1:length(fieldNames[which(!(is.element(fieldNames, formulaFields)))]))
    {
      internalAttr[, fieldNames[which(!(is.element(fieldNames, formulaFields)))][k]] <- NULL
    }
  }

  for (i in 1:ncol(internalAttr))
  {
    mf <- getNodeSet(ms, paste0("p:MiningField[@name='", colnames(internalAttr)[i], "']"), c(p = namespace))[[1]]
    s <- structure(names = as.character(rownames(internalAttr)[which(!is.na(internalAttr[, i]))]), internalAttr[which(!is.na(internalAttr[, i])), i])
    addAttributes(mf, .attrs = s, append = TRUE)
  }

  return(modelInternalDocument)
}
