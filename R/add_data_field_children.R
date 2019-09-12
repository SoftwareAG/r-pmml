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

#' Add 'Interval' and 'Value' child elements to a given DataField element in a
#' given PMML file.
#'
#'
#' @param xml_model The PMML model in a XML node format. If the model is a text
#' file, it should be converted to an XML node, for example, using the
#' file_to_xml_node function.
#' @param field The field to which the attributes are to be added. This is used
#' when the attributes are a vector of name-value pairs, intended for this one
#' field.
#' @param intervals The 'Interval' elements given as a list
#' @param values The 'Value' elements given as a list.
#' @param namespace The namespace of the PMML model. This is frequently also
#' the PMML version of the model.
#' @param \dots Further arguments passed to or from other methods.
#'
#' @return An object of class \code{XMLNode} as that defined by the \pkg{XML} package.
#' This represents the top level, or root node, of the XML document and is of
#' type PMML. It can be written to file with \code{saveXML}.
#'
#' @details
#' The PMML format allows a DataField element to have 'Interval' and 'Value'
#' child elements which although useful, may not always be present in a PMML
#' model. This function allows one to take an existing PMML file and add these
#' elements to the DataFields.
#'
#' The 'Interval' elements or the 'Value' elements can be typed in, but more
#' conveniently created by using the helper functions 'make_intervals' and
#' 'MakeValues'. This function can then add these extra information to the
#' PMML.
#'
#' @author Tridivesh Jena
#'
#' @keywords interface
#'
#' @examples
#' # Make a sample model:
#' fit <- lm(Sepal.Length ~ ., data = iris[, -5])
#' fit_pmml <- pmml(fit)
#'
#' # The resulting model has data fields but with no 'Interval' or Value'
#' # elements. This object is already an xml node (not an external text
#' # file), so there is no need to convert it to an xml node object.
#'
#' # Add an 'Interval' element node by typing it in
#' fit_pmml_2 <- add_data_field_children(fit_pmml,
#'   field = "Sepal.Length",
#'   intervals = list(newXMLNode("Interval",
#'     attrs = c(closure = "openClosed", rightMargin = 3)
#'   ))
#' )
#'
#' # Use helper functions to create list of 'Interval' and 'Value'
#' # elements. We define the 3 Intervals as ,1]  (1,2)  and [2,
#' mi <- make_intervals(
#'   list("openClosed", "openOpen", "closedOpen"),
#'   list(NULL, 1, 2), list(1, 2, NULL)
#' )
#'
#' # Define 3 values, none with a 'displayValue' attribute and 1 value
#' # defined as 'invalid'. The 2nd one is 'valid' by default.
#' mv <- make_values(
#'   list(1.1, 2.2, 3.3), list(NULL, NULL, NULL),
#'   list("valid", NULL, "invalid")
#' )
#'
#' # As an example, apply these to the Sepal.Length field:
#' fit_pmml_3 <- add_data_field_children(fit_pmml, field = "Sepal.Length", intervals = mi, values = mv)
#'
#' # Only defined 'Interval's:
#' fit_pmml_3 <- add_data_field_children(fit_pmml, field = "Sepal.Length", intervals = mi)
#' @importFrom XML getNodeSet addChildren addAttributes xmlTreeParse toString.XMLNode
#'
#' @export
add_data_field_children <- function(xml_model = NULL, field = NULL, intervals = NULL, values = NULL, namespace = "4_4", ...) {

  # Flush to avoid malloc error.
  flush.console()
  namespace <- .getNamespace(namespace)

  modelstring <- toString.XMLNode(xml_model)
  modelInternalDocument <- xmlTreeParse(modelstring, asText = TRUE, useInternalNodes = TRUE)

  dd <- getNodeSet(modelInternalDocument, "/p:PMML/p:DataDictionary", c(p = namespace))[[1]]
  formulaFields <- getNodeSet(modelInternalDocument, "/p:PMML/p:DataDictionary/p:DataField/@name", c(p = namespace))
  formulaString <- lapply(formulaFields, FUN = toString)

  fieldName <- field

  if (!is.element(fieldName, formulaFields)) {
    print("WARNING: the following field additions will be ignored")
    stop(cat(fieldName, "not in model\n"))
  }

  df <- getNodeSet(dd, paste0("p:DataField[@name='", fieldName, "']"), c(p = namespace))[[1]]
  addChildren(df, kids = intervals, append = TRUE, supressNamespaceWarning = TRUE)
  addChildren(df, kids = values, append = TRUE, supressNamespaceWarning = TRUE)
  return(modelInternalDocument)
}
