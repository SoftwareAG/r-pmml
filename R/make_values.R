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

#' Create Values element, most likely to add to a DataDictionary element.
#'
#' @param value The 'value' attribute of each 'Value' element to be created in order.
#' @param displayValue The 'displayValue' attribute of each 'Value' element to be created in order.
#' @param property The 'property' attribute of each 'Value' element to be created in order.
#' @param namespace The namespace of the PMML model
#'
#' @details
#' This function is used the same way as the \code{make_intervals} function. If certain attributes for an
#'  element should not be included, they should be input in the list as NULL.
#'
#' @return PMML Values elements.
#'
#' @author Tridivesh Jena
#'
#' @examples
#' # define 3 values, none with a 'displayValue' attribute and 1 value
#' # defined as 'invalid'. The 2nd one is 'valid' by default.
#' mv <- make_values(
#'   list(1.1, 2.2, 3.3), list(NULL, NULL, NULL),
#'   list("valid", NULL, "invalid")
#' )
#' @seealso \code{\link{make_intervals}} to make Interval child elements, \code{\link{add_data_field_children}}
#' to add these xml fragments to the DataDictionary PMML element.
#'
#' @export
make_values <- function(value = NULL, displayValue = NULL, property = NULL, namespace = "4_4") {
  namespace <- .getNamespace(namespace)
  if ((length(value) != length(displayValue)) || (length(value) != length(property)) ||
    (length(displayValue) != length(property))) {
    stop("all parameters must have same length.")
  }

  nodes <- vector(mode = "list", length = length(value))
  for (i in 1:length(value))
  {
    nodes[[i]] <- newXMLNode("Value",
      attrs = c(
        value = value[[i]],
        displayValue = displayValue[[i]], property = property[[i]]
      )
    )
  }
  return(nodes)
}
