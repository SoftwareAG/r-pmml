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

#' Create Interval elements, most likely to add to a DataDictionary element.
#'
#' @param closure The 'closure' attribute of each 'Interval' element to be created in order.
#' @param leftMargin The 'leftMargin' attribute of each 'Interval' element to be created in order.
#' @param rightMargin The 'rightMargin' attribute of each 'Interval' element to be created in order.
#' @param namespace The namespace of the PMML model
#'
#' @details
#'   The 'Interval' element allows 3 attributes, all of which may be defined in the 'make_intervals'
#' function. The value of these attributes should be provided as a list. Thus the elements of the
#' 'leftMargin' for example define the value of that attribute for each 'Interval' element in order.
#'
#' @return PMML Intervals elements.
#'
#' @author Tridivesh Jena
#'
#' @examples
#' # make 3 Interval elements
#' # we define the 3 Intervals as ,1]  (1,2)  and [2,
#' mi <- make_intervals(
#'   list("openClosed", "openOpen", "closedOpen"),
#'   list(NULL, 1, 2), list(1, 2, NULL)
#' )
#' @seealso \code{\link{make_values}} to make Values child elements, \code{\link{add_data_field_children}}
#' to add these xml fragments to the DataDictionary PMML element.
#'
#' @export
make_intervals <- function(closure = NULL, leftMargin = NULL, rightMargin = NULL, namespace = "4_4") {
  namespace <- .getNamespace(namespace)

  if ((length(closure) != length(leftMargin)) || (length(closure) != length(rightMargin))
  || (length(leftMargin) != length(rightMargin))) {
    stop("all parameters must have same length.")
  }

  nodes <- vector(mode = "list", length = length(closure))
  for (i in 1:length(closure))
  {
    nodes[[i]] <- newXMLNode("Interval", attrs = c(closure = closure[[i]], leftMargin = leftMargin[[i]], rightMargin = rightMargin[[i]]))
  }
  return(nodes)
}
