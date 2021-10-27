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

#' Add Output nodes to a PMML object.
#'
#' @param name The name of the element to be created.
#' @param attributes The node attributes to be added.
#' @param expression Post-processing information to be included in the element.
#' This expression will be processed by \code{function_to_pmml}.
#' @param namespace The namespace of the PMML model.
#' @details
#' Create a list of nodes with names \code{'name'}, attributes \code{'attributes'} and
#' child elements \code{'expression'}. \code{'expression'} is a string converted to XML
#' similar to \code{function_to_pmml}.
#'
#' Meant to create OutputField elements, 'expressions' can be used to add
#' post-processing transformations to a model. To create multiple such nodes,
#' all the parameters must be given as lists of equal length.
#'
#' @return List of nodes
#'
#' @author Tridivesh Jena
#'
#' @examples
#' # Make two nodes, one with attributes
#' two_nodes <- make_output_nodes(
#'   name = list("OutputField", "OutputField"),
#'   attributes = list(list(name = "dbl", optype = "continuous"), NULL),
#'   expression = list("ln(x)", "ln(x/(1-x))")
#' )
#' @export
make_output_nodes <- function(name = "OutputField", attributes = NULL, expression = NULL, namespace = "4_4") {
  if (!is.list(name)) {
    stop("Please provide name, attributes and expression as a list")
  }
  if (!all(sapply(1:length(name), function(i) {
    !is.null(name[[i]])
  }))) {
    stop("All Output names are required")
  }

  nodes <- vector(mode = "list", length = length(name))
  for (i in 1:length(name)) {
    nodes[[i]] <- newXMLNode(name[[i]])
    if (!is.null(attributes[[i]])) {
      if (length(name) == 1) {
        addAttributes(nodes[[i]], .attrs = attributes)
      } else {
        addAttributes(nodes[[i]], .attrs = attributes[[i]])
      }
    }
    if (!is.null(expression[[i]])) {
      addChildren(nodes[[i]], kids = list(.pmmlU(expression[[i]])))
    }
  }

  return(nodes)
}
