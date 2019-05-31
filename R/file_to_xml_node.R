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

#' Read in a file and parse it into an object of type XMLNode.
#'
#'
#' @param file The external file to be read in. This file can be any file in
#' PMML format, regardless of the source or model type.
#'
#' @return An object of class \code{XMLNode} as that defined by the \pkg{XML} package.
#' This represents the top level, or root node, of the XML document and is of
#' type PMML. It can be written to file with \code{saveXML}.
#'
#' @details
#' Read in an external file and convert it into an XMLNode to be used
#' subsequently by other R functions.
#'
#' This format is the one that will be obtained when a model is constructed in
#' R and output in PMML format.
#'
#' This function is mainly meant to be used to read in external files instead
#' of depending on models saved in R. As an example, the pmml package requires
#' as input an object of type XMLNode before its functions can be applied.
#' Function 'file_to_xml_node' can be used to read in an existing PMML file,
#' convert it to an XML node and then make it available for use by any of the
#' pmml functions.
#'
#' @author Tridivesh Jena
#'
#' @keywords interface
#'
#' @examples
#' \dontrun{
#' # Define some transformations:
#' iris_box <- xform_wrap(iris)
#' iris_box <- xform_z_score(iris_box, xform_info = "column1->d1")
#' iris_box <- xform_z_score(iris_box, xform_info = "column2->d2")
#'
#' # Make a LocalTransformations element and save it to an external file:
#' pmml_trans <- pmml(NULL, transforms = iris_box)
#' write(toString(pmml_trans), file = "xform_iris.pmml")
#'
#' # Later, we may need to read in the PMML model into R
#' # 'lt' below is now a XML Node, as opposed to a string:
#' lt <- file_to_xml_node("xform_iris.pmml")
#' }
#'
#' @export
file_to_xml_node <- function(file) {
  filexml <- xmlTreeParse(file, useInternalNodes = FALSE)$doc$children[[1]]

  return(filexml)
}
