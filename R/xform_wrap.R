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

#' Wrap data in a data transformations object.
#'
#'
#' @param data The raw data set.
#' @param use_matrix Boolean value indicating whether data should be stored in
#' matrix format as well.
#'
#' @return An R object containing information on the data to be transformed.
#'
#' @details
#' Wrap raw data read in an R object. This object can then be passed to various
#' transform functions, and the data in it transformed.
#'
#' The object consists of the data itself and various properties for each data
#' variable. Since the data is not always required to be in matrix format as
#' well as a data frame, the 'use_matrix' value lets the user decide if the data
#' should be stored in both formats, giving the user a choice in reducing the
#' speed of the transformation operations and the memory required. If there is
#' not enough information about the data, they are given default values; the
#' data is assumed to be the original data of data type string. The variable
#' names are assumed to be \bold{X1}, \bold{X2}, ...  This information is then
#' used by the transformation functions to calculate the derived variable
#' values.
#'
#' @author Tridivesh Jena
#'
#' @seealso \code{\link[pmml]{pmml}}
#'
#' @examples
#' # Load the standard iris dataset
#' data(iris)
#'
#' # Make a object for the iris dataset to use with
#' # transformation functions
#' iris_box <- xform_wrap(iris)
#'
#' # Output only the transformations in PMML format.
#' # This example will output just an empty "LocalTransformations"
#' # element as no transformations were performed.
#' trans_pmml <- pmml(NULL, transforms = iris_box)
#'
#' # The following will also work
#' trans_pmml_2 <- pmml(, transforms = iris_box)
#' @export
xform_wrap <- function(data, use_matrix = FALSE) {
  data_box <- NULL
  field_names <- NULL
  nrows <- NULL
  ncols <- NULL
  type <- NULL
  dataType <- NULL
  orig_field_name <- NULL

  if (use_matrix) {
    data_box$matrixData <- as.matrix(data)
  } else {
    data_box$matrixData <- NULL
  }
  if ((!is.data.frame(data)) | (inherits(data, "tbl"))) {
    indatafrm <- data.frame(data, stringsAsFactors = TRUE)
  } else {
    indatafrm <- data
  }

  data_box$data <- indatafrm
  data_box$nrows <- nrow(indatafrm)
  data_box$ncols <- ncol(indatafrm)

  if (is.matrix(indatafrm)) {
    if (!is.numeric(indatafrm)) {
      stop("Non-numeric matrices not yet supported for transformations")
    }
  }

  field_names <- names(indatafrm)

  for (i in 1:data_box$ncols)
  {
    orig_field_name <- NA
    type[i] <- "original"

    # if (is.numeric(data[, i])) {
    if (is.numeric(indatafrm[, i])) {
      dataType[i] <- "numeric"
    } else {
      dataType[i] <- "factor"
    }
  }

  sampleMin <- NA
  sampleMax <- NA
  xformedMin <- NA
  xformedMax <- NA
  centers <- NA
  scales <- NA
  fieldsMap <- NA
  transform <- NA
  default <- NA
  missingValue <- NA
  xform_function <- NA

  df <- data.frame(type, dataType, orig_field_name, sampleMin, sampleMax, xformedMin, xformedMax,
    centers, scales, fieldsMap, transform, default, missingValue, xform_function,
    row.names = field_names, stringsAsFactors = TRUE
  )

  data_box$field_data <- df

  return(data_box)
}
