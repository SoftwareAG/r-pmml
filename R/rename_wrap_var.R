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

#' Rename a variable in the xform_wrap transform object.
#'
#'
#' @param wrap_data Wrapper object obtained by using the xform_wrap function on the
#' raw data.
#' @param xform_info Specification of details of the renaming.
#' @param \dots Further arguments passed to or from other methods.
#'
#' @return R object containing the raw data, the transformed data and data
#' statistics.
#'
#' @details
#' Once input data is wrapped by the \bold{xform_wrap} function, it is somewhat
#' involved to rename a variable inside. This function makes it easier to do
#' so.  Given an variable named \bold{InputVar} and the name one wishes to
#' rename it to, \bold{OutputVar}, the rename command options are:
#'
#' xform_info="InputVar -> OutputVar"
#'
#' There are two methods in which the variables can be referred to. The first
#' method is to use its column number; given the \bold{data} attribute of the
#' \bold{boxData} object, this would be the order at which the variable
#' appears. This can be indicated in the format "column#". The second method is
#' to refer to the variable by its name. This method will work even if the
#' renamed value already exists; in which case there will be two variables with
#' the same name.
#'
#' If no input variable name is provided, the original object is returned with
#' no renamings performed.
#'
#' @author Tridivesh Jena
#'
#' @seealso \code{\link{xform_wrap}}
#'
#' @keywords manip utilities methods
#'
#' @examples
#' # Load the standard iris dataset, already built into R
#' data(iris)
#'
#' # First wrap the data
#' iris_box <- xform_wrap(iris)
#'
#' # We wish to refer to the variables "Sepal.Length" and
#' # "Sepal.Width" as "SL" and "SW"
#' iris_box <- rename_wrap_var(iris_box, "column1->SL")
#' iris_box <- rename_wrap_var(iris_box, "Sepal.Width->SW")
#' @export
rename_wrap_var <- function(wrap_data, xform_info = NA, ...) {
  i <- NULL
  j <- NULL
  colnm <- NULL

  boxData <- .init_wrap_params(wrap_data)

  if (is.na(xform_info)) {
    warning("No field name to rename found")
    return(wrap_data)
  } else {
    # For each argument given:
    coln <- as.character(xform_info)
    # Split to find initial and final names.
    if (grepl("[^-]->", coln)) {
      st <- strsplit(coln, "->")
    } else {
      st <- strsplit(coln, "-->")
    }
    if (!is.na(st[[1]][2])) {
      derivedFieldName <- st[[1]][2]
    }
    colnm <- st[[1]][1]
    if (grepl("column", colnm, ignore.case = TRUE)) {
      colnm <- gsub("column", "", colnm, ignore.case = TRUE)
    }
    if (grepl("^[-,_]", colnm)) {
      colnm <- gsub("^[-,_]*", "", colnm)
    }

    if (is.na(st[[1]][2])) {
      derivedFieldName <- paste("derived_", row.names(boxData$field_data)[coln2], sep = "")
    }

    # If column number, find the appropriate field .
    if (suppressWarnings(!is.na(as.numeric(colnm)))) {
      coln2 <- as.numeric(colnm)
      dataType <- boxData$field_data[names(boxData$data)[coln2], "dataType"]
      if (dataType == "numeric") {
        row.names(boxData$field_data)[coln2] <- derivedFieldName
        names(boxData$data)[coln2] <- derivedFieldName

        if (!is.null(boxData$matrixData)) {
          names(boxData$matrixData)[coln2] <- derivedFieldName
        }
      }
    } else {
      i <- which(names(boxData$data) == colnm)
      if (is.null(i)) {
        j <- which(names(boxData$data) == colnm)
      }

      if (is.null(i) && is.null(j)) {
        stop("field name not found.")
      }
      if (is.null(j)) {
        row.names(boxData$field_data)[i] <- derivedFieldName
        names(boxData$data)[i] <- derivedFieldName

        if (!is.null(boxData$matrixData)) {
          names(boxData$matrixData)[i] <- derivedFieldName
        }
      } else {
        row.names(boxData$field_data)[j] <- derivedFieldName
        names(boxData$data)[j] <- derivedFieldName

        if (!is.null(boxData$matrixData)) {
          names(boxData$matrixData)[j] <- derivedFieldName
        }
      }
    }
  }

  return(boxData)
}
