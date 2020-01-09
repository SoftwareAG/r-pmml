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

#' Perform a z-score normalization on continuous values in accordance with the
#' PMML element \bold{NormContinuous}.
#'
#'
#' @param wrap_object Output of xform_wrap or another transformation function.
#' @param xform_info Specification of details of the transformation.
#' @param map_missing_to Value to be given to the transformed variable if the
#' value of the input variable is missing.
#' @param \dots Further arguments passed to or from other methods.
#'
#' @return R object containing the raw data, the transformed data and data
#' statistics.
#'
#' @details
#' Perform a z-score normalization on data given in \code{xform_wrap} format.
#'
#' Given an input variable named \bold{InputVar}, the name of the transformed
#' variable \bold{OutputVar}, and the desired value of the transformed variable
#' if the input variable value is missing \bold{missingVal}, the xform_z_score
#' command including all the optional parameters is:
#'
#' \code{
#' xform_info="InputVar -> OutputVar", map_missing_to="missingVal"
#' }
#'
#' Two methods can be used to refer to the variables. The first
#' method is to use its column number; given the \bold{data} attribute of the
#' \bold{boxData} object, this would be the order at which the variable
#' appears. This can be indicated in the format "column#". The second method is
#' to refer to the variable by its name.
#'
#' The name of the transformed
#' variable is optional; if the name is not provided, the transformed variable
#' is given the name: "derived_" + \emph{original_variable_name}
#'
#' \bold{missingValue}, an optional parameter, is the value to be given to the
#' output variable if the input variable value is missing. If no input variable
#' names are provided, by default all numeric variables are transformed. Note
#' that in this case a replacement value for missing input values cannot be
#' specified.
#'
#' @author Tridivesh Jena
#'
#' @seealso \code{\link{xform_wrap}}
#'
#' @keywords manip utilities methods
#'
#' @examples
#'
#' # Load the standard iris dataset, already built into R
#' data(iris)
#'
#' # First wrap the data
#' iris_box <- xform_wrap(iris)
#'
#' # Perform a z-transform on all numeric variables of the loaded
#' # iris dataset. These would be Sepal.Length, Sepal.Width,
#' # Petal.Length, and Petal.Width. The 4 new derived variables
#' # will be named derived_Sepal.Length, derived_Sepal.Width,
#' # derived_Petal.Length, and derived_Petal.Width
#' iris_box_1 <- xform_z_score(iris_box)
#'
#' # Perform a z-transform on the 1st column of the dataset (Sepal.Length)
#' # and give the derived variable the name "dsl"
#' iris_box_2 <- xform_z_score(iris_box, xform_info = "column1 -> dsl")
#'
#' # Repeat the above operation; adding the new transformed variable
#' # to the iris_box object
#' iris_box <- xform_z_score(iris_box, xform_info = "column1 -> dsl")
#'
#' # Transform Sepal.Width(the 2nd column)
#' # The new transformed variable will be given the default name
#' # "derived_Sepal.Width"
#' iris_box_3 <- xform_z_score(iris_box, xform_info = "column2")
#'
#' # Repeat the same operation as above, this time using the variable
#' # name
#' iris_box_4 <- xform_z_score(iris_box, xform_info = "Sepal.Width")
#'
#' # Repeat the same operation as above, assign the transformed variable
#' # "derived_Sepal.Width". The value of 1.0 if the input value of the
#' # "Sepal.Width" variable is missing. Add the new information to the
#' # iris_box object.
#' iris_box <- xform_z_score(iris_box,
#'   xform_info = "Sepal.Width",
#'   "map_missing_to=1.0"
#' )
#' @export
xform_z_score <-
  function(wrap_object, xform_info = NA, map_missing_to = NA, ...) {
    colmn <- NULL
    newrow <- NULL
    colnamesGiven <- FALSE
    j <- 0
    transform <- "zxform"
    centers <- NA
    scales <- NA
    xformedMin <- NA
    xformedMax <- NA
    sampleMin <- NA
    sampleMax <- NA
    fieldsMap <- NA
    default <- NA
    missingValue <- NA
    xform_function <- NA

    if (!is.na(map_missing_to)) {
      missingValue <- map_missing_to
    }

    newBoxData <- .init_wrap_params(wrap_object)

    initLength <- nrow(newBoxData$field_data)



    if (is.na(xform_info)) {
      # Transform all numeric fields if no arguments given.
      for (i in 1:newBoxData$ncols)
      {
        name <- names(newBoxData$data)[i]
        dataType <- newBoxData$field_data[name, "dataType"]
        if (dataType == "numeric") {
          dataType <- "numeric"
          type <- "derived"
          transform <- "zxform"
          orig_field_name <- names(newBoxData$data)[i]
          derivedFieldName <- paste("derived_", names(newBoxData$data)[i], sep = "")
          newrow <- data.frame(type, dataType, orig_field_name, sampleMin, sampleMax, xformedMin, xformedMax, centers, scales, fieldsMap, transform, default, missingValue, xform_function, row.names = derivedFieldName)

          newBoxData$field_data <- rbind(newBoxData$field_data, newrow)
        }
      }
      d <- newBoxData$field_data[names(newBoxData$data), "dataType"]
      colmn <- newBoxData$data[, which(d == "numeric")]
    } else {
      # For each argument given:
      coln <- as.character(xform_info)
      if (grepl("\\]", coln) || grepl("\\[", coln)) {
        stop("Only input and output variable names are allowed")
      }

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

      # If column number, find the appropriate field.
      if (suppressWarnings(!is.na(as.numeric(colnm)))) {
        coln2 <- as.numeric(colnm)
        dataType <- newBoxData$field_data[names(newBoxData$data)[coln2], "dataType"]
        if (dataType == "numeric") {
          if (is.na(st[[1]][2])) {
            derivedFieldName <- paste("derived_", row.names(newBoxData$field_data)[coln2], sep = "")
          }
          colmn <- cbind(colmn, newBoxData$data[, coln2])
          dataType <- "numeric"
          type <- "derived"
          orig_field_name <- row.names(newBoxData$field_data)[coln2]
          newrow <- data.frame(type, dataType, orig_field_name, sampleMin, sampleMax, xformedMin, xformedMax, centers, scales, fieldsMap, transform, default, missingValue, xform_function, row.names = derivedFieldName)

          newBoxData$field_data <- rbind(newBoxData$field_data, newrow)
        }
      } else {
        if (!any(which(names(newBoxData$data) == colnm))) {
          stop("Variable not found in input data set")
        }
        i <- which(names(newBoxData$data) == colnm)
        dataType <- newBoxData$field_data[names(newBoxData$data)[i], "dataType"]
        if (dataType == "numeric") {
          if (is.na(st[[1]][2])) {
            derivedFieldName <- paste("derived_", row.names(newBoxData$field_data)[i], sep = "")
          }
          colmn <- cbind(colmn, newBoxData$data[, i])
          dataType <- "numeric"
          type <- "derived"
          orig_field_name <- row.names(newBoxData$field_data)[i]

          transform <- "zxform"
          newrow <- data.frame(type, dataType, orig_field_name, sampleMin, sampleMax, xformedMin, xformedMax, centers, scales, fieldsMap, transform, default, missingValue, xform_function, row.names = derivedFieldName)

          newBoxData$field_data <- rbind(newBoxData$field_data, newrow)
        }
      }
    }
    newBoxData$field_data[nrow(newBoxData$field_data), "missingValue"] <- missingValue
    newBoxData$field_data[nrow(newBoxData$field_data), "default"] <- default

    xformed <- scale(colmn, T, T)

    begin <- initLength + 1
    end <- nrow(newBoxData$field_data)
    for (i in begin:end)
    {
      j <- j + 1
      name <- row.names(newBoxData$field_data)[i]
      newMatrix <- cbind(newBoxData$data, xformed[, j])
      newBoxData$data <- newMatrix
      colLength <- length(names(newBoxData$data))
      names(newBoxData$data)[i] <- name

      if (!is.null(newBoxData$matrixData)) {
        newBoxData$matrixData <- cbind(newBoxData$matrixData, newBoxData$data[, j])
        names(newBoxData$matrixData)[i] <- name
      }
      newBoxData$field_data[i, "centers"] <- attributes(xformed)$"scaled:center"[j]
      newBoxData$field_data[i, "scales"] <- attributes(xformed)$"scaled:scale"[j]
    }

    return(newBoxData)
  }
