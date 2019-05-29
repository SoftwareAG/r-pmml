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

#' Normalize continuous values in accordance with the PMML element
#' \bold{NormContinuous}.
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
#' Given input data in a xform_wrap format, normalize the given data values to
#' lie between provided limits.
#'
#' Given an input variable named \bold{InputVar}, the name of the transformed
#' variable \bold{OutputVar}, the desired minimum value the transformed
#' variable may have \bold{low_limit}, the desired maximum value the
#' transformed variable may have \bold{high_limit}, and the desired value of
#' the transformed variable if the input variable value is missing
#' \bold{missingVal}, the \bold{xform_min_max} command including all the optional
#' parameters is in the format:
#'
#' \preformatted{
#' formInfo = "InputVar -> OutputVar[low_limit,high_limit]"
#' map_missing_to = "missingVal"
#' }
#'
#' There are two methods in which the variables can be referred to. The first
#' method is to use its column number; given the \bold{data} attribute of the
#' \bold{boxData} object, this would be the order at which the variable
#' appears. This can be indicated in the format "column#". The second method is
#' to refer to the variable by its name.
#'
#' The name of the transformed variable is optional; if the name is not
#' provided, the transformed variable is given the name: "derived_" +
#' \emph{original_variable_name} \cr Similarly, the low and high limit values
#' are optional; they have the default values of 0 and 1 respectively.
#' \bold{missingValue} is an optional parameter as well. It is the value of the
#' derived variable if the input value is missing.
#'
#' If no input variable names are provided, by default all numeric variables
#' are transformed. Note that in this case a replacement value for missing
#' input values cannot be specified; the same applies to the \bold{low_limit}
#' and \bold{high_limit} parameters.
#'
#' @author Tridivesh Jena
#'
#' @seealso \code{\link{xform_wrap}}
#'
#' @keywords manip
#'
#' @examples
#' # Load the standard iris dataset:
#' data(iris)
#'
#' # First wrap the data:
#' iris_box <- xform_wrap(iris)
#'
#' # Normalize all numeric variables of the loaded iris dataset to lie
#' # between 0 and 1. These would normalize "Sepal.Length", "Sepal.Width",
#' # "Petal.Length", "Petal.Width" to the 4 new derived variables named
#' # derived_Sepal.Length, derived_Sepal.Width, derived_Petal.Length,
#' # derived_Petal.Width.
#' xform_min_max(iris_box)
#'
#' # Normalize the 1st column values of the dataset (Sepal.Length) to lie
#' # between 0 and 1 and give the derived variable the name "dsl"
#' xform_min_max(iris_box, xform_info = "column1 -> dsl")
#'
#' # Repeat the above operation; adding the new transformed variable to
#' # the iris_box object
#' iris_box <- xform_min_max(iris_box, xform_info = "column1 -> dsl")
#'
#' # Transform Sepal.Width(the 2nd column)
#' # The new transformed variable will be given the default name
#' # "derived_Sepal.Width"
#' xform_min_max(iris_box, xform_info = "column2")
#'
#' # Repeat the same operation as above, this time using the variable name
#' xform_min_max(iris_box, xform_info = "Sepal.Width")
#'
#' # Repeat the same operation as above, assign the transformed variable,
#' # "derived_Sepal.Width". the value of 0.5 if the input value of the
#' # "Sepal.Width" variable is missing
#' xform_min_max(iris_box, xform_info = "Sepal.Width", "map_missing_to=0.5")
#'
#' # Transform Sepal.Width(the 2nd column) to lie between 2 and 3.
#' # The new transformed variable will be given the default name
#' # "derived_Sepal.Width"
#' xform_min_max(iris_box, xform_info = "column2->[2,3]")
#'
#' # Repeat the above transformation, this time the transformed variable
#' # lies between 0 and 10
#' iris_box <- xform_min_max(iris_box, xform_info = "column2->[,10]")
#' @export
xform_min_max <- function(wrap_object, xform_info = NA, map_missing_to = NA, ...) {
  colmn <- NULL
  newrow <- NULL
  center <- NULL
  scale <- NULL
  centers <- NA
  scales <- NA
  fieldsMap <- NA
  default <- NA
  missingValue <- NA

  xform_function <- NA

  colnamesGiven <- FALSE
  columnFormat <- FALSE
  j <- 0

  newBoxData <- .init_wrap_params(wrap_object)

  initLength <- nrow(newBoxData$field_data)

  if (!is.na(map_missing_to)) {
    missingValue <- as.character(map_missing_to)
  }

  if (is.na(xform_info)) {
    # if no arguments given, normalize all numeric fields between 0 and 1
    for (i in 1:newBoxData$ncols)
    {
      MIN <- 0
      MAX <- 1
      name <- row.names(newBoxData$field_data)[i]
      dataType <- newBoxData$field_data[name, "dataType"]
      if (dataType == "numeric") {
        minimum <- min(na.omit(newBoxData$data[, i]))
        maximum <- max(na.omit(newBoxData$data[, i]))
        factor <- (MAX - MIN) / (maximum - minimum)
        st <- 1 / factor
        dif <- maximum - (MAX / factor)
        center <- c(center, dif)
        scale <- c(scale, st)
        type <- "derived"
        dataType <- "numeric"
        orig_field_name <- names(newBoxData$data)[i]
        derivedFieldName <- paste("derived_", names(newBoxData$data)[i], sep = "")
        xformedMin <- MIN
        xformedMax <- MAX
        sampleMin <- minimum
        sampleMax <- maximum
        transform <- "minmax"
        newrow <- data.frame(type, dataType, orig_field_name, sampleMin, sampleMax, xformedMin, xformedMax, centers, scales, fieldsMap, transform, default, missingValue, xform_function, row.names = derivedFieldName)
        newBoxData$field_data <- rbind(newBoxData$field_data, newrow)
      }
    }
    d <- newBoxData$field_data[names(newBoxData$data), "dataType"]
    colmn <- newBoxData$data[, which(d == "numeric")]
  } else {
    # default limits
    MIN <- 0
    MAX <- 1

    coln <- as.character(xform_info)
    # expected format: initName -> finalName[MIN,MAX]
    if (grepl("\\]\\[", coln)) {
      stop("Only input and output variable names are allowed")
    }

    if (grepl("[^-]->", coln)) {
      st <- strsplit(coln, "->")
    } else {
      st <- strsplit(coln, "-->")
    }

    # origName either column-number or field name
    origName <- st[[1]][1]
    st2 <- NA
    st3 <- ""
    if (!is.na(st[[1]][2])) {
      st2 <- strsplit(st[[1]][2], "\\[")
      finalName <- st2[[1]][1]
      if (finalName == "") {
        finalName <- paste("derived_", origName, sep = "")
      }
    }
    # finalName is name of derived field

    # find MIN and MAX, if given
    if (!is.na(st2[[1]][2])) {
      st3 <- strsplit(st2[[1]][2], ",")
      if (st3[[1]][1] != "") {
        MIN <- st3[[1]][1]
      }
    }

    endVal <- gsub("]", "", st3[[1]][2])
    if (!is.na(endVal) && endVal != "") {
      MAX <- endVal
    }
    if (is.na(as.numeric(MIN)) || is.na(as.numeric(MAX))) {
      stop("Invalid xform_info. Please ensure the minimum or maximum value specified is numeric.")
    }

    MIN <- as.numeric(MIN)
    MAX <- as.numeric(MAX)

    if (grepl("column", origName, ignore.case = TRUE)) {
      origName <- gsub("column", "", origName, ignore.case = TRUE)
    }
    if (grepl("^[-,_]", origName)) {
      origName <- gsub("^[-,_]*", "", origName)
    }
    colnm <- origName

    if (suppressWarnings(!is.na(as.numeric(colnm)))) {
      coln2 <- as.numeric(colnm)
      # column number is numeric but data type of categorical below gives null
      dataType <- newBoxData$field_data[names(newBoxData$data)[coln2], "dataType"]

      if (dataType == "numeric") {
        colmn <- cbind(colmn, newBoxData$data[, coln2])

        # if input was in the format: inintName -> [MIN,MAX]
        if (st2[[1]][1] == "" || is.na(st[[1]][2])) {
          finalName <- paste("derived_", row.names(newBoxData$field_data)[coln2], sep = "")
        }

        minimum <- min(na.omit(newBoxData$data[, coln2]))
        maximum <- max(na.omit(newBoxData$data[, coln2]))

        # derive numbers so as to use the 'scale' function to normalize
        factor <- (MAX - MIN) / (maximum - minimum)
        st <- 1 / factor
        dif <- maximum - (MAX / factor)
        center <- c(center, dif)
        scale <- c(scale, st)
        type <- "derived"
        dataType <- "numeric"
        orig_field_name <- row.names(newBoxData$field_data)[coln2]
        derivedFieldName <- finalName

        xformedMin <- MIN
        xformedMax <- MAX
        sampleMin <- minimum
        sampleMax <- maximum
        transform <- "minmax"
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
        colmn <- cbind(colmn, newBoxData$data[, i])

        if (st2[[1]][1] == "" || is.na(st[[1]][2])) {
          finalName <- paste("derived_", names(newBoxData$data)[i], sep = "")
        }
        minimum <- min(na.omit(newBoxData$data[, i]))
        maximum <- max(na.omit(newBoxData$data[, i]))
        factor <- (MAX - MIN) / (maximum - minimum)
        st <- 1 / factor
        dif <- maximum - (MAX / factor)
        center <- c(center, dif)
        scale <- c(scale, st)
        type <- "derived"
        dataType <- "numeric"
        orig_field_name <- row.names(newBoxData$field_data)[i]
        derivedFieldName <- finalName
        xformedMin <- MIN
        xformedMax <- MAX
        sampleMin <- minimum
        sampleMax <- maximum
        transform <- "minmax"
        newrow <- data.frame(type, dataType, orig_field_name, sampleMin, sampleMax, xformedMin, xformedMax, centers, scales, fieldsMap, transform, default, missingValue, xform_function, row.names = derivedFieldName)
        newBoxData$field_data <- rbind(newBoxData$field_data, newrow)
      }
    }
  }

  newBoxData$field_data[nrow(newBoxData$field_data), "missingValue"] <- missingValue
  newBoxData$field_data[nrow(newBoxData$field_data), "default"] <- default
  xformed <- scale(colmn, center, scale)
  begin <- initLength + 1
  end <- nrow(newBoxData$field_data)
  for (i in begin:end)
  {
    j <- j + 1

    name <- row.names(newBoxData$field_data)[i]
    newMatrix <- cbind(newBoxData$data, xformed[, j])
    newBoxData$data <- newMatrix
    colLength <- length(names(newBoxData$data))

    if (!is.null(newBoxData$matrixData)) {
      newBoxData$matrixData <- cbind(newBoxData$matrixData, newBoxData$data[, j])
    }
    names(newBoxData$data)[i] <- name

    newBoxData$field_data[i, "centers"] <- attributes(xformed)$"scaled:center"[j]
    newBoxData$field_data[i, "scales"] <- attributes(xformed)$"scaled:scale"[j]
  }

  return(newBoxData)
}
