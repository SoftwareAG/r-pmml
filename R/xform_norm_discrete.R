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

#' Normalize discrete values in accordance with the PMML element \bold{NormDiscrete}.
#'
#'
#' @param wrap_object Output of xform_wrap or another transformation function.
#' @param xform_info Specification of details of the transformation: the name of
#' the input variable to be transformed.
#' @param inputVar The input variable name in the data on which the
#' transformation is to be applied
#' @param map_missing_to Value to be given to the transformed variable if the
#' value of the input variable is missing.
#' @param \dots Further arguments passed to or from other methods.
#'
#' @return R object containing the raw data, the transformed data and data
#' statistics.
#'
#' @details
#' Define a new derived variable for each possible value of a categorical
#' variable. Given a categorical variable \bold{catVar} with possible discrete
#' values \bold{A} and \bold{B}, this will create 2 derived variables
#' \bold{catVar_A} and \bold{catVar_B}. If, for example, the input value of
#' \bold{catVar} is \bold{A} then \bold{catVar_A} equals 1 and \bold{catVar_B}
#' equals 0.
#'
#' Given an input variable, \bold{InputVar} and \bold{missingVal}, the desired
#' value of the transformed variable if the input variable value is missing,
#' the xform_norm_discrete command including all optional parameters is in the
#' format:
#'
#' xform_info="inputVar=input_variable, map_missing_to=missingVal"
#'
#' There are two methods in which the input variable can be referred to. The
#' first method is to use its column number; given the \bold{data} attribute of
#' the \bold{boxData} object, this would be the order at which the variable
#' appears. This can be indicated in the format "column#". The second method is
#' to refer to the variable by its name.
#'
#' The \bold{xform_info} and \bold{inputVar} parameters provide the same
#' information. While either one may be used when using this function, at least
#' one of them is required. If both parameters are given, the \bold{inputVar}
#' parameter is used as the default.
#'
#' The output of this transformation is a set of transformed variables, one for
#' each possible value of the input variable. For example, given possible
#' values of the input variable \bold{val1}, \bold{val2}, ... these transformed
#' variables are by default named \bold{InputVar_val1}, \bold{InputVar_val2},
#' ...
#'
#' @author Tridivesh Jena
#'
#' @seealso \code{\link{xform_wrap}}
#'
#' @keywords manip
#'
#' @examples
#' # Load the standard iris dataset, already available in R
#' data(iris)
#'
#' # First wrap the data
#' iris_box <- xform_wrap(iris)
#'
#' # Discretize the "Species" variable. This will find all possible
#' # values of the "Species" variable and define new variables. The
#' # parameter name used here should be replaced by the new preferred
#' # parameter name as shown in the next example below.
#' #
#' # 	"Species_setosa" such that it is 1 if
#' #      "Species" equals "setosa", else 0;
#' # 	"Species_versicolor" such that it is 1 if
#' #      "Species" equals "versicolor", else 0;
#' # 	"Species_virginica" such that it is 1 if
#' #      "Species" equals "virginica", else 0
#'
#' iris_box <- xform_norm_discrete(iris_box, inputVar = "Species")
#'
#' # Exact same operation performed with a different parameter name.
#' # Use of this new parameter is the preferred method as the previous
#' # parameter will be deprecated soon.
#'
#' iris_box <- xform_wrap(iris)
#' iris_box <- xform_norm_discrete(iris_box, xform_info = "Species")
#' @export
xform_norm_discrete <-
  function(wrap_object, xform_info = NA, inputVar = NA, map_missing_to = NA, ...) {
    map <- NULL
    colmn <- NULL
    newrow <- NULL
    colnamesGiven <- FALSE
    j <- 0
    sampleMin <- NA
    sampleMax <- NA
    xformedMin <- NA
    xformedMax <- NA
    centers <- NA
    scales <- NA
    default <- NA
    missingValue <- NA
    xform_function <- NA

    if (is.na(xform_info) && is.na(inputVar)) {
      stop("xform_info/inputVar parameter required.")
    }

    if (is.na(inputVar)) {
      inputVar <- xform_info
    }

    newBoxData <- .init_wrap_params(wrap_object)

    dots <- list(...)
    if (!is.null(dots$levelSeparator)) {
      variableLevelSeparator <- dots$levelSeparator
    } else {
      variableLevelSeparator <- "_"
    }
    ignoreOperators <- FALSE
    if (!is.null(dots$ignoreOperatorSigns)) {
      ignoreOperators <- TRUE
    }

    if (!is.na(map_missing_to)) {
      missingValue <- as.character(map_missing_to)
    }

    # expected input format: initialName or [initialName]
    input <- as.character(inputVar)
    fromName <- gsub("\\[", "", input)
    fromName <- gsub("\\]", "", fromName)
    fromName <- gsub("^[ ]*", "", fromName)
    fromName <- gsub("[ $]*", "", fromName)
    origName <- fromName
    if (grepl("column", origName, ignore.case = TRUE)) {
      origName <- gsub("column", "", origName, ignore.case = TRUE)
    }
    if (grepl("^[-,_]", origName)) {
      origName <- gsub("^[-,_]*", "", origName)
    }
    if (suppressWarnings(!is.na(as.numeric(origName)))) {
      colmn <- as.numeric(origName)
      fromName <- names(newBoxData$data)[colmn]
    }

    catNames <- NULL
    toNames <- NULL
    levels <- unique(newBoxData$data[fromName])[[1]]
    for (i in 1:length(levels))
    {
      catNames <- c(catNames, as.character(levels[i]))
      # name all derived fields as [original field name]_[category name].
      # Replace special characters with '_'.
      name <- paste0(fromName, variableLevelSeparator, levels[i])
      if (!ignoreOperators) {
        name <- gsub("-", "_", name)
        name <- gsub("\\+", "_", name)
        name <- gsub("\\*", "_", name)
        name <- gsub(":", "_", name)
        name <- gsub("'", "_", name)
      }
      toNames <- c(toNames, name)
    }

    for (i in 1:length(catNames))
    {
      type <- "derived"
      dataType <- "numeric"
      orig_field_name <- fromName
      derivedFieldName <- toNames[i]
      fieldsMap <- list(as.character(catNames[i]))

      transform <- "NormDiscrete"
      newrow <- data.frame(type, dataType, orig_field_name, sampleMin, sampleMax, xformedMin, xformedMax, centers, scales, I(fieldsMap), transform, default, missingValue, xform_function, row.names = derivedFieldName, check.names = FALSE)
      suppressWarnings(newBoxData$field_data <- rbind(newBoxData$field_data, newrow))

      newcol <- NULL
      newcol <- 1 * (newBoxData$data[, fromName] == catNames[i])
      newcol[is.na(newcol)] <- missingValue

      names <- toNames[i]
      newmat <- as.matrix(newcol)
      colnames(newmat) <- names
      rownames(newmat) <- NULL

      newBoxData$data <- data.frame(newBoxData$data, newmat, check.names = FALSE)

      if (!is.null(newBoxData$matrixData)) {
        newBoxData$matrixData <- cbind(newBoxData$matrixData, newmat)
      }
    }

    newBoxData$field_data[nrow(newBoxData$field_data), "missingValue"] <- missingValue
    newBoxData$field_data[nrow(newBoxData$field_data), "default"] <- default

    return(newBoxData)
  }
