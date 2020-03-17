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

#' Implement a map between discrete values in accordance with the PMML element
#' \bold{MapValues}.
#'
#'
#' @param wrap_object Output of xform_wrap or another transformation function.
#' @param xform_info Specification of details of the transformation. It can be a
#' text giving the external file name or a list of data frames. Even if only 1
#' variable is to be transformed, the information for that map should be given
#' as a list with 1 element.
#' @param table Name of external CSV file containing the map from input to
#' output values.
#' @param default_value The default value to be given to the transformed
#' variable. If 'xform_info' is a list, this is a vector with each element
#' corresponding to the corresponding list element.
#' @param map_missing_to Value to be given to the transformed variable if the
#' value of the input variable is missing.  If 'xform_info' is a list, this is a
#' vector with each element corresponding to the corresponding list element.
#' @param \dots Further arguments passed to or from other methods.
#'
#' @return R object containing the raw data, the transformed data and data
#' statistics.
#'
#' @details
#' Map discrete values of an input variable to a discrete value of the
#' transformed variable. The map can be given in an external table file
#' referred to in the transform command or as a list of data frames, each data
#' frame defining a map transform for one variable.
#'
#' Given a map from the combination of variables \bold{InVar1}, \bold{InVar2},
#' ... to the transformed variable \bold{OutVar}, where the variables have the
#' data types \bold{InType1}, \bold{InType2}, ... and \bold{OutType}, the map
#' command is in the format:
#'
#' \preformatted{
#' xform_info = "[InVar1,InVar2,... -> OutVar][InType1,InType2,... -> OutType]"
#' table = "TableFileName", default_value = "defVal", map_missing_to = "missingVal"
#' }
#'
#' where \bold{TableFileName} is the name of the CSV file containing the map.
#' The map can be a N to 1 map where N is greater or equal to 1. The data types
#' of the variables can be any of the ones defined in the PMML format including
#' integer, double or string. \bold{defVal} is the default value of the
#' transformed variable and if any of the map input values are missing,
#' \bold{missingVal} is the value of the transformed variable.
#'
#' The arguments InType, OutType, default_value and map_missing_to are optional.
#' The CSV file containing the table should not have any row and column
#' identifiers, and the values given must be in the same order as in the map
#' command. If the data types of the variables are not given, the data types of
#' the input variables are attempted to be determined from the \bold{boxData}
#' argument.  If that is not possible, the data type is assumed to be string.
#'
#' It is also possible to give the maps to be implemented without an external
#' file using a list of data frames. Each data frame defines a map for 1 input
#' variable. Given a data frame with N+1 columns, it is assumed that the map is
#' a N to 1 map where the last column of the data frame corresponds to the
#' derived field. The 1st row is assumed to be the names of the fields and the
#' second row the data types of the fields. The rest of the rows define the
#' map; each combination of the input values in a row is mapped to the value in
#' the last column of that row. The second row with the data types of the
#' fields is not required. If not given, all fields are assumed to be strings.
#' In this input format, the 'default_value' and 'map_missing_to' parameters
#' should be vectors. The first element of each vector will correspond to the
#' derived field defined in the 1st element of the 'xform_info' list etc. These
#' are made clearer in the example below.
#'
#' @author Tridivesh Jena
#'
#' @seealso \code{\link{xform_wrap}}, \code{\link[pmml]{pmml}}
#'
#' @keywords manip
#'
#' @examples
#' # Load the standard audit dataset, part of the pmml package:
#' data(audit)
#'
#' # First wrap the data:
#' audit_box <- xform_wrap(audit)
#' \dontrun{
#' # One of the variables, "Sex", has 2 possible values: "Male"
#' # and "Female". If these string values have to be mapped to a
#' # numeric value, a file has to becreated, say "MapGender.csv"
#' # whose content is, for example:
#' #
#' #  Male,1
#' #  Female,2
#' #
#' # Transform the variable "Gender" to a variable "d_gender"
#' # such that:
#' #    if Sex = "Male" then d_sex = "1"
#' #    if Sex = "Female" then d_sex = "0"
#' #
#' # Give "d_sex" the value 0 if the input variable value is
#' # missing.
#' audit_box <- xform_map(audit_box,
#'   xform_info = "[Sex -> d_sex][string->integer]",
#'   table = "MapGender.csv", map_missing_to = "0"
#' )
#' }
#' # Same as above, with an extra variable, but using data frames.
#' # The top 2 rows gives the variable names and their data types.
#' # The rest represent the map. So for example, the third row
#' # indicates that when the input variable "Sex" has the value
#' # "Male" and the input variable "Employment" has
#' # the value "PSLocal", the output variable "d_sex" should have
#' # the value 1.
#' t <- list()
#' m <- data.frame(
#'   c("Sex", "string", "Male", "Female"),
#'   c("Employment", "string", "PSLocal", "PSState"),
#'   c("d_sex", "integer", 1, 0),
#'   stringsAsFactors = TRUE
#' )
#' t[[1]] <- m
#'
#' # Give default value as a vector and missing value as a string,
#' # this is only possible as there is only one map defined. If
#' # default values is not given, it will simply not be given in
#' # the PMML file as well. In general, the default values and the
#' # missing values should be given as a vector, each element of
#' # the vector corresponding to the element at the same index in
#' # the list. If these values are not given as a vector, they will
#' # be used for the first list element only.
#' audit_box <- xform_map(audit_box,
#'   xform_info = t, default_value = c(3),
#'   map_missing_to = "2"
#' )
#'
#' # check what the pmml looks like
#' fit <- lm(Adjusted ~ ., data = audit_box$data)
#' fit_pmml <- pmml(fit, transforms = audit_box)
#' @export
xform_map <- function(wrap_object, xform_info, table = NA, default_value = NA, map_missing_to = NA, ...) {
  newrow <- NULL
  colnamesGiven <- FALSE
  j <- 0
  orig_field_name <- NULL
  derivedFieldName <- NA
  sampleMin <- NA
  sampleMax <- NA
  xformedMin <- NA
  xformedMax <- NA
  centers <- NA
  scales <- NA
  missingValue <- NA

  xform_function <- NA

  dataMatrix <- NULL
  default <- NA

  newBoxData <- .init_wrap_params(wrap_object)

  if (!is.list(xform_info)) {
    # Extract data from xform_info
    # [a,b->c][d,d->s]
    minf <- as.character(xform_info)

    # separate variable names and data types
    split0 <- strsplit(minf, "]\\[")[[1]]
    split0[1] <- gsub("\\[", "", split0[1])

    # is dataTypes given?
    given <- length(split0)

    # mapVal: a,b->c
    # datType: d,d->s
    if (given == 2) {
      mapVal <- split0[1]
      split0[2] <- gsub("]", "", split0[2])
      datType <- split0[2]
    } else {
      split0[1] <- gsub("]", "", split0[1])
      mapVal <- split0[1]
      datType <- NA
    }

    # mapVal: a,b-> c
    mapVal <- gsub("^[ ]*", "", mapVal)
    mapVal <- gsub("[ $]*", "", mapVal)

    # split the variable and dataType strings
    if (grepl("[^-]->", mapVal)) {
      st <- "->"
    } else {
      st <- "-->"
    }

    val <- strsplit(mapVal, st)[[1]]

    # inval: a,b
    inVal <- val[1]
    valsplit <- strsplit(inVal, ",")[[1]]

    # inVals: "a" "b"
    inVals <- valsplit
    for (i in 1:length(inVals))
    {
      inVals[i] <- gsub("^[ ]*", "", inVals[i])
      inVals[i] <- gsub("[ $]*", "", inVals[i])
    }

    # outVal: "c"
    outVal <- val[2]
    outVal <- gsub("^[ ]*", "", outVal)
    outVal <- gsub("[ $]*", "", outVal)

    # if data types provided
    inDats <- NULL
    outDat <- NULL
    if (!is.na(datType)) {
      if (grepl("[^-]->", datType)) {
        st <- "->"
      } else {
        st <- "-->"
      }
      datsplit <- strsplit(datType, st)[[1]]
      inDat <- datsplit[1]

      # inDats: "d" "d"
      inDats <- strsplit(inDat, ",")[[1]]
      for (i in 1:length(inDats))
      {
        inDats[i] <- gsub("^[ ]*", "", inDats[i])
        inDats[i] <- gsub("[ $]*", "", inDats[i])
      }

      # outDat: "s"
      outDat <- datsplit[2]
      outDat <- gsub("^[ ]*", "", outDat)
      outDat <- gsub("[ $]*", "", outDat)
    }

    # convert double, integer to numeric for field_data
    if (!is.null(outDat)) {
      if ((outDat == "double") || (outDat == "integer")) {
        outDat <- "numeric"
      }
    } else {
      outDat <- "string"
    }

    # make variable name and data type rows to bind on data map later
    vnames <- NULL
    vdtype <- NULL
    for (i in 1:length(inVals))
    {
      vnames <- c(vnames, inVals[i])
      if (is.null(inDats[i])) {
        vdtype <- c(vdtype, "string")
      } else {
        vdtype <- c(vdtype, inDats[i])
      }
    }
    vnames <- c(vnames, outVal)

    if (is.na(outDat)) {
      vdtype <- c(vdtype, "string")
    } else {
      vdtype <- c(vdtype, outDat)
    }

    # Extract data from table
    # read data from csv file
    tabl <- as.character(table)
    file <- scan(tabl, what = character(0), sep = ",", quiet = T)
    ndat <- length(file)
    nrows <- length(scan(tabl, what = character(0), sep = "\n", quiet = T))
    numcols <- ndat / nrows
    dataMatrix <- matrix(file, nrow = nrows, byrow = TRUE)

    if (!is.na(default_value)) {
      default <- as.character(default_value)
    }
    if (!is.na(map_missing_to)) {
      missingValue <- as.character(map_missing_to)
    }

    # add variable info to the data matrix
    top <- rbind(vnames, vdtype)
    dataMatrix <- rbind(top, dataMatrix)
    rownames(dataMatrix) <- NULL

    type <- "derived"

    orig_field_name <- paste(inVals, collapse = ",")

    if (is.null(outDat)) {
      dataType <- "string"
    } else {
      dataType <- outDat
    }
    fieldsMap <- list(dataMatrix)
    transform <- "MapValues"
    derivedFieldName <- outVal


    suppressWarnings(newrow <- data.frame(type, dataType, I(orig_field_name),
      sampleMin, sampleMax, xformedMin,
      xformedMax, centers, scales, I(fieldsMap),
      transform, default, missingValue,
      xform_function,
      row.names = derivedFieldName,
      check.names = FALSE,
      stringsAsFactors = TRUE
    ))
    suppressWarnings(newBoxData$field_data <- rbind(newBoxData$field_data, newrow))

    newcol <- NULL
    newmatrixcol <- NULL

    if (!is.na(default)) {
      if (outDat == "numeric") {
        newcol <- rep(as.numeric(default), nrow(newBoxData$data))
        newmatrixcol <- rep(as.numeric(default), nrow(newBoxData$data))
      } else if (outDat == "boolean") {
        newcol <- rep(as.logical(default), nrow(newBoxData$data))
      }
      else {
        newcol <- rep(default, nrow(newBoxData$data))
        newmatrixcol <- rep(default, nrow(newBoxData$data))
      }
    } else {
      newcol <- rep(NA, nrow(newBoxData$data))
      newmatrixcol <- rep(NA, nrow(newBoxData$data))
    }

    if (!is.na(missingValue)) {
      for (i in 1:length(inVals))
      {
        na <- which(is.na(newBoxData$data[, inVals[i]]))
        for (j in na)
        {
          if (outDat == "numeric") {
            newcol[j] <- rep(as.numeric(missingValue), nrow(newBoxData$data))
            newmatrixcol[j] <- rep(as.numeric(missingValue), nrow(newBoxData$data))
          } else if (outDat == "boolean") {
            newcol[j] <- rep(as.logical(missingValue), nrow(newBoxData$data))
            newmatrixcol[j] <- rep(as.logical(missingValue), nrow(newBoxData$data))
          } else {
            newcol[j] <- rep(missingValue, nrow(newBoxData$data))
            newmatrixcol[j] <- rep(missingValue, nrow(newBoxData$data))
          }
        }
      }
    }

    # for each mapvalue row given except the top 2 (var name and dataType)
    for (j in 3:nrow(dataMatrix))
    {
      if (outDat == "numeric") {
        # For each column of dataMatrix (i.e., each input map value), find if input data has that value.
        # Do this by creating a matrix with the input values from dataMatrix repeated as many times as the number of
        # data input variables, so each input value has the same matrix row to compare with.
        # The result is a set of True and False. Apply function 'all' by row to see if all values in a row are true.
        # Resulting rows are the rows which match all the input map values.
        newcol[apply(as.matrix(newBoxData$data[, dataMatrix[1, 1:(ncol(dataMatrix) - 1)]] == matrix(rep(dataMatrix[j, 1:(ncol(dataMatrix) - 1)], nrow(newBoxData$data)), nrow = nrow(newBoxData$data), byrow = T)), 1, all)] <- as.numeric(dataMatrix[j, ncol(dataMatrix)])
        newmatrixcol[apply(as.matrix(newBoxData$data[, dataMatrix[1, 1:(ncol(dataMatrix) - 1)]] == matrix(rep(dataMatrix[j, 1:(ncol(dataMatrix) - 1)], nrow(newBoxData$data)), nrow = nrow(newBoxData$data), byrow = T)), 1, all)] <- as.numeric(dataMatrix[j, ncol(dataMatrix)])
      } else if (outDat == "boolean") {
        newcol[apply(as.matrix(newBoxData$data[, dataMatrix[1, 1:(ncol(dataMatrix) - 1)]] == matrix(rep(dataMatrix[j, 1:(ncol(dataMatrix) - 1)], nrow(newBoxData$data)), nrow = nrow(newBoxData$data), byrow = T)), 1, all)] <- as.logical(dataMatrix[j, ncol(dataMatrix)])
        newmatrixcol[apply(as.matrix(newBoxData$data[, dataMatrix[1, 1:(ncol(dataMatrix) - 1)]] == matrix(rep(dataMatrix[j, 1:(ncol(dataMatrix) - 1)], nrow(newBoxData$data)), nrow = nrow(newBoxData$data), byrow = T)), 1, all)] <- as.logical(dataMatrix[j, ncol(dataMatrix)])
      } else {
        newcol[apply(as.matrix(newBoxData$data[, dataMatrix[1, 1:(ncol(dataMatrix) - 1)]] == matrix(rep(dataMatrix[j, 1:(ncol(dataMatrix) - 1)], nrow(newBoxData$data)), nrow = nrow(newBoxData$data), byrow = T)), 1, all)] <- dataMatrix[j, ncol(dataMatrix)]
        newmatrixcol[apply(as.matrix(newBoxData$data[, dataMatrix[1, 1:(ncol(dataMatrix) - 1)]] == matrix(rep(dataMatrix[j, 1:(ncol(dataMatrix) - 1)], nrow(newBoxData$data)), nrow = nrow(newBoxData$data), byrow = T)), 1, all)] <- dataMatrix[j, ncol(dataMatrix)]
      }
    }

    col <- as.matrix(newcol)
    newBoxData$data <- data.frame(newBoxData$data, col, check.names = FALSE, stringsAsFactors = TRUE)
    if (outDat == "string") {
      newBoxData$data[, ncol(newBoxData$data)] <- as.factor(newBoxData$data[, ncol(newBoxData$data)])
    }
    colnames(newBoxData$data)[ncol(newBoxData$data)] <- dataMatrix[1, ncol(dataMatrix)]
    rownames(newBoxData$data) <- NULL

    if (!is.null(newBoxData$matrixData)) {
      matrixcol <- as.matrix(newmatrixcol)
      newBoxData$matrixData <- cbind(newBoxData$matrixData, matrixcol)
      colnames(newBoxData$matrixData) <- colnames(newBoxData$data)
      rownames(newBoxData$matrixData) <- NULL
    }
  } else {
    for (k in 1:length(xform_info))
    {
      ifelse(is.list(xform_info), xform <- xform_info[[k]], xform <- table[[k]])
      datatypes <- c("string", "String", "double", "Double", "boolean", "Boolean", "integer", "Integer", "float", "Float")
      if (!(xform[2, ncol(xform)] %in% datatypes)) {
        datype <- rep("string", ncol(xform))
        for (l in 1:ncol(xform)) {
          xform[[l]] <- as.character(xform[[l]])
        }
        xform <- rbind(xform[1, ], datype, xform[-1, ])
      }
      orig <- as.matrix(xform[1, -ncol(xform)])
      colnames(orig) <- NULL
      rownames(orig) <- NULL
      orig_field_name <- list(orig)

      derivedFieldName <- as.character(xform[1, ncol(xform)])
      default <- default_value[k]
      missingValue <- map_missing_to[k]
      if ((as.character(xform[2, ncol(xform)]) == "double") || (as.character(xform[2, ncol(xform)]) == "integer")) {
        outDat <- "numeric"
      } else if (as.character(xform[2, ncol(xform)]) == "double") {
        outDat <- "boolean"
      } else {
        outDat <- "string"
      }

      if (derivedFieldName %in% rownames(newBoxData$field_data)[newBoxData$field_data[, "type"] == "derived"]) {
        newBoxData$field_data <- newBoxData$field_data[-which(rownames(newBoxData$field_data) == derivedFieldName), ]
      }

      suppressWarnings(newrow <- data.frame("derived", outDat, I(orig_field_name),
                                            sampleMin, sampleMax, xformedMin, xformedMax,
                                            centers, scales, I(list(as.matrix(xform))),
                                            "MapValues", default_value[k], map_missing_to[k],
                                            xform_function, row.names = derivedFieldName,
                                            check.names = FALSE, stringsAsFactors = TRUE))
      colnames(newrow) <- c("type", "dataType", "orig_field_name", "sampleMin", "sampleMax", "xformedMin", "xformedMax", "centers", "scales", "fieldsMap", "transform", "default", "missingValue", "xform_function")



      suppressWarnings(newBoxData$field_data <- rbind(newBoxData$field_data, newrow))
      dataMatrix <- as.matrix(xform)

      newcol <- NULL
      newmatrixcol <- NULL
      if (!is.na(default)) {
        if (outDat == "numeric") {
          newcol <- rep(as.numeric(default), nrow(newBoxData$data))
          newmatrixcol <- rep(as.numeric(default), nrow(newBoxData$data))
        } else if (outDat == "boolean") {
          newcol <- rep(as.logical(default), nrow(newBoxData$data))
        }
        else {
          newcol <- rep(default, nrow(newBoxData$data))
          newmatrixcol <- rep(default, nrow(newBoxData$data))
        }
      }

      if (!is.na(missingValue)) {
        for (i in 1:length(orig[1, ]))
        {
          na <- which(is.na(newBoxData$data[, orig[1, i]]))
          for (j in na)
          {
            if (outDat == "numeric") {
              newcol[j] <- rep(as.numeric(missingValue), nrow(newBoxData$data))
              newmatrixcol[j] <- rep(as.numeric(missingValue), nrow(newBoxData$data))
            } else if (outDat == "boolean") {
              newcol[j] <- rep(as.logical(missingValue), nrow(newBoxData$data))
              newmatrixcol[j] <- rep(as.logical(missingValue), nrow(newBoxData$data))
            } else {
              newcol[j] <- rep(missingValue, nrow(newBoxData$data))
              newmatrixcol[j] <- rep(missingValue, nrow(newBoxData$data))
            }
          }
        }
      }

      for (j in 3:nrow(dataMatrix))
      {
        if (outDat == "numeric") {
          # For each column of dataMatrix (ie, each input map value), find if input data has that value.
          # Do this by creating a matrix with the input values from dataMatrix repeated as many times as the number of
          # data input variables; so each input value has the same matrix row to compare with.
          # The result is a set of True and False. Apply function 'all' by row to see if all values in a row are true.
          # Resulting rows are the rows which match all the input map values.
          newcol[apply(as.matrix(newBoxData$data[, dataMatrix[1, 1:(ncol(dataMatrix) - 1)]] == matrix(rep(dataMatrix[j, 1:(ncol(dataMatrix) - 1)], nrow(newBoxData$data)), nrow = nrow(newBoxData$data), byrow = T)), 1, all)] <- as.numeric(dataMatrix[j, ncol(dataMatrix)])
          newmatrixcol[apply(as.matrix(newBoxData$data[, dataMatrix[1, 1:(ncol(dataMatrix) - 1)]] == matrix(rep(dataMatrix[j, 1:(ncol(dataMatrix) - 1)], nrow(newBoxData$data)), nrow = nrow(newBoxData$data), byrow = T)), 1, all)] <- as.numeric(dataMatrix[j, ncol(dataMatrix)])
        } else if (outDat == "boolean") {
          newcol[apply(as.matrix(newBoxData$data[, dataMatrix[1, 1:(ncol(dataMatrix) - 1)]] == matrix(rep(dataMatrix[j, 1:(ncol(dataMatrix) - 1)], nrow(newBoxData$data)), nrow = nrow(newBoxData$data), byrow = T)), 1, all)] <- as.logical(dataMatrix[j, ncol(dataMatrix)])
          newmatrixcol[apply(as.matrix(newBoxData$data[, dataMatrix[1, 1:(ncol(dataMatrix) - 1)]] == matrix(rep(dataMatrix[j, 1:(ncol(dataMatrix) - 1)], nrow(newBoxData$data)), nrow = nrow(newBoxData$data), byrow = T)), 1, all)] <- as.logical(dataMatrix[j, ncol(dataMatrix)])
        } else {
          newcol[apply(as.matrix(newBoxData$data[, dataMatrix[1, 1:(ncol(dataMatrix) - 1)]] == matrix(rep(dataMatrix[j, 1:(ncol(dataMatrix) - 1)], nrow(newBoxData$data)), nrow = nrow(newBoxData$data), byrow = T)), 1, all)] <- dataMatrix[j, ncol(dataMatrix)]
          newmatrixcol[apply(as.matrix(newBoxData$data[, dataMatrix[1, 1:(ncol(dataMatrix) - 1)]] == matrix(rep(dataMatrix[j, 1:(ncol(dataMatrix) - 1)], nrow(newBoxData$data)), nrow = nrow(newBoxData$data), byrow = T)), 1, all)] <- dataMatrix[j, ncol(dataMatrix)]
        }
      }

      col <- as.matrix(newcol)
      matrixcol <- as.matrix(newmatrixcol)

      newBoxData$data <- data.frame(newBoxData$data, col, check.names = FALSE, stringsAsFactors = TRUE)
      colnames(newBoxData$data)[ncol(newBoxData$data)] <- dataMatrix[1, ncol(dataMatrix)]
      rownames(newBoxData$data) <- NULL

      if (!is.null(newBoxData$matrixData)) {
        newBoxData$matrixData <- cbind(newBoxData$matrixData, matrixcol)
        colnames(newBoxData$matrixData) <- colnames(newBoxData$data)
        rownames(newBoxData$matrixData) <- NULL
      }
    }
  }
  return(newBoxData)
}
