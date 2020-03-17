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

#' Discretize a continuous variable as indicated by interval mappings in
#' accordance with the PMML element \bold{Discretize}.
#'
#'
#' @param wrap_object Output of xform_wrap or another transformation function.
#' @param xform_info Specification of details of the transformation. This may be
#' a name of an external file or a list of data frames. Even if only 1 variable
#' is to be transformed, the information for that transform should be given as
#' a list with 1 element.
#' @param table Name of external CSV file containing the map from input to
#' output values.
#' @param default_value Value to be given to the transformed variable if the
#' value of the input variable does not lie in any of the defined intervals. If
#' 'xform_info' is a list, this is a vector with each element corresponding to
#' the corresponding list element.
#' @param map_missing_to Value to be given to the transformed variable if the
#' value of the input variable is missing.  If 'xform_info' is a list, this is a
#' vector with each element corresponding to the corresponding list element.
#' @param \dots Further arguments passed to or from other methods.
#'
#' @return R object containing the raw data, the transformed data and data
#' statistics.
#'
#' @details
#' Create a discrete variable from a continuous one as indicated by interval
#' mappings. The discrete  variable value depends on interval in which the
#' continuous variable value  lies. The mapping from intervals to discrete
#' values can be given in an  external table file referred to in the
#' transform command or as a list of data frames.
#'
#' Given a list of intervals and the discrete value each interval is linked to,
#' a discrete variable is defined with the value indicated by the interval
#' where it lies in. If a continuous variable \bold{InVar} of data type
#' \bold{InType} is to be converted to a variable \bold{OutVar} of data type
#' \bold{OutType}, the transformation command is in the format:
#'
#' xform_info = "[InVar->OutVar][InType->OutType]", table="TableFileName", \cr
#' default_value="defVal", map_missing_to="missingVal"
#'
#' where \bold{TableFileName} is the name of the CSV file containing the
#' interval to discrete value map.  The data types of the variables can be any
#' of the ones defined in the PMML format including integer, double or string.
#' \bold{defVal} is the default value of the transformed variable and if any of
#' the input values are missing, \bold{missingVal} is the value of the
#' transformed variable.
#'
#' The arguments InType, OutType, default_value and map_missing_to are optional.
#' The CSV file containing the table should not have any row and column
#' identifiers, and the values given must be in the same order as in the map
#' command. If the data types of the variables are not given, the data types of
#' the input variables are attempted to be determined from the \bold{boxData}
#' argument.  If that is not possible, the data types are assumed to be string.
#'
#' Intervals are either given by the left or right limits, in which case the
#' other limit is considered as infinite. It may also be given by both the left
#' and right limits separated by the character ":". An example of how intervals
#' should be defined in the external file are:
#'
#' \preformatted{
#' rightVal1),outVal1
#' rightVal2],outVal2
#' [leftVal1:rightVal3),outVal3
#' (leftVal2:rightVal4],outVal4
#' (leftVal,outVal5
#' }
#'
#' which, given an input value \bold{inVal} and the output value to be
#' calculated \bold{out}, means that:
#'
#' \preformatted{
#' if(inVal < rightVal1) out=outVal1
#' f(inVal <= rightVal2) out=outVal2
#' if( (inVal >= leftVal1) and (inVal < rightVal3) ) out=outVal3
#' if( (inVal > leftVal2) and (inVal <= rightVal4) ) out=outVal4
#' if(inVal > leftVal) out=outVal5
#' }
#'
#' It is also possible to give the information about the transforms without an
#' external file, using a list of data frames.  Each data frame defines a
#' discretization operation for 1 input variable. The first row of the data
#' frame gives the original field name, the derived field name, the left
#' interval, the left value, the right interval and the right value. The second
#' row gives the data type of the values as listed in the first row. The second
#' row with the data types of the fields is not required. If not given, all
#' fields are assumed to be strings. In this input format, the 'default_value'
#' and 'map_missing_to' parameters should be vectors. The first element of each
#' vector will correspond to the derived field defined in the 1st element of
#' the 'xform_info' list etc. Although somewhat more complicated, this method is
#' designed to not require any external features. Further, once the initial list
#' is constructed, modifying it is a simple operation; making this a better
#' method to use if the parameters of the transformation are to be modified
#' frequently and/or automatically. This is made more clear in the example
#' below.
#'
#' @author Tridivesh Jena
#'
#' @seealso \code{\link{xform_wrap}}
#'
#' @keywords manip
#'
#' @examples
#' # First wrap the data
#' iris_box <- xform_wrap(iris)
#' \dontrun{
#' # Convert the continuous variable "Sepal.Length" to a discrete
#' # variable "dsl". The intervals to be used for this transformation is
#' # given in a file, "intervals.csv", whose content is, for example,:
#' #
#' #  5],val1
#' #  (5:6],22
#' #  (6,val2
#' #
#' # This will be used to create a discrete variable named "dsl" of dataType
#' # "string" such that:
#' #    if(Sepal.length <= 5) then dsl = "val1"
#' #    if((Sepal.Lenght > 5) and (Sepal.Length <= 6)) then dsl = "22"
#' #    if(Sepal.Length > 6) then dsl = "val2"
#' #
#' # Give "dsl" the value 0 if the input variable value is missing.
#' iris_box <- xform_discretize(iris_box,
#'   xform_info = "[Sepal.Length -> dsl][double -> string]",
#'   table = "intervals.csv", map_missing_to = "0"
#' )
#' }
#'
#' # A different transformation using a list of data frames, of size 1:
#' t <- list()
#' m <- data.frame(rbind(
#'   c(
#'     "Petal.Length", "dis_pl", "leftInterval", "leftValue",
#'     "rightInterval", "rightValue"
#'   ),
#'   c(
#'     "double", "integer", "string", "double", "string",
#'     "double"
#'   ),
#'   c("0)", 0, "open", NA, "Open", 0),
#'   c(NA, 1, "closed", 0, "Open", 1),
#'   c(NA, 2, "closed", 1, "Open", 2),
#'   c(NA, 3, "closed", 2, "Open", 3),
#'   c(NA, 4, "closed", 3, "Open", 4),
#'   c("[4", 5, "closed", 4, "Open", NA)
#' ), stringsAsFactors = TRUE)
#'
#' # Give column names to make it look nice; not necessary!
#' colnames(m) <- c(
#'   "Petal.Length", "dis_pl", "leftInterval", "leftValue",
#'   "rightInterval", "rightValue"
#' )
#'
#' # A textual representation of the data frame is:
#' #   Petal.Length  dis_pl leftInterval leftValue rightInterval rightValue
#' # 1 Petal.Length  dis_pl leftInterval leftValue rightInterval rightValue
#' # 2       double integer       string    double        string     double
#' # 3           0)       0         open      <NA>          Open          0
#' # 4         <NA>       1       closed         0          Open          1
#' # 5         <NA>       2       closed         1          Open          2
#' # 6         <NA>       3       closed         2          Open          3
#' # 7         <NA>       4       closed         3          Open          4
#' # 8           (4       5       closed         4          Open       <NA>
#' #
#' # This is a transformation that defines a derived field 'dis_pl'
#' # which has the integer value '0' if the original field
#' # 'Petal.Length' has a value less than 0. The derived field has a
#' # value '1' if the input is greater than or equal to 0 and less
#' # than 1. Note that the values of the 1st column after row 2 have
#' # been deliberately given NA values in the middle. This is to
#' # show that that column is meant for a textual representation of
#' # the transformation as defined for the method involving external
#' # files; however in this methodtheir values are not used.
#'
#' # Add the data frame to a list. The default values and the missing
#' # values should be given as a vector, each element of the vector
#' # corresponding to the element at the same index in the list. If
#' # these values are not given as a vector, they will be used for the
#' # first list element only.
#' t[[1]] <- m
#' def <- c(11)
#' mis <- c(22)
#' iris_box <- xform_discretize(iris_box,
#'   xform_info = t, default_value = def,
#'   map_missing_to = mis
#' )
#'
#' # Make a simple model to see the effect.
#' fit <- lm(Petal.Width ~ ., iris_box$data[, -5])
#' fit_pmml <- pmml(fit, transforms = iris_box)
#' @export
xform_discretize <-
  function(wrap_object, xform_info, table, default_value = NA, map_missing_to = NA, ...) {
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
    fieldsMap <- NA
    dataMatrix <- NULL
    default <- NA
    missingValue <- NA
    sep <- ":"
    xform_function <- NA

    newBoxData <- .init_wrap_params(wrap_object)

    # if command given in older format
    if (!is.list(xform_info)) {
      # Extract data from xform_info
      # [a->c][d->s]
      minf <- as.character(xform_info)

      # separate variable names and data types
      split0 <- strsplit(minf, "]\\[")[[1]]
      split0[1] <- gsub("\\[", "", split0[1])

      # is dataTypes given?
      given <- length(split0)

      # discretize: a->c
      # dataTypes: d->s
      if (given == 2) {
        discretize <- split0[1]
        split0[2] <- gsub("]", "", split0[2])
        datType <- split0[2]
      } else {
        split0[1] <- gsub("]", "", split0[1])
        discretize <- split0[1]
        datType <- NA
      }

      # discretize: a->c
      # split the variable and dataType strings
      if (grepl("[^-]->", discretize)) {
        st <- "->"
      } else {
        st <- "-->"
      }

      val <- strsplit(discretize, st)[[1]]

      # inVal: a
      inVal <- gsub(" ", "", val[1])

      # outVal: "c"
      outVal <- gsub(" ", "", val[2])

      # if data types provided
      inDat <- NULL
      outDat <- NULL
      if (!is.na(datType)) {
        if (grepl("[^-]->", datType)) {
          st <- "->"
        } else {
          st <- "-->"
        }
        datsplit <- strsplit(datType, st)[[1]]
        inDat <- gsub(" ", "", datsplit[1])

        # outDat: "s"
        outDat <- gsub(" ", "", datsplit[2])
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
      vname <- NULL
      vdtype <- NULL
      for (i in 1:length(inVal))
      {
        vname <- c(vname, inVal)
        if (is.null(inDat)) {
          vdtype <- c(vdtype, "numeric")
        } else {
          vdtype <- c(vdtype, inDat)
        }
      }
      vname <- c(vname, outVal)

      if (is.na(outDat)) {
        vdtype <- c(vdtype, "string")
      } else {
        vdtype <- c(vdtype, outDat)
      }

      # placeholder for interval and values
      vname <- c(vname, "leftInterval", "leftValue", "rightInterval", "rightValue")
      vdtype <- c(vdtype, "string", "double", "string", "double")

      # Extract data from table
      # read interval data from csv file
      tabl <- as.character(table)
      file <- scan(tabl, what = character(0), sep = ",", quiet = T)
      ndat <- length(file)
      nrows <- length(scan(tabl, what = character(0), sep = "\n", quiet = T))
      numcols <- ndat / nrows
      dataMatrix <- matrix(file, nrow = nrows, byrow = TRUE)

      # add columns for left/right intervalType/value
      dataMatrix <- cbind(dataMatrix, NA)
      dataMatrix <- cbind(dataMatrix, NA)
      dataMatrix <- cbind(dataMatrix, NA)
      dataMatrix <- cbind(dataMatrix, NA)

      # default_value=f,map_missing_to=g
      if (!is.na(default_value)) {
        default <- as.character(default_value)
      }

      if (!is.na(map_missing_to)) {
        missingValue <- as.character(map_missing_to)
      }


      # add variable info to the data matrix
      top <- rbind(vname, vdtype)
      dataMatrix <- rbind(top, dataMatrix)
      rownames(dataMatrix) <- NULL

      type <- "derived"
      orig_field_name <- inVal

      if (is.null(outDat)) {
        dataType <- "string"
      } else {
        dataType <- outDat
      }

      transform <- "discretize"
      derivedFieldName <- outVal

      # for each field map row given except the top 2 (name and type)
      for (j in 3:nrow(dataMatrix))
      {
        leftValue <- NA
        rightValue <- NA
        leftInterval <- NA
        rightInterval <- NA

        if (grepl(sep, dataMatrix[j, 1])) {
          range <- strsplit(dataMatrix[j, 1], sep)[[1]]

          if (grepl("^\\[", range[1])) {
            leftValue <- gsub("\\[", "", range[1])
            leftInterval <- "closed"
          } else if (grepl("^\\(", range[1])) {
            leftValue <- gsub("\\(", "", range[1])
            leftInterval <- "open"
          }

          if (grepl("\\]$", range[2])) {
            rightValue <- gsub("\\]", "", range[2])
            rightInterval <- "Closed"
          } else if (grepl("\\)$", range[2])) {
            rightValue <- gsub("\\)", "", range[2])
            rightInterval <- "Open"
          }
          # end if both left and right limits given
        } else {
          range <- dataMatrix[j, 1]
          if (grepl("^\\[", range[1])) {
            leftValue <- gsub("\\[", "", range[1])
            leftInterval <- "closed"
            rightInterval <- "Open"
          } else if (grepl("^\\(", range[1])) {
            leftValue <- gsub("\\(", "", range[1])
            leftInterval <- "open"
            rightInterval <- "Open"
          } else if (grepl("\\]$", range[1])) {
            rightValue <- gsub("\\]", "", range[1])
            leftInterval <- "open"
            rightInterval <- "Closed"
          } else if (grepl("\\)$", range[1])) {
            rightValue <- gsub("\\)", "", range[1])
            leftInterval <- "open"
            rightInterval <- "Open"
          }
        }

        dataMatrix[j, 3] <- leftInterval
        dataMatrix[j, 4] <- leftValue
        dataMatrix[j, 5] <- rightInterval
        dataMatrix[j, 6] <- rightValue
      }

      colnames(dataMatrix) <- dataMatrix[1, ]
      fieldsMap <- list(dataMatrix)
      suppressWarnings(newrow <- data.frame(type, dataType, I(orig_field_name),
                                            sampleMin, sampleMax, xformedMin,
                                            xformedMax, centers, scales, I(fieldsMap),
                                            transform, default, missingValue,
                                            xform_function, row.names = derivedFieldName,
                                            check.names = FALSE, stringsAsFactors = TRUE))

      suppressWarnings(newBoxData$field_data <- rbind(newBoxData$field_data, newrow))

      newBoxData <- .xformData(dataMatrix, default, missingValue, dataType, newBoxData)
    } else {
      # for each xform indicated
      for (k in 1:length(xform_info))
      {
        dataMatrix <- as.matrix(xform_info[[k]])
        colnames(dataMatrix) <- NULL
        datatypes <- c("string", "String", "double", "Double", "boolean", "Boolean", "integer", "Integer", "float", "Float")

        for (l in 1:ncol(dataMatrix)) {
          dataMatrix[[l]] <- as.character(dataMatrix[[l]])
        }

        if (!(dataMatrix[2, 1] %in% datatypes)) {
          dataMatrix <- rbind(c("string", "numeric", "string", "double", "string", "double"), dataMatrix)
        }

        fieldsMap <- list(dataMatrix)
        dataType <- dataMatrix[2, 2]
        colnames(dataMatrix) <- dataMatrix[1, ]

        orig_field_name <- dataMatrix[1, 1]
        derivedFieldName <- dataMatrix[1, 2]
        default <- default_value[k]
        missingValue <- map_missing_to[k]

        suppressWarnings(newrow <- data.frame(type = "derived", dataType = dataType,
                                              I(orig_field_name), sampleMin, sampleMax,
                                              xformedMin, xformedMax, centers, scales,
                                              I(fieldsMap), transform = "discretize",
                                              default, missingValue, xform_function,
                                              row.names = derivedFieldName,
                                              check.names = FALSE, stringsAsFactors = TRUE))

        suppressWarnings(newBoxData$field_data <- rbind(newBoxData$field_data, newrow))

        colnames(dataMatrix) <- dataMatrix[1, ]

        newBoxData <- .xformData(dataMatrix, default, missingValue, dataType, newBoxData)
      }
    }
    return(newBoxData)
  }
.xformData <- function(dataMatrix, default, missingValue, dataType, newBoxData) {
  type <- NULL

  dataMatrix <- tolower(dataMatrix)

  for (j in 3:nrow(dataMatrix))
  {
    if (is.na(dataMatrix[j, "leftValue"]) && (dataMatrix[j, "rightInterval"] == "closed")) {
      type[j] <- 1
      next
    }
    if (is.na(dataMatrix[j, "leftValue"]) && (dataMatrix[j, "rightInterval"] == "open")) {
      type[j] <- 2
      next
    }
    if ((dataMatrix[j, "leftInterval"] == "closed") && is.na(dataMatrix[j, "rightValue"])) {
      type[j] <- 7
      next
    }
    if ((dataMatrix[j, "leftInterval"] == "open") && is.na(dataMatrix[j, "rightValue"])) {
      type[j] <- 8
      next
    }
    if ((dataMatrix[j, "leftInterval"] == "closed") && (dataMatrix[j, "rightInterval"] == "closed")) {
      type[j] <- 3
      next
    }
    if ((dataMatrix[j, "leftInterval"] == "closed") && (dataMatrix[j, "rightInterval"] == "open")) {
      type[j] <- 4
      next
    }
    if ((dataMatrix[j, "leftInterval"] == "open") && (dataMatrix[j, "rightInterval"] == "closed")) {
      type[j] <- 5
      next
    }
    if ((dataMatrix[j, "leftInterval"] == "open") && (dataMatrix[j, "rightInterval"] == "open")) {
      type[j] <- 6
      next
    }
  }

  newcol <- NULL
  origName <- colnames(dataMatrix)[1]
  derivedName <- colnames(dataMatrix)[2]

  if (newBoxData$field_data[ origName, "dataType" ] != "numeric") {
    stop("Non-numeric matrices not yet supported for transformations")
  }

  # Initialize values to default rather than missing

  if (!is.na(default)) {
    if (dataType == "numeric") {
      newcol <- rep(as.numeric(default), nrow(newBoxData$data))
      newmatrixcol <- rep(as.numeric(default), nrow(newBoxData$data))
    } else if (dataType == "boolean") {
      newcol <- rep(as.logical(default), nrow(newBoxData$data))
    }
    else {
      newcol <- rep(default, nrow(newBoxData$data))
      newmatrixcol <- rep(default, nrow(newBoxData$data))
    }
  } else if (!is.na(missingValue)) {
    if (dataType == "numeric") {
      newcol <- rep(as.numeric(missingValue), nrow(newBoxData$data))
      newmatrixcol <- rep(as.numeric(missingValue), nrow(newBoxData$data))
    } else if (missingValue == "boolean") {
      newcol <- rep(as.logical(missingValue), nrow(newBoxData$data))
    }
    else {
      newcol <- rep(missingValue, nrow(newBoxData$data))
      newmatrixcol <- rep(missingValue, nrow(newBoxData$data))
    }
  } else {
    newcol <- rep(NA, nrow(newBoxData$data))
    newmatrixcol <- rep(NA, nrow(newBoxData$data))
  }


  orig_field_name <- colnames(dataMatrix)[1]
  derivedName <- colnames(dataMatrix)[2]

  # for each field map row given except the top 2 (name and type)
  for (j in 3:nrow(dataMatrix))
  {
    if (type[j] == 1) {
      newcol[newBoxData$data[, origName] <= as.numeric(dataMatrix[j, "rightValue"])] <- dataMatrix[j, derivedName]
    }
    if (type[j] == 2) {
      newcol[newBoxData$data[, origName] < as.numeric(dataMatrix[j, "rightValue"])] <- dataMatrix[j, derivedName]
    }
    if (type[j] == 3) {
      newcol[(as.numeric(dataMatrix[j, "leftValue"]) <= newBoxData$data[, origName]) & (newBoxData$data[, orig_field_name] <= as.numeric(dataMatrix[j, "rightValue"]))] <- dataMatrix[j, derivedName]
    }
    if (type[j] == 4) {
      newcol[(as.numeric(dataMatrix[j, "leftValue"]) <= newBoxData$data[, origName]) & (newBoxData$data[, orig_field_name] < as.numeric(dataMatrix[j, "rightValue"]))] <- dataMatrix[j, derivedName]
    }
    if (type[j] == 5) {
      newcol[(as.numeric(dataMatrix[j, "leftValue"]) < newBoxData$data[, origName]) & (newBoxData$data[, orig_field_name] <= as.numeric(dataMatrix[j, "rightValue"]))] <- dataMatrix[j, derivedName]
    }
    if (type[j] == 6) {
      newcol[(as.numeric(dataMatrix[j, "leftValue"]) < newBoxData$data[, origName]) & (newBoxData$data[, orig_field_name] < as.numeric(dataMatrix[j, "rightValue"]))] <- dataMatrix[j, derivedName]
    }
    if (type[j] == 7) {
      newcol[as.numeric(dataMatrix[j, "leftValue"]) <= newBoxData$data[, origName]] <- dataMatrix[j, derivedName]
    }
    if (type[j] == 8) {
      newcol[as.numeric(dataMatrix[j, "leftValue"]) < newBoxData$data[, origName]] <- dataMatrix[j, derivedName]
    }
  }

  col <- as.matrix(newcol)
  colnames(col) <- colnames(dataMatrix)[2]
  rownames(col) <- NULL

  if (dataType == "numeric") {
    newBoxData$data <- data.frame(newBoxData$data, as.numeric(col),
                                  check.names = FALSE, stringsAsFactors = TRUE)
    colnames(newBoxData$data)[ncol(newBoxData$data)] <- derivedName
  } else {
    newBoxData$data <- data.frame(newBoxData$data, col, check.names = FALSE, stringsAsFactors = TRUE)
    newBoxData$data[, dim(newBoxData$data)[2]] <- as.factor(newBoxData$data[, dim(newBoxData$data)[2]])
  }
  if (!is.null(newBoxData$matrixData)) {
    if (dataType == "numeric") {
      newBoxData$matrixData <- cbind(newBoxData$matrixData, as.numeric(col))
      colnames(newBoxData$matrixData) <- colnames(newBoxData$data)
    } else {
      newBoxData$matrixData <- cbind(newBoxData$matrixData, col)
    }
  }

  return(newBoxData)
}
