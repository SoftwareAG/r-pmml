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

.pmmlDataDictionary <- function(field, dataset = NULL, weights = NULL, transformed = NULL, target = NULL) {
  number.of.fields <- length(field$name)

  if (field$name[1] == "ZementisClusterIDPlaceHolder" || field$name[1] == "ZementisHiddenTargetField") {
    begin <- 2
  } else {
    begin <- 1
  }

  namelist <- list()
  dnamelist <- list()
  optypelist <- list()
  datypelist <- NULL
  fname <- NULL
  data.fields <- list()
  # Discrete place holder variable name as set in pmml.naiveBayes.R.
  DPL1 <- "DiscretePlaceHolder"
  DPL2 <- "Temp"
  DPL3 <- "predictedScore"

  if (!is.null(transformed)) {
    for (i in 1:nrow(transformed$field_data))
    {
      type <- as.character(transformed$field_data[i, "dataType"])
      if (type == "numeric") {
        datypelist[[row.names(transformed$field_data)[i]]] <- "double"
      } else if (type == "logical") {
        datypelist[[row.names(transformed$field_data)[i]]] <- "boolean"
      } else {
        datypelist[[row.names(transformed$field_data)[i]]] <- "string"
      }

      if (type == "numeric") {
        optypelist[[row.names(transformed$field_data)[i]]] <- "continuous"
      } else {
        optypelist[[row.names(transformed$field_data)[i]]] <- "categorical"
      }
    }
    if (field$name[1] == "survival") {
      datypelist[[field$name[1]]] <- "double"
      optypelist[[field$name[1]]] <- "continuous"
    }
    if (DPL1 %in% field$name) {
      datypelist[[DPL1]] <- "string"
      optypelist[[DPL1]] <- "categorical"
    }
    if (DPL2 %in% field$name) {
      datypelist[[DPL2]] <- "string"
      optypelist[[DPL2]] <- "categorical"
    }
    if (DPL3 %in% field$name) {
      datypelist[[DPL3]] <- "double"
      optypelist[[DPL3]] <- "continuous"
    }
  } else {
    for (i in begin:number.of.fields)
    {
      fname <- field$name[i]
      if (length(grep("as\\.factor\\(", field$name[i])) == 1) {
        fname <- gsub("as.factor\\((\\w*)\\)", "\\1", field$name[i], perl = TRUE)
      }
      optype <- "UNKNOWN"
      datype <- "UNKNOWN"
      values <- NULL

      if (field$class[[field$name[i]]] == "numeric") {
        optypelist[[fname]] <- "continuous"
        datypelist[[fname]] <- "double"
      } else if (field$class[[field$name[i]]] == "logical") {
        optypelist[[fname]] <- "categorical"
        datypelist[[fname]] <- "boolean"
      } else if (field$class[[field$name[i]]] == "factor") {
        optypelist[[fname]] <- "categorical"
        datypelist[[fname]] <- "string"
      } else {
        disallowed_class <- field$class[[field$name[i]]]
        stop(paste(disallowed_class, "class is not supported for features. Supported classes: numeric, logical, factor."))
      }
    }
  }

  if (!is.null(transformed)) {
    for (i in begin:number.of.fields) {
      if (.removeAsFactor(field$name[i]) %in% c(target, .removeAsFactor(target), DPL1, DPL2, DPL3)) {
        namelist <- c(namelist, .removeAsFactor(field$name[i]))
        next
      }
      if (transformed$field_data[field$name[i], "type"] == "original") {
        if (!(.removeAsFactor(field$name[i]) %in% namelist)) {
          namelist <- c(namelist, .removeAsFactor(field$name[i]))
        }
      } else {
        ofnames <- strsplit(transformed$field_data[field$name[i], "orig_field_name"][[1]], ",")[[1]]
        for (j in 1:length(ofnames)) {
          ofname <- gsub("^\\s+|\\s+$", "", ofnames[j])
          hname <- transformed$field_data[ofname, "orig_field_name"]
          ancestorField <- ofname
          while (!is.na(hname)) {
            ancestorField <- hname
            hname <- transformed$field_data[hname, "orig_field_name"]
          }
          fname <- .removeAsFactor(ancestorField)
          if ((!(fname %in% namelist)) && (!(fname %in% dnamelist))) {
            namelist <- c(namelist, fname)
            if (!(.removeAsFactor(field$name[i]) %in% dnamelist)) {
              dnamelist <- c(dnamelist, .removeAsFactor(field$name[i]))
            }
          }
        }
      }
    }
  } else {
    for (i in begin:number.of.fields) {
      fName <- field$name[i]
      if (length(grep("as\\.factor\\(", field$name[i])) == 1) {
        fName <- gsub("as.factor\\((\\w*)\\)", "\\1", field$name[i], perl = TRUE)
      }

      if (!is.na(field$class[fName]) && field$class[fName] == "factor") {
        optypelist[[fName]] <- "categorical"
      }

      if (!(fName %in% namelist) && fName != "ZementisClusterIDPlaceHolder") {
        namelist <- c(namelist, fName)
      }
    }
  }

  # DataDictionary
  data.dictionary <- xmlNode("DataDictionary",
    attrs = c(numberOfFields = length(namelist))
  )

  if (!is.null(weights) && length(weights)) {
    data.dictionary <- append.XMLNode(data.dictionary, xmlNode("Extension",
      attrs = c(
        name = "Weights",
        value = weights, extender = "Rattle"
      )
    ))
  }

  nmbr <- 1
  for (ndf2 in 1:length(namelist))
  {
    optype <- optypelist[[namelist[ndf2][[1]]]]
    datype <- datypelist[[namelist[ndf2][[1]]]]
    data.fields[[nmbr]] <- xmlNode("DataField", attrs = c(
      name = namelist[ndf2],
      optype = optype, dataType = datype
    ))

    # DataDictionary -> DataField -> Interval
    fname <- namelist[ndf2][[1]]
    if (optypelist[[fname]] == "continuous" && !is.null(dataset) && fname != "survival") {
      dataval <- NULL
      for (j in 1:length(dataset[[fname]]))
      {
        dataval <- c(dataval, as.numeric(dataset[[fname]][j]))
      }

      interval <- xmlNode("Interval",
        attrs = c(
          closure = "closedClosed",
          leftMargin = min(dataval, na.rm = TRUE), # 091025 Handle missing values
          rightMargin = max(dataval, na.rm = TRUE)
        )
      ) # 091025 Handle missing values
      data.fields[[nmbr]] <- append.XMLNode(data.fields[[nmbr]], interval)
    }

    # DataDictionary -> DataField -> Value
    name <- namelist[nmbr][[1]]
    if (optypelist[[name]] == "categorical") {
      if (is.null(field$levels[[name]]) && !is.null(transformed)) {
        lev <- levels(as.list(unique(transformed$data[name]))[[1]])
        for (j in seq_along(lev))
        {
          data.fields[[nmbr]][[j]] <- xmlNode("Value",
            attrs = c(value = .markupSpecials(lev[j]))
          )
        }
      } else {
        for (j in seq_along(field$levels[[namelist[nmbr][[1]]]]))
        {
          data.fields[[nmbr]][[j]] <- xmlNode("Value",
            attrs = c(value = field$levels[[namelist[nmbr][[1]]]][j])
          )
          # attrs=c(value=.markupSpecials(field$levels[[namelist[nmbr][[1]]]][j])))
        }
      }
    }

    data.dictionary <- append.XMLNode(data.dictionary, data.fields[[nmbr]])
    nmbr <- nmbr + 1
  }

  return(data.dictionary)
}

.pmmlDataDictionarySurv <- function(field, timeName, statusName, dataset = NULL, weights = NULL, transformed = NULL) {
  # Modify for a survival model. Survival forests do not typically have
  # a predicted field. Add a generic predicted field. If a predicted
  # field is included, this field will be ignored by the model.

  number.of.fields <- length(field$name)
  ii <- 0

  optypelist <- list()
  namelist <- list()
  datypelist <- list()
  data.fields <- list()

  if (field$name[1] == "ZementisClusterIDPlaceHolder") {
    begin <- 2
  } else {
    begin <- 1
  }

  if (!is.null(transformed)) {
    for (i in 1:nrow(transformed$field_data))
    {
      type <- as.character(transformed$field_data[i, "dataType"])
      if (type == "numeric") {
        datypelist[[row.names(transformed$field_data)[i]]] <- "double"
      } else if (type == "logical") {
        datypelist[[row.names(transformed$field_data)[i]]] <- "boolean"
      } else {
        datypelist[[row.names(transformed$field_data)[i]]] <- "categorical"
      }

      if (type == "numeric") {
        optypelist[[row.names(transformed$field_data)[i]]] <- "continuous"
      } else {
        optypelist[[row.names(transformed$field_data)[i]]] <- "categorical"
      }
    }
    if (field$name[1] == "survival") {
      datypelist[[field$name[1]]] <- "double"
      optypelist[[field$name[1]]] <- "continuous"
    }
  } else {
    for (i in begin:number.of.fields)
    {
      optype <- "UNKNOWN"
      datype <- "UNKNOWN"
      values <- NULL

      if (field$class[[field$name[i]]] == "numeric") {
        optypelist[[field$name[i]]] <- "continuous"
        datypelist[[field$name[i]]] <- "double"
      } else if (field$class[[field$name[i]]] == "logical") {
        optypelist[[field$name[i]]] <- "categorical"
        datypelist[[field$name[i]]] <- "boolean"
      } else if (field$class[[field$name[i]]] == "factor") {
        optypelist[[field$name[i]]] <- "categorical"
        datypelist[[field$name[i]]] <- "string"
      }
    }
  }

  for (i in 1:number.of.fields)
  {
    if (length(grep(":", field$name[i])) == 1) {
    } else {
      ii <- ii + 1

      # DataDictionary -> DataField
      if (!is.null(transformed)) {
        if (is.na(transformed$field_data[field$name[i], "orig_field_name"])) {
          if (is.na(transformed$field_data[field$name[i], "transform"])) {
            if (!(field$name[i] %in% namelist)) {
              namelist <- c(namelist, field$name[i])
            }
          }
        } else {
          ofname <- transformed$field_data[field$name[i], "orig_field_name"][[1]]
          for (j in 1:length(ofname))
          {
            fname <- ofname[j]
            while (!is.na(ofname[j])) {
              fname <- ofname[j]
              xvalue <- transformed$field_data[fname, "transform"]
              if (!is.na(xvalue) && xvalue == "MapValues") {
                parents <- transformed$field_data[fname, "orig_field_name"][[1]]
                for (j in 1:length(parents))
                {
                  if (!(parents[j] %in% namelist)) {
                    namelist <- c(namelist, parents[j])
                  }
                }
                fname <- NA
                break
              }
              ofname[j] <- transformed$field_data[ofname[j], "orig_field_name"][[1]]
            }
            if (!(fname %in% namelist)) {
              namelist <- c(namelist, fname)
            }
          }
        }
      } else {
        if (length(grep("as\\.factor\\(", field$name[ii])) == 1) {
          fName <- gsub("as.factor\\((\\w*)\\)", "\\1", field$name[ii], perl = TRUE)
        } else {
          fName <- field$name[ii]
        }

        data.fields[[ii]] <- xmlNode("DataField", attrs = c(
          name = fName,
          optype = optypelist[[fName]],
          dataType = datypelist[[fName]]
        ))

        if (!(fName %in% namelist)) {
          namelist <- c(namelist, fName)
        }
      }
    }
  }

  # DataDictionary -> DataField -> Interval
  nmbr <- 1
  for (ndf2 in 1:length(namelist))
  {
    fname <- namelist[[ndf2]]
    if (optypelist[[fname]] == "continuous" && !is.null(dataset)) {
      interval <- xmlNode("Interval",
        attrs = c(
          closure = "closedClosed",
          leftMargin = min(dataset[[namelist[[ndf2]]]],
            na.rm = TRUE
          ), # 091025 Handle missing values
          rightMargin = max(dataset[[namelist[[nmbr]]]],
            na.rm = TRUE
          )
        )
      ) # 091025 Handle missing values
      data.fields[[ii]] <- append.XMLNode(data.fields[[ii]], interval)
    }

    # DataDictionary -> DataField -> Value

    if (optypelist[[fname]] == "categorical") {
      if (is.null(field$levels[[fname]]) && !is.null(transformed)) {
        lev <- levels(as.list(unique(transformed$data[fname]))[[1]])
        for (j in seq_along(lev))
        {
          data.fields[[ii]][[j]] <- xmlNode("Value",
            attrs = c(value = .markupSpecials(lev[j]))
          )
        }
      } else {
        for (j in seq_along(field$levels[[fname]]))
        {
          data.fields[[ii]][[j]] <- xmlNode("Value",
            attrs = c(value = .markupSpecials(field$levels[[fname]][j]))
          )
        }
      }
    }
  }

  if (!is.null(weights) && length(weights)) {
    data.dictionary <- append.XMLNode(data.dictionary, xmlNode("Extension",
      attrs = c(
        name = "Weights",
        value = weights,
        extender = "Rattle"
      )
    ))
  }

  data.fields[[ii + 1]] <- xmlNode("DataField", attrs = c(
    name = statusName,
    optype = "continuous", dataType = "double"
  ))
  data.fields[[ii + 2]] <- xmlNode("DataField", attrs = c(
    name = timeName,
    optype = "continuous", dataType = "double"
  ))
  data.fields[[ii + 3]] <- xmlNode("DataField", attrs = c(
    name = "cumulativeHazard",
    optype = "continuous", dataType = "double"
  ))

  data.dictionary <- xmlNode("DataDictionary",
    attrs = c(numberOfFields = length(namelist) + 3)
  )
  data.dictionary <- append.XMLNode(data.dictionary, data.fields)


  return(data.dictionary)
}
