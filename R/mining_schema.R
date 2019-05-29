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

.pmmlMiningSchema <- function(field, target = NULL, transformed = NULL,
                              missing_value_replacement = NULL,
                              invalidValueTreatment = "returnInvalid") {
  namelist <- .origFieldList(field, transformed, target)

  mining.schema <- xmlNode("MiningSchema")
  target <- .removeAsFactor(target)

  unknownVal <- NULL
  for (j in 1:length(namelist)) {
    if (!is.na(namelist[[j]])) {
      usage <- ifelse(namelist[[j]] == target, "predicted", "active")
      if ((!is.null(target)) && (namelist[[j]] != target)) {
        if (!is.null(missing_value_replacement)) {
          unknownVal <- missing_value_replacement
          invalidValueTreatment <- "asMissing"
        }
      } else if (is.null(target) && !is.null(missing_value_replacement)) {
        unknownVal <- missing_value_replacement
        invalidValueTreatment <- "asMissing"
      }
      if (namelist[[j]] == "Temp" || namelist[[j]] == "DiscretePlaceHolder") {
        # If field name is the naive bayes categorical field place holder, add missingValueReplacement.
        if (length(field$levels[[namelist[[j]]]]) == 1) {
          mf <- xmlNode("MiningField", attrs = c(
            name = namelist[[j]],
            usageType = usage, missingValueReplacement = field$levels[[namelist[[j]]]]
          ))
        }
      } else {
        mf <- xmlNode("MiningField", attrs = c(
          name = namelist[[j]], usageType = usage,
          missingValueReplacement = unknownVal, invalidValueTreatment = invalidValueTreatment
        ))
      }

      mining.schema <- append.XMLNode(mining.schema, mf)
    }
  }

  return(mining.schema)
}

.pmmlMiningSchemaRF <- function(field, target = NULL, inactive = NULL, transformed = NULL, missing_value_replacement = NULL) {
  # Generate the PMML for the MinimgSchema element.
  number.of.fields <- length(field$name)
  mining.fields <- list()
  unknownVal <- NULL
  invalidVal <- NULL
  namelist <- list()
  dnamelist <- list()
  nmbr <- 1
  for (i in 1:number.of.fields)
  {
    usage <- ifelse(field$name[i] == target, "predicted", "active")
    if (field$name[i] != target) {
      unknownVal <- missing_value_replacement
      invalidVal <- ifelse(is.null(missing_value_replacement), "asIs", "asMissing")
    }

    if (!is.null(transformed)) {
      if (transformed$field_data[field$name[i], "type"] == "original") {
        if (!(.removeAsFactor(field$name[i]) %in% namelist)) {
          namelist <- c(namelist, .removeAsFactor(field$name[i]))
          mining.fields[[nmbr]] <- xmlNode("MiningField", attrs = c(
            name = namelist[nmbr],
            usageType = usage, missingValueReplacement = unknownVal, invalidValueTreatment = invalidVal
          ))
          nmbr <- nmbr + 1
        }
      } else {
        ofnames <- strsplit(transformed$field_data[field$name[i], "orig_field_name"][[1]], ",")[[1]]
        for (j in 1:length(ofnames))
        {
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
            if (!(.removeAsFactor(fname) %in% dnamelist)) {
              dnamelist <- c(dnamelist, .removeAsFactor(field$name[i]))
            }
          }
        }
      }
    } else {
      fName <- .removeAsFactor(field$name[i])
      mining.fields[[i]] <- xmlNode("MiningField", attrs = c(
        name = fName,
        usageType = usage, missingValueReplacement = unknownVal, invalidValueTreatment = invalidVal
      ))
    }
  }
  mining.schema <- xmlNode("MiningSchema")
  mining.schema$children <- mining.fields
  return(mining.schema)
}

.pmmlMiningSchemaSurv <- function(field, timeName, statusName, target = NULL, inactive = NULL, transformed = NULL, missing_value_replacement = NULL) {
  # Generate the PMML for the MinimgSchema element for a survival model.
  # A survival forest has an output not usually in the input field names.
  # Add an extra mining field of type predicted.

  # Currently we only include the name and usageType
  # attributes. We could also include relative importance (like a
  # correlation between 0 and 1), invalidValueTreatment (returnInvalid
  # to return a value indicating an invalid result; asis to return a
  # value without modification; asMissing to treat it as a missing
  # value and return the missingValueReplacement value instead),
  # missingValueReplacement, and outliers (asis, asMisingValues,
  # asExtremeValues).

  # Add inactive to list (as supplementary) those variables
  # that should be marked as inactive in the model. This was added so
  # that singularities can be identified as inactive for a linear
  # model. It could also be used to capture ignored variables, if they
  # were to ever be included in the variable list.

  namelist <- NULL
  number.of.fields <- length(field$name)
  mining.fields <- list()
  targetExists <- 0
  ii <- 0
  unknownVal <- NULL
  for (i in 1:number.of.fields)
  {
    if (length(grep(":", field$name[i])) == 1) {
    } else {
      ii <- ii + 1
      if (is.null(target)) {
        usage <- "active"
      } else {
        usage <- ifelse(field$name[i] == target, "predicted", "active")
      }

      if (usage != "predicted") {
        unknownVal <- missing_value_replacement
      }

      if (usage == "predicted") {
        targetExists <- 1
      }

      # Find out which variables should be marked as inactive.

      if (field$name[i] %in% inactive) usage <- "supplementary"

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
        nmbr <- 1
        for (ndf2 in 1:length(namelist))
        {
          if (!is.na(namelist[ndf2])) {
            mining.fields[[nmbr]] <- xmlNode("MiningField", attrs = c(
              name = namelist[ndf2],
              usageType = usage, missingValueReplacement = unknownVal,
              invalidValueTreatment = "asMissing"
            ))
            nmbr <- nmbr + 1
          }
        }
      } else {
        fName <- .removeAsFactor(field$name[i])

        mining.fields[[i]] <- xmlNode("MiningField",
          attrs = c(
            name = fName,
            usageType = usage, missingValueReplacement = unknownVal,
            invalidValueTreatment = "asMissing"
          )
        )
      }
    }
  }
  # Add a predicted mining field if none exist.
  if (targetExists == 0) {
    mining.fields[[ii + 1]] <- xmlNode("MiningField",
      attrs = c(
        name = statusName,
        usageType = "active"
      )
    )
  }
  mining.fields[[ii + 2]] <- xmlNode("MiningField",
    attrs = c(
      name = timeName,
      usageType = "active"
    )
  )
  mining.fields[[ii + 3]] <- xmlNode("MiningField",
    attrs = c(
      name = "cumulativeHazard",
      usageType = "predicted"
    )
  )
  mining.schema <- xmlNode("MiningSchema")
  mining.schema$children <- mining.fields


  return(mining.schema)
}


.origFieldList <- function(field, transformed = NULL, target = NULL) {
  # Create a list of original field names from which any input fields may be derived.
  number.of.fields <- length(field$name)
  mining.fields <- list()

  if (field$name[1] == "ZementisClusterIDPlaceHolder" || field$name[1] == "ZementisHiddenTargetField") {
    begin <- 2
  } else {
    begin <- 1
  }

  DPL1 <- "DiscretePlaceHolder"
  DPL2 <- "Temp"
  DPL3 <- "predictedScore"

  namelist <- list()
  dnamelist <- list()
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
      }
      else {
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
            if (!(.removeAsFactor(fname) %in% dnamelist)) {
              dnamelist <- c(dnamelist, .removeAsFactor(field$name[i]))
            }
          }
        }
      }
    }
  } else {
    for (i in begin:number.of.fields) {
      fName <- .removeAsFactor(field$name[i])
      if (!(fName %in% namelist) && fName != "ZementisClusterIDPlaceHolder") {
        namelist <- c(namelist, fName)
      }
    }
  }

  return(namelist)
}
