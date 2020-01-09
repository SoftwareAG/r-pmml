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

.removeAsFactor <- function(fieldName) {
  if (length(grep("as\\.factor\\(", fieldName)) == 1) {
    fieldName <- gsub("as.factor\\((\\w*)\\)", "\\1", fieldName, perl = TRUE)
  }
  return(fieldName)
}


.getNamespace <- function(x) {
  return(paste("http://www.dmg.org/PMML-", x, sep = ""))
}



.markupSpecials <- function(x) {
  gsub("<", "&lt;", gsub(">", "&gt;", gsub("&", "&amp;", x)))
}


.generateCopyright <- function() {
  return(paste("Copyright (c)", format(Sys.time(), "%Y"), Sys.info()["user"]))
}


.sdecimal2binary <- function(x) {
  return(rev(.sdecimal2binary.smallEndian(x)))
}


.sdecimal2binary.smallEndian <- function(x) {
  if (x == 0) {
    return(0)
  }
  if (x < 0) stop("The input must be positive.")
  dec <- x

  n <- floor(log(x) / log(2))
  bin <- c(1)
  dec <- dec - 2^n

  while (n > 0) {
    if (dec >= 2^(n - 1)) {
      bin <- c(bin, 1)
      dec <- dec - 2^(n - 1)
    }
    else {
      bin <- c(bin, 0)
    }
    n <- n - 1
  }
  return(bin)
}


.pmmlRootNode <- function(version = "4.4") {
  if (version == "4.4") {
    node <- xmlNode("PMML",
      attrs = c(
        version = "4.4",
        xmlns = "http://www.dmg.org/PMML-4_4",
        "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
        "xsi:schemaLocation" = paste(
          "http://www.dmg.org/PMML-4_4",
          "http://www.dmg.org/pmml/v4-4/pmml-4-4.xsd"
        )
      )
    )
  } else if (version == "4.3") {
    node <- xmlNode("PMML",
      attrs = c(
        version = "4.3",
        xmlns = "http://www.dmg.org/PMML-4_3",
        "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
        "xsi:schemaLocation" = paste(
          "http://www.dmg.org/PMML-4_3",
          "http://www.dmg.org/pmml/v4-3/pmml-4-3.xsd"
        )
      )
    )
  } else if (version == "4.3Ext") {
    node <- xmlNode("PMML",
      attrs = c(
        version = "4.3Ext",
        xmlns = "http://www.dmg.org/PMML-4_3"
      )
    )
  } else if (version == "4.2") {
    node <- xmlNode("PMML",
      attrs = c(
        version = "4.2",
        xmlns = "http://www.dmg.org/PMML-4_2",
        "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
        "xsi:schemaLocation" = paste(
          "http://www.dmg.org/PMML-4_2",
          "http://www.dmg.org/v4-2/pmml-4-2.xsd"
        )
      )
    )
  } else if (version == "4.1") {
    node <- xmlNode("PMML",
      attrs = c(
        version = "4.1",
        xmlns = "http://www.dmg.org/PMML-4_1",
        "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
        "xsi:schemaLocation" = paste(
          "http://www.dmg.org/PMML-4_1",
          "http://www.dmg.org/v4-1/pmml-4-1.xsd"
        )
      )
    )
  } else {
    node <- xmlNode("PMML",
      attrs = c(
        version = "4.0",
        xmlns = "http://www.dmg.org/PMML-4_0",
        "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
        "xsi:schemaLocation" = paste(
          "http://www.dmg.org/PMML-4_0",
          "http://www.dmg.org/v4-0/pmml-4-0.xsd"
        )
      )
    )
  }

  return(node)
}



.pmmlHeader <- function(description, copyright, app_name) {
  if (is.null(copyright)) copyright <- .generateCopyright()

  # Header Node
  header <- xmlNode("Header", attrs = c(copyright = copyright, description = description))

  # Header -> Extension for user info
  header <- append.XMLNode(
    header,
    xmlNode("Extension",
      attrs = c(
        name = "user",
        value = sprintf("%s", Sys.info()["user"]),
        extender = app_name
      )
    )
  )

  # Header -> Application
  header <- append.XMLNode(header, xmlNode("Application",
    attrs = c(
      name = app_name,
      version = toString(packageVersion("pmml"))
    )
  ))

  # Header -> Timestamp
  header <- append.XMLNode(header, xmlNode("Timestamp", sprintf("%s", Sys.time())))

  return(header)
}


.pmmlLocalTransformations <- function(field, transforms = NULL, LTelement = NULL, target = NULL, ...) {
  # Generate and return a LocalTransformations element that incldues
  # each supplied field.
  #
  # field$name is a vector of strings, and includes target
  # field$class is indexed by fields$name

  dots <- list(...)
  if (!is.null(dots$transformationDictionary)) {
    transformation.dictionary <- xmlNode("TransformationDictionary")
  }

  # LocalTransformations
  if (is.null(LTelement)) {
    local.transformations <- xmlNode("LocalTransformations")
  }

  if (is.null(target)) {
    target <- field$name[1]
  }

  if (!is.null(transforms)) {
    inputs <- transforms$field_data

    targetDL <- NULL
    targetDL <- c(targetDL, target)

    # Output all fields, possibly to allow user to output any derived fields via OutputField element.
    for (i in 1:nrow(inputs))
    {
      if (inputs[i, "orig_field_name"] %in% targetDL) {
        targetDL <- c(targetDL, rownames(inputs)[i])
        if (rownames(inputs)[i] %in% field$name[-1]) {
          stop("Target variable and derivations are not allowed to be used as input variables.")
        }
      } else {
        fname <- rownames(inputs)[i]

        if (inputs[fname, "type"] == "derived" && fname != target) {
          if (inputs[fname, "transform"] == "zxform") {
            origName <- inputs[fname, "orig_field_name"]
            missing <- inputs[fname, "missingValue"]
            dfNode <- xmlNode("DerivedField", attrs = c(name = fname, dataType = "double", optype = "continuous"))
            if (!is.na(missing)) {
              ncNode <- xmlNode("NormContinuous", attrs = c(mapMissingTo = missing, field = origName))
            } else {
              ncNode <- xmlNode("NormContinuous", attrs = c(field = origName))
            }

            o1 <- as.numeric(inputs[fname, "centers"])
            o2 <- as.numeric(inputs[fname, "centers"]) + as.numeric(inputs[fname, "scales"])
            lnNode1 <- xmlNode("LinearNorm", attrs = c(orig = o1, norm = "0"))
            lnNode2 <- xmlNode("LinearNorm", attrs = c(orig = o2, norm = "1"))

            ncNode <- append.XMLNode(ncNode, lnNode1)
            ncNode <- append.XMLNode(ncNode, lnNode2)
            dfNode <- append.XMLNode(dfNode, ncNode)
          } else if (inputs[fname, "transform"] == "minmax") {
            origName <- inputs[fname, "orig_field_name"]
            missing <- inputs[fname, "missingValue"]
            dfNode <- xmlNode("DerivedField", attrs = c(name = fname, dataType = "double", optype = "continuous"))
            if (!is.na(missing)) {
              ncNode <- xmlNode("NormContinuous", attrs = c(mapMissingTo = missing, field = origName))
            } else {
              ncNode <- xmlNode("NormContinuous", attrs = c(field = origName))
            }

            o1 <- inputs[fname, "sampleMin"]
            n1 <- inputs[fname, "xformedMin"]
            o2 <- inputs[fname, "sampleMax"]
            n2 <- inputs[fname, "xformedMax"]
            lnNode1 <- xmlNode("LinearNorm", attrs = c(orig = o1, norm = n1))
            lnNode2 <- xmlNode("LinearNorm", attrs = c(orig = o2, norm = n2))
            ncNode <- append.XMLNode(ncNode, lnNode1)
            ncNode <- append.XMLNode(ncNode, lnNode2)
            dfNode <- append.XMLNode(dfNode, ncNode)
          } else if (inputs[fname, "transform"] == "MapValues") {
            map <- inputs[fname, "fieldsMap"][[1]]

            dtype <- map[2, ncol(map)]
            if (dtype == "numeric") {
              dtype <- "double"
              otype <- "continuous"
            } else if (dtype == "boolean") {
              dtype <- "boolean"
              otype <- "categorical"
            } else {
              dtype <- "string"
              otype <- "categorical"
            }

            dfNode <- xmlNode("DerivedField", attrs = c(name = fname, dataType = as.character(dtype), optype = otype))
            default <- inputs[fname, "default"]
            missing <- inputs[fname, "missingValue"]
            if (dtype == "boolean") {
              if ((default == 1) || (toupper(default) == TRUE)) {
                default <- "true"
              } else {
                default <- "false"
              }
              if ((missing == 1) || (toupper(missing) == TRUE)) {
                missing <- "true"
              } else {
                missing <- "false"
              }
            }

            if (!is.na(default) && !is.na(missing)) {
              mapvNode <- xmlNode("MapValues", attrs = c(mapMissingTo = missing, defaultValue = default, outputColumn = "output"))
            } else if (!is.na(default) && is.na(missing)) {
              mapvNode <- xmlNode("MapValues", attrs = c(defaultValue = default, outputColumn = "output"))
            } else if (is.na(default) && !is.na(missing)) {
              mapvNode <- xmlNode("MapValues", attrs = c(mapMissingTo = missing, outputColumn = "output"))
            } else {
              mapvNode <- xmlNode("MapValues", attrs = c(outputColumn = "out"))
            }

            for (j in 1:(ncol(map) - 1))
            {
              colname <- paste("input", j, sep = "")
              val <- as.character(map[1, j])
              fcpNode <- xmlNode("FieldColumnPair", attrs = c(field = val, column = colname))
              mapvNode <- append.XMLNode(mapvNode, fcpNode)
            }

            inline <- xmlNode("InlineTable")
            for (j in 3:nrow(map))
            {
              row <- xmlNode("row")
              for (k in 1:(ncol(map) - 1))
              {
                initNode <- xmlNode(paste("input", k, sep = ""), value = as.character(map[j, k]))
                row <- append.XMLNode(row, initNode)
              }
              out <- xmlNode("output", value = as.character(map[j, ncol(map)]))
              row <- append.XMLNode(row, out)
              inline <- append.XMLNode(inline, row)
            }

            mapvNode <- append.XMLNode(mapvNode, inline)
            dfNode <- append.XMLNode(dfNode, mapvNode)
          } else if (inputs[fname, "transform"] == "NormDiscrete") {
            map <- inputs[fname, "fieldsMap"][[1]]
            dfName <- row.names(inputs)[i]
            missing <- inputs[fname, "missingValue"]

            dfNode <- xmlNode("DerivedField", attrs = c(name = dfName, dataType = "double", optype = "continuous"))
            if (!is.na(missing)) {
              normNode <- xmlNode("NormDiscrete", attrs = c(field = as.character(inputs[fname, "orig_field_name"]), value = as.character(map[1]), mapMissingTo = missing))
            } else {
              normNode <- xmlNode("NormDiscrete", attrs = c(field = as.character(inputs[fname, "orig_field_name"]), value = as.character(map[1])))
            }
            dfNode <- append.XMLNode(dfNode, normNode)
          } else if (inputs[fname, "transform"] == "discretize") {
            maps <- inputs[fname, "fieldsMap"][[1]]
            missingVal <- inputs[fname, "missingValue"]
            defVal <- inputs[fname, "default"]

            origName <- as.character(inputs[fname, "orig_field_name"])
            map <- maps[c(-1, -2), ]
            dtype <- as.character(inputs[fname, "dataType"])
            if ((dtype == "numeric") || (dtype == "integer") || (dtype == "double")) {
              dtype <- "double"
              otype <- "continuous"
            }
            else {
              dtype <- "string"
              otype <- "categorical"
            }

            if (dtype == "boolean") {
              if ((default == 1) || (toupper(default) == TRUE)) {
                default <- "true"
              } else {
                default <- "false"
              }
              if ((missing == 1) || (toupper(missing) == TRUE)) {
                missing <- "true"
              } else {
                missing <- "false"
              }
            }

            dfNode <- xmlNode("DerivedField", attrs = c(name = fname, dataType = dtype, optype = otype))
            if (!is.na(defVal) && !is.na(missingVal)) {
              discNode <- xmlNode("Discretize", attrs = c(field = origName, mapMissingTo = missingVal, defaultValue = defVal))
            } else if (!is.na(defVal) && is.na(missingVal)) {
              discNode <- xmlNode("Discretize", attrs = c(field = origName, defaultValue = defVal))
            } else if (is.na(defVal) && !is.na(missingVal)) {
              discNode <- xmlNode("Discretize", attrs = c(field = origName, mapMissingTo = missingVal))
            } else {
              discNode <- xmlNode("Discretize", attrs = c(field = origName))
            }

            for (i in 1:nrow(map))
            {
              dbinNode <- xmlNode("DiscretizeBin", attrs = c(binValue = as.character(map[i, 2])))

              clsr <- as.character(paste(map[i, 3], map[i, 5], sep = ""))

              if (!is.na(map[i, 4])) {
                if (!is.na(map[i, 6])) {
                  intrNode <- xmlNode("Interval", attrs = c(closure = clsr, leftMargin = as.character(map[i, 4]), rightMargin = as.character(map[i, 6])))
                } else {
                  intrNode <- xmlNode("Interval", attrs = c(closure = clsr, leftMargin = as.character(map[i, 4])))
                }
              } else {
                intrNode <- xmlNode("Interval", attrs = c(closure = clsr, rightMargin = as.character(map[i, 6])))
              }
              dbinNode <- append.XMLNode(dbinNode, intrNode)
              discNode <- append.XMLNode(discNode, dbinNode)
            }
            dfNode <- append.XMLNode(dfNode, discNode)
          } else if (!is.na(inputs[fname, "xform_function"])) {
            origName <- inputs[fname, "orig_field_name"]
            missing <- inputs[fname, "missingValue"]

            dfNode <- xmlNode("DerivedField", attrs = c(name = fname, dataType = "double", optype = "continuous"))

            funcNode <- .pmmlU(inputs[fname, "xform_function"])

            dfNode <- append.XMLNode(dfNode, funcNode)
          }

          if (!is.null(dots$transformationDictionary)) {
            transformation.dictionary <- append.XMLNode(transformation.dictionary, dfNode)
          } else {
            if (is.null(LTelement)) {
              local.transformations <- append.XMLNode(local.transformations, dfNode)
            } else {
              LTelement <- append.XMLNode(LTelement, dfNode)
            }
          }
        }
      }
    }
  }

  if (!is.null(dots$transformationDictionary)) {
    return(transformation.dictionary)
  } else {
    if (is.null(LTelement)) {
      return(local.transformations)
    } else {
      return(LTelement)
    }
  }
}



.pmmlLocalTransformationsAD <- function(field, transforms = NULL, LTelement = NULL, target = NULL, ...) {
  # Local transformations for anomaly detection models. For OCSVM, target is NULL and should not
  # be used in comparisons.
  # field$name is a vector of strings. Target is assumed to be NULL for anomaly detection.
  # field$class is indexed by fields$name.

  dots <- list(...)
  if (!is.null(dots$transformationDictionary)) {
    transformation.dictionary <- xmlNode("TransformationDictionary")
  }

  # LocalTransformations
  if (is.null(LTelement)) {
    local.transformations <- xmlNode("LocalTransformations")
  }

  if (!is.null(transforms)) {
    inputs <- transforms$field_data

    # list of all fields derived from the target field
    targetDL <- NULL
    targetDL <- c(targetDL, target)

    # code to output all fields, possibly to allow user to output any derived fields via OutputField element
    for (i in 1:nrow(inputs)) {
      if (inputs[i, "orig_field_name"] %in% targetDL) {
        targetDL <- c(targetDL, rownames(inputs)[i])
        if (rownames(inputs)[i] %in% field$name[-1]) {
          stop("Target variable and derivations are not allowed to be used as input variables.")
        }
      } else {
        fname <- rownames(inputs)[i]

        if (inputs[fname, "type"] == "derived") {
          if (inputs[fname, "transform"] == "zxform") {
            origName <- inputs[fname, "orig_field_name"]
            missing <- inputs[fname, "missingValue"]
            dfNode <- xmlNode("DerivedField", attrs = c(name = fname, dataType = "double", optype = "continuous"))
            if (!is.na(missing)) {
              ncNode <- xmlNode("NormContinuous", attrs = c(mapMissingTo = missing, field = origName))
            } else {
              ncNode <- xmlNode("NormContinuous", attrs = c(field = origName))
            }

            o1 <- as.numeric(inputs[fname, "centers"])
            o2 <- as.numeric(inputs[fname, "centers"]) + as.numeric(inputs[fname, "scales"])
            lnNode1 <- xmlNode("LinearNorm", attrs = c(orig = o1, norm = "0"))
            lnNode2 <- xmlNode("LinearNorm", attrs = c(orig = o2, norm = "1"))

            ncNode <- append.XMLNode(ncNode, lnNode1)
            ncNode <- append.XMLNode(ncNode, lnNode2)
            dfNode <- append.XMLNode(dfNode, ncNode)
          } else if (inputs[fname, "transform"] == "minmax") {
            origName <- inputs[fname, "orig_field_name"]
            missing <- inputs[fname, "missingValue"]
            dfNode <- xmlNode("DerivedField", attrs = c(name = fname, dataType = "double", optype = "continuous"))
            if (!is.na(missing)) {
              ncNode <- xmlNode("NormContinuous", attrs = c(mapMissingTo = missing, field = origName))
            } else {
              ncNode <- xmlNode("NormContinuous", attrs = c(field = origName))
            }

            o1 <- inputs[fname, "sampleMin"]
            n1 <- inputs[fname, "xformedMin"]
            o2 <- inputs[fname, "sampleMax"]
            n2 <- inputs[fname, "xformedMax"]
            lnNode1 <- xmlNode("LinearNorm", attrs = c(orig = o1, norm = n1))
            lnNode2 <- xmlNode("LinearNorm", attrs = c(orig = o2, norm = n2))
            ncNode <- append.XMLNode(ncNode, lnNode1)
            ncNode <- append.XMLNode(ncNode, lnNode2)
            dfNode <- append.XMLNode(dfNode, ncNode)
            #       local.transformations <- append.XMLNode(local.transformations, dfNode)
          } else if (inputs[fname, "transform"] == "MapValues") {
            map <- inputs[fname, "fieldsMap"][[1]]

            dtype <- map[2, ncol(map)]
            if (dtype == "numeric") {
              dtype <- "double"
              otype <- "continuous"
            } else if (dtype == "boolean") {
              dtype <- "boolean"
              otype <- "categorical"
            } else {
              dtype <- "string"
              otype <- "categorical"
            }

            dfNode <- xmlNode("DerivedField", attrs = c(name = fname, dataType = as.character(dtype), optype = otype))
            default <- inputs[fname, "default"]
            missing <- inputs[fname, "missingValue"]
            if (dtype == "boolean") {
              if ((default == 1) || (toupper(default) == TRUE)) {
                default <- "true"
              } else {
                default <- "false"
              }
              if ((missing == 1) || (toupper(missing) == TRUE)) {
                missing <- "true"
              } else {
                missing <- "false"
              }
            }

            if (!is.na(default) && !is.na(missing)) {
              mapvNode <- xmlNode("MapValues", attrs = c(mapMissingTo = missing, defaultValue = default, outputColumn = "output"))
            } else if (!is.na(default) && is.na(missing)) {
              mapvNode <- xmlNode("MapValues", attrs = c(defaultValue = default, outputColumn = "output"))
            } else if (is.na(default) && !is.na(missing)) {
              mapvNode <- xmlNode("MapValues", attrs = c(mapMissingTo = missing, outputColumn = "output"))
            } else {
              mapvNode <- xmlNode("MapValues", attrs = c(outputColumn = "out"))
            }

            for (j in 1:(ncol(map) - 1))
            {
              colname <- paste("input", j, sep = "")
              val <- as.character(map[1, j])
              fcpNode <- xmlNode("FieldColumnPair", attrs = c(field = val, column = colname))
              mapvNode <- append.XMLNode(mapvNode, fcpNode)
            }

            inline <- xmlNode("InlineTable")
            for (j in 3:nrow(map))
            {
              row <- xmlNode("row")
              for (k in 1:(ncol(map) - 1))
              {
                initNode <- xmlNode(paste("input", k, sep = ""), value = as.character(map[j, k]))
                row <- append.XMLNode(row, initNode)
              }
              out <- xmlNode("output", value = as.character(map[j, ncol(map)]))
              row <- append.XMLNode(row, out)
              inline <- append.XMLNode(inline, row)
            }

            mapvNode <- append.XMLNode(mapvNode, inline)
            dfNode <- append.XMLNode(dfNode, mapvNode)
          } else if (inputs[fname, "transform"] == "NormDiscrete") {
            map <- inputs[fname, "fieldsMap"][[1]]
            dfName <- row.names(inputs)[i]
            missing <- inputs[fname, "missingValue"]

            dfNode <- xmlNode("DerivedField", attrs = c(name = dfName, dataType = "double", optype = "continuous"))
            if (!is.na(missing)) {
              normNode <- xmlNode("NormDiscrete", attrs = c(field = as.character(inputs[fname, "orig_field_name"]), value = as.character(map[1]), mapMissingTo = missing))
            } else {
              normNode <- xmlNode("NormDiscrete", attrs = c(field = as.character(inputs[fname, "orig_field_name"]), value = as.character(map[1])))
            }
            dfNode <- append.XMLNode(dfNode, normNode)
          } else if (inputs[fname, "transform"] == "discretize") {
            maps <- inputs[fname, "fieldsMap"][[1]]
            missingVal <- inputs[fname, "missingValue"]
            defVal <- inputs[fname, "default"]

            origName <- as.character(inputs[fname, "orig_field_name"])
            map <- maps[c(-1, -2), ]
            dtype <- as.character(inputs[fname, "dataType"])
            if ((dtype == "numeric") || (dtype == "integer") || (dtype == "double")) {
              dtype <- "double"
              otype <- "continuous"
            }

            else {
              dtype <- "string"
              otype <- "categorical"
            }

            if (dtype == "boolean") {
              if ((default == 1) || (toupper(default) == TRUE)) {
                default <- "true"
              } else {
                default <- "false"
              }
              if ((missing == 1) || (toupper(missing) == TRUE)) {
                missing <- "true"
              } else {
                missing <- "false"
              }
            }

            dfNode <- xmlNode("DerivedField", attrs = c(name = fname, dataType = dtype, optype = otype))
            if (!is.na(defVal) && !is.na(missingVal)) {
              discNode <- xmlNode("Discretize", attrs = c(field = origName, mapMissingTo = missingVal, defaultValue = defVal))
            } else if (!is.na(defVal) && is.na(missingVal)) {
              discNode <- xmlNode("Discretize", attrs = c(field = origName, defaultValue = defVal))
            } else if (is.na(defVal) && !is.na(missingVal)) {
              discNode <- xmlNode("Discretize", attrs = c(field = origName, mapMissingTo = missingVal))
            } else {
              discNode <- xmlNode("Discretize", attrs = c(field = origName))
            }

            for (i in 1:nrow(map))
            {
              dbinNode <- xmlNode("DiscretizeBin", attrs = c(binValue = as.character(map[i, 2])))

              clsr <- as.character(paste(map[i, 3], map[i, 5], sep = ""))

              if (!is.na(map[i, 4])) {
                if (!is.na(map[i, 6])) {
                  intrNode <- xmlNode("Interval", attrs = c(closure = clsr, leftMargin = as.character(map[i, 4]), rightMargin = as.character(map[i, 6])))
                } else {
                  intrNode <- xmlNode("Interval", attrs = c(closure = clsr, leftMargin = as.character(map[i, 4])))
                }
              } else {
                intrNode <- xmlNode("Interval", attrs = c(closure = clsr, rightMargin = as.character(map[i, 6])))
              }
              dbinNode <- append.XMLNode(dbinNode, intrNode)
              discNode <- append.XMLNode(discNode, dbinNode)
            }
            dfNode <- append.XMLNode(dfNode, discNode)
          } else if (!is.na(inputs[fname, "xform_function"])) {
            origName <- inputs[fname, "orig_field_name"]
            missing <- inputs[fname, "missingValue"]

            dfNode <- xmlNode("DerivedField", attrs = c(name = fname, dataType = "double", optype = "continuous"))

            funcNode <- .pmmlU(inputs[fname, "xform_function"])

            dfNode <- append.XMLNode(dfNode, funcNode)
          }

          if (!is.null(dots$transformationDictionary)) {
            transformation.dictionary <- append.XMLNode(transformation.dictionary, dfNode)
          } else {
            if (is.null(LTelement)) {
              local.transformations <- append.XMLNode(local.transformations, dfNode)
            } else {
              LTelement <- append.XMLNode(LTelement, dfNode)
            }
          }
        }
      }
    }
  }

  if (!is.null(dots$transformationDictionary)) {
    return(transformation.dictionary)
  } else {
    if (is.null(LTelement)) {
      return(local.transformations)
    } else {
      return(LTelement)
    }
  }
}


.pmmlOutput <- function(field, target = NULL, optype = NULL) {
  # Creates PMML Output element.
  number.of.fields <- length(field$name)

  output <- xmlNode("Output")
  output.fields <- list()

  for (i in 1:number.of.fields)
  {
    if (field$name[i] == target) {
      targetout <- target
      if (length(grep("as\\.factor\\(", targetout)) == 1) {
        targetout <- gsub("as.factor\\((\\w*)\\)", "\\1", targetout, perl = TRUE)
      }

      if (is.null(optype)) {
        r_class <- unname(field$class[field$name[i]])
        new_optype <- ifelse(r_class == "factor", "categorical", "continuous")
        new_dataType <- ifelse(r_class == "factor", "string", "double")
        output.fields[[1]] <- xmlNode("OutputField",
          attrs = c(
            name = gsub(" ", "", paste("Predicted_", targetout)),
            optype = new_optype, dataType = new_dataType,
            feature = "predictedValue"
          )
        )
      } else {
        output.fields[[1]] <- xmlNode("OutputField",
          attrs = c(
            name = gsub(" ", "", paste("Predicted_", targetout)),
            optype = optype,
            dataType = ifelse(optype == "continuous",
              "double", "string"
            ),
            feature = "predictedValue"
          )
        )
      }


      for (j in seq_along(field$levels[[field$name[i]]])) {
        output.fields[[j + 1]] <- xmlNode("OutputField",
          attrs = c(
            name = paste("Probability_",
              field$levels[[field$name[i]]][j],
              sep = ""
            ),
            optype = "continuous",
            dataType = "double",
            feature = "probability",
            value = field$levels[[field$name[i]]][j]
          )
        )
      }
    }
  }

  output$children <- output.fields
  return(output)
}


.addXML <- function(x) {
  if (is.numeric(x)) {
    node <- newXMLNode(name = "Constant", attrs = c("dataType" = toString(typeof(x))), text = x)
  } else if (is.logical(x)) {
    node <- newXMLNode(name = "Constant", attrs = c("dataType" = "boolean"), text = x)
  } else if (is.character(x)) {
    node <- newXMLNode(name = "Constant", attrs = c("dataType" = "string"), text = x)
  } else {
    node <- newXMLNode(name = "FieldRef", attrs = c("field" = toString(x)))
  }
  return(node)
}


.addXMLfunc <- function(x) {
  # addXMLfunc is used only when x is assumed to be a function

  op_list <- c("+", "-", "/", "*", "pow", "lessThan", "lessOrEqual", "greaterThan", "greaterOrEqual", "and", "and", "or", "or", "equal", "notEqual", "not", "ceil", "product", "ln")
  names(op_list) <- c("+", "-", "/", "*", "^", "<", "<=", ">", ">=", "&&", "&", "|", "||", "==", "!=", "!", "ceiling", "prod", "log")


  if (toString(x) %in% names(op_list)) {
    node <- newXMLNode(name = "Apply", attrs = c("function" = op_list[toString(x)][[1]]))
  } else {
    node <- newXMLNode(name = "Apply", attrs = c("function" = toString(x)[[1]]))
  }
  return(node)
}


.trans <- function(x) {
  if (length(x) >= 3) {
    # If length>3, then assume a function with multiple arguments.
    # Assume that first element is the function, and all others are arguments.

    node <- .addXMLfunc(x[[1]]) # first element assumed to be an operator or function

    for (k in 2:length(x)) { # starting from second element
      if (length(x[[k]]) == 1) {
        c1 <- .addXML(x[[k]])
      } else {
        c1 <- .trans(as.list(x[[k]]))
      }
      addChildren(node, c1)
    }
  } else if (length(x) == 2) {


    # If grouping with parentheses:
    if (x[[1]] == "(" || x[[1]] == "{") {
      node <- .trans(as.list(x[[2]])) # do not assign ( as a node
    } else if (x[[1]] == "+" || x[[1]] == "-") {
      # If first element is "-" or "+", then it belongs to the number or grouping and is not a function.
      new_list <- append(x, values = 0, after = 1) # insert a "0" after "-"
      node <- .trans(as.list(new_list))
    } else {
      # If length is 2 and first element is not "(" and is not "-" or "+" that belongs to the number,
      # then it must be a function.
      node <- .addXMLfunc(x[[1]]) # first element is a function

      if (length(x[[2]]) == 1) { # check if second element is a grouping
        c1 <- .addXML(x[[2]])
      } else {
        c1 <- .trans(as.list(x[[2]]))
      }
      addChildren(node, c1)
    }
  } else if (length(x) == 1) {
    node <- .addXML(x[[1]]) # Always assumes a field or constant.
  }

  return(node)
}


.pmmlU <- function(expr0) {
  expr <- parse(text = expr0)


  sub_expr <- substitute(z, list(z = expr))[[1]]
  list_expr <- as.list(sub_expr)

  trans_expr <- .trans(list_expr)

  return(trans_expr)
}

.init_wrap_params <- function(inbox) {
  xformedMin <- NULL
  xformedMax <- NULL
  sampleMin <- NULL
  sampleMax <- NULL
  centers <- NULL
  scales <- NULL
  fieldsMap <- NULL
  transform <- NULL
  default <- NULL
  missingValue <- NULL
  xform_function <- NULL

  if (is.null(inbox$field_data[1, "xformedMax"])) {
    for (i in 1:nrow(inbox$field_data))
    {
      xformedMin <- c(xformedMin, NA)
      xformedMax <- c(xformedMax, NA)
      sampleMin <- c(sampleMin, NA)
      sampleMax <- c(sampleMax, NA)
    }
    inbox$field_data <- cbind(inbox$field_data, sampleMin)
    inbox$field_data <- cbind(inbox$field_data, sampleMax)
    inbox$field_data <- cbind(inbox$field_data, xformedMin)
    inbox$field_data <- cbind(inbox$field_data, xformedMax)
  }

  if (is.null(inbox$field_data[1, "centers"])) {
    for (i in 1:nrow(inbox$field_data))
    {
      centers <- c(centers, NA)
      scales <- c(scales, NA)
    }
    inbox$field_data <- cbind(inbox$field_data, centers)
    inbox$field_data <- cbind(inbox$field_data, scales)
  }

  if (is.null(inbox$field_data[1, "fieldsMap"])) {
    for (i in 1:nrow(inbox$field_data))
    {
      fieldsMap <- c(fieldsMap, NA)
    }
    inbox$field_data <- cbind(inbox$field_data, fieldsMap)
  }

  if (is.null(inbox$field_data[1, "transform"])) {
    for (i in 1:nrow(inbox$field_data))
    {
      transform <- c(transform, NA)
    }
    inbox$field_data <- cbind(inbox$field_data, transform)
  }

  if (is.null(inbox$field_data[1, "default"])) {
    for (i in 1:nrow(inbox$field_data))
    {
      default <- c(default, NA)
      missingValue <- c(missingValue, NA)
    }
    inbox$field_data <- cbind(inbox$field_data, default)
    inbox$field_data <- cbind(inbox$field_data, missingValue)
  }

  if (is.null(inbox$field_data[1, "xform_function"])) {
    for (i in 1:nrow(inbox$field_data))
    {
      xform_function <- c(xform_function, NA)
    }
    inbox$field_data <- cbind(inbox$field_data, xform_function)
  }

  return(inbox)
}



# deprecate_arg <- function(old_arg, new_arg) {
#   # deprecate an argument in a function
#
#   if (!missing(old_arg)) {
#     warning_string <- paste0("argument ", old_arg, " is deprecated; please use ", new_arg, " instead.")
#     warning(warning_string, call. = FALSE)
#   }
#   new_arg <- old_arg
# }
