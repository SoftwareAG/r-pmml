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

#' Generate the PMML representation for a kmeans object from
#' the package \pkg{stats}.
#'
#' The kmeans object (a cluster described by k centroids) is
#' converted into a PMML representation.
#'
#' A kmeans object is obtained by applying the \code{kmeans} function from the
#' \code{stats} package. This method typically requires the user to normalize
#' all the variables; these operations can be done using transforms so that the
#' normalization information is included in PMML.
#'
#' @param model A kmeans object.
#' @param missing_value_replacement Value to be used as the 'missingValueReplacement'
#' attribute for all MiningFields.
#' @param algorithm.name The variety of kmeans used.
#'
#' @inheritParams pmml
#'
#' @author Graham Williams
#'
#' @references
#' \href{http://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html}{R
#' project: K-Means Clustering}
#'
#' @examples
#'
#' ds <- rbind(
#'   matrix(rnorm(100, sd = 0.3), ncol = 2),
#'   matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2)
#' )
#' colnames(ds) <- c("Dimension1", "Dimension2")
#' cl <- kmeans(ds, 2)
#' pmml(cl)
#' 
#' @export pmml.kmeans
#' @export
pmml.kmeans <- function(model,
                        model_name = "KMeans_Model",
                        app_name = "SoftwareAG PMML Generator",
                        description = "KMeans cluster model",
                        copyright = NULL,
                        transforms = NULL,
                        missing_value_replacement = NULL,
                        algorithm.name = "KMeans: Hartigan and Wong", ...) {
  if (!inherits(model, "kmeans")) stop("Not a legitimate kmeans object")

  field <- NULL
  field$name <- colnames(model$centers)
  number.of.fields <- length(field$name)
  field$class <- rep("numeric", number.of.fields) # All fields are numeric
  names(field$class) <- field$name
  orig.fields <- field$name

  field2 <- NULL
  field2$name[1] <- "ZementisClusterIDPlaceHolder"
  field2$class[1] <- "ID"
  names(field2$class)[1] <- "ZementisClusterIDPlaceHolder"
  for (i in 1:number.of.fields)
  {
    field2$name[i + 1] <- field$name[i]
    field2$class[i + 1] <- field$class[i]
    names(field2$class)[i + 1] <- names(field$class[i])
  }

  number.of.clusters <- length(model$size)
  cluster.names <- rownames(model$centers)

  #----------------------------------------------------------
  # PMML

  pmml <- .pmmlRootNode()

  #----------------------------------------------------------
  # PMML -> Header

  pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, app_name))

  #-----------------------------------------------------------
  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field2, transformed = transforms))

  #------------------------------------------------------------
  # PMML -> ClusteringModel

  the.model <- xmlNode("ClusteringModel",
    attrs = c(
      modelName = model_name,
      functionName = "clustering", # Required
      algorithmName = algorithm.name,
      modelClass = "centerBased", # Required
      numberOfClusters = number.of.clusters
    )
  ) # Required

  #---------------------------------------------------------------
  # PMML -> ClusteringModel -> MiningSchema

  the.model <- append.XMLNode(the.model, .pmmlMiningSchema(field2, transformed = transforms, missing_value_replacement = missing_value_replacement))

  #-----------------------------------------------------------------
  # PMML -> ClusteringModel -> Output

  output <- xmlNode("Output")
  out <- xmlNode("OutputField", attrs = c(
    name = "predictedValue",
    feature = "predictedValue",
    dataType = "string",
    optype = "categorical"
  ))
  output <- append.XMLNode(output, out)

  for (i in 1:number.of.clusters)
  {
    affinityFieldName <- paste("clusterAffinity_", i, sep = "")
    out <- xmlNode("OutputField", attrs = c(
      name = affinityFieldName,
      feature = "clusterAffinity",
      value = i, dataType = "double"
    ))
    output <- append.XMLNode(output, out)
  }

  the.model <- append.XMLNode(the.model, output)

  #-----------------------------------------------------------------
  # PMML -> ClusteringModel -> LocalTransformations

  if (!is.null(transforms)) {
    the.model <- append.XMLNode(the.model, .pmmlLocalTransformations(field2, transforms))
  }

  #------------------------------------------------------------------
  # PMML -> ClusteringModel -> ComparisonMeasure

  the.model <- append.XMLNode(
    the.model,
    append.XMLNode(xmlNode("ComparisonMeasure", attrs = c(kind = "distance")), xmlNode("squaredEuclidean"))
  )

  #------------------------------------------------------------------
  # PMML -> ClusteringField

  for (i in orig.fields)
  {
    the.model <- append.xmlNode(the.model, xmlNode("ClusteringField", attrs = c(field = i, compareFunction = "absDiff")))
  }

  #-------------------------------------------------------------------
  # PMML -> ClusteringModel -> Cluster -> Array

  for (i in 1:number.of.clusters)
  {
    the.model <- append.XMLNode(the.model, xmlNode("Cluster",
      attrs = c(name = cluster.names[i], size = model$size[i], id = i),
      xmlNode("Array",
        attrs = c(n = number.of.fields, type = "real"),
        paste(model$centers[i, ], collapse = " ")
      )
    ))
  }

  #---------------------------------------------
  pmml <- append.XMLNode(pmml, the.model)

  return(pmml)
}
