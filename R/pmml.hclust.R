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

#' Generate the PMML representation for a hclust object from the package \pkg{amap}.
#'
#'
#' @param model A hclust object.
#' @param missing_value_replacement Value to be used as the 'missingValueReplacement'
#' attribute for all MiningFields.
#' @param centers A list of means to represent the clusters.
#'
#' @inheritParams pmml
#'
#' @return PMML representation of the hclust object.
#'
#' @details
#' This function converts a hclust object created by the \code{hclusterpar} function
#' from the \pkg{amap} package.  A \code{hclust} object is a cluster model created
#' hierarchically. The data is divided recursively until a criteria is met.
#' This function then takes the final model and represents it as a standard
#' k-means cluster model. This is possible since while the method of
#' constructing the model is different, the final model can be represented in
#' the same way.
#'
#' To use this pmml function, therefore, one must pick the number of clusters
#' desired and the coordinate values at those cluster centers. This can be done
#' using the \code{hclusterpar} and \code{centers.hclust} functions from the
#' \pkg{amap} and \code{rattle} packages repectively.
#'
#' The hclust object will be approximated by \code{k} centroids and is
#' converted into a PMML representation for kmeans clusters.
#'
#' @author Graham Williams
#'
#' @references
#' \href{http://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html}{R project: Hierarchical Clustering}
#'
#' @examples
#' \dontrun{
#'
#' # Cluster the 4 numeric variables of the iris dataset.
#' library(amap)
#' library(rattle)
#'
#' model <- hclusterpar(iris[, -5])
#'
#' # Get the information about the cluster centers. The last
#' # parameter of the function used is the number of clusters
#' # desired.
#' centerInfo <- centers.hclust(iris[, -5], model, 3)
#'
#' # Convert to pmml
#' pmml(model, centers = centerInfo)
#' }
#'
#' @export pmml.hclust
#' @export
pmml.hclust <- function(model,
                        model_name = "HClust_Model",
                        app_name = "SoftwareAG PMML Generator",
                        description = "Hierarchical Cluster Model",
                        copyright = NULL,
                        transforms = NULL,
                        missing_value_replacement = NULL,
                        centers,
                        ...) {
  if (!inherits(model, "hclust")) stop("Not a legitimate hclust object")

  field <- NULL
  field$name <- colnames(centers)
  number.of.fields <- length(field$name)
  field$class <- rep("numeric", number.of.fields) # All fields are numeric
  names(field$class) <- field$name

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

  orig.fields <- field$name

  number.of.clusters <- nrow(centers)
  cluster.names <- 1:number.of.clusters

  #-------------------------------------------------------------------
  # PMML

  pmml <- .pmmlRootNode()

  #-------------------------------------------------------------------
  # PMML -> Header

  pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, app_name))

  #-------------------------------------------------------------------
  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field2, transformed = transforms))

  # PMML -> ClusteringModel

  cl.model <- xmlNode("ClusteringModel",
    attrs = c(
      modelName = model_name,
      functionName = "clustering", # Required
      algorithmName = "HClust",
      modelClass = "centerBased", # Required
      numberOfClusters = number.of.clusters
    )
  ) # Required

  # PMML -> ClusteringModel -> MiningSchema

  cl.model <- append.XMLNode(cl.model, .pmmlMiningSchema(field2, transformed = transforms, missing_value_replacement = missing_value_replacement))

  #----------------------------------------------------------------------
  # Outputs
  output <- xmlNode("Output")
  out <- xmlNode("OutputField", attrs = c(name = "predictedValue", feature = "predictedValue"))
  output <- append.XMLNode(output, out)
  cl.model <- append.XMLNode(cl.model, output)

  #----------------------------------------------------------------------
  # PMML -> ClusteringModel -> LocalTransformations -> DerivedField -> NormContiuous

  if (!is.null(transforms)) {
    the.model <- append.XMLNode(the.model, .pmmlLocalTransformations(field2, transforms))
  }

  #------------------------------------------------------------------
  # PMML -> ClusteringModel -> ComparisonMeasure

  cl.model <- append.XMLNode(
    cl.model,
    append.XMLNode(
      xmlNode("ComparisonMeasure",
        attrs = c(kind = "distance")
      ),
      xmlNode("squaredEuclidean")
    )
  )

  #-------------------------------------------------------------------
  # PMML -> ClusteringField

  for (i in orig.fields)
  {
    cl.model <- append.xmlNode(cl.model, xmlNode("ClusteringField", attrs = c(field = i)))
  }

  # PMML -> ClusteringModel -> Cluster -> Array

  clusters <- list()
  for (i in 1:number.of.clusters)
  {
    cl.model <- append.XMLNode(
      cl.model,
      xmlNode("Cluster",
        attrs = c(name = cluster.names[i], size = model$size[i]),
        xmlNode("Array",
          attrs = c(n = number.of.fields, type = "real"),
          paste(centers[i, ], collapse = " ")
        )
      )
    )
  }
  pmml <- append.XMLNode(pmml, cl.model)

  return(pmml)
}
