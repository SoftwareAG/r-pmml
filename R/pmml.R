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

#' Generate the PMML representation for R objects.
#'
#'
#' \code{pmml} is a generic function implementing S3 methods used to produce
#' the PMML (Predictive Model Markup Language) representation of an R model.
#' The resulting PMML file can then be imported into other systems that accept
#' PMML.
#'
#'
#' @param model An object to be converted to PMML.
#' @param model_name A name to be given to the PMML model.
#' @param app_name The name of the application that generated the PMML.
#' @param description A descriptive text for the Header element of the PMML.
#' @param copyright The copyright notice for the model.
#' @param transforms Data transformations.
#' @param \dots Further arguments passed to or from other methods.
#'
#' @return An object of class \code{XMLNode} as that defined by the \pkg{XML} package.
#' This represents the top level, or root node, of the XML document and is of
#' type PMML. It can be written to file with \code{saveXML}.
#'
#' @details
#' The data transformation functions previously available in the separate
#' \code{pmmlTransformations} package have been merged into \code{pmml}
#' starting with version 2.0.0.
#'
#' This function can also be used to output variable transformations in PMML
#' format. In particular, it can be used as a transformations generator.
#' Various transformation operations can be implemented in R and those
#' transformations can then be output in PMML format by calling the function
#' with a NULL value for the model input and a data transformation object as
#' the transforms input. Please see the documentation for \code{xform_wrap} for
#' more information on how to create a data transformation object.
#'
#' In addition, the \code{pmml} function can also be called using a
#' pre-existing PMML model as the first input and a data transformation object
#' as the transforms input.  The result is a new PMML model with the
#' transformation inserted as a "LocalTransformations" element in the original
#' model. If the original model already had a "LocalTransformations" element,
#' the new information will be appended to that element. If the model variables
#' are derived directly from a chain of transformations defined in the
#' transforms input, the field names in the model are replaced with the
#' original field names with the correct data types to make a consistent model.
#' The covered cases include model fields derived from an original field, model
#' fields derived from a chain of transformations starting from an original
#' field and multiple fields derived from the same original field.
#'
#' This package exports models to PMML version 4.4.
#'
#' Please note that package \pkg{XML_3.95-0.1} or later is required to perform
#' the full and correct functionality of \pkg{pmml}.
#'
#' If data used for an R model contains features of type \code{character},
#' these must be converted to factors before the model is trained and converted
#' with \code{pmml}.
#'
#' A list of all the supported models and packages is available in the
#' vignette:
#'
#' \code{vignette("packages_and_functions", package="pmml")}.
#'
#'
#' @author Graham Williams
#'
#' @seealso \code{\link{pmml.ada}}, \code{\link{pmml.rules}},
#' \code{\link{pmml.coxph}}, \code{\link{pmml.cv.glmnet}},
#' \code{\link{pmml.glm}}, \code{\link{pmml.hclust}},
#' \code{\link{pmml.kmeans}}, \code{\link{pmml.ksvm}}, \code{\link{pmml.lm}},
#' \code{\link{pmml.multinom}}, \code{\link{pmml.naiveBayes}},
#' \code{\link{pmml.neighbr}}, \code{\link{pmml.nnet}},
#' \code{\link{pmml.randomForest}}, \code{\link{pmml.rfsrc}},
#' \code{\link{pmml.rpart}}, \code{\link{pmml.svm}},
#' \code{\link{pmml.xgb.Booster}}
#' @references
#'
#' \itemize{
#'
#' \item \href{http://dmg.org/pmml/v4-3/GeneralStructure.html}{PMML home page}
#'
#' \item \href{http://dmg.org/pmml/v4-3/Transformations.html}{PMML transformations}
#'
#' }
#' @examples
#' # Build an lm model
#' iris_lm <- lm(Sepal.Length ~ ., data = iris)
#'
#' # Convert to pmml
#' iris_lm_pmml <- pmml(iris_lm)
#'
#' # Create a data transformation object
#' iris_trans <- xform_wrap(iris)
#'
#' # Transform the 'Sepal.Length' variable
#' iris_trans <- xform_min_max(iris_trans, xform_info = "column1->d_sl")
#'
#' # Output the tranformation in PMML format
#' iris_trans_pmml <- pmml(NULL, transforms = iris_trans)
#' @import XML
#' @importFrom stringr str_detect str_split str_subset str_replace str_interp str_extract
#' @importFrom stats coef coefficients na.omit
#' @importFrom methods getMethod
#' @importFrom utils capture.output compareVersion flush.console packageVersion
#'
#' @export
pmml <- function(model = NULL,
                 model_name = "R_Model",
                 app_name = "SoftwareAG PMML Generator",
                 description = NULL,
                 copyright = NULL,
                 transforms = NULL,
                 ...) {
  dots <- list(...)

  if (is.null(model) && !is.null(transforms)) {
    field <- NULL
    field$name <- names(transforms$field_data)
    field$class <- transforms$field_data[, "dataType"]
    names(field$class) <- row.names(transforms$field_data)

    if (!is.null(dots$transformationDictionary)) {
      return(.pmmlLocalTransformations(field, transforms, NULL, transformationDictionary = T))
    } else {
      return(.pmmlLocalTransformations(field, transforms, NULL))
    }
  }
  else {
    UseMethod("pmml")
  }
}
