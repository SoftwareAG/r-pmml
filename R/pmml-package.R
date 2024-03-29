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

#' pmml: Generate PMML for R Models
#'
#' Export various R models to PMML and generate data transformations in PMML
#' format.
#'
#' @section Functions:
#' \itemize{
#' \item \code{\link{pmml}} exports R model objects to PMML format.
#' \item \code{\link{xform_wrap}} creates an object with that can then be used
#' to describe transformations to be exported to PMML.
#' }
#'
#' @section Data Transformations:
#'
#' The data transformation functions previously available in the separate
#' \pkg{pmmlTransformations} package have been merged into \pkg{pmml} starting
#' with version 2.0.0.
#'
#' The general methodology is to first wrap the data with \code{xform_wrap},
#' and then perform transformations using the following functions:
#' \code{xform_discretize}, \code{xform_function}, \code{xform_map},
#' \code{xform_min_max}, \code{xform_norm_discrete}, \code{xform_z_score}.
#' The model, including the transformations, can then be output in PMML format by
#' calling the \code{pmml} function. The \code{pmml} function in this
#' case has to be given an additional parameter, \code{transforms}.
#'
#' @section PMML:
#'
#' The Predictive Model Markup Language (PMML) is an XML-based language which
#' provides a way for applications to define machine learning, statistical and
#' data mining models and to share models between PMML compliant applications.
#' More information about the PMML industry standard and the Data Mining Group
#' can be found at <http://www.dmg.org>. The generated PMML can be imported
#' into any PMML consuming application, such as Zementis Predictive Analytics
#' products, which integrate with web services, relational database systems and
#' deploy natively on Hadoop in conjunction with Hive, Spark or Storm, as well
#' as allow predictive analytics to be executed for IBM z Systems mainframe
#' applications and real-time, streaming analytics platforms.
#'
#' @section References:
#' \itemize{
#' \item \href{http://dmg.org/pmml/v4-4-1/GeneralStructure.html}{PMML home page}
#'
#' \item \href{http://dmg.org/pmml/v4-4-1/Transformations.html}{PMML transformations}
#'
#' \item A. Guazzelli, W. Lin, T. Jena (2012), \emph{PMML in Action: Unleashing
#' the Power of Open Standards for Data Mining and Predictive Analytics}.
#' CreativeSpace (Second Edition) -
#' \href{https://www.amazon.com/dp/1470003244}{Available on Amazon.com}.
#'
#' \item A. Guazzelli, M. Zeller, W. Lin, G. Williams (2009), PMML: An Open
#' Standard for Sharing Models. \emph{The R journal}, Volume 1/1, 60-65
#'
#' \item T. Jena, A. Guazzelli, W. Lin, M. Zeller (2013).
#' \href{https://kdd13pmml.files.wordpress.com/2013/07/jena_et_al.pdf}{The R
#' pmmlTransformations Package.} In \emph{Proceedings of the 19th ACM SIGKDD
#' Conference on Knowledge Discovery and Data Mining}
#'
#' }
#'
#' @docType package
#' @aliases pmml-package
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
