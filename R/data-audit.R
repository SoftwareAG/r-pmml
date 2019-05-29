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

#' Audit: artificially constructed dataset
#'
#' This is an artificial dataset consisting of fictional clients who have been
#' audited, perhaps for tax refund compliance. For each case an outcome is
#' recorded (whether the taxpayer's claims had to be adjusted or not) and any
#' amount of adjustment that resulted is also recorded.
#'
#'
#' @name audit
#'
#' @docType data
#'
#' @format A data frame containing: \tabular{ll}{ Age \tab Numeric \cr
#' Employment \tab Categorical string with 7 levels \cr Education \tab
#' Categorical string with 16 levels \cr Marital \tab Categorical string with 6
#' levels \cr Occupation \tab Categorical string with 14 levels \cr Income \tab
#' Numeric \cr Sex \tab Categorical string with 2 levels \cr Deductions \tab
#' Numeric \cr Hours \tab Numeric \cr Accounts \tab Categorical string with 32
#' levels \cr Adjustment \tab Numeric \cr Adjusted \tab Numeric value 0 or 1
#' \cr }
#'
#' @references
#' \itemize{
#' \item Togaware rattle package : \emph{\bold{Audit dataset}} \cr
#' \item \href{http://www.dmg.org/pmml_examples/index.html#Audit}{DMG description of the Audit dataset}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(audit, package = "pmml")
NULL
