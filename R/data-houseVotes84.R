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

#' Modified 1984 United States Congressional Voting Records Database
#'
#' This data set includes votes for each of the U.S. House of Representatives
#' Congressmen on the 16 key votes identified by the CQA.  The CQA lists nine
#' different types of votes: voted for, paired for, and announced for (these
#' three simplified to yea), voted against, paired against, and announced
#' against (these three simplified to nay), voted present, voted present to
#' avoid conflict of interest, and did not vote or otherwise make a position
#' known (these three simplified to an unknown disposition).  Originally
#' containing a binomial variable "class" and 16 other binary variables, those
#' 16 variables have been renamed to simply "V1","V2",...,"V16".
#'
#'
#' @name houseVotes84
#'
#' @docType data
#'
#' @format A data frame containing: \tabular{ll}{ Class \tab Boolean variable
#' \cr V1 \tab Boolean variable \cr V2 \tab Boolean variable \cr V3 \tab
#' Boolean variable \cr V4 \tab Boolean variable \cr V5 \tab Boolean variable
#' \cr V6 \tab Boolean variable \cr V7 \tab Boolean variable \cr V8 \tab
#' Boolean variable \cr V9 \tab Boolean variable \cr V10 \tab Boolean variable
#' \cr V11 \tab Boolean variable \cr V12 \tab Boolean variable \cr V13 \tab
#' Boolean variable \cr V14 \tab Boolean variable \cr V15 \tab Boolean variable
#' \cr V16 \tab Boolean variable \cr }
#' @references
#'
#' \href{http://archive.ics.uci.edu/ml/datasets/Congressional+Voting+Records}{UCI
#' Machine Learning Repository}
#'
#' @keywords datasets
#'
#' @examples
#' data(houseVotes84, package = "pmml")
NULL
