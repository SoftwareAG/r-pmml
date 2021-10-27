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

#' Save a pmml object as an external PMML file.
#'
#'
#' Save a pmml object to an external PMML file.
#'
#'
#' @param doc The pmml model.
#' @param name The name of the external file where the PMML is to be saved.
#'
#' @author Tridivesh Jena
#'
#' @keywords interface
#'
#' @examples
#' \dontrun{
#' # Make a gbm model:
#' library(gbm)
#' data(audit)
#'
#' mod <- gbm(Adjusted ~ .,
#'   data = audit[, -c(1, 4, 6, 9, 10, 11, 12)],
#'   n.trees = 3,
#'   interaction.depth = 4
#' )
#'
#' # Export to PMML:
#' pmod <- pmml(mod)
#'
#' # Save to an external file:
#' save_pmml(pmod, "GBMModel.pmml")
#' }
#'
#' @export
save_pmml <- function(doc, name) {
  output_file <- file(name, "wb")
  saveXML(doc, output_file)
  close(output_file)
}
