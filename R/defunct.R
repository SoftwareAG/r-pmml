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

# Functions from pmmlTransformations that have been renamed.

defunct <- function(msg = "This function is defunct;", new_name) {
  function(...) {
    return(stop(paste(msg, "use", new_name, "instead.")))
  }
}

DiscretizeXform <- defunct(new_name = "xform_discretize")
FunctionXform <- defunct(new_name = "xform_function")
MapXform <- defunct(new_name = "xform_map")
MinMaxXform <- defunct(new_name = "xform_min_max")
NormDiscreteXform <- defunct(new_name = "xform_norm_discrete")
RenameVar <- defunct(new_name = "rename_wrap_var")
WrapData <- defunct(new_name = "xform_wrap")
ZScoreXform <- defunct(new_name = "xform_z_score")
