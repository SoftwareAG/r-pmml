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

#' Convert an R expression to PMML.
#'
#' @param expr An R expression enclosed in quotes.

#' @details
#' As long as the expression passed to the function is a valid R expression (e.g., no unbalanced parenthesis),
#' it can contain arbitrary function names not defined in R. Variables in the expression passed
#' to `xform_function` are always assumed to be fields, and not substituted. That is, even if `x` has a value in the
#' R environment, the resulting expression will still use `x`.
#'
#' An expression such as `foo(x)` is treated as a function `foo` with argument `x`. Consequently, passing in an
#' R vector `c(1,2,3)` to `function_to_pmml()` will produce PMML where `c` is a function and `1,2,3` are the arguments.
#'
#' An expression starting with '-' or '+' (for example, "-3" or "-(a+b)") will be treated as if there is a 0
#' before the initial '-' or '+' sign. This makes it possible to represent expressions that start with a sign,
#' since PMML's '-' and '+' functions require two arguments. The resulting PMML node will have a constant 0 as a child.
#'
#' @return PMML version of the input expression
#'
#' @author Dmitriy Bolotov
#'
#' @examples
#' # Operator precedence and parenthesis
#' func_pmml <- function_to_pmml("1 + 3/5 - (4 * 2)")
#'
#' # Nested arbitrary functions
#' func_pmml <- function_to_pmml("foo(bar(x)) - bar(foo(y-z))")
#'
#' # If-else expression
#' func_pmml <- function_to_pmml("if (x==3) { 3 } else { 0 }")
#'
#' # If-else with boolean output
#' func_pmml <- function_to_pmml("if (x==3) { TRUE } else { FALSE }")
#'
#' # Function with string argument types
#' func_pmml <- function_to_pmml("colors('red','green','blue')")
#'
#' # Sign in front of expression
#' func_pmml <- function_to_pmml("-(x/y)")
#' @export
function_to_pmml <- function(expr) {
  xformed <- .pmmlU(expr)

  return(xformed)
}
