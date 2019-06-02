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

#' Generate the PMML representation for a rules or an itemset object from
#' package \pkg{arules}.
#'
#' @param model A rules or itemsets object.
#'
#' @inheritParams pmml
#'
#' @return PMML representation of the rules or itemsets object.
#'
#' @details
#' The model is represented in the PMML AssociationModel format.
#'
#' @aliases pmml.rules pmml.itemsets
#'
#' @author Graham Williams, Michael Hahsler
#'
#' @references
#' \href{https://CRAN.R-project.org/package=arules}{arules: Mining Association
#' Rules and Frequent Itemsets}
#'
#' @export pmml.rules
#' @export
pmml.rules <- function(model,
                       model_name = "arules_Model",
                       app_name = "SoftwareAG PMML Generator",
                       description = "Association Rules Model",
                       copyright = NULL,
                       transforms = NULL, ...) {
  if (!inherits(model, "rules")) stop("Not a legitimate arules rules object")

  requireNamespace("arules", quietly = TRUE)

  # PMML

  pmml <- .pmmlRootNode()

  # PMML -> Header

  pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, app_name))

  # PMML -> DataDictionary

  data.dictionary <- xmlNode("DataDictionary", attrs = c(numberOfFields = 2L))
  data.dictionary <- append.xmlNode(data.dictionary, list(
    xmlNode("DataField", attrs = c(
      name = "transaction",
      optype = "categorical", dataType = "string"
    )),
    xmlNode("DataField", attrs = c(
      name = "item",
      optype = "categorical", dataType = "string"
    ))
  ))

  pmml <- append.XMLNode(pmml, data.dictionary)

  # Association rule model

  quality <- quality(model)
  is <- c(arules::lhs(model), arules::rhs(model))

  is.unique <- getMethod("unique", "itemMatrix")(is)

  association.model <- xmlNode("AssociationModel",
    attrs = c(
      functionName = "associationRules",
      numberOfTransactions = arules::info(model)$ntransactions,
      numberOfItems = length(arules::itemLabels(model)),
      minimumSupport = arules::info(model)$support,
      minimumConfidence = arules::info(model)$confidence,
      numberOfItemsets = length(is.unique),
      numberOfRules = length(model)
    )
  )

  # Mining schema
  mining.schema <- xmlNode("MiningSchema")
  mining.schema <- append.xmlNode(mining.schema, list(
    xmlNode("MiningField", attrs = c(name = "transaction", usageType = "group")),
    xmlNode("MiningField", attrs = c(name = "item", usageType = "active"))
  ))

  association.model <- append.xmlNode(association.model, mining.schema)

  # Items
  items <- list()
  il <- .markupSpecials(arules::itemLabels(model))
  for (i in 1:length(il)) {
    items[[i]] <- xmlNode("Item", attrs = list(id = i, value = il[i]))
  }

  association.model <- append.xmlNode(association.model, items)

  # Itemsets
  itemsets <- list()
  sizes <- arules::size(is.unique)
  isl <- arules::LIST(is.unique, decode = FALSE)
  for (i in 1:length(isl)) {
    itemsets[[i]] <- xmlNode("Itemset", attrs = list(
      id = i,
      numberOfItems = sizes[i]
    ))

    items <- list()
    if (sizes[i] > 0) {
      for (j in 1:sizes[i]) {
        items[[j]] <- xmlNode("ItemRef", attrs = list(itemRef = isl[[i]][j]))
      }
    }

    itemsets[[i]] <- append.xmlNode(itemsets[[i]], items)
  }

  association.model <- append.xmlNode(association.model, itemsets)

  # Rules
  mlhs <- getMethod("match", c("itemMatrix", "itemMatrix"))(arules::lhs(model), is.unique)
  mrhs <- getMethod("match", c("itemMatrix", "itemMatrix"))(arules::rhs(model), is.unique)
  rules <- list()

  for (i in 1:length(model)) {
    rules[[i]] <- xmlNode("AssociationRule", attrs = list(
      support = quality$support[i], confidence = quality$confidence[i],
      lift = quality$lift[i],
      antecedent = mlhs[i], consequent = mrhs[i]
    ))
  }

  association.model <- append.xmlNode(association.model, rules)

  pmml <- append.XMLNode(pmml, association.model)

  return(pmml)
}
