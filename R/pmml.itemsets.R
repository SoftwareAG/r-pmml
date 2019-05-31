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

#' @export
pmml.itemsets <- function(model,
                          model_name = "arules_Model",
                          app_name = "SoftwareAG PMML Generator",
                          description = "Frequent Itemsets Model",
                          copyright = NULL,
                          transforms = NULL, ...) {
  if (!inherits(model, "itemsets")) stop("Not a legitimate arules itemsets rules object")

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


  # Model
  quality <- quality(model)
  is <- items(model)

  association.model <- xmlNode("AssociationModel",
    attrs = c(
      functionName = "associationRules",
      numberOfTransactions = attr(quality(model), "size.data"),
      numberOfItems = length(arules::itemLabels(model)),
      minimumSupport = min(quality$support),
      minimumConfidence = 0L,
      numberOfItemsets = length(is),
      numberOfRules = 0L
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

  ## itemsets
  itemsets <- list()
  sizes <- arules::size(is)
  isl <- arules::LIST(is, decode = FALSE)
  for (i in 1:length(isl)) {
    itemsets[[i]] <- xmlNode("Itemset", attrs = list(
      id = i,
      numberOfItems = sizes[i], support = quality$support[i]
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

  pmml <- append.XMLNode(pmml, association.model)

  return(pmml)
}
