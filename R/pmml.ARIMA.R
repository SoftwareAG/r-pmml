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

#' Generate PMML for an ARIMA object the \bold{forecast} package.
#' 
#' @param model an ARIMA object from the package \pkg{forecast}.
#' @param missing_value_replacement Value to be used as the 'missingValueReplacement'
#' attribute for all MiningFields.
#'
#' @inheritParams pmml
#' 
#' @return PMML representation of the \code{ARIMA} object.
#' 
#' @details The model is represented in the PMML TimeSeriesModel format.
#' TODO
#' 
#' @author Dmitriy Bolotov
#' 
#' @examples
#' TODO
#' 
#'
#' @export pmml.ARIMA
#' @export
pmml.ARIMA <- function(model,
                       model_name="ARIMA_model",
                       app_name = "SoftwareAG PMML Generator",
                       description="ARIMA Time Series Model",
                       copyright=NULL,
                       transforms=NULL,
                       missing_value_replacement = NULL,
                       ...)
  
{
  if (!inherits(model, "ARIMA")) stop("Not a legitimate ARIMA forecast object.")
  
  if(!is.null(transforms)) stop("Transforms not supported for ARIMA forecast models.")
  # 
  # field <- NULL
  # field$name <- c("date","value")
  # field$class <- c("numeric","numeric")
  # names(field$class) <- c("date","value")
  # functionName <- "timeSeries"
  # target <- "value"
  # 
  
  field <- NULL
  field$name <- c("ts_value")
  field$class <- c("numeric")
  names(field$class) <- c("ts_value")
  functionName <- "timeSeries"
  target <- "ts_value"
  
  
  
  # PMML  
  pmml <- .pmmlRootNode("4.4")
  
  # PMML -> Header
  pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, app_name))
  
  # PMML -> DataDictionary
  pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field,transformed=transforms))
  
  # PMML -> TimeSeriesModel
  ts_model <- xmlNode("TimeSeriesModel",
                      attrs=c(modelName=model_name,functionName="timeSeries",bestFit="ARIMA"))
  
  ts_model <- append.XMLNode(ts_model,
                             .pmmlMiningSchema(field,target,transforms,missing_value_replacement))
  
  
  ts_model <- append.XMLNode(ts_model,.make_ts_node(model))
  
  
  arima_rmse <- sqrt(sum(model$residuals^2)/(length(model$residuals)-1))
  
  # constantTerm is used when d=0. Set the constantTerm to 0 when d != 0.
  arima_constant <- if(length(model$model$Delta)==0) {unname(model$coef["intercept"])} else {0}
  arima_node <- xmlNode("ARIMA", attrs = c(RMSE=arima_rmse,
                                           transformation="none",
                                           constantTerm=arima_constant,
                                           predictionMethod="conditionalLeastSquares"))
  
  arima_node <- append.XMLNode(arima_node,.make_nsc_node(model))
  
  ts_model <- append.XMLNode(ts_model,arima_node)
  
  
  pmml <- append.XMLNode(pmml, ts_model)
  
  return(pmml)
  
}

#TODO: Number of residuals necessary changes; not always one


.make_nsc_node <- function(model) {
  # creates NonseasonalComponent node
  
  # model$model$theta may contain values of 0. This should not count towards q.
  num_ma_elements <- length(grep("ma",names(model$coef)))
  
    
  # (p,d,q) arrays
  ns_p <- model$model$phi
  ns_d <- model$model$Delta
  ns_q <- model$model$theta[1:num_ma_elements]
  
  nsc_node <- xmlNode("NonseasonalComponent",attrs = c(p=length(ns_p), d=length(ns_d), q=length(ns_q)))
  
  ar_node <- append.XMLNode(xmlNode("AR"),xmlNode("Array",attrs = c(type="real",n=length(ns_p)),value=paste(ns_p,collapse=" ")))
  
  ma_coef_node <- append.XMLNode(xmlNode("MACoefficients"),xmlNode("Array",attrs = c(type="real",n=length(ns_q)),value=paste(ns_q,collapse=" ")))
  
  residual_at_t <- model$residuals[length(model$residuals)] #residual at time t
  ma_resid_node <- append.XMLNode(xmlNode("Residuals"),xmlNode("Array",attrs = c(type="real",value=residual_at_t)))
  ma_node <- append.XMLNode(xmlNode("MA"),ma_coef_node,ma_resid_node)
  
  nsc_node <- append.XMLNode(nsc_node,ar_node,ma_node)
  
  return(nsc_node)
}


.make_ts_node <- function(model) {
  # creates TimeSeries node
  
  # start_time reflects the number of values necessary to make the first forecast.
  num_ma_elements <- length(grep("ma",names(model$coef)))
  num_ar_elements <- length(grep("ar",names(model$coef)))
  start_time <- length(model$x) - max(num_ma_elements, num_ar_elements)
  end_time <- length(model$x)
  
  ts_node <- xmlNode("TimeSeries", attrs = c(usage="logical",
                                             startTime=toString(start_time), endTime=toString(end_time)))
  
  for (ts_index in c(start_time:end_time)){
    ts_node <- append.XMLNode(ts_node,
                              xmlNode("TimeValue",
                                      attrs = c(index = toString(ts_index),
                                                value = model$x[ts_index])))
  }
  
  
  return(ts_node)
}


.make_time_anchor_node <- function(){
  # creates TimeAnchor node in TimeSeries
  time_anchor_node <- xmlNode("TimeAnchor")
  return(time_anchor_node)
}
