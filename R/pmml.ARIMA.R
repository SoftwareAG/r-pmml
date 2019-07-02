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
#' 
#' @author Dmitriy Bolotov
#' 
#' @examples
#' library(forecast)
#' data("WWWusage")
#' mod <- Arima(WWWusage,order=c(3,1,1))
#' mod_pmml <- pmml(mod)
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
  if (!inherits(model, "ARIMA")) stop("Not a legitimate ARIMA stats object.")
  
  if(!is.null(transforms)) stop("Transforms not supported for ARIMA stats models.")

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
  
  ts_model <- append.XMLNode(ts_model,
                             .pmmlOutput(field,target))
  
  
  ts_model <- append.XMLNode(ts_model,.make_ts_node(model))
  
  arima_rmse <- sqrt(sum((model$residuals)^2)/length(model$residuals))
  
  # constantTerm is used when d=0. Set the constantTerm to 0 when d != 0.
  # constantTerm = 0 by default.
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


.make_nsc_node <- function(model) {
  # creates NonseasonalComponent node
  
  # model$model$theta may contain values of 0. This should not count towards q.
  num_ma_elements <- length(grep("ma",names(model$coef)))
  
  mod_len <- length(model$x)
    
  # (p,d,q) arrays
  phi_array <- model$model$phi
  d_array <- model$model$Delta
  theta_array <- if (num_ma_elements > 0) {model$model$theta[1:num_ma_elements]} else {0}
  
  # Reverse sign of theta coefficients for PMML representation
  theta_array <- (-1)*theta_array
  
  # Check if theta_array is 0
  ns_p <- .get_p_q(phi_array)
  ns_d <- length(d_array)
  ns_q <- .get_p_q(theta_array)
  
  nsc_node <- xmlNode("NonseasonalComponent",
                      attrs = c(p=ns_p, d=ns_d, q=ns_q))
  
  if (ns_p>0){
  ar_node <- append.XMLNode(xmlNode("AR"),
                            xmlNode("Array",attrs = c(type="real",n=ns_p),value=paste(phi_array,collapse=" ")))
  nsc_node <- append.XMLNode(nsc_node,ar_node)
  }
  
  if (ns_q > 0){
    ma_coef_node <- append.XMLNode(xmlNode("MACoefficients"),
                                   xmlNode("Array",attrs = c(type="real",n=ns_q),value=paste(theta_array,collapse=" ")))
    
    resids <- model$residuals[(mod_len - ns_q + 1):mod_len]
    ma_resid_node <- append.XMLNode(xmlNode("Residuals"),
                                    xmlNode("Array",
                                            attrs = c(type="real",
                                                      n=length(resids)),
                                            value=paste(resids,collapse=" ")))
    
    ma_node <- append.XMLNode(xmlNode("MA"),ma_coef_node,ma_resid_node)
    
    nsc_node <- append.XMLNode(nsc_node,ma_node)
  }
  
  return(nsc_node)
}


.get_p_q <- function(x_array){
  if(length(x_array)==0){
    return(0)
  } else if (length(x_array)==1) {
    if (x_array==0) {
      return(0)
    } else {
      return(1)
    }
  } else {
    return(length(x_array))
  }
}


.make_ts_node <- function(model) {
  # creates TimeSeries node
  
  # start_time corresponds to the number of values necessary to make the first forecast.
  num_ar_elements <- length(grep("ar",names(model$coef)))
  end_time <- length(model$x)
  start_time <- end_time - num_ar_elements
  
  
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
