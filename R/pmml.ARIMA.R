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

  # Stop if model includes both intercept and drift terms
  if (("intercept" %in% names(model$coef)) & ("drift") %in% names(model$coef)) {
    stop("ARIMA models with both mean and drift terms not supported.")
  }
  
  field <- NULL
  field$name <- c("ts_value","h")
  field$class <- c("numeric","numeric")
  names(field$class) <- c("ts_value","h")
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
                             .pmmlMiningSchemaARIMA(field,target,transforms,missing_value_replacement))
  
  ts_model <- append.XMLNode(ts_model,
                             .pmmlOutput(field,target))
  
  
  ts_model <- append.XMLNode(ts_model,.make_ts_node(model))
  
  arima_rmse <- sqrt(sum((model$residuals)^2)/length(model$residuals))
  
  # constantTerm = 0 by default. Set the constantTerm to 0 when d != 0.
  if(any(is.element(c("intercept","drift"), names(model$coef)))) {
    if(is.element("intercept",names(model$coef))){
      arima_constant <- unname(model$coef["intercept"])
    } else {
      arima_constant <- unname(model$coef["drift"])
    }
  } else {
    arima_constant <- 0
  }
  
  
  arima_node <- xmlNode("ARIMA", attrs = c(RMSE=arima_rmse,
                                           transformation="none",
                                           constantTerm=arima_constant,
                                           predictionMethod="conditionalLeastSquares"))
  
  if(.is_nonseasonal(model)){
    arima_node <- append.XMLNode(arima_node,.make_nsc_node(model))
  }
  
  if(.is_seasonal(model)){
    arima_node <- append.XMLNode(arima_node,.make_sc_node(model))
  }
  
  ts_model <- append.XMLNode(ts_model,arima_node)
  
  pmml <- append.XMLNode(pmml, ts_model)
  
  return(pmml)
}

.make_nsc_node <- function(model) {
  # Creates NonseasonalComponent node.

  mod_len <- length(model$x)

  # (p,d,q) arrays
  ar_ind <- grep("^ar",names(model$coef))
  phi_array <- unname(model$coef[ar_ind])
  

  ma_ind <- grep("^ma",names(model$coef))
  theta_array <- unname(model$coef[ma_ind])
  
  # Reverse sign of theta coefficients for PMML representation.
  # theta_array <- (-1)*theta_array

  ns_p <- model$arma[1]
  ns_d <- model$arma[6]
  ns_q <- model$arma[2]
  
  mod_resids <- model$residuals
  
  nsc_node <- xmlNode("NonseasonalComponent",
                      attrs = c(p=ns_p, d=ns_d, q=ns_q))
  nsc_node <- .make_arma_nodes(c_node = nsc_node, p_val = ns_p, d_val = ns_d,
                               q_val = ns_q, phi_array = phi_array, theta_array = theta_array,
                               mod_len = mod_len, mod_resids = mod_resids, seas = FALSE,
                               ns_q = NA, seas_period = NA)
  return(nsc_node)
}


.make_sc_node <- function(model) {
  # Creates SeasonalComponent node.
  
  num_sma_elements <- length(grep("^sma",names(model$coef))) # deprecated
  
  mod_len <- length(model$x)
  
  # (P, D, Q) arrays
  sar_ind <- grep("^sar",names(model$coef))
  s_phi_array <- unname(model$coef[sar_ind])
  
  sma_ind <- grep("^sma",names(model$coef))
  s_theta_array <- unname(model$coef[sma_ind])
  
  # Reverse sign of theta coefficients for PMML representation.
  # s_theta_array <- (-1)*s_theta_array
  
  s_p <- model$arma[3]
  s_d <- model$arma[7]
  s_q <- model$arma[4]
  s_period <- model$arma[5]

  mod_resids <- model$residuals
  
  sc_node <- xmlNode("SeasonalComponent",
                     attrs = c(P=s_p, D=s_d, Q=s_q, period=s_period))
  sc_node <- .make_arma_nodes(c_node = sc_node, p_val = s_p, d_val = s_d,
                              q_val = s_q, phi_array = s_phi_array, theta_array = s_theta_array,
                              mod_len = mod_len, mod_resids = mod_resids,
                              seas = TRUE, ns_q = model$arma[2], seas_period = model$arma[5])
  return(sc_node)
}


.make_arma_nodes <- function(c_node, p_val, d_val, q_val, phi_array, theta_array, mod_len, mod_resids, seas, ns_q = NA, seas_period = NA){
  # Creates the AR and MA nodes for either non-seasonal or seasonal component nodes.
  # seas: logical indicating if node is seasonal.
  # ns_q: q value for non-seasonal component; only used when creating seasonal MA node.
  # seas_period: period for seasonal component; only used when creating seasonal MA node.
  
  if (p_val > 0){
    ar_node <- append.XMLNode(xmlNode("AR"),
                              xmlNode("Array",attrs = c(type="real",n=p_val),value=paste(phi_array,collapse=" ")))
    c_node <- append.XMLNode(c_node,ar_node)
  }
  
  if (q_val > 0){
    ma_coef_node <- append.XMLNode(xmlNode("MACoefficients"),
                                   xmlNode("Array",attrs = c(type="real",n=q_val),value=paste(theta_array,collapse=" ")))
    
    # Calculate the earliest necessary residual.
    if(seas) {
      resids <- mod_resids[(mod_len - (q_val*seas_period+ns_q) + 1):mod_len]
    } else {
      resids <- mod_resids[(mod_len - q_val + 1):mod_len]
    }
    ma_resid_node <- append.XMLNode(xmlNode("Residuals"),
                                    xmlNode("Array",
                                            attrs = c(type="real",
                                                      n=length(resids)),
                                            value=paste(resids,collapse=" ")))
    
    ma_node <- append.XMLNode(xmlNode("MA"),ma_coef_node,ma_resid_node)
    
    c_node <- append.XMLNode(c_node,ma_node)
  }
  
  return(c_node)
}



.is_seasonal <- function(model){
  # Checks if model has seasonal component.
  a <- model$arma
  return(a[3]!=0 | a[4]!=0 | a[7]!=0)
}

.is_nonseasonal <- function(model){
  # Checks if model has nonseasonal component.
  a <- model$arma
  return(a[1]!=0 | a[2]!=0 | a[6]!=0)
}

.make_ts_node <- function(model) {
  # Creates TimeSeries node. Exports the full time series.
  
  end_time <- length(model$x)
  start_time <- 1
  
  ts_node <- xmlNode("TimeSeries", attrs = c(usage="original",
                                             startTime=toString(start_time), endTime=toString(end_time)))
  
  for (ts_index in c(start_time:end_time)){
    tv_node <- xmlNode("TimeValue",attrs = c(index = toString(ts_index),value = model$x[ts_index]))
    ts_node <- append.XMLNode(ts_node, tv_node)
  }
  return(ts_node)
}


.make_time_anchor_node <- function(){
  # creates TimeAnchor node in TimeSeries
  time_anchor_node <- xmlNode("TimeAnchor")
  return(time_anchor_node)
}
