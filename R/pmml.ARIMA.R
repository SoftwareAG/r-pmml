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

#' Generate PMML for an ARIMA object the \bold{forecast} package.
#'
#' @param model An ARIMA object from the package \pkg{forecast}.
#' @param missing_value_replacement Value to be used as the 'missingValueReplacement'
#' attribute for all MiningFields.
#' @param ts_type The type of time series representation for PMML: "arima" or "statespace".
#' @param cpi_levels Vector of confidence levels for prediction intervals.
#'
#' @inheritParams pmml
#'
#' @return PMML representation of the \code{ARIMA} object.
#'
#' @details The model is represented as a PMML TimeSeriesModel.
#'
#' When \code{ts_type = "statespace"} (by default), the R object is exported as StateSpaceModel in PMML.
#'
#' When \code{ts_type = "arima"}, the R object is exported as ARIMA in PMML with conditional
#' least squares (CLS). Note that ARIMA models in R are
#' estimated using a state space representation. Therefore, when using CLS with seasonal models,
#' forecast results between R and PMML may not match exactly. Additionally, when ts_type="arima", prediction intervals
#' are exported for non-seasonal models only. For ARIMA models with d=2, the prediction intervals
#' between R and PMML may not match.
#'
#' OutputField elements are exported with
#' dataType "string", and contain a collection of all values up to and including the steps-ahead value supplied
#' during scoring.
#' String output in this form is facilitated by Extension elements in the PMML file,
#' and is supported by Zementis Server since version 10.6.0.0.
#'
#' \code{cpi_levels} behaves similar to \code{levels} in \code{forecast::forecast}: values must be
#' between 0 and 100, non-inclusive.
#'
#' Models with a drift term will be supported in a future version.
#'
#' Transforms are currently not supported for ARIMA models.
#'
#' @author Dmitriy Bolotov
#'
#' @examples
#' library(forecast)
#'
#' # non-seasonal model
#' data("WWWusage")
#' mod <- Arima(WWWusage, order = c(3, 1, 1))
#' mod_pmml <- pmml(mod)
#'
#' # seasonal model
#' data("JohnsonJohnson")
#' mod_02 <- Arima(JohnsonJohnson,
#'   order = c(1, 1, 1),
#'   seasonal = c(1, 1, 1)
#' )
#' mod_02_pmml <- pmml(mod_02)
#'
#' # non-seasonal model exported with Conditional Least Squares
#' data("WWWusage")
#' mod <- Arima(WWWusage, order = c(3, 1, 1))
#' mod_pmml <- pmml(mod, ts_type = "arima")
#' @export pmml.ARIMA
#' @export
pmml.ARIMA <- function(model,
                       model_name = "ARIMA_model",
                       app_name = "SoftwareAG PMML Generator",
                       description = "ARIMA Time Series Model",
                       copyright = NULL,
                       model_version = NULL,
                       transforms = NULL,
                       missing_value_replacement = NULL,
                       ts_type = "statespace",
                       cpi_levels = c(80, 95),
                       ...) {
  if (!inherits(model, "ARIMA")) stop("Not a legitimate ARIMA object.")

  if (!(ts_type %in% c("arima", "statespace"))) {
    stop('ts_type must be one of "arima" or "statespace".')
  }

  if (ts_type == "arima") {
    output_type <- "double"
  } else {
    output_type <- "string"
  }

  cpi_levels <- .check_cpi_levels(cpi_levels)

  if (!is.null(transforms)) stop("Transforms are not supported for ARIMA forecast models.")

  # Stop if model includes drift term
  if ("drift" %in% names(model$coef)) {
    stop("ARIMA models with a drift term are not supported.")
  }

  # # Stop if model includes both intercept and drift terms
  # if (("intercept" %in% names(model$coef)) & ("drift") %in% names(model$coef)) {
  #   stop("ARIMA models with both mean and drift terms not supported.")
  # }

  field <- NULL
  field$name <- c("ts_value", "h")
  field$class <- c("numeric", "numeric")
  names(field$class) <- c("ts_value", "h")
  functionName <- "timeSeries"
  target <- "ts_value"

  # PMML
  pmml <- .pmmlRootNode()

  # PMML -> Header
  pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, model_version, app_name))

  # PMML -> DataDictionary
  pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field, transformed = transforms))

  if (ts_type == "arima") {

    # PMML -> TimeSeriesModel
    ts_model <- xmlNode("TimeSeriesModel",
      attrs = c(modelName = model_name, functionName = "timeSeries", bestFit = "ARIMA")
    )

    ts_model <- append.XMLNode(
      ts_model,
      .pmmlMiningSchemaARIMA(field, target, transforms, missing_value_replacement)
    )

    ts_model <- append.XMLNode(
      ts_model,
      # .pmmlOutput(field, target)
      # .make_arima_output_node(target, .has_seasonal_comp(model), cpi_levels, output_type)
      .make_arima_output_node(target, .has_seasonal_comp(model), cpi_levels, "string") # set output="string"
    )


    ts_model <- append.XMLNode(ts_model, .make_ts_node(model))

    # arima_rmse <- sqrt(sum((model$residuals)^2) / length(model$residuals))
    arima_rmse <- sqrt(model$sigma2) # use the approximation in ARIMA object

    arima_constant <- .get_model_constant(model)

    prediction_method <- "conditionalLeastSquares"

    arima_node <- xmlNode("ARIMA", attrs = c(
      RMSE = arima_rmse,
      transformation = "none",
      constantTerm = arima_constant,
      predictionMethod = prediction_method
    ))

    if (.has_nonseasonal_comp(model)) {
      # arima_node <- append.XMLNode(arima_node, .make_nsc_node(model, exact_least_squares))
      arima_node <- append.XMLNode(arima_node, .make_nsc_node(model, FALSE))
    } else {
      # crete a non-seasonal node with all zeros
      # arima_node <- append.XMLNode(arima_node, .make_zero_nsc_node(model, exact_least_squares))
      arima_node <- append.XMLNode(arima_node, .make_zero_nsc_node(model, FALSE))
    }

    if (.has_seasonal_comp(model)) {
      # arima_node <- append.XMLNode(arima_node, .make_sc_node(model, exact_least_squares))
      arima_node <- append.XMLNode(arima_node, .make_sc_node(model, FALSE))
    }

    # if (exact_least_squares) {
    #   arima_node <- append.XMLNode(
    #     arima_node,
    #     .make_mls_node(model, ts_type)
    #   )
    # }


    ts_model <- append.XMLNode(ts_model, arima_node)
  } else {
    # PMML -> TimeSeriesModel
    ts_model <- xmlNode("TimeSeriesModel",
      attrs = c(
        modelName = model_name,
        functionName = "timeSeries",
        bestFit = "StateSpaceModel"
      )
    )

    ts_model <- append.XMLNode(
      ts_model,
      .pmmlMiningSchemaARIMA(field, target, transforms, missing_value_replacement)
    )

    ts_model <- append.XMLNode(
      ts_model,
      # .pmmlOutput(field, target)
      .make_arima_output_node(target, FALSE, cpi_levels, output_type)
    )

    ts_model <- append.XMLNode(ts_model, .make_ts_node(model))

    state_space_node <- xmlNode("StateSpaceModel",
      attrs = c(
        # intercept = toString(.get_model_constant(model)),
        variance = model$sigma2
        # observationVariance = toString(model$model$h)
      )
    )


    # state vector
    state_v_node <- .make_fs_vector_node(model, ts_type = ts_type)

    # transition matrix
    trans_m_node <- append.XMLNode(
      xmlNode("TransitionMatrix"),
      .make_matrix_node(model$model$T)
    )

    # measurement matrix
    meas_m_node <- append.XMLNode(
      xmlNode("MeasurementMatrix"),
      .make_matrix_node(matrix(model$model$Z, nrow = 1))
    )

    # intercept vector - replaces intercept attribute
    intercept_v_node <- .make_intercept_v_node(model)

    # ObservationVarianceMatrix - replaces observationVariance attribute
    ovm_node <- append.XMLNode(
      xmlNode("ObservationVarianceMatrix"),
      .make_matrix_node(matrix(model$model$h, nrow = 1))
    )

    # psi vector - not used

    # dynamic regressor - not used

    # PredictedCovarianceMatrix
    pscm_node <- append.XMLNode(
      xmlNode("PredictedStateCovarianceMatrix"),
      .make_matrix_node(matrix(model$model$P,
        nrow = NROW(model$model$P)
      ))
    )

    # SelectedCovarianceMatrix
    sscm_node <- append.XMLNode(
      xmlNode("SelectedStateCovarianceMatrix"),
      .make_matrix_node(matrix(model$model$V,
        nrow = NROW(model$model$V)
      ))
    )


    # add elements to state_space_node
    state_space_node <- append.XMLNode(
      state_space_node,
      state_v_node,
      trans_m_node,
      meas_m_node,
      intercept_v_node,
      pscm_node,
      sscm_node,
      ovm_node
    )

    ts_model <- append.XMLNode(ts_model, state_space_node)
  }

  pmml <- append.XMLNode(pmml, ts_model)

  return(pmml)
}


.check_cpi_levels <- function(cpi_levels) {
  if (length(cpi_levels) == 0) {
    stop("Length of cpi_levels must be greater than 0.")
  }

  if (!is.numeric(cpi_levels)) {
    stop("cpi_levels must be numeric.")
  }

  # If levels are between 0 and 1, they should be converted to percentage first.
  if (min(cpi_levels) > 0 & max(cpi_levels) < 1) {
    cpi_levels <- 100 * cpi_levels
  }

  if (min(cpi_levels) < 0 | max(cpi_levels) > 99.99) {
    stop("cpi_levels out of range.")
  }

  return(cpi_levels)
}


.get_model_constant <- function(model) {
  # Get the constant term from model.
  # constantTerm = 0 by default in PMML. Set the constantTerm to 0 when d != 0.
  if (any(is.element(c("intercept", "drift"), names(model$coef)))) {
    if (is.element("intercept", names(model$coef))) {
      arima_constant <- unname(model$coef["intercept"])
    } else {
      arima_constant <- unname(model$coef["drift"])
    }
  } else {
    arima_constant <- 0
  }
  return(arima_constant)
}


.make_intercept_v_node <- function(model) {
  iv_node <- xmlNode("InterceptVector",
    attrs = c(type = "observation")
  )
  const <- .get_model_constant(model)
  iv_node <- append.XMLNode(iv_node, xmlNode("Array",
    attrs = c(type = "real", n = "1"),
    value = const
  ))
  return(iv_node)
}


.make_arima_output_node <- function(target,
                                    has_seasonal_comp,
                                    cpi_levels,
                                    output_type) {
  output_node <- xmlNode("Output")

  if (output_type == "string") {
    point_forecast_node <- xmlNode("OutputField", attrs = c(
      name = paste("Predicted_", target, sep = ""),
      optype = "continuous",
      dataType = "string",
      feature = "predictedValue"
    ))

    point_forecast_node <- append.XMLNode(point_forecast_node, .make_ext_json_node())

    output_node <- append.XMLNode(output_node, point_forecast_node)

    if (!has_seasonal_comp) {
      # if model has no seasonal component, include prediction intervals in Output
      for (lev in cpi_levels) {
        output_node <- append.XMLNode(
          output_node,
          .make_pi_node(lev, "Lower", output_type),
          .make_pi_node(lev, "Upper", output_type)
        )
      }
    }
  } else {
    point_forecast_node <- xmlNode("OutputField", attrs = c(
      name = paste("Predicted_", target, sep = ""),
      optype = "continuous",
      dataType = "double",
      feature = "predictedValue"
    ))

    output_node <- append.XMLNode(output_node, point_forecast_node)

    if (!has_seasonal_comp) {
      # if model has no seasonal component, include prediction intervals in Output
      for (lev in cpi_levels) {
        output_node <- append.XMLNode(
          output_node,
          .make_pi_node(lev, "Lower", output_type),
          .make_pi_node(lev, "Upper", output_type)
        )
      }
    }
  }

  return(output_node)
}


.make_ext_json_node <- function() {
  return(xmlNode("Extension", attrs = c(
    extender = "ADAPA",
    name = "dataType",
    value = "json"
  )))
}


.make_pi_node <- function(perc, interv, output_type) {
  # create prediction interval output node
  perc <- toString(perc)

  if (output_type == "string") {
    pi_node <- xmlNode("OutputField", attrs = c(
      name = paste("cpi_", perc, "_", tolower(interv), sep = ""),
      optype = "continuous",
      dataType = "string",
      feature = paste("confidenceInterval", interv, sep = ""),
      value = perc
    ))
    pi_node <- append.XMLNode(pi_node, .make_ext_json_node())
  } else {
    pi_node <- xmlNode("OutputField", attrs = c(
      name = paste("cpi_", perc, "_", tolower(interv), sep = ""),
      optype = "continuous",
      dataType = "double",
      feature = paste("confidenceInterval", interv, sep = ""),
      value = perc
    ))
  }




  return(pi_node)
}


.make_mls_node <- function(model, ts_type) {
  # Creates MaximumLikelihoodStat node to be used with predictionMethod="exactLeastSquares"
  mls_node <- xmlNode("MaximumLikelihoodStat",
    attrs = c(method = "kalman", periodDeficit = "0")
  )

  kalman_state_node <- xmlNode("KalmanState")

  # extension_node <- .make_extension_node(model)

  # trans_m_node <- .make_transition_matrix_node(model)
  # meas_m_node <- .make_measurement_matrix_node(model)


  final_omega_node <- .make_final_omega_node(model)

  final_state_vector <- .make_fs_vector_node(model, ts_type = ts_type)

  # h_vector <- .make_h_vector_node(model)

  trans_m_node <- append.XMLNode(
    xmlNode("TransitionMatrix"),
    .make_matrix_node(model$model$T)
  )

  meas_m_node <- append.XMLNode(
    xmlNode("MeasurementMatrix"),
    .make_matrix_node(matrix(model$model$Z, nrow = 1))
  )

  kalman_state_node <- append.XMLNode(
    kalman_state_node,
    # extension_node,
    final_omega_node,
    final_state_vector,
    # h_vector,
    trans_m_node,
    meas_m_node
  )


  mls_node <- append.XMLNode(mls_node, kalman_state_node)

  return(mls_node)
}


# .make_extension_node <- function(model) {
#   e_node <- xmlNode("Extension", attrs = c(
#     name = "KALMAN_STATE_TYPE",
#     value = "r-pmml",
#     extender = "ADAPA"
#   ))
#
#   trans_matrix <- model$model$T
#   meas_matrix <- matrix(model$model$Z, nrow = 1)
#
#   tm_node <- append.XMLNode(
#     xmlNode("TransitionMatrix"),
#     .make_matrix_node(trans_matrix)
#   )
#
#   mm_node <- append.XMLNode(
#     xmlNode("MeasurementMatrix"),
#     .make_matrix_node(meas_matrix)
#   )
#
#   e_node <- append.XMLNode(e_node, tm_node, mm_node)
#
#   return(e_node)
# }


.make_matrix_node <- function(the_matrix) {
  # create a matrix node given a matrix input
  matrix_node <- xmlNode("Matrix",
    attrs = c(
      nbRows = toString(NROW(the_matrix)),
      nbCols = toString(NCOL(the_matrix))
    )
  )

  for (i in c(1:NROW(the_matrix))) {
    matrix_node <- append.XMLNode(
      matrix_node,
      xmlNode("Array",
        attrs = c(type = "real"),
        value = paste(the_matrix[i, ], collapse = " ")
      )
    )
  }

  return(matrix_node)
}


.make_final_omega_node <- function(model) {
  fo_node <- xmlNode("FinalOmega")

  matrix_node <- append.XMLNode(
    xmlNode("Matrix",
      attrs = c(kind = "symmetric", nbRows = "1", nbCols = "1")
    ),
    xmlNode("Array", attrs = c(type = "real", n = "1"), value = 0)
  )

  fo_node <- append.XMLNode(fo_node, matrix_node)

  return(fo_node)
}


.make_fs_vector_node <- function(model, ts_type) {
  # Create FinalStateVector node or StateVector node, depending on type

  f_matrix <- model$model$T # transition matrix
  s_t0 <- model$model$a # current state estimate
  # s_t1 <- (f_matrix %*% s_t0)[1]
  s_t1 <- (f_matrix %*% s_t0)

  p <- model$arma[1]
  q <- model$arma[2]

  # final_state_vector <- s_t1[1:max(p,q)]
  final_state_vector <- s_t1


  # fsv_node <- xmlNode("FinalStateVector")

  if (ts_type == "arima") {
    fsv_node <- xmlNode("FinalStateVector")
  } else {
    fsv_node <- xmlNode("StateVector")
  }

  fsv_node <- append.XMLNode(
    fsv_node,
    xmlNode("Array",
      attrs = c(
        type = "real",
        n = toString(length(final_state_vector))
      ),
      value = paste(final_state_vector, collapse = " ")
    )
  )

  return(fsv_node)
}


.make_nsc_node <- function(model, exact_least_squares) {
  # Creates NonseasonalComponent node.

  mod_len <- length(model$x)

  # (p,d,q) arrays
  ar_ind <- grep("^ar", names(model$coef))
  phi_array <- unname(model$coef[ar_ind])


  ma_ind <- grep("^ma", names(model$coef))
  theta_array <- unname(model$coef[ma_ind])

  # Reverse sign of theta coefficients for PMML representation.
  # theta_array <- (-1)*theta_array

  ns_p <- model$arma[1]
  ns_d <- model$arma[6]
  ns_q <- model$arma[2]

  mod_resids <- model$residuals

  nsc_node <- xmlNode("NonseasonalComponent",
    attrs = c(p = ns_p, d = ns_d, q = ns_q)
  )
  nsc_node <- .make_arma_nodes(
    c_node = nsc_node, p_val = ns_p, d_val = ns_d,
    q_val = ns_q, phi_array = phi_array, theta_array = theta_array,
    mod_len = mod_len, mod_resids = mod_resids, seas = FALSE,
    ns_q = NA, seas_period = NA, exact_least_squares = exact_least_squares
  )
  return(nsc_node)
}


.make_zero_nsc_node <- function(model, exact_least_squares) {
  # Creates a NonSeasonalComponent node with 0 for pdq values
  nsc_node <- xmlNode("NonseasonalComponent",
    attrs = c(p = 0, d = 0, q = 0)
  )
  return(nsc_node)
}


.make_sc_node <- function(model, exact_least_squares) {
  # Creates SeasonalComponent node.

  num_sma_elements <- length(grep("^sma", names(model$coef))) # deprecated

  mod_len <- length(model$x)

  # (P, D, Q) arrays
  sar_ind <- grep("^sar", names(model$coef))
  s_phi_array <- unname(model$coef[sar_ind])

  sma_ind <- grep("^sma", names(model$coef))
  s_theta_array <- unname(model$coef[sma_ind])

  # Reverse sign of theta coefficients for PMML representation.
  # s_theta_array <- (-1)*s_theta_array

  s_p <- model$arma[3]
  s_d <- model$arma[7]
  s_q <- model$arma[4]
  s_period <- model$arma[5]

  mod_resids <- model$residuals

  sc_node <- xmlNode("SeasonalComponent",
    attrs = c(P = s_p, D = s_d, Q = s_q, period = s_period)
  )
  sc_node <- .make_arma_nodes(
    c_node = sc_node, p_val = s_p, d_val = s_d,
    q_val = s_q, phi_array = s_phi_array, theta_array = s_theta_array,
    mod_len = mod_len, mod_resids = mod_resids,
    seas = TRUE, ns_q = model$arma[2], seas_period = model$arma[5],
    exact_least_squares = exact_least_squares
  )
  return(sc_node)
}


.make_arma_nodes <- function(c_node, p_val, d_val, q_val,
                             phi_array, theta_array, mod_len,
                             mod_resids, seas, ns_q = NA,
                             seas_period = NA,
                             exact_least_squares) {
  # Creates the AR and MA nodes for either non-seasonal or seasonal component nodes.
  # seas: logical indicating if node is seasonal.
  # ns_q: q value for non-seasonal component; only used when creating seasonal MA node.
  # seas_period: period for seasonal component; only used when creating seasonal MA node.

  if (p_val > 0) {
    ar_node <- append.XMLNode(
      xmlNode("AR"),
      xmlNode("Array", attrs = c(type = "real", n = p_val), value = paste(phi_array, collapse = " "))
    )
    c_node <- append.XMLNode(c_node, ar_node)
  }

  if (q_val > 0) {
    ma_coef_node <- append.XMLNode(
      xmlNode("MACoefficients"),
      xmlNode("Array", attrs = c(type = "real", n = q_val), value = paste(theta_array, collapse = " "))
    )

    # Calculate the earliest necessary residual.
    if (seas) {
      resids <- mod_resids[(mod_len - (q_val * seas_period + ns_q) + 1):mod_len]
    } else {
      resids <- mod_resids[(mod_len - q_val + 1):mod_len]
    }

    if (!exact_least_squares) {
      ma_resid_node <- append.XMLNode(
        xmlNode("Residuals"),
        xmlNode("Array",
          attrs = c(
            type = "real",
            n = length(resids)
          ),
          value = paste(resids, collapse = " ")
        )
      )
      ma_node <- append.XMLNode(xmlNode("MA"), ma_coef_node, ma_resid_node)
    } else {
      ma_node <- append.XMLNode(xmlNode("MA"), ma_coef_node)
    }

    c_node <- append.XMLNode(c_node, ma_node)
  }

  return(c_node)
}



.has_seasonal_comp <- function(model) {
  # Checks if model has seasonal component.
  a <- model$arma
  return(a[3] != 0 | a[4] != 0 | a[7] != 0)
}


.has_nonseasonal_comp <- function(model) {
  # Checks if model has nonseasonal component.
  a <- model$arma
  return(a[1] != 0 | a[2] != 0 | a[6] != 0)
}


.make_ts_node <- function(model) {
  # Creates TimeSeries node. Exports the full time series.

  end_time <- length(model$x)
  start_time <- 1

  ts_node <- xmlNode("TimeSeries", attrs = c(
    usage = "original",
    startTime = toString(start_time), endTime = toString(end_time)
  ))

  for (ts_index in c(start_time:end_time)) {
    tv_node <- xmlNode("TimeValue", attrs = c(index = toString(ts_index), value = model$x[ts_index]))
    ts_node <- append.XMLNode(ts_node, tv_node)
  }
  return(ts_node)
}


# .make_time_anchor_node <- function() {
#   # creates TimeAnchor node in TimeSeries
#   time_anchor_node <- xmlNode("TimeAnchor")
#   return(time_anchor_node)
# }
