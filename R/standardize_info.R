#' Get Standardization Information
#'
#' This function extracts information, such as the deviations (SD or MAD) from parent variables, that are necessary for post-hoc standardization of parameters.
#'
#' @inheritParams parameters_standardize
#'
#'
#' @examples
#' model <- lm(Sepal.Width ~ Sepal.Length + Species, data = iris)
#' standardize_info(model)
#'
#'
#' @export
standardize_info <- function(model, robust = FALSE, ...) {

  params <- insight::find_parameters(model, effects = "fixed", flatten = TRUE, ...)
  types <- parameters_type(model)
  model_matrix = as.data.frame(stats::model.matrix(model))
  data <- insight::get_data(model)

  out <- data.frame(
    Parameter = params,
    Type = types$Type,
    Factor = types$Factor,
    stringsAsFactors = FALSE)

  # Response
  response <- .std_info_response(model, robust = robust)
  out$Deviation_Response <- response$sd
  out$Mean_Response <- response$mean

  # Classic
  out <- merge(out,
               .std_info_predictors_classic(model_matrix, types, robust = robust))

  # Smart
  out <- merge(out,
               .std_info_predictors_smart(data, params, types, robust = robust))

  # Reorder
  out <- out[match(params, out$Parameter), ]
  row.names(out) <- NULL

  out
}




# Predictors - Smart ------------------------------------------------------------


#' @keywords internal
.std_info_predictors_smart <- function(data, params, types, robust = FALSE, ...) {

  # Get deviations for all parameters
  deviations <- c()
  means <- c()
  for(var in params){
    info <-  .std_info_predictor_smart(data = data,
                                        variable = types[types$Parameter == var, "Variable"],
                                        type = types[types$Parameter == var, "Type"],
                                        robust = robust
    )
    deviations <- c(deviations, info$sd)
    means <- c(means, info$mean)
  }

  # Out
  data.frame(Parameter = params,
             Deviation_Smart = deviations,
             Mean_Smart = means)
}



#' @keywords internal
.std_info_predictor_smart <- function(data, variable, type, robust = FALSE, ...) {

  if (type == "intercept") {
    info <- list(sd = 0, mean = 0)
  } else if (type == "numeric") {
    info <- .compute_std_info(data, variable, robust)
  } else if (type == "factor") {
    info <- list(sd = 1, mean = 0)

    # TO BE IMPROVED: Adjust if involved in interactions
    # interactions <- types[types$Type %in% c("interaction"), ]
    # if(variable %in% interactions$Secondary_Variable){
    #   interac_var <- unique(interactions[interactions$Secondary_Variable == variable, "Variable"])
    #   for(i in interac_var){
    #     if(types[types$Parameter == i, "Type"] == "numeric"){
    #       sd_x <- sd_x * .get_deviation(data, i, robust)
    #     }
    #   }
    # }

  } else if (type %in% c("interaction", "nested")) {
    if (is.numeric(data[, variable])) {
      info <- .compute_std_info(data, variable, robust)
    } else if (is.factor(data[, variable])) {
      info <- list(sd = 1, mean = 0)
    } else {
      info <- list(sd = 1, mean = 0)
    }
  } else {
    info <- list(sd = 1, mean = 0)
  }

  list(sd = info$sd, mean = info$mean)
}


# Predictors - Classic ------------------------------------------------------------


#' @keywords internal
.std_info_predictors_classic <- function(model_matrix, types, robust = FALSE, ...) {

  # Get deviations for all parameters
  deviations <- c()
  means <- c()
  for(var in names(model_matrix)){
    if(types[types$Parameter == var, "Type"] == "intercept"){
      deviations <- c(deviations, 0)
      means <- c(means, 0)
    } else{
      std_info <- .compute_std_info(model_matrix, var, robust)
      deviations <- c(deviations, std_info$sd)
      means <- c(means, std_info$mean)
    }
  }

  # Out
  data.frame(Parameter = names(model_matrix),
             Deviation_Classic = deviations,
             Mean_Classic = means)
}



#' @keywords internal
.compute_std_info <- function(data, variable, robust = FALSE) {
  if (robust == FALSE) {
    sd_x <- stats::sd(as.numeric(data[, variable]))
    mean_x <- mean(as.numeric(data[, variable]))
  } else {
    sd_x <- stats::mad(as.numeric(data[, variable]))
    mean_x <- stats::median(as.numeric(data[, variable]))
  }

  list(sd = sd_x, mean = mean_x)
}



# Response ------------------------------------------------------------


#' @keywords internal
.std_info_response <- function(model, robust = FALSE, ...) {
  info <- insight::model_info(model)
  response <- insight::get_response(model)

  if (info$is_linear) {
    if (robust == FALSE) {
      sd_y <- stats::sd(response)
      mean_y <- mean(response)
    } else {
      sd_y <- stats::mad(response)
      mean_y <- stats::median(response)
    }
  } else {
    sd_y <- 1
    mean_y <- 0
  }

  list(sd = sd_y, mean = mean_y)
}


