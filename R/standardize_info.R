#' Get Standardization Information
#'
#' This function extracts information, such as the deviations (SD or MAD) from
#' parent variables, that are necessary for post-hoc standardization of
#' parameters. This function gives a window on how standardized are obtained,
#' i.e., by what they are divided. The "basic" method of standardization uses.
#'
#' @inheritParams standardize_parameters
#' @param include_pseudo (For (G)LMMs) Should Pseudo-standardized information be
#'   included?
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame with information on each parameter (see
#'   [parameters_type()]), and various standardization coefficients
#'   for the post-hoc methods (see [standardize_parameters()]) for the predictor
#'   and the response.
#'
#' @family standardize
#'
#' @examples
#' model <- lm(mpg ~ ., data = mtcars)
#' standardize_info(model)
#' standardize_info(model, robust = TRUE)
#' standardize_info(model, two_sd = TRUE)
#' @aliases standardise_info
#' @export
standardize_info <- function(model, ...) {
  UseMethod("standardize_info")
}

#' @export
standardise_info <- standardize_info

#' @rdname standardize_info
#' @export
standardize_info.default <- function(model,
                                     robust = FALSE,
                                     two_sd = FALSE,
                                     include_pseudo = FALSE,
                                     verbose = TRUE,
                                     ...) {
  # check for valid input
  .is_model_valid(model)

  mi <- .get_model_info(model, ...)

  params <- if (inherits(model, c("glmmTMB", "MixMod"))) {
    insight::find_parameters(model, effects = "fixed", component = "conditional", flatten = TRUE, ...)
  } else {
    insight::find_parameters(model, effects = "fixed", flatten = TRUE, ...)
  }
  types <- parameters_type(model)
  # model_matrix <- as.data.frame(stats::model.matrix(model))
  model_matrix <- as.data.frame(insight::get_modelmatrix(model))
  model_data <- insight::get_data(model, source = "mf", verbose = FALSE)
  wgts <- insight::get_weights(model, na_rm = TRUE)

  # validation check for ZI
  if (mi$is_zero_inflated && verbose) {
    insight::format_alert(
      "Non-refit parameter standardization is ignoring the zero-inflation component."
    )
    # would need to also get the binomial model matrix...
  }

  # validation check for glmmTMB with dispersion
  if (length(params) != nrow(types)) {
    types <- types[types$Parameter %in% params, ]
  }

  out <- data.frame(
    Parameter = params,
    Type = types$Type,
    Link = types$Link,
    Secondary_Parameter = types$Secondary_Parameter,
    stringsAsFactors = FALSE
  )

  # Type of effect size
  out$EffectSize_Type <- ifelse(types$Type == "interaction", "interaction",
    ifelse(types$Link == "Association", "r", # nolint
      ifelse(types$Link == "Difference", "d", NA) # nolint
    )
  )


  # Response - Basic
  out <- merge(
    out,
    .std_info_response_basic(model, mi, params, robust = robust, w = wgts),
    by = "Parameter", all = TRUE
  )

  # Response - Smart
  out <- merge(
    out,
    .std_info_response_smart(model, mi, data = model_data, model_matrix, types, robust = robust, w = wgts),
    by = "Parameter", all = TRUE
  )

  # Basic
  out <- merge(
    out,
    .std_info_predictors_basic(model, model_matrix, types, robust = robust, two_sd = two_sd, w = wgts),
    by = "Parameter", all = TRUE
  )

  # Smart
  out <- merge(
    out,
    .std_info_predictors_smart(model,
      data = model_data,
      params,
      types,
      robust = robust,
      two_sd = two_sd,
      w = wgts
    ),
    by = "Parameter", all = TRUE
  )

  # sdy (see Mood 2009, 10.1093/esr/jcp006)
  out <- merge(
    out,
    .std_info_predictors_sdy(model, model_matrix, types, robust = robust, two_sd = two_sd, w = wgts),
    by = "Parameter", all = TRUE
  )

  # Pseudo (for LMM)
  if (include_pseudo && mi$is_mixed && length(insight::find_random(model)$random) == 1L) {
    out <- merge(
      out,
      .std_info_pseudo(
        model, mi,
        params,
        model_matrix,
        data = model_data,
        types = types$Type,
        robust = robust,
        two_sd = two_sd,
        verbose = verbose
      )
    )
  }

  # Reorder
  out <- out[match(params, out$Parameter), ]
  out$Parameter <- params
  row.names(out) <- NULL

  # Remove all means for now (because it's not used)
  out <- out[!grepl("Mean_", names(out), fixed = TRUE)]

  # Select only desired columns
  # if(method == "all") method <- c("smart", "basic")
  # if(!any(method == "smart")){
  #   out <- out[!grepl("_Smart", names(out))]
  # }
  # if(!any(method == "basic")){
  #   out <- out[!grepl("_Basic", names(out))]
  # }

  out
}




# Predictors - Smart ------------------------------------------------------------


#' @keywords internal
.std_info_predictors_smart <- function(model,
                                       data,
                                       params,
                                       types,
                                       robust = FALSE,
                                       two_sd = FALSE,
                                       w = NULL,
                                       ...) {
  # Get deviations for all parameters
  means <- deviations <- rep(NA_real_, times = length(params))
  for (i in seq_along(params)) {
    variable <- params[i]
    info <- .std_info_predictor_smart(
      data = data,
      variable = types[types$Parameter == variable, "Variable"],
      type = types[types$Parameter == variable, "Type"],
      robust = robust,
      two_sd = two_sd,
      weights = w
    )
    deviations[i] <- info$sd
    means[i] <- info$mean
  }

  # Out
  data.frame(
    Parameter = params,
    Deviation_Smart = deviations,
    Mean_Smart = means,
    stringsAsFactors = FALSE
  )
}



#' @keywords internal
.std_info_predictor_smart <- function(data,
                                      variable,
                                      type,
                                      robust = FALSE,
                                      two_sd = FALSE,
                                      weights = NULL,
                                      ...) {
  if (type == "intercept") { # nolint
    info <- list(sd = 0, mean = 0)
  } else if (type == "numeric") {
    info <- .compute_std_info(
      data = data,
      variable = variable,
      robust = robust,
      two_sd = two_sd,
      weights = weights
    )
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
      info <- .compute_std_info(
        data = data,
        variable = variable,
        robust = robust,
        two_sd = two_sd,
        weights = weights
      )
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


# Predictors - Basic ------------------------------------------------------------


#' @keywords internal
.std_info_predictors_basic <- function(model,
                                       model_matrix,
                                       types,
                                       robust = FALSE,
                                       two_sd = FALSE,
                                       w = NULL,
                                       ...) {
  # Get deviations for all parameters
  means <- deviations <- rep(NA_real_, length = length(names(model_matrix)))
  for (i in seq_along(names(model_matrix))) {
    variable <- names(model_matrix)[i]
    if (types[i, "Type"] == "intercept") {
      means[i] <- deviations[i] <- 0
    } else {
      std_info <- .compute_std_info(
        data = model_matrix, variable = variable,
        robust = robust, two_sd = two_sd, weights = w
      )
      deviations[i] <- std_info$sd
      means[i] <- std_info$mean
    }
  }

  # Out
  data.frame(
    Parameter = types$Parameter[seq_along(names(model_matrix))],
    Deviation_Basic = deviations,
    Mean_Basic = means,
    stringsAsFactors = FALSE
  )
}





# Predictors - sdy ------------------------------------------------------------


#' @keywords internal
.std_info_predictors_sdy <- function(model,
                                     model_matrix,
                                     types,
                                     ...) {
  deviations <- NA_real_
  # fitted values
  fitted_values <- .safe(stats::fitted(model))
  if (!is.null(fitted_values)) {
    deviations <- 1 / sum(c(stats::sd(fitted_values), sqrt(pi^2 / 3)))
  }

  # Out
  data.frame(
    Parameter = types$Parameter[seq_along(names(model_matrix))],
    Deviation_SDy = deviations,
    stringsAsFactors = FALSE
  )
}





# Response ------------------------------------------------------------

#' @keywords internal
.std_info_response_smart <- function(model, info, data, model_matrix, types, robust = FALSE, w = NULL, ...) {
  if (info$is_linear) {
    if (inherits(model, c("gls", "lme"))) {
      response <- insight::get_response(model)
    } else {
      response <- stats::model.frame(model)[[1]]
    }
    means <- deviations <- rep(NA_real_, length = length(names(model_matrix)))
    for (i in seq_along(names(model_matrix))) {
      variable <- names(model_matrix)[i]
      if (any(types$Parameter == variable) && types$Link[types$Parameter == variable] == "Difference") {
        parent_var <- types$Variable[types$Parameter == variable]
        intercept <- unique(data[[parent_var]])[1]
        response_at_intercept <- response[data[[parent_var]] == intercept]
        weights_at_intercept <- if (length(w)) w[data[[parent_var]] == intercept] else NULL

        std_info <- .compute_std_info(
          response = response_at_intercept,
          robust = robust, weights = weights_at_intercept
        )
      } else {
        std_info <- .compute_std_info(
          response = response,
          robust = robust, weights = w
        )
      }
      deviations[i] <- std_info$sd
      means[i] <- std_info$mean
    }
  } else {
    deviations <- 1
    means <- 0
  }

  # Out
  data.frame(
    Parameter = types$Parameter[seq_along(names(model_matrix))],
    Deviation_Response_Smart = deviations,
    Mean_Response_Smart = means,
    stringsAsFactors = FALSE
  )
}



#' @keywords internal
.std_info_response_basic <- function(model, info, params, robust = FALSE, w = NULL, ...) {
  if (inherits(model, c("gls", "lme"))) {
    response <- insight::get_response(model)
  } else {
    response <- stats::model.frame(model)[[1]]
  }

  if (info$is_linear) {
    if (robust) {
      sd_y <- datawizard::weighted_mad(response, w)
      mean_y <- datawizard::weighted_median(response, w)
    } else {
      sd_y <- datawizard::weighted_sd(response, w)
      mean_y <- datawizard::weighted_mean(response, w)
    }
  } else {
    sd_y <- 1
    mean_y <- 0
  }

  # Out
  data.frame(
    Parameter = params,
    Deviation_Response_Basic = sd_y,
    Mean_Response_Basic = mean_y,
    stringsAsFactors = FALSE
  )
}



# Pseudo (GLMM) -----------------------------------------------------------

.std_info_pseudo <- function(model,
                             mi,
                             params,
                             model_matrix,
                             data,
                             types,
                             robust = FALSE,
                             two_sd = FALSE,
                             verbose = verbose,
                             ...) {
  if (robust && verbose) {
    insight::format_alert("`robust` standardization not available for `pseudo` method.")
  }

  insight::check_if_installed("performance")

  f <- if (two_sd) 2 else 1

  within_vars <- unclass(performance::check_heterogeneity_bias(model))
  id <- insight::get_random(model)[[1]]
  w <- insight::get_weights(model, na_rm = TRUE)

  ## Find which parameters vary on level 1 ("within")
  is_within <- logical(length = length(params))
  is_within[] <- NA
  for (i in seq_along(params)) {
    if (types[i] == "intercept") { # nolint
      is_within[i] <- FALSE
    } else if (types[i] == "numeric") {
      is_within[i] <- insight::clean_names(params[i]) %in% within_vars
    } else if (types[i] == "factor") {
      is_within[i] <- any(sapply(paste0("^", within_vars), grepl, insight::clean_names(params[i])))
    } else if (types[i] == "interaction") {
      ints <- unlist(strsplit(params[i], ":", fixed = TRUE))
      is_within[i] <- any(sapply(ints, function(int) {
        int <- insight::clean_names(int)
        int %in% within_vars | # numeric
          any(sapply(paste0("^", within_vars), grepl, int)) # factor
      }))
    }
  }

  ## test "within"s are fully "within"
  # only relevant to numeric predictors that can have variance
  check_within <- is_within & types == "numeric"
  if (any(check_within)) {
    p_check_within <- params[check_within]
    temp_d <- data.frame(model_matrix[, p_check_within, drop = FALSE])
    colnames(temp_d) <- paste0("W", seq_len(ncol(temp_d))) # overwrite because can't deal with ":"

    dm <- datawizard::demean(cbind(id, temp_d),
      select = colnames(temp_d),
      by = "id"
    )
    dm <- dm[, paste0(colnames(temp_d), "_between"), drop = FALSE]

    has_lvl2_var <- sapply(seq_along(colnames(temp_d)), function(i) {
      # If more than 1% of the variance in the within-var is between:
      stats::var(dm[, i]) / stats::var(temp_d[, i])
    }) > 0.01
    also_between <- p_check_within[has_lvl2_var]

    if (length(also_between) && verbose) {
      insight::format_alert(
        "The following within-group terms have between-group variance:",
        toString(also_between),
        "This can inflate standardized within-group parameters associated with these terms.",
        "See `help(\"demean\", package = \"datawizard\")` for modeling between- and within-subject effects."
      )
    }
  }


  ## Get 2 types of Deviation_Response_Pseudo
  sd_y_within <- sd_y_between <- 1
  if (mi$is_linear) {
    insight::check_if_installed("lme4")

    rand_name <- insight::find_random(model)$random

    # maintain any y-transformations
    frm <- insight::find_formula(model)
    frm <- paste0(frm$conditional[2], " ~ (1|", rand_name, ")")

    m0 <- suppressWarnings(suppressMessages(
      lme4::lmer(stats::as.formula(frm),
        weights = w,
        data = data
      )
    ))
    m0v <- insight::get_variance(m0)

    sd_y_between <- unname(sqrt(m0v$var.intercept))
    sd_y_within <- unname(sqrt(m0v$var.residual))
  }


  ## Get scaling factors for each parameter
  Deviation_Response_Pseudo <- Deviation_Pseudo <- numeric(ncol(model_matrix))
  for (i in seq_along(params)) {
    if (types[i] == "intercept") {
      Deviation_Response_Pseudo[i] <- sd_y_between # doesn't matter
      Deviation_Pseudo[i] <- 0
    } else {
      ## dumb way
      if (is_within[i]) {
        ## is within
        X <- model_matrix[[i]]
        Deviation_Response_Pseudo[i] <- sd_y_within
      } else {
        ## is between
        X <- tapply(model_matrix[[i]], id, mean)
        Deviation_Response_Pseudo[i] <- sd_y_between
      }
      Deviation_Pseudo[i] <- f * datawizard::weighted_sd(X, w)

      ## smart way?
      ## DONT USE: see correspondence with between Mattan and Eran BC
      # m <- suppressWarnings(suppressMessages(lme4::lmer(model_matrix[[i]] ~ (1|id))))
      # if (is_within[i]) {
      #   ## is within
      #   Deviation_Pseudo[i] <- sqrt(unname(unlist(suppressWarnings(
      #     insight::get_variance(m, component = "residual")
      #   ))))
      #   Deviation_Response_Pseudo[i] <- sd_y_within
      # } else {
      #   ## is between
      #   Deviation_Pseudo[i] <- sqrt(unname(unlist(suppressWarnings(
      #     insight::get_variance(m, component = "intercept")
      #   ))))
      #   Deviation_Response_Pseudo[i] <- sd_y_between
      # }
    }
  }

  data.frame(
    Parameter = params,
    Deviation_Response_Pseudo,
    Deviation_Pseudo,
    stringsAsFactors = FALSE
  )
}



# Utils -------------------------------------------------------------------


#' @keywords internal
.compute_std_info <- function(data = NULL,
                              variable = NULL,
                              response = NULL,
                              robust = FALSE,
                              two_sd = FALSE,
                              weights = NULL) {
  f <- if (two_sd) 2 else 1
  if (is.null(response)) {
    response <- as.numeric(data[, variable])
  }

  if (robust) {
    sd_x <- datawizard::weighted_mad(response, weights)
    mean_x <- datawizard::weighted_median(response, weights)
  } else {
    sd_x <- datawizard::weighted_sd(response, weights)
    mean_x <- datawizard::weighted_mean(response, weights)
  }

  list(sd = f * sd_x, mean = mean_x)
}
