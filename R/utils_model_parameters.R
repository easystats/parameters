# This function add meta-information to the returned parameters data frame,
# usually used for printing etc.

#' @keywords internal
.add_model_parameters_attributes <- function(params,
                                             model,
                                             ci,
                                             exponentiate = FALSE,
                                             bootstrap = FALSE,
                                             iterations = 1000,
                                             ci_method = NULL,
                                             p_adjust = NULL,
                                             include_info = FALSE,
                                             verbose = TRUE,
                                             group_level = FALSE,
                                             wb_component = FALSE,
                                             ...) {
  # capture additional arguments
  dot.arguments <- list(...)

  # model info
  info <- .safe(suppressWarnings(insight::model_info(model, verbose = FALSE)))

  if (is.null(info)) {
    info <- list(family = "unknown", link_function = "unknown")
  }

  # for simplicity, we just use the model information from the first formula
  # when we have multivariate response models...
  if (insight::is_multivariate(model) && !"is_zero_inflated" %in% names(info) && !inherits(model, c("vgam", "vglm"))) {
    info <- info[[1]]
  }

  # add regular attributes
  if (isFALSE(dot.arguments$pretty_names)) {
    attr(params, "pretty_names") <- params$Parameter
  } else if (is.null(attr(params, "pretty_names", exact = TRUE))) {
    attr(params, "pretty_names") <- suppressWarnings(format_parameters(model, model_info = info, ...))
  }

  attr(params, "ci") <- ci
  attr(params, "ci_method") <- .format_ci_method_name(ci_method)
  attr(params, "df_method") <- .format_ci_method_name(ci_method)
  attr(params, "verbose") <- verbose
  attr(params, "exponentiate") <- exponentiate
  attr(params, "ordinal_model") <- isTRUE(info$is_ordinal) | isTRUE(info$is_multinomial)
  attr(params, "linear_model") <- isTRUE(info$is_linear)
  attr(params, "mixed_model") <- isTRUE(info$is_mixed)
  attr(params, "n_obs") <- info$n_obs
  attr(params, "model_class") <- as.character(class(model))
  attr(params, "bootstrap") <- bootstrap
  attr(params, "iterations") <- iterations
  attr(params, "p_adjust") <- p_adjust
  attr(params, "robust_vcov") <- "vcov" %in% names(list(...))
  attr(params, "ignore_group") <- isFALSE(group_level)
  attr(params, "ran_pars") <- isFALSE(group_level)
  attr(params, "show_summary") <- isTRUE(include_info)
  attr(params, "log_link") <- isTRUE(grepl("log", info$link_function, fixed = TRUE))
  attr(params, "logit_link") <- isTRUE(identical(info$link_function, "logit"))

  # save model call
  attr(params, "model_call") <- .safe(insight::get_call(model))

  # use tryCatch, these might fail...
  attr(params, "test_statistic") <- .safe(insight::find_statistic(model))
  attr(params, "log_response") <- .safe(isTRUE(grepl("log", insight::find_transformation(model), fixed = TRUE)))
  attr(params, "log_predictors") <- .safe(any(grepl("log", unlist(insight::find_terms(model)[c("conditional", "zero_inflated", "instruments")]), fixed = TRUE))) # nolint

  # save if model is multivariate response model
  if (isTRUE(info$is_multivariate)) {
    attr(params, "multivariate_response") <- TRUE
  }

  # if we have a complex random-within-between model, don't show first title element
  if (isTRUE(wb_component) && !is.null(params$Component) && any(c("within", "between") %in% params$Component)) {
    attr(params, "no_caption") <- TRUE
  }


  # for additional infos, add R2, RMSE
  if (isTRUE(include_info) && requireNamespace("performance", quietly = TRUE)) {
    rsq <- .safe(suppressWarnings(performance::r2(model)))
    attr(params, "r2") <- rsq
    rmse <- .safe(performance::performance_rmse(model))
    attr(params, "rmse") <- rmse
  }


  # Models for which titles should be removed - here we add exceptions for
  # objects that should not have a table headline like "# Fixed Effects", when
  # there is nothing else than fixed effects (redundant title)
  if (inherits(model, c(
    "mediate", "emmGrid", "emm_list", "summary_emm", "lm", "averaging",
    "glm", "coxph", "bfsl", "deltaMethod", "phylolm", "phyloglm"
  ))) {
    attr(params, "no_caption") <- TRUE
    attr(params, "title") <- ""
  }


  # weighted nobs
  weighted_nobs <- .safe({
    w <- insight::get_weights(model, remove_na = TRUE, null_as_ones = TRUE)
    round(sum(w))
  })
  attr(params, "weighted_nobs") <- weighted_nobs


  # model formula
  model_formula <- .safe(insight::safe_deparse(insight::find_formula(model)$conditional))
  attr(params, "model_formula") <- model_formula


  # column name for coefficients - for emm_list, we can have
  # multiple different names for the parameter column. for other
  # models, check whether we have coefficient, odds ratios, IRR etc.
  if (inherits(model, "emm_list")) {
    coef_col1 <- .find_coefficient_type(info, exponentiate, model[[1]])
    coef_col2 <- .find_coefficient_type(info, exponentiate, model[[2]])
    attr(params, "coefficient_name") <- coef_col1
    attr(params, "coefficient_name2") <- coef_col2
  } else {
    coef_col <- .find_coefficient_type(info, exponentiate, model)
    attr(params, "coefficient_name") <- coef_col
    attr(params, "zi_coefficient_name") <- if (isTRUE(exponentiate)) {
      "Odds Ratio"
    } else {
      "Log-Odds"
    }
  }


  # special handling for meta analysis. we need additional
  # information about study weights
  if (inherits(model, c("rma", "rma.uni"))) {
    rma_data <- .safe(insight::get_data(model, verbose = FALSE))
    attr(params, "data") <- rma_data
    attr(params, "study_weights") <- 1 / model$vi
  }


  # special handling for meta analysis again, but these objects save the
  # inverse weighting information in a different column.
  if (inherits(model, c("meta_random", "meta_fixed", "meta_bma"))) {
    rma_data <- .safe(insight::get_data(model, verbose = FALSE))
    attr(params, "data") <- rma_data
    attr(params, "study_weights") <- 1 / params$SE^2
  }

  # should coefficients be grouped?
  if ("groups" %in% names(dot.arguments)) {
    attr(params, "coef_groups") <- dot.arguments[["groups"]]
  }

  # now comes all the digits stuff...
  if ("digits" %in% names(dot.arguments)) {
    attr(params, "digits") <- dot.arguments[["digits"]]
  } else {
    attr(params, "digits") <- 2
  }

  if ("ci_digits" %in% names(dot.arguments)) {
    attr(params, "ci_digits") <- dot.arguments[["ci_digits"]]
  } else {
    attr(params, "ci_digits") <- NULL
  }

  if ("p_digits" %in% names(dot.arguments)) {
    attr(params, "p_digits") <- dot.arguments[["p_digits"]]
  } else {
    attr(params, "p_digits") <- 3
  }

  if ("footer_digits" %in% names(dot.arguments)) {
    attr(params, "footer_digits") <- dot.arguments[["footer_digits"]]
  } else {
    attr(params, "footer_digits") <- 3
  }

  if ("s_value" %in% names(dot.arguments)) {
    attr(params, "s_value") <- dot.arguments[["s_value"]]
  }

  # pd?
  if (isTRUE(dot.arguments[["pd"]]) && !is.null(params[["p"]])) {
    params$pd <- bayestestR::p_to_pd(params[["p"]])
  }

  # add CI, and reorder
  if (!"CI" %in% colnames(params) && length(ci) == 1) {
    params$CI <- ci
    ci_pos <- grep("CI_low", colnames(params), fixed = TRUE)
    if (length(ci_pos)) {
      if (length(ci_pos) > 1) {
        ci_pos <- ci_pos[1]
      }
      a <- attributes(params)
      params <- params[c(1:(ci_pos - 1), ncol(params), ci_pos:(ncol(params) - 1))]
      attributes(params) <- utils::modifyList(a, attributes(params))
    }
  }

  # include reference level?
  if (isTRUE(dot.arguments[["include_reference"]])) {
    a <- attributes(params)
    params <- .safe(.add_reference_level(params, model), params)
    attributes(params) <- utils::modifyList(a, attributes(params))
  }

  # add parameters with value and variable
  attr(params, "pretty_labels") <- .format_value_labels(params, model)

  row.names(params) <- NULL
  params
}


#' Format CI method name when stored as an attribute
#'
#' @keywords internal
#' @noRd
.format_ci_method_name <- function(ci_method) {
  if (is.null(ci_method)) {
    return(NULL)
  }

  switch(tolower(ci_method),
    # abbreviations
    eti = ,
    hdi = ,
    si = toupper(ci_method),
    # named after people
    satterthwaite = ,
    kenward = ,
    wald = insight::format_capitalize(ci_method),
    # special cases
    bci = ,
    bcai = "BCa",
    # no change otherwise
    ci_method
  )
}

.find_coefficient_type <- function(info, exponentiate, model = NULL) {
  # column name for coefficients
  coef_col <- "Coefficient"
  if (!is.null(model) && inherits(model, "emmGrid")) {
    s <- summary(model)
    name <- attributes(s)$estName
    if (!is.null(name)) {
      coef_col <- switch(name,
        prob       = "Probability",
        odds.ratio = "Odds Ratio",
        emmean     = "Marginal Means",
        rate       = "Estimated Counts",
        ratio      = "Ratio",
        "Coefficient"
      )
    }
  } else if (!is.null(info) && info$family != "unknown") {
    if (isTRUE(exponentiate)) {
      if (info$is_exponential && identical(info$link_function, "log")) {
        coef_col <- "Prevalence Ratio"
      } else if ((info$is_binomial && info$is_logit) || info$is_ordinal || info$is_multinomial || info$is_categorical) {
        coef_col <- "Odds Ratio"
      } else if (info$is_binomial && !info$is_logit) {
        if (info$link_function == "identity") {
          coef_col <- "Exp. Risk"
        } else {
          coef_col <- "Risk Ratio"
        }
      } else if (info$is_count) {
        coef_col <- "IRR"
      }
    } else if (info$is_exponential && identical(info$link_function, "log")) {
      coef_col <- "Log-Prevalence"
    } else if ((info$is_binomial && info$is_logit) || info$is_ordinal || info$is_multinomial || info$is_categorical) {
      coef_col <- "Log-Odds"
    } else if (info$is_binomial && !info$is_logit) {
      if (info$link_function == "identity") {
        coef_col <- "Risk"
      } else {
        coef_col <- "Log-Risk"
      }
    } else if (info$is_count) {
      coef_col <- "Log-Mean"
    }
  }

  coef_col
}


.is_valid_exponentiate_argument <- function(exponentiate) {
  isTRUE(exponentiate) || identical(exponentiate, "nongaussian")
}


#' @keywords internal
.exponentiate_parameters <- function(params, model = NULL, exponentiate = TRUE) {
  # "exponentiate" must be
  # - TRUE, will always exponentiate all coefficients
  # - "nongaussian", will exponentiate all coefficients for models with non-gaussian family
  if (!.is_valid_exponentiate_argument(exponentiate)) {
    return(params)
  }

  # check if non-gaussian applies
  if (!is.null(model) && insight::model_info(model, verbose = FALSE)$is_linear &&
    identical(exponentiate, "nongaussian")) {
    return(params)
  }

  # pattern for marginaleffects objects
  if (is.null(attr(params, "coefficient_name"))) {
    pattern <- "^(Coefficient|Mean|Median|MAP|Std_Coefficient|CI_|Std_CI)"
  } else {
    pattern <- sprintf(
      "^(Coefficient|Mean|Median|MAP|Std_Coefficient|%s|CI_|Std_CI)",
      attr(params, "coefficient_name")
    )
  }

  columns <- grepl(pattern = pattern, colnames(params))
  if (any(columns)) {
    if (inherits(model, "mvord")) {
      rows <- params$Component != "correlation"
    } else if (is.null(params$Component)) {
      # don't exponentiate dispersion
      rows <- seq_len(nrow(params))
    } else if (inherits(model, c("clm", "clm2", "clmm"))) {
      ## TODO: make sure we catch all ordinal models properly here
      rows <- !tolower(params$Component) %in% c("location", "scale")
    } else {
      rows <- !tolower(params$Component) %in% c("dispersion", "residual")
    }
    params[rows, columns] <- exp(params[rows, columns])
    if (all(c("Coefficient", "SE") %in% names(params))) {
      params$SE[rows] <- params$Coefficient[rows] * params$SE[rows]
    }
  }
  params
}




.add_pretty_names <- function(params, model) {
  attr(params, "model_class") <- class(model)
  cp <- insight::clean_parameters(model)
  clean_params <- cp[cp$Parameter %in% params$Parameter, ]

  named_clean_params <- stats::setNames(
    clean_params$Cleaned_Parameter[match(params$Parameter, clean_params$Parameter)],
    params$Parameter
  )

  # add Group variable
  if (!is.null(clean_params$Group) && any(nzchar(clean_params$Group, keepNA = TRUE))) {
    params$Group <- .safe(gsub("(.*): (.*)", "\\2", clean_params$Group))
  }

  attr(params, "cleaned_parameters") <- named_clean_params
  attr(params, "pretty_names") <- named_clean_params

  params
}




#' @keywords internal
.add_anova_attributes <- function(params, model, ci, test = NULL, alternative = NULL, ...) {
  dot.arguments <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x) # nolint

  attr(params, "ci") <- ci
  attr(params, "model_class") <- class(model)
  attr(params, "anova_type") <- .anova_type(model)
  attr(params, "text_alternative") <- .anova_alternative(params, alternative)

  if (inherits(model, "Anova.mlm") && !identical(test, "univariate")) {
    attr(params, "anova_test") <- model$test
  }

  # some tweaks for MANOVA, so outputs of manova(model) and car::Manova(model)
  # look the same, see #833
  if (inherits(model, "maov") && is.null(test) && "Pillai" %in% names(params)) {
    attr(params, "anova_test") <- "Pillai"
    names(params)[names(params) == "Pillai"] <- "Statistic"
  }

  # here we add exception for objects that should not have a table headline
  if (inherits(model, c("aov", "anova", "lm"))) {
    attr(params, "title") <- ""
  }

  if ("digits" %in% names(dot.arguments)) {
    attr(params, "digits") <- eval(dot.arguments[["digits"]])
  } else {
    attr(params, "digits") <- 2
  }

  if ("ci_digits" %in% names(dot.arguments)) {
    attr(params, "ci_digits") <- eval(dot.arguments[["ci_digits"]])
  } else {
    attr(params, "ci_digits") <- NULL
  }

  if ("p_digits" %in% names(dot.arguments)) {
    attr(params, "p_digits") <- eval(dot.arguments[["p_digits"]])
  } else {
    attr(params, "p_digits") <- 3
  }

  if ("s_value" %in% names(dot.arguments)) {
    attr(params, "s_value") <- eval(dot.arguments[["s_value"]])
  }

  params
}



.additional_arguments <- function(x, value, default) {
  add_args <- attributes(x)$additional_arguments

  if (length(add_args) > 0 && value %in% names(add_args)) {
    out <- add_args[[value]]
  } else {
    out <- attributes(x)[[value]]
  }

  if (is.null(out)) {
    out <- default
  }

  out
}



# checks for valid inputs in model_parameters(). E.g., some models don't support
# the "vcov" argument - this should not be silently ignored, but rather the user
# should be informed that robust SE are not available for that model.

.check_dots <- function(dots, not_allowed, model_class, function_name = "model_parameters", verbose = TRUE) {
  # remove arguments that are NULL
  dots <- insight::compact_list(dots)

  # return if no args
  if (!length(dots) || is.null(dots)) {
    return(NULL)
  }

  not_allowed <- not_allowed[which(not_allowed %in% names(dots))]
  if (length(not_allowed)) {
    if (verbose) {
      not_allowed_string <- datawizard::text_concatenate(not_allowed, enclose = "\"")
      insight::format_alert(
        sprintf("Following arguments are not supported in `%s()` for models of class `%s` and will be ignored: %s", function_name, model_class, not_allowed_string), # nolint
        sprintf("Please run `%s()` again without specifying the above mentioned arguments to obtain expected results.", function_name) # nolint
      )
    }
    dots[not_allowed] <- NULL
    if (!length(dots)) {
      dots <- NULL
    }
  }
  dots
}



# functions to check if necessary default argument was provided ------------

.is_model_valid <- function(model) {
  if (missing(model) || is.null(model)) {
    insight::format_error(
      "You must provide a model-object. Argument `model` cannot be missing or `NULL`."
    )
  }
}
