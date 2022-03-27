#' @keywords internal
.add_model_parameters_attributes <- function(params,
                                             model,
                                             ci,
                                             exponentiate = FALSE,
                                             bootstrap = FALSE,
                                             iterations = 1000,
                                             ci_method = NULL,
                                             p_adjust = NULL,
                                             summary = FALSE,
                                             verbose = TRUE,
                                             group_level = FALSE,
                                             wb_component = FALSE,
                                             ...) {
  # capture additional arguments
  dot.arguments <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)

  # model info
  info <- tryCatch(
    {
      suppressWarnings(insight::model_info(model, verbose = FALSE))
    },
    error = function(e) {
      NULL
    }
  )

  ## TODO remove is.list() when insight 0.8.3 on CRAN
  if (is.null(info) || !is.list(info)) {
    info <- list(family = "unknown", link_function = "unknown")
  }

  if (!is.null(info) && insight::is_multivariate(model) && !"is_zero_inflated" %in% names(info)) {
    info <- info[[1]]
  }


  # add regular attributes
  if (isFALSE(dot.arguments$pretty_names)) {
    attr(params, "pretty_names") <- params$Parameter
  } else if (is.null(attr(params, "pretty_names", exact = TRUE))) {
    attr(params, "pretty_names") <- suppressWarnings(format_parameters(model, model_info = info))
  }

  attr(params, "ci") <- ci
  attr(params, "ci_method") <- ci_method
  attr(params, "df_method") <- ci_method
  attr(params, "test_statistic") <- insight::find_statistic(model)
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
  attr(params, "robust_vcov") <- isTRUE(list(...)$robust) || "vcov" %in% names(list(...))
  attr(params, "ignore_group") <- isFALSE(group_level)
  attr(params, "ran_pars") <- isFALSE(group_level)
  attr(params, "show_summary") <- isTRUE(summary)

  # save if model is multivariate response model
  if (isTRUE(info$is_multivariate)) {
    attr(params, "multivariate_response") <- TRUE
  }

  # if we have a complex random-within-between model, don't show first title element
  if (isTRUE(wb_component) && !is.null(params$Component) && any(c("within", "between") %in% params$Component)) {
    attr(params, "no_caption") <- TRUE
  }


  # for summaries, add R2
  if (isTRUE(summary)) {
    if (requireNamespace("performance", quietly = TRUE)) {
      rsq <- tryCatch(suppressWarnings(performance::r2(model)), error = function(e) NULL)
      attr(params, "r2") <- rsq
    }
  }


  # Models for which titles should be removed -
  # here we add exceptions for objects that should
  # not have a table headline
  if (inherits(model, c("emmGrid", "emm_list", "lm", "glm", "coxph", "bfsl"))) {
    attr(params, "title") <- ""
  }


  # weighted nobs
  weighted_nobs <- tryCatch(
    {
      w <- insight::get_weights(model, na_rm = TRUE, null_as_ones = TRUE)
      round(sum(w))
    },
    error = function(e) {
      NULL
    }
  )
  attr(params, "weighted_nobs") <- weighted_nobs


  # model formula
  model_formula <- tryCatch(
    {
      insight::safe_deparse(insight::find_formula(model)$conditional)
    },
    error = function(e) {
      NULL
    }
  )
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
    attr(params, "zi_coefficient_name") <- ifelse(isTRUE(exponentiate), "Odds Ratio", "Log-Odds")
  }


  # special handling for meta analysis. we need additional
  # information about study weights
  if (inherits(model, c("rma", "rma.uni"))) {
    rma_data <- tryCatch(
      {
        insight::get_data(model, verbose = FALSE)
      },
      error = function(e) {
        NULL
      }
    )
    attr(params, "data") <- rma_data
    attr(params, "study_weights") <- 1 / model$vi
  }


  # special handling for meta analysis again, but these objects save the
  # inverse weighting information in a different column.
  if (inherits(model, c("meta_random", "meta_fixed", "meta_bma"))) {
    rma_data <- tryCatch(
      {
        insight::get_data(model, verbose = FALSE)
      },
      error = function(e) {
        NULL
      }
    )
    attr(params, "data") <- rma_data
    attr(params, "study_weights") <- 1 / params$SE^2
  }

  # should coefficients be grouped?
  if ("groups" %in% names(dot.arguments)) {
    attr(params, "coef_groups") <- eval(dot.arguments[["groups"]])
  }

  # now comes all the digits stuff...
  if ("digits" %in% names(dot.arguments)) {
    attr(params, "digits") <- eval(dot.arguments[["digits"]])
  } else {
    attr(params, "digits") <- 2
  }

  if ("ci_digits" %in% names(dot.arguments)) {
    attr(params, "ci_digits") <- eval(dot.arguments[["ci_digits"]])
  } else {
    attr(params, "ci_digits") <- 2
  }

  if ("p_digits" %in% names(dot.arguments)) {
    attr(params, "p_digits") <- eval(dot.arguments[["p_digits"]])
  } else {
    attr(params, "p_digits") <- 3
  }

  if ("footer_digits" %in% names(dot.arguments)) {
    attr(params, "footer_digits") <- eval(dot.arguments[["footer_digits"]])
  } else {
    attr(params, "footer_digits") <- 3
  }

  if ("s_value" %in% names(dot.arguments)) {
    attr(params, "s_value") <- eval(dot.arguments[["s_value"]])
  }


  # add CI, and reorder
  if (!"CI" %in% colnames(params) && length(ci) == 1) {
    params$CI <- ci
    ci_pos <- grep("CI_low", colnames(params))
    if (length(ci_pos)) {
      if (length(ci_pos) > 1) {
        ci_pos <- ci_pos[1]
      }
      a <- attributes(params)
      params <- params[c(1:(ci_pos - 1), ncol(params), ci_pos:(ncol(params) - 1))]
      attributes(params) <- utils::modifyList(a, attributes(params))
    }
  }

  row.names(params) <- NULL
  params
}



.find_coefficient_type <- function(info, exponentiate, model = NULL) {
  # column name for coefficients
  coef_col <- "Coefficient"
  if (!is.null(model) && inherits(model, "emmGrid")) {
    s <- summary(model)
    name <- attributes(s)$estName
    if (!is.null(name)) {
      coef_col <- switch(name,
        "prob"       = "Probability",
        "odds.ratio" = "Odds Ratio",
        "emmean"     = "Marginal Means",
        "rate"       = "Estimated Counts",
        "ratio"      = "Ratio",
        "Coefficient"
      )
    }
  } else if (!is.null(info)) {
    if (!info$family == "unknown") {
      if (isTRUE(exponentiate)) {
        if ((info$is_binomial && info$is_logit) || info$is_ordinal || info$is_multinomial || info$is_categorical) {
          coef_col <- "Odds Ratio"
        } else if (info$is_binomial && !info$is_logit) {
          coef_col <- "Risk Ratio"
        } else if (info$is_count) {
          coef_col <- "IRR"
        }
      } else {
        if (info$is_binomial || info$is_ordinal || info$is_multinomial || info$is_categorical) {
          coef_col <- "Log-Odds"
        } else if (info$is_count) {
          coef_col <- "Log-Mean"
        }
      }
    }
  }

  coef_col
}


.all_coefficient_types <- function() {
  c("Odds Ratio", "Risk Ratio", "IRR", "Log-Odds", "Log-Mean", "Probability", "Marginal Means", "Estimated Counts", "Ratio")
}



#' @keywords internal
.exponentiate_parameters <- function(params, model = NULL, exponentiate = TRUE) {
  if (!is.null(model) && insight::model_info(model, verbose = FALSE)$is_linear && identical(exponentiate, "nongaussian")) {
    return(params)
  }
  columns <- grepl(pattern = "^(Coefficient|Mean|Median|MAP|Std_Coefficient|CI_|Std_CI)", colnames(params))
  if (any(columns)) {
    if (inherits(model, "mvord")) {
      rows <- params$Component != "correlation"
      params[rows, columns] <- exp(params[rows, columns])
      if (all(c("Coefficient", "SE") %in% names(params))) {
        params$SE[rows] <- params$Coefficient[rows] * params$SE[rows]
      }
    } else {
      params[columns] <- exp(params[columns])
      if (all(c("Coefficient", "SE") %in% names(params))) {
        params$SE <- params$Coefficient * params$SE
      }
    }
  }
  params
}




.add_pretty_names <- function(params, model, group_level = FALSE) {
  attr(params, "model_class") <- class(model)
  cp <- insight::clean_parameters(model)

  # add information about group levels
  if (isTRUE(group_level)) {
    params <- datawizard::data_merge(params, cp, join = "left")

    rand_eff <- grepl("^r_(.*)\\[(.*)\\]", params$Parameter)
    if (any(rand_eff)) {
      r_levels <- gsub("^r_(.*)\\[(.*),(.*)\\]", "\\2", params$Parameter[rand_eff])
      r_grpname <- gsub("^r_(.*)\\[(.*),(.*)\\]", "\\1", params$Parameter[rand_eff])
      r_levels <- gsub("__zi", "", r_levels)
      r_grpname <- gsub("__zi", "", r_grpname)

      params$Groupname <- params$Group
      params$Level <- NA
      params$Group <- ""
      params$Group[rand_eff] <- r_grpname
      params$Level[rand_eff] <- r_levels
      cp$Group <- params$Group
    }

    params$Cleaned_Parameter <- NULL
  }

  clean_params <- cp[cp$Parameter %in% params$Parameter, ]
  attr(params, "cleaned_parameters") <- stats::setNames(clean_params$Cleaned_Parameter[match(params$Parameter, clean_params$Parameter)], params$Parameter)
  attr(params, "pretty_names") <- stats::setNames(clean_params$Cleaned_Parameter[match(params$Parameter, clean_params$Parameter)], params$Parameter)

  params
}




#' @keywords internal
.add_anova_attributes <- function(params, model, ci, test = NULL, ...) {
  dot.arguments <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)

  attr(params, "ci") <- ci
  attr(params, "model_class") <- class(model)
  attr(params, "anova_type") <- .anova_type(model)

  if (inherits(model, "Anova.mlm") && !identical(test, "univariate")) {
    attr(params, "anova_test") <- model$test
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
    attr(params, "ci_digits") <- 2
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
  args <- attributes(x)$additional_arguments

  if (length(args) > 0 && value %in% names(args)) {
    out <- args[[value]]
  } else {
    out <- attributes(x)[[value]]
  }

  if (is.null(out)) {
    out <- default
  }

  out
}
