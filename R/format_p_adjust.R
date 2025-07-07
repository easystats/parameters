#' Format the name of the p-value adjustment methods
#'
#' Format the name of the p-value adjustment methods.
#'
#' @param method Name of the method.
#'
#' @examples
#' library(parameters)
#'
#' format_p_adjust("holm")
#' format_p_adjust("bonferroni")
#' @return A string with the full surname(s) of the author(s), including year of publication, for the adjustment-method.
#' @export
format_p_adjust <- function(method) {
  method <- tolower(method)

  switch(method,
    holm = "Holm (1979)",
    hochberg = "Hochberg (1988)",
    hommel = "Hommel (1988)",
    bonferroni = "Bonferroni",
    fdr = "Benjamini & Hochberg (1995)",
    bh = "Benjamini & Hochberg (1995)",
    by = "Benjamini & Yekutieli (2001)",
    tukey = "Tukey",
    scheffe = "Scheffe",
    sidak = "Sidak",
    `sup-t` = "Simultaneous confidence bands",
    method
  )
}


# p-value adjustment -----

.p_adjust <- function(params, p_adjust, model = NULL, verbose = TRUE) {
  # check if we have any adjustment at all, and a p-column
  if (!is.null(p_adjust) && "p" %in% colnames(params) && p_adjust != "none") {
    ## TODO add "mvt" method from emmeans

    # prepare arguments
    all_methods <- c(stats::p.adjust.methods, "tukey", "scheffe", "sidak", "sup-t")

    # for interaction terms, e.g. for "by" argument in emmeans
    # pairwise comparison, we have to adjust the rank resp. the
    # number of estimates in a comparison family
    rank_adjust <- .p_adjust_rank(model, params)

    # only proceed if valid argument-value
    if (tolower(p_adjust) %in% tolower(all_methods)) {
      # save old values, to check if p-adjustment worked
      old_p_vals <- params$p
      # find statistic column
      stat_column <- match(c("F", "t", "Statistic"), colnames(params))
      stat_column <- stat_column[!is.na(stat_column)]

      if (tolower(p_adjust) %in% tolower(stats::p.adjust.methods)) {
        # base R adjustments
        params$p <- stats::p.adjust(params$p, method = p_adjust)
      } else if (tolower(p_adjust) == "tukey") {
        # tukey adjustment
        result <- .p_adjust_tukey(params, stat_column, rank_adjust, verbose)
        params <- result$params
        verbose <- result$verbose
      } else if (tolower(p_adjust) == "scheffe" && !is.null(model)) {
        # scheffe adjustment
        params <- .p_adjust_scheffe(model, params, stat_column, rank_adjust)
      } else if (tolower(p_adjust) == "sidak") {
        # sidak adjustment
        params$p <- 1 - (1 - params$p)^(nrow(params) / rank_adjust)
      }  else if (tolower(p_adjust) == "sup-t") {
        # sup-t adjustment
        params <- .p_adjust_supt(model, params)
      }

      if (isTRUE(all(old_p_vals == params$p)) && !identical(p_adjust, "none") && verbose) {
        insight::format_warning(paste0("Could not apply ", p_adjust, "-adjustment to p-values. Either something went wrong, or the non-adjusted p-values were already very large.")) # nolint
      }
    } else if (verbose) {
      insight::format_alert(paste0("`p_adjust` must be one of ", toString(all_methods)))
    }
  }
  params
}


# calculate rank adjustment -----

.p_adjust_rank <- function(model, params) {
  tryCatch(
    {
      correction <- 1
      by_vars <- model@misc$by.vars
      if (!is.null(by_vars) && by_vars %in% colnames(params)) {
        correction <- insight::n_unique(params[[by_vars]])
      }
      correction
    },
    error = function(e) {
      1
    }
  )
}


# tukey adjustment -----

.p_adjust_tukey <- function(params, stat_column, rank_adjust = 1, verbose = TRUE) {
  df_column <- colnames(params)[stats::na.omit(match(c("df", "df_error"), colnames(params)))][1]
  if (length(df_column) && length(stat_column)) {
    params$p <- suppressWarnings(stats::ptukey(
      sqrt(2) * abs(params[[stat_column]]),
      nmeans = nrow(params) / rank_adjust,
      df = params[[df_column]],
      lower.tail = FALSE
    ))
    # for specific contrasts, ptukey might fail, and the tukey-adjustement
    # could just be simple p-value calculation
    if (all(is.na(params$p))) {
      params$p <- 2 * stats::pt(
        abs(params[[stat_column]]),
        df = params[[df_column]],
        lower.tail = FALSE
      )
      verbose <- FALSE
    }
  }
  list(params = params, verbose = verbose)
}


# scheffe adjustment -----

.p_adjust_scheffe <- function(model, params, stat_column, rank_adjust = 1) {
  df_column <- colnames(params)[stats::na.omit(match(c("df", "df_error"), colnames(params)))][1]
  if (length(df_column) && length(stat_column)) {
    # 1st try
    scheffe_ranks <- try(qr(model@linfct)$rank, silent = TRUE)

    # 2nd try
    if (inherits(scheffe_ranks, "try-error") || is.null(scheffe_ranks)) {
      scheffe_ranks <- try(model$qr$rank, silent = TRUE)
    }

    if (inherits(scheffe_ranks, "try-error") || is.null(scheffe_ranks)) {
      scheffe_ranks <- nrow(params)
    }
    scheffe_ranks <- scheffe_ranks / rank_adjust
    params$p <- stats::pf(params[[stat_column]]^2 / scheffe_ranks,
      df1 = scheffe_ranks,
      df2 = params[[df_column]],
      lower.tail = FALSE
    )
  }
  params
}


# sup-t adjustment -----

.p_adjust_supt <- function(model, params) {
  if ("Component" %in% colnames(params) && insight::n_unique(params$Component) > 1) {
    # if we have multiple components, we adjust each component separately
    for (component in unique(params$Component)) {
      if (!is.na(component)) {
        params[which(params$Component == component), ] <- .supt_adjust(
          params[which(params$Component == component), ],
          model,
          component = component
        )
      }
    }
    params
  } else {
    .supt_adjust(params, model)
  }
}


.supt_adjust <- function(params, model, component = NULL) {
  insight::check_if_installed("mvtnorm")
  # get correlation matrix, based on the covariance matrix
  vc <- .safe(stats::cov2cor(insight::get_varcov(model, component = component)))
  if (is.null(vc)) {
    insight::format_warning("Could not calculate covariance matrix for `sup-t` adjustment.")
    return(params)
  }
  # get confidence interval level, or set default
  ci_level <- .safe(params$CI[1])
  if (is.null(ci_level)) {
    ci_level <- 0.95
  }
  # find degrees of freedom column, if available
  df_column <- colnames(params)[stats::na.omit(match(c("df", "df_error"), colnames(params)))][1]
  if (length(df_column) == 0) {
    return(params)
  }
  # calculate updated confidence interval level, based on simultaenous
  # confidence intervals (https://onlinelibrary.wiley.com/doi/10.1002/jae.2656)
  crit <- mvtnorm::qmvt(ci_level, df = params[[df_column]][1], tail = "both.tails", corr = vc)$quantile
  # update confidence intervals
  params$CI_low <- params$Coefficient - crit * params$SE
  params$CI_high <- params$Coefficient + crit * params$SE
  # udpate p-values
  for (i in 1:nrow(params)) {
    params$p[i] <- 1 - mvtnorm::pmvt(
      lower = rep(-abs(stats::qt(params$p[i] / 2, df = params[[df_column]][i])), nrow(vc)),
      upper = rep(abs(stats::qt(params$p[i] / 2, df = params[[df_column]][i])), nrow(vc)),
      corr = vc,
      df = params[[df_column]][i]
    )
  }
  params
}
