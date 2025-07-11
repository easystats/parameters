#' Number of components/factors to retain in PCA/FA
#'
#' This function runs many existing procedures for determining how many factors
#' to retain/extract from factor analysis (FA) or dimension reduction (PCA). It
#' returns the number of factors based on the maximum consensus between methods.
#' In case of ties, it will keep the simplest model and select the solution
#' with the fewer factors.
#'
#' @param x A data frame.
#' @param type Can be `"FA"` or `"PCA"`, depending on what you want to do.
#' @param rotation Only used for VSS (Very Simple Structure criterion, see
#'   [psych::VSS()]). The rotation to apply. Can be `"none"`, `"varimax"`,
#'   `"quartimax"`, `"bentlerT"`, `"equamax"`, `"varimin"`, `"geominT"` and
#'   `"bifactor"` for orthogonal rotations, and `"promax"`, `"oblimin"`,
#'   `"simplimax"`, `"bentlerQ"`, `"geominQ"`, `"biquartimin"` and `"cluster"`
#'   for oblique transformations.
#' @param algorithm Factoring method used by VSS. Can be `"pa"` for Principal
#'   Axis Factor Analysis, `"minres"` for minimum residual (OLS) factoring,
#'   `"mle"` for Maximum Likelihood FA and `"pc"` for Principal Components.
#'   `"default"` will select `"minres"` if `type = "FA"` and `"pc"` if
#'   `type = "PCA"`.
#' @param package Package from which respective methods are used. Can be
#'   `"all"` or a vector containing `"nFactors"`, `"psych"`, `"PCDimension"`,
#'   `"fit"` or `"EGAnet"`. Note that `"fit"` (which actually also relies on the
#'   `psych` package) and `"EGAnet"` can be very slow for bigger datasets. Thus,
#'   the default is `c("nFactors", "psych")`. You must have the respective
#'   packages installed for the methods to be used.
#' @param safe If `TRUE`, the function will run all the procedures in try
#'   blocks, and will only return those that work and silently skip the ones
#'   that may fail.
#' @param correlation_matrix An optional correlation matrix that can be used
#'   (note that the data must still be passed as the first argument). If `NULL`,
#'   will compute it by running `cor()` on the passed data.
#' @param n_max If set to a value (e.g., `10`), will drop from the results all
#' methods that suggest a higher number of components. The interpretation becomes
#' 'from all the methods that suggested a number lower than n_max, the results
#' are ...'.
#' @param ... Arguments passed to or from other methods.
#'
#' @details `n_components()` is actually an alias for `n_factors()`, with
#'   different defaults for the function arguments.
#'
#' @note There is also a
#'   [`plot()`-method](https://easystats.github.io/see/articles/parameters.html)
#'   implemented in the [**see**-package](https://easystats.github.io/see/).
#'   `n_components()` is a convenient short-cut  for `n_factors(type = "PCA")`.
#'
#' @examplesIf require("PCDimension", quietly = TRUE) && require("nFactors", quietly = TRUE) && require("EGAnet", quietly = TRUE) && require("psych", quietly = TRUE)
#' library(parameters)
#' n_factors(mtcars, type = "PCA")
#'
#' result <- n_factors(mtcars[1:5], type = "FA")
#' as.data.frame(result)
#' summary(result)
#' \donttest{
#' # Setting package = 'all' will increase the number of methods (but is slow)
#' n_factors(mtcars, type = "PCA", package = "all")
#' n_factors(mtcars, type = "FA", algorithm = "mle", package = "all")
#' }
#'
#' @return A data frame.
#'
#' @references
#'
#' - Bartlett, M. S. (1950). Tests of significance in factor analysis.
#'   British Journal of statistical psychology, 3(2), 77-85.
#'
#' - Bentler, P. M., & Yuan, K. H. (1996). Test of linear trend in
#'   eigenvalues of a covariance matrix with application to data analysis.
#'   British Journal of Mathematical and Statistical Psychology, 49(2), 299-312.
#'
#' - Cattell, R. B. (1966). The scree test for the number of factors.
#'   Multivariate behavioral research, 1(2), 245-276.
#'
#' - Finch, W. H. (2019). Using Fit Statistic Differences to Determine the
#'   Optimal Number of Factors to Retain in an Exploratory Factor Analysis.
#'   Educational and Psychological Measurement.
#'
#' - Zoski, K. W., & Jurs, S. (1996). An objective counterpart to the
#'   visual scree test for factor analysis: The standard error scree.
#'   Educational and Psychological Measurement, 56(3), 443-451.
#'
#' - Zoski, K., & Jurs, S. (1993). Using multiple regression to determine
#'   the number of factors to retain in factor analysis. Multiple Linear
#'   Regression Viewpoints, 20(1), 5-9.
#'
#' - Nasser, F., Benson, J., & Wisenbaker, J. (2002). The performance of
#'   regression-based variations of the visual scree for determining the number
#'   of common factors. Educational and psychological measurement, 62(3),
#'   397-419.
#'
#' - Golino, H., Shi, D., Garrido, L. E., Christensen, A. P., Nieto, M.
#'   D., Sadana, R., & Thiyagarajan, J. A. (2018). Investigating the performance
#'   of Exploratory Graph Analysis and traditional techniques to identify the
#'   number of latent factors: A simulation and tutorial.
#'
#' - Golino, H. F., & Epskamp, S. (2017). Exploratory graph analysis: A
#'   new approach for estimating the number of dimensions in psychological
#'   research. PloS one, 12(6), e0174035.
#'
#' - Revelle, W., & Rocklin, T. (1979). Very simple structure: An
#'   alternative procedure for estimating the optimal number of interpretable
#'   factors. Multivariate Behavioral Research, 14(4), 403-414.
#'
#' - Velicer, W. F. (1976). Determining the number of components from the
#'   matrix of partial correlations. Psychometrika, 41(3), 321-327.
#'
#' @export
n_factors <- function(x,
                      type = "FA",
                      rotation = "varimax",
                      algorithm = "default",
                      package = c("nFactors", "psych"),
                      correlation_matrix = NULL,
                      safe = TRUE,
                      n_max = NULL,
                      ...) {
  if (all(package == "all")) {
    package <- c("nFactors", "EGAnet", "psych", "fit", "pcdimension")
  }

  # Get number of observations
  if (is.data.frame(x)) {
    n_obs <- nrow(x)
  } else if (is.numeric(x) && !is.null(correlation_matrix)) {
    n_obs <- x
    package <- package[!package %in% c("pcdimension", "PCDimension")]
  } else if (is.matrix(x) || inherits(x, "easycormatrix")) {
    insight::format_error(
      "Please input the correlation matrix via the `correlation_matrix` argument and the number of rows / observations via the first argument." # nolint
    )
  }

  # Get only numeric
  numerics <- vapply(x, is.numeric, TRUE)
  if (!all(numerics)) {
    insight::format_warning(paste0(
      "Some variables are not numeric (",
      toString(names(x)[!numerics]),
      "). Dropping them."
    ))
  }
  x <- x[numerics]

  # Correlation matrix
  if (is.null(correlation_matrix)) {
    correlation_matrix <- stats::cor(x, use = "pairwise.complete.obs", ...)
  }
  eigen_values <- eigen(correlation_matrix)$values

  # Smooth matrix if negative eigen values
  if (any(eigen_values < 0)) {
    insight::check_if_installed("psych")
    correlation_matrix <- psych::cor.smooth(correlation_matrix, ...)
    eigen_values <- eigen(correlation_matrix)$values
  }

  # Initialize dataframe
  out <- data.frame()

  # nFactors -------------------------------------------
  if ("nFactors" %in% package) {
    insight::check_if_installed("nFactors")

    # Model
    if (tolower(type) %in% c("fa", "factor", "efa")) {
      model <- "factors"
    } else {
      model <- "components"
    }

    # Compute all
    if (safe) {
      out <- rbind(
        out,
        tryCatch(
          .n_factors_bartlett(eigen_values, model, n_obs),
          warning = function(w) data.frame(),
          error = function(e) data.frame()
        )
      )
      out <- rbind(
        out,
        tryCatch(
          .n_factors_bentler(eigen_values, model, n_obs),
          warning = function(w) data.frame(),
          error = function(e) data.frame()
        )
      )
      out <- rbind(
        out,
        tryCatch(
          .n_factors_cng(eigen_values, model),
          warning = function(w) data.frame(),
          error = function(e) data.frame()
        )
      )
      out <- rbind(
        out,
        tryCatch(
          .n_factors_mreg(eigen_values, model),
          warning = function(w) data.frame(),
          error = function(e) data.frame()
        )
      )
      out <- rbind(
        out,
        tryCatch(
          .n_factors_scree(eigen_values, model),
          warning = function(w) data.frame(),
          error = function(e) data.frame()
        )
      )
      out <- rbind(
        out,
        tryCatch(
          .n_factors_sescree(eigen_values, model),
          warning = function(w) data.frame(),
          error = function(e) data.frame()
        )
      )
    } else {
      out <- rbind(out, .n_factors_bartlett(eigen_values, model, n_obs))
      out <- rbind(out, .n_factors_bentler(eigen_values, model, n_obs))
      out <- rbind(out, .n_factors_cng(eigen_values, model))
      out <- rbind(out, .n_factors_mreg(eigen_values, model))
      out <- rbind(out, .n_factors_scree(eigen_values, model))
      out <- rbind(out, .n_factors_sescree(eigen_values, model))
    }
  }

  # EGAnet -------------------------------------------
  if ("EGAnet" %in% package) {
    insight::check_if_installed("EGAnet")

    if (safe) {
      out <- rbind(
        out,
        tryCatch(
          .n_factors_ega(x, correlation_matrix, n_obs, eigen_values, type),
          # warning = function(w) data.frame(),
          error = function(e) data.frame()
        )
      )
    } else {
      out <- rbind(out, .n_factors_ega(x, correlation_matrix, n_obs, eigen_values, type))
    }
  }

  # psych -------------------------------------------
  if ("psych" %in% package) {
    insight::check_if_installed("psych")

    if (safe) {
      out <- rbind(
        out,
        tryCatch(
          .n_factors_vss(x, correlation_matrix, n_obs, type, rotation, algorithm),
          # warning = function(w) data.frame(),
          error = function(e) data.frame()
        )
      )
    } else {
      out <- rbind(out, .n_factors_vss(x, correlation_matrix, n_obs, type, rotation, algorithm))
    }
  }

  # fit -------------------------------------------
  if ("fit" %in% package) {
    insight::check_if_installed("psych")

    if (safe) {
      out <- rbind(
        out,
        tryCatch(
          .n_factors_fit(x, correlation_matrix, n_obs, type, rotation, algorithm),
          warning = function(w) data.frame(),
          error = function(e) data.frame()
        )
      )
    } else {
      out <- rbind(out, .n_factors_fit(x, correlation_matrix, n_obs, type, rotation, algorithm))
    }
  }

  # pcdimension -------------------------------------------
  if ("pcdimension" %in% tolower(package)) {
    insight::check_if_installed("PCDimension")

    if (safe) {
      out <- rbind(
        out,
        tryCatch(
          .n_factors_PCDimension(x, type),
          warning = function(w) data.frame(),
          error = function(e) data.frame()
        )
      )
    } else {
      out <- rbind(out, .n_factors_PCDimension(x, type))
    }
  }

  # OUTPUT ----------------------------------------------
  # TODO created weighted composite score
  out <- out[!is.na(out$n_Factors), ] # Remove empty methods
  out <- out[order(out$n_Factors), ] # Arrange by n factors
  row.names(out) <- NULL # Reset row index

  if (!is.null(n_max)) {
    out <- out[out$n_Factors <= n_max, ]
  }

  # Add summary
  by_factors <- .data_frame(
    n_Factors = as.numeric(unique(out$n_Factors)),
    n_Methods = as.numeric(by(out, as.factor(out$n_Factors), function(out) n <- nrow(out)))
  )

  # Add cumulative percentage of variance explained
  fa <- psych::fa(
    correlation_matrix,
    nfactors = max(by_factors$n_Factors),
    n.obs = nrow(x),
    rotate = "none"
  )
  varex <- .get_fa_variance_summary(fa)
  # Extract number of factors from EFA output (usually MR1, ML1, etc.)
  varex$n_Factors <- as.numeric(gsub("[^\\d]+", "", varex$Component, perl = TRUE))
  # Merge (and like that filter out empty methods)
  by_factors <- merge(by_factors, varex[, c("n_Factors", "Variance_Cumulative")], by = "n_Factors")

  attr(out, "Variance_Explained") <- varex # We add all the variance explained (for plotting)
  attr(out, "summary") <- by_factors
  attr(out, "n") <- min(as.numeric(as.character(by_factors[
    by_factors$n_Methods == max(by_factors$n_Methods),
    "n_Factors"
  ])))

  class(out) <- c("n_factors", "see_n_factors", class(out))
  out
}


#' @rdname n_factors
#' @export
n_components <- function(x,
                         type = "PCA",
                         rotation = "varimax",
                         algorithm = "default",
                         package = c("nFactors", "psych"),
                         correlation_matrix = NULL,
                         safe = TRUE,
                         ...) {
  n_factors(
    x,
    type = type,
    rotation = rotation,
    algorithm = algorithm,
    package = package,
    correlation_matrix = correlation_matrix,
    safe = safe,
    ...
  )
}


#' @export
print.n_factors <- function(x, ...) {
  results <- attributes(x)$summary

  # Extract info
  max_methods <- max(results$n_Methods)
  best_n <- attributes(x)$n

  # Extract methods
  if ("n_Factors" %in% names(x)) {
    type <- "factor"
    methods_text <- toString(as.character(x[x$n_Factors == best_n, "Method"]))
  } else {
    type <- "cluster"
    methods_text <- toString(as.character(x[x$n_Clusters == best_n, "Method"]))
  }


  # Text
  msg_text <- paste0(
    "The choice of ",
    as.character(best_n),
    ifelse(type == "factor", " dimensions ", " clusters "),
    "is supported by ",
    max_methods,
    " (",
    sprintf("%.2f", max_methods / nrow(x) * 100),
    "%) methods out of ",
    nrow(x),
    " (",
    methods_text,
    ").\n"
  )

  insight::print_color("# Method Agreement Procedure:\n\n", "blue")
  cat(msg_text)
  invisible(x)
}


#' @export
summary.n_factors <- function(object, ...) {
  attributes(object)$summary
}

#' @export
as.numeric.n_factors <- function(x, ...) {
  attributes(x)$n
}

#' @export
as.double.n_factors <- as.numeric.n_factors

#' @export
summary.n_clusters <- summary.n_factors

#' @export
as.numeric.n_clusters <- as.numeric.n_factors

#' @export
as.double.n_clusters <- as.double.n_factors

#' @export
print.n_clusters <- print.n_factors


# Methods -----------------------------------------------------------------


#' Bartlett, Anderson and Lawley Procedures
#' @keywords internal
.n_factors_bartlett <- function(eigen_values = NULL, model = "factors", nobs = NULL) {
  nfac <- nFactors::nBartlett(
    eigen_values,
    N = nobs,
    cor = TRUE,
    alpha = 0.05,
    details = FALSE
  )$nFactors

  .data_frame(
    n_Factors = as.numeric(nfac),
    Method = insight::format_capitalize(names(nfac)),
    Family = "Barlett"
  )
}


#' Bentler and Yuan's Procedure
#' @keywords internal
.n_factors_bentler <- function(eigen_values = NULL, model = "factors", nobs = NULL) {
  nfac <- .nBentler(
    x = eigen_values,
    N = nobs,
    model = model,
    alpha = 0.05,
    details = FALSE
  )$nFactors

  .data_frame(
    n_Factors = as.numeric(nfac),
    Method = "Bentler",
    Family = "Bentler"
  )
}


#' Cattell-Nelson-Gorsuch CNG Indices
#' @keywords internal
.n_factors_cng <- function(eigen_values = NULL, model = "factors") {
  if (length(eigen_values) < 6) {
    nfac <- NA
  } else {
    nfac <- nFactors::nCng(x = eigen_values, cor = TRUE, model = model)$nFactors
  }

  .data_frame(
    n_Factors = as.numeric(nfac),
    Method = "CNG",
    Family = "CNG"
  )
}


#' Multiple Regression Procedure
#' @keywords internal
.n_factors_mreg <- function(eigen_values = NULL, model = "factors") {
  if (length(eigen_values) < 6) {
    nfac <- NA
  } else {
    nfac <- nFactors::nMreg(x = eigen_values, cor = TRUE, model = model)$nFactors
  }

  .data_frame(
    n_Factors = as.numeric(nfac),
    Method = c("beta", "t", "p"),
    Family = "Multiple_regression"
  )
}


#' Non Graphical Cattell's Scree Test
#' @keywords internal
.n_factors_scree <- function(eigen_values = NULL, model = "factors") {
  nfac <- unlist(nFactors::nScree(x = eigen_values, cor = TRUE, model = model)$Components)

  .data_frame(
    n_Factors = as.numeric(nfac),
    Method = c("Optimal coordinates", "Acceleration factor", "Parallel analysis", "Kaiser criterion"),
    Family = "Scree"
  )
}


#' Standard Error Scree and Coefficient of Determination Procedures
#' @keywords internal
.n_factors_sescree <- function(eigen_values = NULL, model = "factors") {
  nfac <- nFactors::nSeScree(x = eigen_values, cor = TRUE, model = model)$nFactors
  .data_frame(
    n_Factors = as.numeric(nfac),
    Method = c("Scree (SE)", "Scree (R2)"),
    Family = "Scree_SE"
  )
}


# EGAnet ------------------------
.n_factors_ega <- function(x = NULL,
                           correlation_matrix = NULL,
                           nobs = NULL,
                           eigen_values = NULL,
                           type = "FA") {
  # Replace with own correlation matrix
  junk <- utils::capture.output(suppressWarnings(suppressMessages(
    nfac_glasso <- EGAnet::EGA(
      correlation_matrix,
      n = nobs,
      model = "glasso",
      plot.EGA = FALSE
    )$n.dim
  )))
  junk <- utils::capture.output(suppressWarnings(suppressMessages(
    nfac_TMFG <- .safe(
      EGAnet::EGA(correlation_matrix, n = nobs, model = "TMFG", plot.EGA = FALSE)$n.dim,
      NA
    )
  )))

  .data_frame(
    n_Factors = as.numeric(c(nfac_glasso, nfac_TMFG)),
    Method = c("EGA (glasso)", "EGA (TMFG)"),
    Family = "EGA"
  )
}


# psych ------------------------

#' @keywords internal
.n_factors_parallel <- function(x = NULL, correlation_matrix = NULL, nobs = NULL, type = "FA") {
  # Altnerative version of parralel analysis
  # Not used because already included in nFactors

  if (tolower(type) %in% c("fa", "factor", "efa")) {
    fa <- "fa"
  } else {
    fa <- "pc"
  }

  insight::check_if_installed("psych")
  out <- psych::fa.parallel(correlation_matrix, n.obs = nobs, fa = fa, plot = FALSE, fm = "ml")

  .data_frame(
    n_Factors = as.numeric(stats::na.omit(c(out$nfact, out$ncomp))),
    Method = "Parallel",
    Family = "psych"
  )
}

#' @keywords internal
.n_factors_vss <- function(x = NULL,
                           correlation_matrix = NULL,
                           nobs = NULL,
                           type = "FA",
                           rotation = "varimax",
                           algorithm = "default") {
  if (algorithm == "default") {
    if (tolower(type) %in% c("fa", "factor", "efa")) {
      algorithm <- "minres"
    } else {
      algorithm <- "pc"
    }
  }

  insight::check_if_installed("psych")
  # Compute VSS
  vss <- psych::VSS(
    correlation_matrix,
    n = ncol(x) - 1,
    n.obs = nobs,
    rotate = rotation,
    fm = algorithm,
    plot = FALSE
  )

  # Format results
  stats <- vss$vss.stats
  stats$map <- vss$map
  stats$n_Factors <- seq_len(nrow(stats))
  names(stats) <- gsub("cfit.", "VSS_Complexity_", names(stats))

  # Indices
  vss_1 <- which.max(stats$VSS_Complexity_1)
  vss_2 <- which.max(stats$VSS_Complexity_2)
  velicer_MAP <- which.min(stats$map)
  BIC_reg <- which.min(stats$BIC)
  BIC_adj <- which.min(stats$SABIC)
  BIC_reg <- ifelse(length(BIC_reg) == 0, NA, BIC_reg)
  BIC_adj <- ifelse(length(BIC_adj) == 0, NA, BIC_adj)

  .data_frame(
    n_Factors = as.numeric(c(vss_1, vss_2, velicer_MAP, BIC_reg, BIC_adj)),
    Method = c("VSS complexity 1", "VSS complexity 2", "Velicer's MAP", "BIC", "BIC (adjusted)"),
    Family = c("VSS", "VSS", "Velicers_MAP", "BIC", "BIC")
  )
}


#' @keywords internal
.n_factors_fit <- function(x = NULL,
                           correlation_matrix = NULL,
                           nobs = NULL,
                           type = "FA",
                           rotation = "varimax",
                           algorithm = "default",
                           threshold = 0.1) {
  if (algorithm == "default") {
    if (tolower(type) %in% c("fa", "factor", "efa")) {
      algorithm <- "minres"
    } else {
      algorithm <- "pc"
    }
  }

  insight::check_if_installed("psych")
  rez <- data.frame()
  for (n in 1:(ncol(correlation_matrix) - 1)) {
    if (tolower(type) %in% c("fa", "factor", "efa")) {
      factors <- tryCatch(
        suppressWarnings(
          psych::fa(
            correlation_matrix,
            nfactors = n,
            n.obs = nobs,
            rotate = rotation,
            fm = algorithm
          )
        ),
        error = function(e) NA
      )
    } else {
      factors <- tryCatch(
        suppressWarnings(
          psych::pca(
            correlation_matrix,
            nfactors = n,
            n.obs = nobs,
            rotate = rotation
          )
        ),
        error = function(e) NA
      )
    }

    if (all(is.na(factors))) {
      next
    }

    rmsea <- ifelse(is.null(factors$RMSEA), NA, factors$RMSEA[1])
    rmsr <- ifelse(is.null(factors$rms), NA, factors$rms)
    crms <- ifelse(is.null(factors$crms), NA, factors$crms)
    bic <- ifelse(is.null(factors$BIC), NA, factors$BIC)
    tli <- ifelse(is.null(factors$TLI), NA, factors$TLI)

    rez <- rbind(
      rez,
      .data_frame(
        n = n,
        Fit = factors$fit.off,
        TLI = tli,
        RMSEA = rmsea,
        RMSR = rmsr,
        CRMS = crms,
        BIC = bic
      )
    )
  }

  # For fit indices that constantly increase / decrease, we need to find
  # an "elbow"/"knee". Here we take the first value that reaches 90 percent
  # of the range between the max and the min (when 'threshold = 0.1').
  # Fit
  if (all(is.na(rez$Fit))) {
    fit_off <- NA
  } else {
    target <- max(rez$Fit, na.rm = TRUE) - threshold * diff(range(rez$Fit, na.rm = TRUE))
    fit_off <- rez[!is.na(rez$Fit) & rez$Fit >= target, "n"][1]
  }
  # TLI
  if (all(is.na(rez$TLI))) {
    TLI <- NA
  } else {
    target <- max(rez$TLI, na.rm = TRUE) - threshold * diff(range(rez$TLI, na.rm = TRUE))
    TLI <- rez[!is.na(rez$TLI) & rez$TLI >= target, "n"][1]
  }
  # RMSEA
  if (all(is.na(rez$RMSEA))) {
    RMSEA <- NA
  } else {
    target <- min(rez$RMSEA, na.rm = TRUE) + threshold * diff(range(rez$RMSEA, na.rm = TRUE))
    RMSEA <- rez[!is.na(rez$RMSEA) & rez$RMSEA <= target, "n"][1]
  }
  # RMSR
  if (all(is.na(rez$RMSR))) {
    RMSR <- NA
  } else {
    target <- min(rez$RMSR, na.rm = TRUE) + threshold * diff(range(rez$RMSR, na.rm = TRUE))
    RMSR <- rez[!is.na(rez$RMSR) & rez$RMSR <= target, "n"][1]
  }
  # CRMS
  if (all(is.na(rez$CRMS))) {
    CRMS <- NA
  } else {
    target <- min(rez$CRMS, na.rm = TRUE) + threshold * diff(range(rez$CRMS, na.rm = TRUE))
    CRMS <- rez[!is.na(rez$CRMS) & rez$CRMS <= target, "n"][1]
  }
  # BIC (this is a penalized method so we can just take the one that minimizes it)
  BayIC <- ifelse(all(is.na(rez$BIC)), NA, rez[!is.na(rez$BIC) & rez$BIC == min(rez$BIC, na.rm = TRUE), "n"])

  .data_frame(
    n_Factors = c(fit_off, TLI, RMSEA, RMSR, CRMS, BayIC),
    Method = c("Fit_off", "TLI", "RMSEA", "RMSR", "CRMS", "BIC"),
    Family = c("Fit", "Fit", "Fit", "Fit", "Fit", "Fit")
  )
}

# PCDimension ------------------------

#' @keywords internal
.n_factors_PCDimension <- function(x = NULL, type = "PCA") {
  # This package is a strict dependency of PCDimension so if users have the
  # former they should have it
  insight::check_if_installed(c("ClassDiscovery", "PCDimension"))

  # Only applies to PCA with full data
  if (tolower(type) %in% c("fa", "factor", "efa") || !is.data.frame(x)) {
    return(data.frame())
  }
  # Randomization-Based Methods
  rez_rnd <- PCDimension::rndLambdaF(x)

  # Broken-Stick
  spca <- ClassDiscovery::SamplePCA(t(x))
  lambda <- spca@variances[1:(ncol(x) - 1)]
  rez_bokenstick <- PCDimension::bsDimension(lambda)

  # Auer-Gervini
  ag <- PCDimension::AuerGervini(spca)
  agfuns <- list(
    twice = PCDimension::agDimTwiceMean,
    specc = PCDimension::agDimSpectral,
    km = PCDimension::agDimKmeans,
    km3 = PCDimension::agDimKmeans3,
    # tt=PCDimension::agDimTtest,  # known to overestimate
    # cpm=PCDimension::makeAgCpmFun("Exponential"), # known to overestimate
    tt2 = PCDimension::agDimTtest2,
    cpt = PCDimension::agDimCPT
  )
  rez_ag <- PCDimension::compareAgDimMethods(ag, agfuns)

  .data_frame(
    n_Factors = as.numeric(c(rez_rnd, rez_bokenstick, rez_ag)),
    Method = c(
      "Random (lambda)", "Random (F)", "Broken-Stick", "Auer-Gervini (twice)",
      "Auer-Gervini (spectral)", "Auer-Gervini (kmeans-2)", "AuerGervini (kmeans-3)",
      "Auer-Gervini (T)", "AuerGervini (CPT)"
    ),
    Family = "PCDimension"
  )
}

# Re-implementation of nBentler in nFactors ------------------------

#' @keywords internal
.nBentler <- function(x,
                      N,
                      model = model,
                      log = TRUE,
                      alpha = 0.05,
                      cor = TRUE,
                      details = TRUE,
                      ...) {
  insight::check_if_installed("nFactors")

  lambda <- nFactors::eigenComputes(x, cor = cor, model = model, ...)
  if (any(lambda < 0)) {
    insight::format_error(
      "These indices are only valid with a principal component solution. So, only positive eigenvalues are permitted."
    )
  }

  minPar <- c(min(lambda) - abs(min(lambda)) + 0.001, 0.001)
  maxPar <- c(max(lambda), stats::lm(lambda ~ I(rev(seq_along(lambda))))$coef[2])


  n <- N
  significance <- alpha
  min.k <- 3
  LRT <- .data_frame(
    q = numeric(length(lambda) - min.k), k = numeric(length(lambda) - min.k),
    LRT = numeric(length(lambda) - min.k), a = numeric(length(lambda) - min.k),
    b = numeric(length(lambda) - min.k),
    p = numeric(length(lambda) - min.k),
    convergence = numeric(length(lambda) - min.k)
  )
  bentler.n <- 0
  for (i in 1:(length(lambda) - min.k)) {
    temp <-
      nFactors::bentlerParameters(
        x = lambda,
        N = n,
        nFactors = i,
        log = log,
        cor = cor,
        minPar = minPar,
        maxPar = maxPar,
        graphic = FALSE
      )
    LRT[i, 3] <- temp$lrt
    LRT[i, 4] <- ifelse(is.null(temp$coef[1]), NA, temp$coef[1])
    LRT[i, 5] <- ifelse(is.null(temp$coef[2]), NA, temp$coef[2])
    LRT[i, 6] <- ifelse(is.null(temp$p.value), NA, temp$p.value)
    LRT[i, 7] <- ifelse(is.null(temp$convergence), NA, temp$convergence)
    LRT[i, 2] <- i
    LRT[i, 1] <- length(lambda) - i
  }
  # LRT     <- LRT[order(LRT[,1],decreasing = TRUE),]
  for (i in 1:(length(lambda) - min.k)) {
    if (i == 1) bentler.n <- bentler.n + as.numeric(LRT$p[i] <= significance)
    if (i > 1 && LRT$p[i - 1] <= 0.05) {
      bentler.n <- bentler.n + as.numeric(LRT$p[i] <= significance)
    }
  }

  if (bentler.n == 0) {
    bentler.n <- length(lambda)
  }

  if (isTRUE(details)) {
    details <- LRT
  } else {
    details <- NULL
  }

  res <- list(detail = details, nFactors = bentler.n)
  class(res) <- c("nFactors", "list")
  res
}
