#' Number of components/factors to retain in PCA/FA
#'
#' This function runs many existing procedures for determining how many factors
#' to retain/extract from factor analysis (FA) or dimension reduction (PCA). It
#' returns the number of factors based on the maximum consensus between methods.
#' In case of ties, it will keep the simplest model and select the solution
#' with the fewer factors.
#'
#' @param x A data frame.
#' @param type Can be `"FA"` or `"PCA"`, depending on what you want to
#'   do.
#' @param rotation Only used for VSS (Very Simple Structure criterion, see
#'   [psych::VSS()]). The rotation to apply. Can be `"none"`,
#'   `"varimax"`, `"quartimax"`, `"bentlerT"`, `"equamax"`,
#'   `"varimin"`, `"geominT"` and `"bifactor"` for orthogonal
#'   rotations, and `"promax"`, `"oblimin"`, `"simplimax"`,
#'   `"bentlerQ"`, `"geominQ"`, `"biquartimin"` and
#'   `"cluster"` for oblique transformations.
#' @param algorithm Factoring method used by VSS. Can be `"pa"` for
#'   Principal Axis Factor Analysis, `"minres"` for minimum residual (OLS)
#'   factoring, `"mle"` for Maximum Likelihood FA and `"pc"` for
#'   Principal Components. `"default"` will select `"minres"` if
#'   `type = "FA"` and `"pc"` if `type = "PCA"`.
#' @param package Package from which respective methods are used. Can be
#'   `"all"` or a vector containing `"nFactors"`, `"psych"`, `"PCDimension"`, `"fit"` or
#'   `"EGAnet"`. Note that `"fit"` (which actually also relies on the `psych`
#'   package) and `"EGAnet"` can be very slow for bigger
#'   datasets. Thus, the default is `c("nFactors", "psych")`. You must have
#'   the respective packages installed for the methods to be used.
#' @param safe If `TRUE`, the function will run all the procedures in try
#'   blocks, and will only return those that work and silently skip the ones
#'   that may fail.
#' @param cor An optional correlation matrix that can be used (note that the
#'   data must still be passed as the first argument). If `NULL`, will
#'   compute it by running `cor()` on the passed data.
#' @param n_max If set to a value (e.g., `10`), will drop from the results all
#' methods that suggest a higher number of components. The interpretation becomes
#' 'from all the methods that suggested a number lower than n_max, the results
#' are ...'.
#' @param ... Arguments passed to or from other methods.
#'
#' @details `n_components` is actually an alias for `n_factors`, with
#'   different defaults for the function arguments.
#'
#' @note There is also a
#'   [`plot()`-method](https://easystats.github.io/see/articles/parameters.html)
#'   implemented in the
#'   \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'   `n_components()` is a convenient short for `n_factors(type =
#'   "PCA")`.
#'
#' @examples
#' library(parameters)
#' if (require("nFactors", quietly = TRUE) && require("EGAnet", quietly = TRUE)) {
#'   n_factors(mtcars, type = "PCA")
#'
#'   result <- n_factors(mtcars[1:5], type = "FA")
#'   as.data.frame(result)
#'   summary(result)
#'   \dontrun{
#'   if (require("PCDimension", quietly = TRUE)) {
#'     # Setting package = 'all' will increase the number of methods (but is slow)
#'     n_factors(mtcars, type = "PCA", package = "all")
#'     n_factors(mtcars, type = "FA", algorithm = "mle", package = "all")
#'   }
#'   }
#' }
#' @return A data frame.
#'
#' @references \itemize{
#'   \item Bartlett, M. S. (1950). Tests of significance in factor analysis.
#'   British Journal of statistical psychology, 3(2), 77-85.
#'
#'   \item Bentler, P. M., & Yuan, K. H. (1996). Test of linear trend in
#'   eigenvalues of a covariance matrix with application to data analysis.
#'   British Journal of Mathematical and Statistical Psychology, 49(2), 299-312.
#'
#'   \item Cattell, R. B. (1966). The scree test for the number of factors.
#'   Multivariate behavioral research, 1(2), 245-276.
#'
#'   \item Finch, W. H. (2019). Using Fit Statistic Differences to Determine the
#'   Optimal Number of Factors to Retain in an Exploratory Factor Analysis.
#'   Educational and Psychological Measurement.
#'
#'   \item Zoski, K. W., & Jurs, S. (1996). An objective counterpart to the
#'   visual scree test for factor analysis: The standard error scree.
#'   Educational and Psychological Measurement, 56(3), 443-451.
#'
#'   \item Zoski, K., & Jurs, S. (1993). Using multiple regression to determine
#'   the number of factors to retain in factor analysis. Multiple Linear
#'   Regression Viewpoints, 20(1), 5-9.
#'
#'   \item Nasser, F., Benson, J., & Wisenbaker, J. (2002). The performance of
#'   regression-based variations of the visual scree for determining the number
#'   of common factors. Educational and psychological measurement, 62(3),
#'   397-419.
#'
#'   \item Golino, H., Shi, D., Garrido, L. E., Christensen, A. P., Nieto, M.
#'   D., Sadana, R., & Thiyagarajan, J. A. (2018). Investigating the performance
#'   of Exploratory Graph Analysis and traditional techniques to identify the
#'   number of latent factors: A simulation and tutorial.
#'
#'   \item Golino, H. F., & Epskamp, S. (2017). Exploratory graph analysis: A
#'   new approach for estimating the number of dimensions in psychological
#'   research. PloS one, 12(6), e0174035.
#'
#'   \item Revelle, W., & Rocklin, T. (1979). Very simple structure: An
#'   alternative procedure for estimating the optimal number of interpretable
#'   factors. Multivariate Behavioral Research, 14(4), 403-414.
#'
#'   \item Velicer, W. F. (1976). Determining the number of components from the
#'   matrix of partial correlations. Psychometrika, 41(3), 321-327.
#' }
#' @export
n_factors <- function(x,
                      type = "FA",
                      rotation = "varimax",
                      algorithm = "default",
                      package = c("nFactors", "psych"),
                      cor = NULL,
                      safe = TRUE,
                      n_max = NULL,
                      ...) {
  if (all(package == "all")) {
    package <- c("nFactors", "EGAnet", "psych", "fit", "pcdimension")
  }

  # Get number of observations
  if(!is.data.frame(x)) {
    if(is.numeric(x) && !is.null(cor)) {
      nobs <- x
      package <- package[!package %in% c("pcdimension", "PCDimension")]
    } else if(is.matrix(x) || inherits(x, "easycormatrix")) {
      stop("Please input the correlation matrix via the `cor = ...` argument and
           the number of rows / observations via the first argument.")
    }
  } else {
    nobs <- nrow(x)
  }

  # Correlation matrix
  if (is.null(cor)) {
    cor <- stats::cor(x, use = "pairwise.complete.obs", ...)
  }
  eigen_values <- eigen(cor)$values

  # Smooth matrix if negative eigen values
  if(any(eigen_values < 0)) {
    insight::check_if_installed("psych")
    cor <- psych::cor.smooth(cor, ...)
    eigen_values <- eigen(cor)$values
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
        tryCatch(.n_factors_bartlett(eigen_values, model, nobs),
          warning = function(w) data.frame(),
          error = function(e) data.frame()
        )
      )
      out <- rbind(
        out,
        tryCatch(.n_factors_bentler(eigen_values, model, nobs),
          warning = function(w) data.frame(),
          error = function(e) data.frame()
        )
      )
      out <- rbind(
        out,
        tryCatch(.n_factors_cng(eigen_values, model),
          warning = function(w) data.frame(),
          error = function(e) data.frame()
        )
      )
      out <- rbind(
        out,
        tryCatch(.n_factors_mreg(eigen_values, model),
          warning = function(w) data.frame(),
          error = function(e) data.frame()
        )
      )
      out <- rbind(
        out,
        tryCatch(.n_factors_scree(eigen_values, model),
          warning = function(w) data.frame(),
          error = function(e) data.frame()
        )
      )
      out <- rbind(
        out,
        tryCatch(.n_factors_sescree(eigen_values, model),
          warning = function(w) data.frame(),
          error = function(e) data.frame()
        )
      )
    } else {
      out <- rbind(
        out,
        .n_factors_bartlett(eigen_values, model, nobs)
      )
      out <- rbind(
        out,
        .n_factors_bentler(eigen_values, model, nobs)
      )
      out <- rbind(
        out,
        .n_factors_cng(eigen_values, model)
      )
      out <- rbind(
        out,
        .n_factors_mreg(eigen_values, model)
      )
      out <- rbind(
        out,
        .n_factors_scree(eigen_values, model)
      )
      out <- rbind(
        out,
        .n_factors_sescree(eigen_values, model)
      )
    }
  }

  # EGAnet -------------------------------------------
  if ("EGAnet" %in% package) {
    insight::check_if_installed("EGAnet")

    if (safe) {
      out <- rbind(
        out,
        tryCatch(.n_factors_ega(x, cor, nobs, eigen_values, type),
          warning = function(w) data.frame(),
          error = function(e) data.frame()
        )
      )
    } else {
      out <- rbind(
        out,
        .n_factors_ega(x, cor, nobs, eigen_values, type)
      )
    }
  }


  # psych -------------------------------------------
  if ("psych" %in% package) {
    insight::check_if_installed("psych")

    if (safe) {
      out <- rbind(
        out,
        tryCatch(.n_factors_vss(x, cor, nobs, type, rotation, algorithm),
          warning = function(w) data.frame(),
          error = function(e) data.frame()
        )
      )
    } else {
      out <- rbind(
        out,
        .n_factors_vss(x, cor, nobs, type, rotation, algorithm)
      )
    }
  }

  # fit -------------------------------------------
  if ("fit" %in% package) {
    insight::check_if_installed("psych")

    if (safe) {
      out <- rbind(
        out,
        tryCatch(.n_factors_fit(x, cor, nobs, type, rotation, algorithm),
          warning = function(w) data.frame(),
          error = function(e) data.frame()
        )
      )
    } else {
      out <- rbind(
        out,
        .n_factors_fit(x, cor, nobs, type, rotation, algorithm)
      )
    }
  }

  # fit -------------------------------------------
  if ("pcdimension" %in% tolower(package)) {
    insight::check_if_installed("PCDimension")

    if (safe) {
      out <- rbind(
        out,
        tryCatch(.n_factors_PCDimension(x, type),
                 warning = function(w) data.frame(),
                 error = function(e) data.frame()
        )
      )
    } else {
      out <- rbind(
        out,
        .n_factors_PCDimension(x, type)
      )
    }
  }

  # OUTPUT ----------------------------------------------
  # TODO created weighted composite score
  out <- out[!is.na(out$n_Factors), ] # Remove empty methods
  out <- out[order(out$n_Factors), ] # Arrange by n factors
  row.names(out) <- NULL # Reset row index

  if(!is.null(n_max)) {
    out <-  out[out$n_Factors <= n_max, ]
  }

  # Add summary
  by_factors <- .data_frame(
    n_Factors = as.numeric(unique(out$n_Factors)),
    n_Methods = as.numeric(by(out, as.factor(out$n_Factors), function(out) n <- nrow(out)))
  )

  attr(out, "summary") <- by_factors
  attr(out, "n") <- min(as.numeric(as.character(by_factors[by_factors$n_Methods == max(by_factors$n_Methods), c("n_Factors")])))

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
                         cor = NULL,
                         safe = TRUE,
                         ...) {
  n_factors(
    x,
    type = type,
    rotation = rotation,
    algorithm = algorithm,
    package = package,
    cor = cor,
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
    methods_text <- paste0(as.character(x[x$n_Factors == best_n, "Method"]), collapse = ", ")
  } else {
    type <- "cluster"
    methods_text <- paste0(as.character(x[x$n_Clusters == best_n, "Method"]), collapse = ", ")
  }


  # Text
  text <- paste0(
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
  cat(text)
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

  data.frame(
    n_Factors = as.numeric(nfac),
    Method = format_capitalize(names(nfac)),
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

  data.frame(
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

  data.frame(
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

  data.frame(
    n_Factors = as.numeric(nfac),
    Method = c("beta", "t", "p"),
    Family = "Multiple_regression"
  )
}


#' Non Graphical Cattell's Scree Test
#' @keywords internal
.n_factors_scree <- function(eigen_values = NULL, model = "factors") {
  nfac <- unlist(nFactors::nScree(x = eigen_values, cor = TRUE, model = model)$Components)

  data.frame(
    n_Factors = as.numeric(nfac),
    Method = c("Optimal coordinates", "Acceleration factor", "Parallel analysis", "Kaiser criterion"),
    Family = "Scree"
  )
}


#' Standard Error Scree and Coefficient of Determination Procedures
#' @keywords internal
.n_factors_sescree <- function(eigen_values = NULL, model = "factors") {
  nfac <- nFactors::nSeScree(x = eigen_values, cor = TRUE, model = model)$nFactors
  data.frame(
    n_Factors = as.numeric(nfac),
    Method = c("Scree (SE)", "Scree (R2)"),
    Family = "Scree_SE"
  )
}



# EGAnet ------------------------

#' @keywords internal
.n_factors_ega <- function(x = NULL,
                           cor = NULL,
                           nobs = NULL,
                           eigen_values = NULL,
                           type = "FA") {


  # Replace with own correlation matrix
  junk <- utils::capture.output(suppressWarnings(suppressMessages(nfac_glasso <- EGAnet::EGA(cor, n = nobs, model = "glasso", plot.EGA = FALSE)$n.dim)))
  junk <- utils::capture.output(suppressWarnings(suppressMessages(nfac_TMFG <- EGAnet::EGA(cor, n = nobs, model = "TMFG", plot.EGA = FALSE)$n.dim)))

  data.frame(
    n_Factors = as.numeric(c(nfac_glasso, nfac_TMFG)),
    Method = c("EGA (glasso)", "EGA (TMFG)"),
    Family = "EGA"
  )
}


# psych ------------------------

#' @keywords internal
.n_factors_vss <- function(x = NULL,
                           cor = NULL,
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


  # Compute VSS
  vss <- psych::VSS(
    cor,
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

  data.frame(
    n_Factors = as.numeric(c(vss_1, vss_2, velicer_MAP, BIC_reg, BIC_adj)),
    Method = c("VSS complexity 1", "VSS complexity 2", "Velicer's MAP", "BIC", "BIC (adjusted)"),
    Family = c("VSS", "VSS", "Velicers_MAP", "BIC", "BIC")
  )
}




#' @keywords internal
.n_factors_fit <- function(x = NULL,
                           cor = NULL,
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

  rez <- data.frame()
  for (n in 1:(ncol(cor) - 1)) {
    if (tolower(type) %in% c("fa", "factor", "efa")) {
      factors <- tryCatch(suppressWarnings(psych::fa(cor,
        nfactors = n,
        n.obs = nobs,
        rotate = rotation,
        fm = algorithm
      )),
      error = function(e) NA
      )
    } else {
      factors <- tryCatch(suppressWarnings(psych::pca(cor,
        nfactors = n,
        n.obs = nobs,
        rotate = rotation
      )),
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
      data.frame(
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
  if(all(is.na(rez$Fit))) {
    fit_off <- NA
  } else {
    target <- max(rez$Fit, na.rm = TRUE) - threshold * diff(range(rez$Fit, na.rm = TRUE))
    fit_off <- rez[!is.na(rez$Fit) & rez$Fit >= target, "n"][1]
  }
  # TLI
  if(all(is.na(rez$TLI))) {
    TLI <- NA
  } else {
    target <- max(rez$TLI, na.rm = TRUE) - threshold * diff(range(rez$TLI, na.rm = TRUE))
    TLI <- rez[!is.na(rez$TLI) & rez$TLI >= target, "n"][1]
  }
  # RMSEA
  if(all(is.na(rez$RMSEA))) {
    RMSEA <- NA
  } else {
    target <- min(rez$RMSEA, na.rm = TRUE) + threshold * diff(range(rez$RMSEA, na.rm = TRUE))
    RMSEA <- rez[!is.na(rez$RMSEA) & rez$RMSEA <= target, "n"][1]
  }
  # RMSR
  if(all(is.na(rez$RMSR))) {
    RMSR <- NA
  } else {
    target <- min(rez$RMSR, na.rm = TRUE) + threshold * diff(range(rez$RMSR, na.rm = TRUE))
    RMSR <- rez[!is.na(rez$RMSR) & rez$RMSR <= target, "n"][1]
  }
  # CRMS
  if(all(is.na(rez$CRMS))) {
    CRMS <- NA
  } else {
    target <- min(rez$CRMS, na.rm = TRUE) + threshold * diff(range(rez$CRMS, na.rm = TRUE))
    CRMS <- rez[!is.na(rez$CRMS) & rez$CRMS <= target, "n"][1]
  }
  # BIC (this is a penalized method so we can just take the one that minimizes it)
  BIC <- ifelse(all(is.na(rez$BIC)), NA, rez[!is.na(rez$BIC) & rez$BIC == min(rez$BIC, na.rm = TRUE), "n"])

  data.frame(
    n_Factors = c(fit_off, TLI, RMSEA, RMSR, CRMS, BIC),
    Method = c("Fit_off", "TLI", "RMSEA", "RMSR", "CRMS", "BIC"),
    Family = c("Fit", "Fit", "Fit", "Fit", "Fit", "Fit")
  )
}

# PCDimension ------------------------

#' @keywords internal
.n_factors_PCDimension <- function(x = NULL, type = "PCA") {
  # This package is a strict dependency of PCDimension so if users have the
  # former they should have it
  insight::check_if_installed("ClassDiscovery")

  # Only applies to PCA with full data
  if(tolower(type) %in% c("fa", "factor", "efa") || !is.data.frame(x)) {
    return(data.frame())
  }
  # Randomization-Based Methods
  rez_rnd <- PCDimension::rndLambdaF(x)

  # Broken-Stick
  spca <- ClassDiscovery::SamplePCA(t(x))
  lambda <- spca@variances[1:(ncol(x)-1)]
  rez_bokenstick <- PCDimension::bsDimension(lambda)

  # Auer-Gervini
  ag <- PCDimension::AuerGervini(spca)
  agfuns <- list(twice=PCDimension::agDimTwiceMean,
                 specc=PCDimension::agDimSpectral,
                 km=PCDimension::agDimKmeans,
                 km3=PCDimension::agDimKmeans3,
                 # tt=PCDimension::agDimTtest,  # known to overestimate
                 # cpm=PCDimension::makeAgCpmFun("Exponential"), # known to overestimate
                 tt2=PCDimension::agDimTtest2,
                 cpt=PCDimension::agDimCPT)
  rez_ag <- PCDimension::compareAgDimMethods(ag, agfuns)

  data.frame(
    n_Factors = as.numeric(c(rez_rnd, rez_bokenstick, rez_ag)),
    Method = c("Random (lambda)", "Random (F)", "Broken-Stick", "Auer-Gervini (twice)", "Auer-Gervini (spectral)", "Auer-Gervini (kmeans-2)", "AuerGervini (kmeans-3)", "Auer-Gervini (T)", "AuerGervini (CPT)"),
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
  if (length(which(lambda < 0)) > 0) {
    stop("These indices are only valid with a principal component solution. So, only positive eigenvalues are permitted.")
  }

  minPar <- c(min(lambda) - abs(min(lambda)) + .001, 0.001)
  maxPar <- c(max(lambda), stats::lm(lambda ~ I(length(lambda):1))$coef[2])


  n <- N
  significance <- alpha
  min.k <- 3
  LRT <- data.frame(
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
    if (i > 1) {
      if (LRT$p[i - 1] <= 0.05) bentler.n <- bentler.n + as.numeric(LRT$p[i] <= significance)
    }
  }
  if (bentler.n == 0) bentler.n <- length(lambda)
  if (details == TRUE) details <- LRT else details <- NULL
  res <- list(detail = details, nFactors = bentler.n)
  class(res) <- c("nFactors", "list")
  res
}
