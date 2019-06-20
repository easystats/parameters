#' Number of Components/Factors to Retain
#'
#' This function runs many existing procedures for determining how many factors to retain for your factor analysis (FA) or dimension reduction (PCA).
#'
#' @param x A dataframe.
#' @param type Can be "FA" or "PCA", depending on what you want to do.
#' @param rotation Only used for VSS. The rotation to apply. Can be "none" "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT" and "bifactor" for orthogonal rotations, and "promax", "oblimin", "simplimax", "bentlerQ, "geominQ" and "biquartimin" and "cluster" for oblique transformations.
#' @param algorithm Factoring method used by VSS. Can be "pa" for Principal Axis Factor Analysis, "minres" for minimum residual (OLS) factoring, "mle" for Maximum Likelihood FA and "pc" for Principal Components. "default" will select "minres" if \code{type} is "FA" and "pc" if \code{type} is "PCA".
#' @param package Can be "all" or a vector containing "nFactors", "EGAnet" and "psych". These are the packages from which methods are used.
#' @param safe If TRUE, will run all the procedures in try blocks, and will only return those that work and silently skip the ones that may fail.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(parameters)
#'
#' n_factors(mtcars, type = "PCA", package = "nFactors")
#' \dontrun{
#'   n_factors(mtcars, type = "PCA")
#'   n_factors(mtcars, type = "FA", algorithm = "mle")
#' }
#'
#' @importFrom stats cor
#' @export
n_factors <- function(x, type = "FA", rotation = "varimax", algorithm = "default", package = "all", safe = TRUE, ...){

  if(package == "all"){
    package <- c("nFactors", "EGAnet", "psych")
  }

  # Initialize parameters
  nobs <- nrow(x)

  # TODO: this could be improved with better correlation
  cormatrix <- cor(x, use = "pairwise.complete.obs")
  eigen_values <- eigen(cormatrix)$values


  # Initialize dataframe
  out <- data.frame()

  # nFactors -------------------------------------------
  if("nFactors" %in% c(package)){

    if (!requireNamespace("nFactors", quietly = TRUE)) {
      stop("The package 'nFactors' is needed. Please install it by running 'install.packages(nFactors)'.")
    }

    if(safe){
      out <- rbind(out,
                   tryCatch(.n_factors_bartlett(x, cormatrix, nobs, eigen_values, type),
                            error = function(e) data.frame()))
      out <- rbind(out,
                   tryCatch(.n_factors_bentler(x, cormatrix, nobs, eigen_values, type),
                            error = function(e) data.frame()))
      out <- rbind(out,
                   tryCatch(.n_factors_cng(x, cormatrix, nobs, eigen_values, type),
                            error = function(e) data.frame()))
      out <- rbind(out,
                   tryCatch(.n_factors_mreg(x, cormatrix, nobs, eigen_values, type),
                            error = function(e) data.frame()))
      out <- rbind(out,
                   tryCatch(.n_factors_scree(x, cormatrix, nobs, eigen_values, type),
                            error = function(e) data.frame()))
      out <- rbind(out,
                   tryCatch(.n_factors_sescree(x, cormatrix, nobs, eigen_values, type),
                            error = function(e) data.frame()))
    } else{
      out <- rbind(out,
                   .n_factors_bartlett(x, cormatrix, nobs, eigen_values, type))
      out <- rbind(out,
                   .n_factors_bentler(x, cormatrix, nobs, eigen_values, type))
      out <- rbind(out,
                   .n_factors_cng(x, cormatrix, nobs, eigen_values, type))
      out <- rbind(out,
                   .n_factors_mreg(x, cormatrix, nobs, eigen_values, type))
      out <- rbind(out,
                   .n_factors_scree(x, cormatrix, nobs, eigen_values, type))
      out <- rbind(out,
                   .n_factors_sescree(x, cormatrix, nobs, eigen_values, type))
    }
  }

  # EGAnet -------------------------------------------
  if("EGAnet" %in% c(package)){

    if (!requireNamespace("EGAnet", quietly = TRUE)) {
      stop("The package 'EGAnet' is needed. Please install it by running 'install.packages(EGAnet)'.")
    }

    if(safe){
      out <- rbind(out,
                   tryCatch(.n_factors_ega(x, cormatrix, nobs, eigen_values, type),
                            error = function(e) data.frame()))
    } else{
      out <- rbind(out,
                   .n_factors_ega(x, cormatrix, nobs, eigen_values, type))
    }
  }


  # psych -------------------------------------------
  if("psych" %in% c(package)){
    if (!requireNamespace("psych", quietly = TRUE)) {
      stop("The package 'psych' is needed. Please install it by running 'install.packages(psych)'.")
    }

    if(safe){
      out <- rbind(out,
                   tryCatch(.n_factors_vss(x, cormatrix, nobs, type, rotation, algorithm),
                            error = function(e) data.frame()))
    } else{
      out <- rbind(out,
                   .n_factors_vss(x, cormatrix, nobs, type, rotation, algorithm))
    }
  }

  # OUTPUT ----------------------------------------------
  # TODO created weighted composite score


  out <- out[order(out$n_Factors), ]  # Arrange by n factors
  row.names(out) <- NULL  # Reset row index
  class(out) <- c("n_factors", class(out))
  out
}





















#' Bartlett, Anderson and Lawley Procedures
#' @keywords internal
.n_factors_bartlett <- function(x = NULL, cormatrix = NULL, nobs = NULL, eigen_values = NULL, type = "FA"){

  nfac <- nFactors::nBartlett(eigen_values, N = nobs, alpha = 0.05, details = FALSE)$nFactors
  data.frame(n_Factors = as.numeric(nfac),
             Method = tools::toTitleCase(names(nfac)),
             Family = "Barlett")
}


#' Bentler and Yuan's Procedure
#' @keywords internal
.n_factors_bentler <- function(x = NULL, cormatrix = NULL, nobs = NULL, eigen_values = NULL, type = "FA"){

  nfac <- nFactors::nBentler(eigen_values, N = nobs, alpha = 0.05)$nFactors

  data.frame(n_Factors = as.numeric(nfac),
             Method = "Bentler",
             Family = "Bentler")
}


#' Cattell-Nelson-Gorsuch CNG Indices
#' @keywords internal
.n_factors_cng <- function(x = NULL, cormatrix = NULL, nobs = NULL, eigen_values = NULL, type = "FA"){

  if(tolower(type) %in% c("fa", "factor", "efa")){
    model <- "factors"
  } else{
    model <- "components"
  }
  nfac <- nFactors::nCng(cormatrix, cor=TRUE, model=model)$nFactors

  data.frame(n_Factors = as.numeric(nfac),
             Method = "CNG",
             Family = "CNG")
}


#' Multiple Regression Procedure
#' @keywords internal
.n_factors_mreg <- function(x = NULL, cormatrix = NULL, nobs = NULL, eigen_values = NULL, type = "FA"){

  if(tolower(type) %in% c("fa", "factor", "efa")){
    model <- "factors"
  } else{
    model <- "components"
  }
  nfac <- nFactors::nMreg(cormatrix, cor=TRUE, model=model)$nFactors
  data.frame(n_Factors = as.numeric(nfac),
             Method = c("beta", "t", "p"),
             Family = "Multiple_regression")
}


#' Non Graphical Cattel's Scree Test
#' @keywords internal
.n_factors_scree <- function(x = NULL, cormatrix = NULL, nobs = NULL, eigen_values = NULL, type = "FA"){

  if(tolower(type) %in% c("fa", "factor", "efa")){
    model <- "factors"
  } else{
    model <- "components"
  }
  nfac <- unlist(nFactors::nScree(cormatrix, cor=TRUE, model=model)$Components)
  data.frame(n_Factors = as.numeric(nfac),
             Method = c("Optimal coordinates", "Acceleration factor", "Parallel analysis", "Kaiser criterion"),
             Family = "Scree")
}


#' Standard Error Scree and Coefficient of Determination Procedures
#' @keywords internal
.n_factors_sescree <- function(x = NULL, cormatrix = NULL, nobs = NULL, eigen_values = NULL, type = "FA"){

  if(tolower(type) %in% c("fa", "factor", "efa")){
    model <- "factors"
  } else{
    model <- "components"
  }
  nfac <- nFactors::nSeScree(cormatrix, cor=TRUE, model=model)$nFactors
  data.frame(n_Factors = as.numeric(nfac),
             Method = c("SE Scree", "R2"),
             Family = "Scree_SE")
}






# EGAnet ------------------------

#' @keywords internal
.n_factors_ega <- function(x = NULL, cormatrix = NULL, nobs = NULL, eigen_values = NULL, type = "FA"){

  # Replace with own corelation matrix
  suppressWarnings(suppressMessages(nfac_glasso <- EGAnet::EGA(x, model = "glasso", plot.EGA = FALSE)$n.dim))
  suppressWarnings(suppressMessages(nfac_TMFG <- EGAnet::EGA(x, model = "TMFG", plot.EGA = FALSE)$n.dim))

  data.frame(n_Factors = as.numeric(c(nfac_glasso, nfac_TMFG)),
             Method = c("Glasso", "TMFG"),
             Family = "EGA")
}


# psych ------------------------

#' @keywords internal
.n_factors_vss <- function(x = NULL, cormatrix = NULL, nobs = NULL, type = "FA", rotation = "varimax", algorithm = "default"){

  if(algorithm == "default"){
    if(tolower(type) %in% c("fa", "factor", "efa")){
      algorithm <- "minres"
    } else{
      algorithm <- "pc"
    }
  }


  # Compute VSS
  vss <- psych::VSS(
    cormatrix,
    n = ncol(x)-1,
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

  data.frame(n_Factors = as.numeric(c(vss_1, vss_2, velicer_MAP, BIC_reg, BIC_adj)),
             Method = c("VSS complexity 1", "VSS complexity 2", "Velicer's MAP", "BIC", "BIC (adjusted)"),
             Family = c("VSS", "VSS", "Velicers_MAP", "BIC", "BIC"))
}






#' @export
print.n_factors <- function(x, ...){

  # Summarize
  results <- data.frame(
    n_Factors = as.factor(unique(x$n_Factors)),
    n_Methods = as.vector(by(x, as.factor(x$n_Factors), function(x) n = nrow(x)))
  )

  # Extract info
  max_methods <- max(results$n_Methods)
  best_n <- results[results$n_Methods == max_methods, ]

  if(nrow(best_n) == 1){
    best_n_text <- as.character(best_n$n_Factors)
  } else{
    best_n_text <- paste0(best_n$n_Factors, collapse = " and ")
  }

  # Extract methods
  methods_text <- c()
  for(i in c(best_n$n_Factors)){
    methods <- x[x$n_Factors == i, ]$Method
    methods <- paste0(methods, collapse = ", ")
    methods_text <- c(methods_text, methods)
  }
  methods_text <- paste0(methods_text, collapse = "; ")


  # Text
  text <- paste0("The choice of ",
                 best_n_text,
                 " dimensions is supported by ",
                 max_methods,
                 " (",
                 sprintf("%.2f", max_methods / nrow(x) * 100),
                 "%) methods out of ",
                 nrow(x),
                 " (",
                 methods_text,
                 ").")

  insight::print_color("# Method Agreement Procedure:\n\n", "blue")
  cat(text)
}






# text <- paste0(
#     "The choice of ",
#     best_n,
#     factor_text,
#     "is supported by ",
#     n_methods,
#     " (out of ",
#     round(nrow(results)),
#     "; ",
#     round(n_methods / nrow(results) * 100, 2),
#     "%) methods (",
#     best_n_methods,
#     ")."
#   )
#
#
#   # Plot
#   # -------------
#   plot_data <- summary
#   plot_data$n.Methods.Ratio <- plot_data$n.Methods / sum(plot_data$n.Methods)
#   plot_data$n.Methods.Ratio <- plot_data$n.Methods.Ratio * (1 / max(plot_data$n.Methods.Ratio))
#   plot_data$area <- plot_data$n.Methods.Ratio / (max(plot_data$n.Methods.Ratio) / max(plot_data$Eigenvalues))
#   plot_data$var <- plot_data$Cum.Variance / (max(plot_data$Cum.Variance) / max(plot_data$Eigenvalues))
#
#   plot <- plot_data %>%
#     ggplot(aes_string(x = "n.Factors", y = "Eigenvalues")) +
#     geom_area(
#       aes_string(y = "area"),
#       fill = "#FFC107",
#       alpha = 0.5
#     ) +
#     geom_line(
#       colour = "#E91E63",
#       size = 1
#     ) +
#     geom_hline(yintercept = 1, linetype = "dashed", colour = "#607D8B") +
#     geom_line(
#       aes_string(y = "var"),
#       colour = "#2196F3",
#       size = 1
#     ) +
#     scale_y_continuous(sec.axis = sec_axis(
#       trans = ~ . * (max(plot_data$Cum.Variance) / max(plot_data$Eigenvalues)),
#       name = "Cumulative Variance\n"
#     )) +
#     ylab("Eigenvalues\n") +
#     xlab("\nNumber of Factors") +
#     theme_minimal()
