#' (Partial) Eta Squared Squared.
#'
#' Computation of (Partial) Eta Squared for ANOVAs.
#'
#' @param model an ANOVA object.
#' @param partial Return partial eta squared.
#' @param ci Scalar between 0 and 1. If not \code{NULL}, lower and upper
#'   confidence intervals are returned as well.
#'
#' @examples
#' library(parameters)
#'
#' df <- iris
#' df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")
#'
#' model <- aov(Sepal.Length ~ Sepal.Big, data = df)
#' eta_squared(model)
#'
#' model <- anova(lm(Sepal.Length ~ Sepal.Big * Species, data = df))
#' eta_squared(model)
#'
#' \donttest{
#' # Don't work for now
#' model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)
#' eta_squared(model)
#' }
#'
#' @return Eta squared values.
#'
#' @export
eta_squared <- function(model, partial = TRUE, ci = NULL) {
  UseMethod("eta_squared")
}



#' @export
eta_squared.aov <- function(model, partial = TRUE, ci = NULL) {
  m <- .eta_squared(model, partial = partial, ci = ci)
  class(m) <- c(ifelse(isTRUE(partial), "partial_eta_squared", "eta_squared"), class(m))
  m
}

#' @export
eta_squared.anova <- eta_squared.aov


#' @export
eta_squared.aovlist <- function(model, partial = TRUE, ci = NULL) {
  stop("Eta squared not implemented yet for repeated-measures ANOVAs.")

  # params <- .extract_parameters_anova(model)
  # values <- .values_aov(params)
  #
  # if (!"Residuals" %in% params$Parameter) {
  #   stop("No residuals data found. Omega squared can only be computed for simple `aov` models.")
  # }
  #
  # mapply(function(p, v) {
  #   .extract_eta_squared(p, v, partial)
  # }, split(params, params$Group), values, SIMPLIFY = FALSE)
}



#' @keywords internal
.eta_squared <- function(model, partial, ci) {
  params <- .extract_parameters_anova(model)
  values <- .values_aov(params)

  if (!"Residuals" %in% params$Parameter) {
    stop("No residuals data found. Eta squared can only be computed for simple `aov` models.")
  }

  eff_size <- .extract_eta_squared(params, values, partial)

  .ci_eta_squared(
    x = eff_size,
    partial = partial,
    ci.lvl = ci,
    df = params[["df"]],
    statistic = params[["F"]]
  )
}



.extract_eta_squared <- function(params, values, partial) {
  if (partial == FALSE) {
    params[params$Parameter == "Residuals", "Eta_Sq"] <- NA
    params$Eta_Sq <- params$Sum_Squares / values$Sum_Squares_total
  } else {
    params$Eta_Sq_partial <- params$Sum_Squares / (params$Sum_Squares + values$Sum_Squares_residuals)
    params[params$Parameter == "Residuals", "Eta_Sq_partial"] <- NA
  }

  params[, intersect(c("Group", "Parameter", "Eta_Sq", "Eta_Sq_partial"), names(params)), drop = FALSE]
}



.ci_eta_squared <- function(x, partial, ci.lvl, df, statistic) {
  if (is.null(ci.lvl) || is.na(ci.lvl)) return(x)

  if (isTRUE(partial)) {
    ci_eta <- lapply(
      1:nrow(x),
      function(.x) {
        if (!is.na(statistic[.x])) {
          ci <- .ci_partial_eta_squared(
            F.value = statistic[.x],
            df1 = df[.x],
            df2 = df[nrow(x)],
            conf.level = ci.lvl
          )

          data.frame(
            CI_low = ci$LL,
            CI_high = ci$UL
          )
        } else {
          data.frame(
            CI_low = NA,
            CI_high = NA
          )
        }
      }
    )

    cbind(x, do.call(rbind, ci_eta))
  } else {
    ## TODO add bootstrapped CIs for normal eta-squared
    warning("Confidence intervals for eta-squared are not implemented yet.")
    x
  }
}