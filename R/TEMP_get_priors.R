# THIS IS TENPORARILY HERE WAITING TO BE POLISHED AND RELOCATED IN INSIGHT

#' Get information about priors specifications
#' @param model A Bayesian statistical model.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' 
#' model <- rstanarm::stan_glm(Sepal.Length ~ Petal.Width, data = iris)
#' get_priors(model)
#' 
#' 
#' library(brms)
#' 
#' model <- brms::brm(Sepal.Length ~ Petal.Width, data = iris)
#' get_priors(model)
#' }
#' @export
get_priors <- function(model, ...) {
  UseMethod("get_priors")
}




#' @importFrom utils tail head
#' @importFrom tools toTitleCase
#' @export
get_priors.stanreg <- function(model, ...) {
  if (!requireNamespace("rstanarm")) {
    warning("This function needs `rstanarm` to be installed... installing now.")
    install.packages("rstanarm")
    requireNamespace("rstanarm")
  }


  info <- rstanarm::prior_summary(model)

  # Intercept
  df <- .priors_to_df(info$prior_intercept)
  df$parameter <- "(Intercept)"

  # Priors
  if (!is.null(info$prior)) {
    priors <- .priors_to_df(info$prior)
    priors$parameter <- head(tail(insight::find_parameters(model)$conditional, -1), nrow(priors)) # This head() thing is to deal with rstanarm_gamm4 for which find_parameters returns all the smooth info.
    df <- rbind(df, priors[names(priors) %in% names(df)])
  }


  # Aux
  if (!is.null(info$prior_aux)) {
    aux <- .priors_to_df(info$prior_aux)
    aux$parameter <- aux$aux_name
    df <- rbind(df, aux[names(aux) %in% names(df)])
  }



  df <- df[c("parameter", names(df)[names(df) != "parameter"])]
  names(df) <- gsub("dist", "distribution", names(df))
  names(df) <- gsub("df", "DoF", names(df))
  names(df) <- gsub("adjusted_scale", "scale_adjusted", names(df))
  names(df) <- tools::toTitleCase(names(df))
  names(df)[names(df) != "Parameter"] <- paste0("Prior_", names(df)[names(df) != "Parameter"])
  return(df)
}




#' @export
get_priors.brmsfit <- function(model, ...) {
  # info <- brms::prior_summary(model)
  # info$coef
  # info$class
  # info$prior
  stop("brms models not supported yet.")
}




#' @keywords internal
.priors_to_df <- function(priors) {
  max_length <- max(sapply(priors, length))
  for (i in names(priors)) {
    if (length(priors[[i]]) < max_length) {
      if (is.null(priors[[i]])) {
        priors[[i]] <- NA
      }
      priors[[i]] <- rep_len(priors[[i]], max_length)
    }
  }
  if (max_length == 1) {
    priors <- as.data.frame(t(sapply(priors, c)))
  } else {
    priors <- as.data.frame(sapply(priors, c))
  }

  priors[!names(priors) %in% c("dist", "aux_name")] <- sapply(priors[!names(priors) %in% c("dist", "aux_name")], function(x) as.numeric(as.character(x)))
  return(priors)
}
