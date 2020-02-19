#' Compute group-meaned and de-meaned variables
#'
#' \code{demean()} computes group- and de-meaned versions of a
#'    variable that can be used in regression analysis to model the between-
#'    and within-subject effect.
#'
#' @param x A data frame.
#' @param select Character vector with names of variables to select that should be group- and de-meaned.
#' @param group Name of the variable that indicates the group- or cluster-ID.
#' @param suffix_demean,suffix_groupmean String value, will be appended to the names of the
#'   group-meaned and de-meaned variables of \code{x}. By default, de-meaned
#'   variables will be suffixed with \code{"_within"} and grouped-meaned variables
#'   with \code{"_between"}.
#'
#' @return A data frame with the group-/de-meaned variables, which get the suffix
#'   \code{"_between"} (for the group-meaned variable) and \code{"_within"} (for
#'   the de-meaned variable) by default.
#'
#' @details
#'   \subsection{Panel data and correlating fixed and group effects}{
#'     \code{demean()} is intended to create group- and de-meaned variables
#'     for panel regression models (fixed effects models), or for complex
#'     random-effect-within-between models (see \cite{Bell et al. 2018}),
#'     where group-effects (random effects) and fixed effects correlate (see
#'     \cite{Bafumi and Gelman 2006)}). This violation of one of the
#'     \emph{Gauss-Markov-assumptions} can happen, for instance, when analysing panel
#'     data. To control for correlating predictors and group effects, it is
#'     recommended to include the group-meaned and de-meaned version of
#'     \emph{time-varying covariates} in the model. By this, one can fit
#'     complex multilevel models for panel data, including time-varying predictors,
#'     time-invariant predictors and random effects. This approach is superior to
#'     classic fixed-effects models, which lack information of variation in the
#'     group-effects or between-subject effects.
#'   }
#'   \subsection{Terminology}{
#'     The group-meaned variable is simply the mean of an independent variable
#'     within each group (or id-level or cluster) represented by \code{group}.
#'     It represents the cluster-mean of an independent variable. The de-meaned
#'     variable is then the centered version of the group-meaned variable. De-meaning
#'     is sometimes also called person-mean centering or centering within clusters.
#'   }
#'   \subsection{De-meaning with continuous predictors}{
#'     For continuous time-varying predictors, the recommendation is to include
#'     both their de-meaned and group-meaned versions as fixed effects, but not
#'     the raw (untransformed) time-varying predictors themselves. The de-meaned
#'     predictor should also be included as random effect (random slope). In
#'     regression models, the coefficient of the de-meaned predictors indicates
#'     the within-subject effect, while the coefficient of the group-meaned
#'     predictor indicates the between-subject effect.
#'   }
#'   \subsection{De-meaning with binary predictors}{
#'     For binary time-varying predictors, the recommendation is to include
#'     the raw (untransformed) binary predictor as fixed effect only and the
#'     \emph{de-meaned} variable as random effect (random slope)
#'     (\cite{Hoffmann 2015, chapter 8-2.I}). \code{demean()} will thus coerce
#'     categorical time-varying predictors to numeric to compute the de- and
#'     group-meaned versions for these variables.
#'   }
#'   \subsection{De-meaning interaction terms}{
#'     There are multiple ways to deal with interaction terms of within- and
#'     between-effects. A classical approach is to simply use the product
#'     term of the de-meaned variables (i.e. introducing the de-meaned variables
#'     as interaction term in the model formula, e.g. \code{y ~ x_within * time_within}).
#'     This approach, however, might be subject to bias (see \cite{Giesselmann & Schmidt-Catran 2018}).
#'     \cr \cr
#'     Another option is to first calculate the product term and then apply the
#'     de-meaning to it. This approach produces an estimator \dQuote{that reflects
#'     unit-level differences of interacted variables whose moderators vary
#'     within units}, which is desirable if \emph{no} within interaction of
#'     two time-dependent variables is required. \cr \cr
#'     A third option, when the interaction should result in a genuine within
#'     estimator, is to "double de-mean" the interaction terms
#'     (\cite{Giesselmann & Schmidt-Catran 2018}), however, this is currently
#'     not supported by \code{demean()}. If this is required, the \code{wmb()}
#'     function from the \pkg{panelr} package should be used. \cr \cr
#'     To de-mean interaction terms for within-between models, simply specify
#'     the term as interaction for the \code{select}-argument, e.g.
#'     \code{select = "a*b"} (see 'Examples').
#'   }
#'   \subsection{Analysing panel data with mixed models using lme4}{
#'     A description of how to translate the
#'     formulas described in \emph{Bell et al. 2018} into R using \code{lmer()}
#'     from \pkg{lme4} or \code{glmmTMB()} from \pkg{glmmTMB} can be found here:
#'     \href{https://strengejacke.github.io/mixed-models-snippets/random-effects-within-between-effects-model.html}{for lmer()}
#'     and \href{https://strengejacke.github.io/mixed-models-snippets/random-effects-within-between-effects-model-glmmtmb.html}{for glmmTMB()}.
#'   }
#'
#' @references \itemize{
#'   \item Bafumi J, Gelman A. 2006. Fitting Multilevel Models When Predictors and Group Effects Correlate. In. Philadelphia, PA: Annual meeting of the American Political Science Association.
#'   \item Bell A, Fairbrother M, Jones K. 2018. Fixed and Random Effects Models: Making an Informed Choice. Quality & Quantity.
#'   \item Giesselmann M, Schmidt-Catran A. (2018). Interactions in fixed effects regression models (Discussion Papers of DIW Berlin No. 1748). DIW Berlin, German Institute for Economic Research. Retrieved from https://ideas.repec.org/p/diw/diwwpp/dp1748.html
#'   \item Hoffman L. 2015. Longitudinal analysis: modeling within-person fluctuation and change. New York: Routledge
#' }

#' @examples
#' data(iris)
#' iris$ID <- sample(1:4, nrow(iris), replace = TRUE) # fake-ID
#' iris$binary <- as.factor(rbinom(150, 1, .35)) # binary variable
#'
#' x <- demean(iris, select = c("Sepal.Length", "Petal.Length"), group = ID)
#' head(x)
#'
#' x <- demean(iris, select = c("Sepal.Length", "binary", "Species"), group = ID)
#' head(x)
#'
#' # demean interaction term x*y
#' dat <- data.frame(
#'   a = c(1, 2, 3, 4, 1, 2, 3, 4),
#'   x = c(4, 3, 3, 4, 1, 2, 1, 2),
#'   y = c(1, 2, 1, 2, 4, 3, 2, 1),
#'   ID = c(1, 2, 3, 1, 2, 3, 1, 2)
#' )
#' demean(dat, select = c("a", "x*y"), group = "ID")
#' @importFrom stats ave
#' @export
demean <- function(x, select, group, suffix_demean = "_within", suffix_groupmean = "_between") {
  interactions_no <- select[!grepl("(\\*|\\:)", select)]
  interactions_yes <- select[grepl("(\\*|\\:)", select)]

  if (length(interactions_yes)) {
    interaction_terms <- lapply(strsplit(interactions_yes, "*", fixed = TRUE), trimws)
    product <- lapply(interaction_terms, function(i) do.call(`*`, x[, i]))
    new_dat <- as.data.frame(stats::setNames(product, gsub("\\s", "", gsub("*", "_", interactions_yes, fixed = TRUE))))
    x <- cbind(x, new_dat)
    select <- c(interactions_no, colnames(new_dat))
  }

  not_found <- setdiff(select, colnames(x))

  if (length(not_found)) {
    insight::print_color(sprintf(
      "%i variables were not found in the dataset: %s\n",
      length(not_found),
      paste0(not_found, collapse = ", ")
    ),
    color = "red"
    )
  }

  select <- intersect(colnames(x), select)

  # parse group-variable name to string
  group <- gsub("\"", "", deparse(substitute(group)), fixed = TRUE)

  # get data to demean...
  dat <- x[, c(select, group)]


  # find categorical predictors that are coded as factors
  categorical_predictors <- sapply(dat[select], is.factor)

  # convert binary predictors to numeric
  if (any(categorical_predictors)) {
    # convert categorical to numeric, and then demean
    dat[select[categorical_predictors]] <- lapply(
      dat[select[categorical_predictors]],
      function(i) as.numeric(i) - 1
    )
    # convert categorical to dummy, and demean each binary dummy
    for (i in select[categorical_predictors]) {
      if (nlevels(x[[i]]) > 2) {
        for (j in levels(x[[i]])) {
          # create vector with zeros
          f <- rep(0, nrow(x))
          # for each matching level, set dummy to 1
          f[x[[i]] == j] <- 1
          dummy <- data.frame(f)
          # colnames = variable name + factor level
          # also add new dummy variables to "select"
          colnames(dummy) <- sprintf("%s_%s", i, j)
          select <- c(select, sprintf("%s_%s", i, j))
          # add to data
          dat <- cbind(dat, dummy)
        }
      }
    }
    # tell user...
    insight::print_color(
      sprintf(
        "Categorical predictors (%s) have been coerced to numeric values to compute de- and group-meaned variables.\n",
        paste0(names(categorical_predictors)[categorical_predictors], collapse = ", ")
      ),
      "yellow"
    )
  }


  # group variables, then calculate the mean-value
  # for variables within each group (the group means). assign
  # mean values to a vector of same length as the data

  x_gm_list <- lapply(select, function(i) {
    stats::ave(dat[[i]], dat[[group]], FUN = function(.gm) mean(.gm, na.rm = TRUE))
  })

  names(x_gm_list) <- select


  # create de-meaned variables by substracting the group mean from each individual value

  x_dm_list <- lapply(select, function(i) dat[[i]] - x_gm_list[[i]])
  names(x_dm_list) <- select


  # convert to data frame and add suffix to column names

  x_gm <- as.data.frame(x_gm_list)
  x_dm <- as.data.frame(x_dm_list)

  colnames(x_dm) <- sprintf("%s%s", colnames(x_dm), suffix_demean)
  colnames(x_gm) <- sprintf("%s%s", colnames(x_gm), suffix_groupmean)

  cbind(x_gm, x_dm)
}
