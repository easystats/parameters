#' @title Compute group-meaned and de-meaned variables
#' @name demean
#'
#' @description \code{demean()} computes group- and de-meaned versions of a
#'    variable that can be used in regression analysis to model the between-
#'    and within-subject effect.
#'
#' @param x A data frame.
#' @param ... Names of variables that should be group- and de-meaned.
#' @param group Quoted or unquoted name of the variable that indicates the
#'    group- or cluster-ID.
#' @param suffix_demean,suffix_groupmean String value, will be appended to the names of the
#'   group-meaned and de-meaned variables of \code{x}. By default, de-meaned
#'   variables will be suffixed with \code{"_dm"} and grouped-meaned variables
#'   with \code{"_gm"}.
#'
#' @return A data frame with the group-/de-meaned variables.
#'
#' @details \code{demean()} is intended to create group- and de-meaned variables
#'    for complex random-effect-within-between models (see \cite{Bell et al. 2018}),
#'    where group-effects (random effects) and fixed effects correlate (see
#'    \cite{Bafumi and Gelman 2006)}). This violation of one of the
#'    \emph{Gauss-Markov-assumptions} can happen, for instance, when analysing panel
#'    data. To control for correlating predictors and group effects, it is
#'    recommended to include the group-meaned and de-meaned version of
#'    \emph{time-varying covariates} in the model. By this, one can fit
#'    complex multilevel models for panel data, including time-varying predictors,
#'    time-invariant predictors and random effects. This approach is superior to
#'    classic fixed-effects models, which lack information of variation in the
#'    group-effects or between-subject effects.
#'    \cr \cr
#'    A description of how to translate the
#'    formulas described in \emph{Bell et al. 2018} into R using \code{lmer()}
#'    from \pkg{lme4} or \code{glmmTMB()} from \pkg{glmmTMB} can be found here:
#'    \href{https://strengejacke.github.io/mixed-models-snippets/random-effects-within-between-effects-model.html}{for lmer()}
#'    and \href{https://strengejacke.github.io/mixed-models-snippets/random-effects-within-between-effects-model-glmmtmb.html}{for glmmTMB()}.
#'
#' @references
#'   Bafumi J, Gelman A. 2006. Fitting Multilevel Models When Predictors and Group Effects Correlate. In. Philadelphia, PA: Annual meeting of the American Political Science Association.
#'   \cr \cr
#'   Bell A, Fairbrother M, Jones K. 2018. Fixed and Random Effects Models: Making an Informed Choice. Quality & Quantity. \doi{10.1007/s11135-018-0802-x}
#'
#' @examples
#' data(iris)
#' iris$ID <- sample(1:4, nrow(iris), replace = TRUE) # fake-ID
#' demean(iris, Sepal.Length, Petal.Length, group = ID)
#' @export
demean <- function(x, ..., group, suffix_demean = "_dm", suffix_groupmean = "_gm") {
  # evaluate arguments, get variables from dots
  dots <- as.character(match.call(expand.dots = FALSE)$`...`)
  vars <- .dot_variables(x, dots)

  # parse group-variable name to string
  group <- gsub("\"", "", deparse(substitute(group)), fixed = TRUE)

  # get data to demean...
  dat <- x[, c(vars, group)]

  # group variables, then calculate the mean-value
  # for variables within each group (the group means). assign
  # mean values to a vector of same length as the data

  x_gm_list <- lapply(vars, function(i) {
    group_means <- tapply(dat[[i]], dat[[group]], mean, na.rm = TRUE)
    group_vector <- vector("numeric", nrow(dat))
    for (j in names(group_means)) {
      group_vector[dat[[group]] == j] <- group_means[j]
    }
    group_vector
  })

  names(x_gm_list) <- vars


  # create de-meaned variables by substracting the group mean from each individual value

  x_dm_list <- lapply(vars, function(i) dat[[i]] - x_gm_list[[i]])
  names(x_dm_list) <- vars


  # convert to data frame and add suffix to column names

  x_gm <- as.data.frame(x_gm_list)
  x_dm <- as.data.frame(x_dm_list)

  colnames(x_dm) <- sprintf("%s%s", colnames(x_dm), suffix_demean)
  colnames(x_gm) <- sprintf("%s%s", colnames(x_gm), suffix_groupmean)

  cbind(x_gm, x_dm)
}
