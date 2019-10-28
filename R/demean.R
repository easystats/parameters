#' @title Compute group-meaned and de-meaned variables
#' @name demean
#'
#' @description \code{demean()} computes group- and de-meaned versions of a
#'    variable that can be used in regression analysis to model the between-
#'    and within-subject effect.
#'
#' @param x A data frame.
#' @param ... Names of variables that should be group- and de-meaned.
#' @param cluster_id Quoted or unquoted name of the variable that indicates the
#'    group- or cluster-ID.
#' @param suffix_dm,suffix_gm String value, will be appended to the names of the
#'   group-meaned and de-meaned variables of \code{x}. By default, de-meaned
#'   variables will be suffixed with \code{"_dm"} and grouped-meaned variables
#'   with \code{"_gm"}.
#' @param append Logical, if \code{TRUE}, new created variables including \code{x}
#'   are returned, else only the new created variables are returned.
#'
#' @return For \code{append = TRUE}, \code{x} including the group-/de-meaned
#'   variables as new columns is returned; if \code{append = FALSE}, only the
#'   group-/de-meaned variables will be returned.
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
#' demean(iris, Sepal.Length, Petal.Length, cluster_id = ID, append = FALSE)
#' @export
demean <- function(x, ..., cluster_id, append = TRUE, suffix_dm = "_dm", suffix_gm = "_gm") {
  # evaluate arguments, get variables from dots
  dots <- as.character(match.call(expand.dots = FALSE)$`...`)
  vars <- .dot_variables(x, dots)

  # parse cluster-id to string
  cluster_id <- gsub("\"", "", deparse(substitute(cluster_id)), fixed = TRUE)

  # get data to demean...
  dat <- x[, c(vars, cluster_id)]

  # group variables by cluster-id, then calculate the mean-value
  # for variables within each group (the group means). assign
  # mean values to a vector of same length as the data

  x_gm_list <- lapply(vars, function(i) {
    group_means <- tapply(dat[[i]], dat[[cluster_id]], mean, na.rm = TRUE)
    group_vector <- vector("numeric", nrow(dat))
    for (j in names(group_means)) {
      group_vector[dat[[cluster_id]] == j] <- group_means[j]
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

  colnames(x_dm) <- sprintf("%s%s", colnames(x_dm), suffix_dm)
  colnames(x_gm) <- sprintf("%s%s", colnames(x_gm), suffix_gm)


  if (append)
    dat <- cbind(dat, x_gm, x_dm)
  else
    dat <- cbind(x_gm, x_dm)

  dat
}
