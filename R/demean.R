#' Compute group-meaned and de-meaned variables
#'
#' \code{demean()} computes group- and de-meaned versions of a
#'    variable that can be used in regression analysis to model the between-
#'    and within-subject effect. \code{check_heterogeneity()} checks if model
#'    predictors or variables may cause a heterogeneity bias, i.e. if variables
#'    have a within- and/or between-effect.
#'    \cr \cr
#'    \code{degroup()} is more generic in terms of the centering-operation.
#'    While \code{demean()} always uses mean-centering, \code{degroup()} can
#'    also use the mode or median for centering.
#'
#' @param x A data frame. For \code{check_heterogeneity()}, may also be a mixed model object.
#' @param select Character vector (or formula) with names of variables to select that should be group- and de-meaned. For \code{check_heterogeneity()}, if \code{x} is a mixed model object, this argument be ignored.
#' @param group Character vector (or formula) with the name of the variable that indicates the group- or cluster-ID. For \code{check_heterogeneity()}, if \code{x} is a model object, this argument be ignored.
#' @param center Method for centering. \code{demean()} always performs mean-centering, while \code{degroup()} can use \code{center = "median"} or \code{center = "mode"} for median- or mode-centering, and also \code{"min"} or \code{"max"}.
#' @param suffix_demean,suffix_groupmean String value, will be appended to the names of the
#'   group-meaned and de-meaned variables of \code{x}. By default, de-meaned
#'   variables will be suffixed with \code{"_within"} and grouped-meaned variables
#'   with \code{"_between"}.
#' @param add_attributes Logical, if \code{TRUE}, the returned variables gain
#'   attributes to indicate the within- and between-effects. This is only relevant
#'   when printing \code{model_parameters()} - in such cases, the within- and
#'   between-effects are printed in separated blocks.
#' @inheritParams p_value
#'
#' @return A data frame with the group-/de-meaned variables, which get the suffix
#'   \code{"_between"} (for the group-meaned variable) and \code{"_within"} (for
#'   the de-meaned variable) by default.
#'
#' @seealso If grand-mean centering (instead of centering within-clusters)
#'   is required, see \code{\link{center}}.
#'
#' @details
#'   \subsection{Heterogeneity Bias}{
#'     Mixed models include different levels of sources of variability, i.e.
#'     error terms at each level. When macro-indicators (or level-2 predictors,
#'     or higher-level units, or more general: \emph{group-level predictors that
#'     \strong{vary} within and across groups}) are included as fixed effects (i.e.
#'     treated as covariate at level-1), the variance that is left unaccounted for
#'     this covariate will be absorbed into the error terms of level-1 and level-2
#'     (\cite{Bafumi and Gelman 2006; Gelman and Hill 2007, Chapter 12.6.}):
#'     \dQuote{Such covariates contain two parts: one that is specific to the
#'     higher-level entity that does not vary between occasions, and one that
#'     represents the difference between occasions, within higher-level entities}
#'     (\cite{Bell et al. 2015}). Hence, the error terms will be correlated with
#'     the covariate, which violates one of the assumptions of mixed models
#'     (iid, independent and identically distributed error terms). This bias is
#'     also called the \emph{heterogeneity bias} (\cite{Bell et al. 2015}). To
#'     resolve this problem, level-2 predictors used as (level-1) covariates should
#'     be separated into their "within" and "between" effects by "de-meaning" and
#'     "group-meaning": After demeaning time-varying predictors, \dQuote{at the
#'     higher level, the mean term is no longer constrained by Level 1 effects,
#'     so it is free to account for all the higher-level variance associated
#'     with that variable} (\cite{Bell et al. 2015}).
#'   }
#'   \subsection{Panel data and correlating fixed and group effects}{
#'     \code{demean()} is intended to create group- and de-meaned variables
#'     for panel regression models (fixed effects models), or for complex
#'     random-effect-within-between models (see \cite{Bell et al. 2015, 2018}),
#'     where group-effects (random effects) and fixed effects correlate (see
#'     \cite{Bafumi and Gelman 2006}). This can happen, for instance, when
#'     analyzing panel data, which can lead to \emph{Heterogeneity Bias}. To
#'     control for correlating predictors and group effects, it is recommended
#'     to include the group-meaned and de-meaned version of \emph{time-varying covariates}
#'     (and group-meaned version of \emph{time-invariant covariates} that are on
#'     a higher level, e.g. level-2 predictors) in the model. By this, one can
#'     fit complex multilevel models for panel data, including time-varying
#'     predictors, time-invariant predictors and random effects.
#'    }
#'   \subsection{Why mixed models are preferred over fixed effects models}{
#'     A mixed models approach can model the causes of endogeneity explicitly
#'     by including the (separated) within- and between-effects of time-varying
#'     fixed effects and including time-constant fixed effects. Furthermore,
#'     mixed models also include random effects, thus a mixed models approach
#'     is superior to classic fixed-effects models, which lack information of
#'     variation in the group-effects or between-subject effects. Furthermore,
#'     fixed effects regression cannot include random slopes, which means that
#'     fixed effects regressions are neglecting \dQuote{cross-cluster differences
#'     in the effects of lower-level controls (which) reduces the precision of
#'     estimated context effects, resulting in unnecessarily wide confidence
#'     intervals and low statistical power} (\cite{Heisig et al. 2017}).
#'   }
#'   \subsection{Terminology}{
#'     The group-meaned variable is simply the mean of an independent variable
#'     within each group (or id-level or cluster) represented by \code{group}.
#'     It represents the cluster-mean of an independent variable. The regression
#'     coefficient of a group-meaned variable is the \emph{between-subject-effect}.
#'     The de-meaned variable is then the centered version of the group-meaned
#'     variable. De-meaning is sometimes also called person-mean centering or
#'     centering within clusters. The regression coefficient of a de-meaned
#'     variable represents the \emph{within-subject-effect}.
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
#'     For binary time-varying predictors, there are two recommendations. First
#'     is to include the raw (untransformed) binary predictor as fixed effect
#'     only and the \emph{de-meaned} variable as random effect (random slope).
#'     The alternative would be to add the de-meaned version(s) of binary
#'     time-varying covariates as additional fixed effect as well (instead of
#'     adding it as random slope). Centering time-varying binary variables to
#'     obtain within-effects (level 1) isn't necessary. They have a sensible
#'     interpretation when left in the typical 0/1 format (\cite{Hoffmann 2015,
#'     chapter 8-2.I}). \code{demean()} will thus coerce categorical time-varying
#'     predictors to numeric to compute the de- and group-meaned versions for
#'     these variables, where the raw (untransformed) binary predictor and the
#'     de-meaned version should be added to the model.
#'   }
#'   \subsection{De-meaning of factors with more than 2 levels}{
#'     Factors with more than two levels are demeaned in two ways: first, these
#'     are also converted to numeric and de-meaned; second, dummy variables
#'     are created (binary, with 0/1 coding for each level) and these binary
#'     dummy-variables are de-meaned in the same way (as described above).
#'     Packages like \pkg{panelr} internally convert factors to dummies before
#'     demeaning, so this behaviour can be mimicked here.
#'   }
#'   \subsection{De-meaning interaction terms}{
#'     There are multiple ways to deal with interaction terms of within- and
#'     between-effects. A classical approach is to simply use the product
#'     term of the de-meaned variables (i.e. introducing the de-meaned variables
#'     as interaction term in the model formula, e.g. \code{y ~ x_within * time_within}).
#'     This approach, however, might be subject to bias (see \cite{Giesselmann & Schmidt-Catran 2020}).
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
#'     from \pkg{lme4} can be found in
#'     \href{https://easystats.github.io/parameters/articles/demean.html}{this vignette}.
#'   }
#'
#' @references \itemize{
#'   \item Bafumi J, Gelman A. 2006. Fitting Multilevel Models When Predictors and Group Effects Correlate. In. Philadelphia, PA: Annual meeting of the American Political Science Association.
#'   \item Bell A, Fairbrother M, Jones K. 2019. Fixed and Random Effects Models: Making an Informed Choice. Quality & Quantity (53); 1051-1074
#'   \item Bell A, Jones K. 2015. Explaining Fixed Effects: Random Effects Modeling of Time-Series Cross-Sectional and Panel Data. Political Science Research and Methods, 3(1), 133–153.
#'   \item Gelman A, Hill J. 2007. Data Analysis Using Regression and Multilevel/Hierarchical Models. Analytical Methods for Social Research. Cambridge, New York: Cambridge University Press
#'   \item Giesselmann M, Schmidt-Catran, AW. 2020. Interactions in fixed effects regression models. Sociological Methods & Research, 1–28. https://doi.org/10.1177/0049124120914934
#'   \item Heisig JP, Schaeffer M, Giesecke J. 2017. The Costs of Simplicity: Why Multilevel Models May Benefit from Accounting for Cross-Cluster Differences in the Effects of Controls. American Sociological Review 82 (4): 796–827.
#'   \item Hoffman L. 2015. Longitudinal analysis: modeling within-person fluctuation and change. New York: Routledge
#' }

#' @examples
#' data(iris)
#' iris$ID <- sample(1:4, nrow(iris), replace = TRUE) # fake-ID
#' iris$binary <- as.factor(rbinom(150, 1, .35)) # binary variable
#'
#' x <- demean(iris, select = c("Sepal.Length", "Petal.Length"), group = "ID")
#' head(x)
#'
#' x <- demean(iris, select = c("Sepal.Length", "binary", "Species"), group = "ID")
#' head(x)
#'
#' check_heterogeneity(iris, select = c("Sepal.Length", "Petal.Length"), group = "ID")
#'
#' # demean interaction term x*y
#' dat <- data.frame(
#'   a = c(1, 2, 3, 4, 1, 2, 3, 4),
#'   x = c(4, 3, 3, 4, 1, 2, 1, 2),
#'   y = c(1, 2, 1, 2, 4, 3, 2, 1),
#'   ID = c(1, 2, 3, 1, 2, 3, 1, 2)
#' )
#' demean(dat, select = c("a", "x*y"), group = "ID")
#'
#' # or in formula-notation
#' demean(dat, select = ~ a + x * y, group = ~ID)
#' @export
demean <- function(x, select, group, suffix_demean = "_within", suffix_groupmean = "_between", add_attributes = TRUE, verbose = TRUE) {
  degroup(
    x = x,
    select = select,
    group = group,
    center = "mean",
    suffix_demean = suffix_demean,
    suffix_groupmean = suffix_groupmean,
    add_attributes = add_attributes,
    verbose = verbose
  )
}






#' @rdname demean
#' @importFrom stats median na.omit ave
#' @export
degroup <- function(x, select, group, center = "mean", suffix_demean = "_within", suffix_groupmean = "_between", add_attributes = TRUE, verbose = TRUE) {
  # ugly tibbles again...
  x <- as.data.frame(x)

  center <- match.arg(tolower(center), choices = c("mean", "median", "mode", "min", "max"))

  if (inherits(select, "formula")) {
    # formula to character, remove "~", split at "+"
    select <- trimws(unlist(strsplit(gsub("~", "", .safe_deparse(select), fixed = TRUE), "+", fixed = TRUE)))
  }

  if (inherits(group, "formula")) {
    group <- all.vars(group)
  }

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

  if (length(not_found) && isTRUE(verbose)) {
    insight::print_color(sprintf("%i variables were not found in the dataset: %s\n",
                                 length(not_found),
                                 paste0(not_found, collapse = ", ")),
                         color = "red")
  }

  select <- intersect(colnames(x), select)

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
    if (isTRUE(verbose)) {
      insight::print_color(
        sprintf(
          "Categorical predictors (%s) have been coerced to numeric values to compute de- and group-meaned variables.\n",
          paste0(names(categorical_predictors)[categorical_predictors], collapse = ", ")
        ),
        "yellow"
      )
    }
  }


  # group variables, then calculate the mean-value
  # for variables within each group (the group means). assign
  # mean values to a vector of same length as the data

  if (center == "mode") {
    x_gm_list <- lapply(select, function(i) {
      stats::ave(dat[[i]], dat[[group]], FUN = function(.gm) .mode(stats::na.omit(.gm)))
    })
  } else if (center == "median") {
    x_gm_list <- lapply(select, function(i) {
      stats::ave(dat[[i]], dat[[group]], FUN = function(.gm) stats::median(.gm, na.rm = TRUE))
    })
  } else if (center == "min") {
    x_gm_list <- lapply(select, function(i) {
      stats::ave(dat[[i]], dat[[group]], FUN = function(.gm) min(.gm, na.rm = TRUE))
    })
  } else if (center == "max") {
    x_gm_list <- lapply(select, function(i) {
      stats::ave(dat[[i]], dat[[group]], FUN = function(.gm) max(.gm, na.rm = TRUE))
    })
  } else {
    x_gm_list <- lapply(select, function(i) {
      stats::ave(dat[[i]], dat[[group]], FUN = function(.gm) mean(.gm, na.rm = TRUE))
    })
  }

  names(x_gm_list) <- select


  # create de-meaned variables by subtracting the group mean from each individual value

  x_dm_list <- lapply(select, function(i) dat[[i]] - x_gm_list[[i]])
  names(x_dm_list) <- select


  # convert to data frame and add suffix to column names

  x_gm <- as.data.frame(x_gm_list)
  x_dm <- as.data.frame(x_dm_list)

  colnames(x_dm) <- sprintf("%s%s", colnames(x_dm), suffix_demean)
  colnames(x_gm) <- sprintf("%s%s", colnames(x_gm), suffix_groupmean)

  if (isTRUE(add_attributes)) {
    x_dm[] <- lapply(x_dm, function(i) {
      attr(i, "within-effect") <- TRUE
      i
    })
    x_gm[] <- lapply(x_gm, function(i) {
      attr(i, "between-effect") <- TRUE
      i
    })
  }

  cbind(x_gm, x_dm)
}


#' @rdname demean
#' @export
detrend <- degroup




# helper ------------------

.mode <- function(x) {
  uniqv <- unique(x)
  tab <- tabulate(match(x, uniqv))
  idx <- which.max(tab)
  uniqv[idx]
}
