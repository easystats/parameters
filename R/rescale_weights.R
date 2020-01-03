#' @title Rescale design weights for multilevel analysis
#' @name rescale_weights
#'
#' @description Most functions to fit multilevel and mixed effects models only
#'    allow to specify frequency weights, but not design (i.e. sampling or probability)
#'    weights, which should be used when analyzing complex samples and survey data.
#'    \code{rescale_weights()} implements an algorithm proposed by \cite{Aaparouhov (2006)}
#'    and \cite{Carle (2009)} to rescale design weights in survey data to account for
#'    the grouping structure of multilevel models, which then can be used for
#'    multilevel modelling.
#'
#' @param data A data frame.
#' @param cluster Variable indicating the grouping structure (strata) of
#'    the survey data (level-2-cluster variable).
#' @param probability_weights Variable indicating the probability (design or sampling)
#'    weights of the survey data (level-1-weight).
#'
#' @return Two new weighting variables: \code{pweights_a} and \code{pweights_b},
#'    which represent the rescaled design weights to use in multilevel models
#'    (use these variables for the \code{weights} argument).
#'
#' @details Rescaling is based on two methods: For \code{pweights_a}, the sample
#'    weights \code{probability_weights} are adjusted by a factor that represents the proportion
#'    of cluster size divided by the sum of sampling weights within each cluster.
#'    The adjustment factor for \code{pweights_b} is the sum of sample weights
#'    within each cluster devided by the sum of squared sample weights within
#'    each cluster (see \cite{Carle (2009)}, Appendix B).
#'    \cr \cr
#'    Regarding the choice between scaling methods A and B, Carle suggests
#'    that "analysts who wish to discuss point estimates should report results
#'    based on weighting method A. For analysts more interested in residual
#'    between-cluster variance, method B may generally provide the least biased
#'    estimates". In general, it is recommended to fit a non-weighted model
#'    and weighted models with both scaling methods and when comparing the
#'    models, see whether the "inferential decisions converge", to gain
#'    confidence in the results.
#'    \cr \cr
#'    Though the bias of scaled weights decreases with increasing cluster size,
#'    method A is preferred when insufficient or low cluster size is a concern.
#'    \cr \cr
#'    The cluster ID and probably PSU may be used as random effects (e.g.
#'    nested design, or cluster and PSU as varying intercepts), depending
#'    on the survey design that should be mimicked.
#'
#' @references \itemize{
#'   \item Carle A.C. (2009). Fitting multilevel models in complex survey data with design weights: Recommendations. BMC Medical Research Methodology 9(49): 1-13
#'   \item Asparouhov T. (2006). General Multi-Level Modeling with Sampling Weights. Communications in Statistics - Theory and Methods 35: 439-460
#'   }
#'
#' @examples
#' library(sjstats)
#' data(nhanes_sample, package = "sjstats")
#' head(rescale_weights(nhanes_sample, "SDMVSTRA", "WTINT2YR"))
#'
#' library(lme4)
#' nhanes_sample <- cbind(
#'   nhanes_sample,
#'   rescale_weights(nhanes_sample, "SDMVSTRA", "WTINT2YR")
#' )
#' glmer(
#'   total ~ factor(RIAGENDR) * (log(age) + factor(RIDRETH1)) + (1 | SDMVPSU),
#'   family = poisson(),
#'   data = nhanes_sample,
#'   weights = pweights_a
#' )
#' @export
rescale_weights <- function(data, cluster, probability_weights) {

  # check if weight has missings. we need to remove them first,
  # and add back weights to correct cases later

  weight_missings <- which(is.na(data[[probability_weights]]))
  weight_non_na <- which(!is.na(data[[probability_weights]]))

  if (length(weight_missings) > 0) {
    data_tmp <- data[weight_non_na, ]
  } else {
    data_tmp <- data
  }


  # compute sum of weights per cluster
  design_weights <- data.frame(
    cluster = sort(unique(data_tmp[[cluster]])),
    sum_weights_by_cluster = tapply(data_tmp[[probability_weights]], as.factor(data_tmp[[cluster]]), sum),
    sum_squared_weights_by_cluster = tapply(data_tmp[[probability_weights]]^2, as.factor(data_tmp[[cluster]]), sum),
    n_per_group = as.vector(table(data_tmp[[cluster]])),
    stringsAsFactors = FALSE
  )

  colnames(design_weights)[1] <- cluster
  data_tmp <- merge(data_tmp, design_weights, by = cluster, sort = FALSE)

  # multiply the original weight by the fraction of the
  # sampling unit total population based on Carle 2009

  w_a <- data_tmp[[probability_weights]] * data_tmp$n_per_group / data_tmp$sum_weights_by_cluster
  w_b <- data_tmp[[probability_weights]] * data_tmp$sum_weights_by_cluster / data_tmp$sum_squared_weights_by_cluster

  data$pweights_a <- NA
  data$pweights_b <- NA

  data$pweights_a[weight_non_na] <- w_a
  data$pweights_b[weight_non_na] <- w_b

  data[, c("pweights_a", "pweights_b")]
}
