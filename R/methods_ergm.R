# .ergm, btergm -----------------------

#' @export
ci.btergm <- function(x, ci = .95, ...) {
  as.data.frame(ci(as.data.frame(x@boot$t), ci = ci, ...))
}


#' @export
standard_error.btergm <- function(model, ...) {
  cf <- model@coef
  bootstraps <- model@boot$t

  sdev <- sapply(1:ncol(bootstraps), function(i) {
    cur <- (bootstraps[, i] - cf[i])^2
    sqrt(sum(cur)/length(cur))
  })

  .data_frame(
    Parameter = insight::find_parameters(model, flatten = TRUE),
    SE = as.vector(sdev)
  )
}


#' @export
p_value.btergm <- function(model, ...) {
  stat <- insight::get_statistic(model)
  pval <- 2 * stats::pnorm(abs(stat$Statistic), lower.tail = FALSE)

  .data_frame(
    Parameter = stat$Parameter,
    p = pval
  )
}
