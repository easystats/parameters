# classes: .gam, .list


#################### .gam ------


#' @rdname model_parameters.cgam
#' @export
model_parameters.gam <- model_parameters.cgam


#' @export
ci.gam <- function(x, ci = .95, method = NULL, ...) {
  .ci_generic(model = x, ci = ci, method = "wald", ...)
}


#' @export
standard_error.gam <- function(model, ...) {
  p.table <- summary(model)$p.table
  s.table <- summary(model)$s.table
  n_cond <- nrow(p.table)
  n_smooth <- nrow(s.table)

  .data_frame(
    Parameter = .remove_backticks_from_string(c(rownames(p.table), rownames(s.table))),
    SE = c(as.vector(p.table[, 2]), rep(NA, n_smooth)),
    Component = c(rep("conditional", n_cond), rep("smooth_terms", n_smooth))
  )
}


#' @export
p_value.gam <- function(model, ...) {
  p.table <- summary(model)$p.table
  s.table <- summary(model)$s.table

  d1 <- .data_frame(
    Parameter = rownames(p.table),
    p = as.vector(p.table[, 4]),
    Component = "conditional"
  )

  d2 <- .data_frame(
    Parameter = rownames(s.table),
    p = as.vector(s.table[, 4]),
    Component = "smooth_terms"
  )

  text_remove_backticks(rbind(d1, d2))
}


#' @export
simulate_model.gam <- function(model, iterations = 1000, ...) {
  insight::check_if_installed("MASS")

  if (is.null(iterations)) iterations <- 1000

  beta <- stats::coef(model)
  varcov <- insight::get_varcov(model, component = "all")

  out <- as.data.frame(MASS::mvrnorm(n = iterations, mu = beta, Sigma = varcov))

  class(out) <- c("parameters_simulate_model", class(out))
  attr(out, "object_name") <- .safe_deparse(substitute(model))
  out
}




#################### .list ------


#' @export
model_parameters.list <- function(model,
                                  ...) {
  if ("gam" %in% names(model)) {
    model <- model$gam
    class(model) <- c("gam", "lm", "glm")
    model_parameters(model, ...)
  } else if ("pamobject" %in% names(model)) {
    model <- model$pamobject
    model_parameters(model, ...)
  } else {
    stop("We don't recognize this object of class 'list'. Please raise an issue.")
  }
}


#' @export
ci.list <- function(x, ci = .95, ...) {
  if ("gam" %in% names(x)) {
    x <- x$gam
    class(x) <- c("gam", "lm", "glm")
    ci(x, ci = ci, ...)
  } else {
    return(NULL)
  }
}


#' @export
simulate_model.list <- function(model, iterations = 1000, ...) {
  if ("gam" %in% names(model)) {
    model <- model$gam
    class(model) <- c("gam", "lm", "glm")
    simulate_model(model, iterations = iterations, ...)
  }
}
