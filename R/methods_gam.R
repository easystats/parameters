# classes: .gam, .list


#################### .gam ------


#' @rdname model_parameters.cgam
#' @export
model_parameters.gam <- function(model,
                                 ci = .95,
                                 bootstrap = FALSE,
                                 iterations = 1000,
                                 standardize = NULL,
                                 exponentiate = FALSE,
                                 robust = FALSE,
                                 p_adjust = NULL,
                                 verbose = TRUE,
                                 ...) {

  # Processing
  if (bootstrap) {
    parameters <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <-
      .extract_parameters_generic(
        model,
        ci = ci,
        component = "all",
        merge_by = c("Parameter", "Component"),
        standardize = standardize,
        robust = robust,
        p_adjust = p_adjust
      )
  }

  # fix statistic column
  if ("t" %in% names(parameters) && !is.null(parameters$Component) && "smooth_terms" %in% parameters$Component) {
    names(parameters)[names(parameters) == "t"] <- "t / F"
  }

  # fix estimated df column
  if (inherits(model, c("gam", "scam")) && "smooth_terms" %in% parameters$Component && !("df" %in% names(parameters))) {
    parameters$df <- parameters$Coefficient
    parameters$df[parameters$Component != "smooth_terms"] <- NA
    parameters$Coefficient[parameters$Component == "smooth_terms"] <- NA
    # reorder
    insert_column <- which(names(parameters) == "df_error")
    if (!length(insert_column)) {
      insert_column <- which(names(parameters) == "p")
    }
    if (length(insert_column)) {
      n_col <- ncol(parameters)
      parameters <- parameters[c(1:(insert_column - 1), n_col, insert_column:(n_col - 1))]
    }
  }

  if (exponentiate) parameters <- .exponentiate_parameters(parameters)
  parameters <- .add_model_parameters_attributes(
    parameters,
    model,
    ci,
    exponentiate,
    p_adjust = p_adjust,
    verbose = verbose,
    ...
  )
  attr(parameters, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))

  # converting CI to fraction
  if ("CI" %in% names(parameters)) {
    parameters$CI <- parameters$CI / 100
  }

  parameters
}


#' @export
ci.gam <- function(x, ci = .95, ...) {
  ci_wald(model = x, ci = ci, ...)
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

  .remove_backticks_from_parameter_names(rbind(d1, d2))
}


#' @importFrom insight get_varcov
#' @export
simulate_model.gam <- function(model, iterations = 1000, ...) {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' needed for this function to work. Please install it.", call. = FALSE)
  }

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
                                  ci = .95,
                                  bootstrap = FALSE,
                                  iterations = 1000,
                                  robust = FALSE,
                                  verbose = TRUE,
                                  ...) {
  if ("gam" %in% names(model)) {
    model <- model$gam
    class(model) <- c("gam", "lm", "glm")
    model_parameters(
      model,
      ci = ci,
      bootstrap = bootstrap,
      iterations = iterations,
      robust = robust,
      ...
    )
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
