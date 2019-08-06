#' @rdname eta_squared
#' @export
epsilon_squared <- function(model) {
  UseMethod("epsilon_squared")
}



#' @export
epsilon_squared.aov <- function(model) {
  m <- .epsilon_squared(model)
  class(m) <- c("epsilon_squared", class(m))
  m
}

#' @export
epsilon_squared.anova <- epsilon_squared.aov


#' @export
epsilon_squared.aovlist <- function(model) {
  stop("Epsilon squared not implemented yet for repeated-measures ANOVAs.")
}



#' @keywords internal
.epsilon_squared <- function(model) {
  params <- .extract_parameters_anova(model)
  values <- .values_aov(params)

  if (!"Residuals" %in% params$Parameter) {
    stop("No residuals data found. Eta squared can only be computed for simple `aov` models.")
  }

  .extract_epsilon_squared(params, values)
}


#' @keywords internal
.extract_epsilon_squared <- function(params, values) {
  params$Epsilon_sq <- (params$Sum_Squares - params$df * values$Mean_Square_residuals) / values$Sum_Squares_total
  params[params$Parameter == "Residuals", "Epsilon_sq"] <- NA

  params[, intersect(c("Group", "Parameter", "Epsilon_sq"), names(params)), drop = FALSE]
}
