#' @export
model_parameters.SemiParBIV <- function(model,
                                        ci = .95,
                                        bootstrap = FALSE,
                                        iterations = 1000,
                                        standardize = NULL,
                                        exponentiate = FALSE,
                                        p_adjust = NULL,
                                        verbose = TRUE,
                                        ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    component = "all",
    merge_by = c("Parameter", "Component"),
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


#' @export
p_value.SemiParBIV <- function(model, ...) {
  s <- summary(model)
  s <- datawizard::compact_list(s[grepl("^tableP", names(s))])
  params <- do.call(rbind, lapply(1:length(s), function(i) {
    out <- as.data.frame(s[[i]])
    out$Parameter <- rownames(out)
    out$Component <- paste0("Equation", i)
    out
  }))
  colnames(params)[4] <- "p"
  rownames(params) <- NULL
  text_remove_backticks(params[c("Parameter", "p", "Component")])
}


#' @export
standard_error.SemiParBIV <- function(model, ...) {
  s <- summary(model)
  s <- datawizard::compact_list(s[grepl("^tableP", names(s))])
  params <- do.call(rbind, lapply(1:length(s), function(i) {
    out <- as.data.frame(s[[i]])
    out$Parameter <- rownames(out)
    out$Component <- paste0("Equation", i)
    out
  }))
  colnames(params)[2] <- "SE"
  rownames(params) <- NULL
  text_remove_backticks(params[c("Parameter", "SE", "Component")])
}
