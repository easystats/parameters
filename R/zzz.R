.onLoad <- function(libname, pkgname) {
  if (requireNamespace("emmeans", quietly = TRUE)) {
    emmeans::.emm_register(c("bootstrap_model", "bootstrap_parameters"), pkgname)
  }

  # session warnings
  options("parameters_warning_exponentiate" = TRUE)
}

.onUnload <- function(libpath) {
  options("parameters_warning_exponentiate" = NULL)
}
