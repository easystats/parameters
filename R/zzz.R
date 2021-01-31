.onLoad <- function(libname, pkgname) {
  if (requireNamespace("emmeans", quietly = TRUE))
    emmeans::.emm_register(c("bootstrap_model", "bootstrap_parameters"), pkgname)
}