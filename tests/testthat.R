if (require("testthat")) {
  library(parameters)

  is_dev_version <- length(strsplit(packageDescription("parameters")$Version, ".", fixed = TRUE)[[1]]) > 3L
  if (is_dev_version) {
    Sys.setenv("RunAllparametersTests" = "yes")
  } else {
    Sys.setenv("RunAllparametersTests" = "no")
  }

  test_check("parameters")
}
