library(testthat)
library(parameters)
if (length(strsplit(packageDescription("parameters")$Version, "\\.")[[1]]) > 3) {
  Sys.setenv("RunAllparametersTests" = "yes")
} else {
  Sys.setenv("RunAllparametersTests" = "no")
}
test_check("parameters")
