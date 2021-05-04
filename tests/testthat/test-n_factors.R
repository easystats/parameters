.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest &&
  require("testthat") &&
  require("parameters") &&
  suppressPackageStartupMessages(require("nFactors", quietly = TRUE)) &&
  suppressPackageStartupMessages(require("EGAnet", quietly = TRUE)) &&
  require("psych")) {
  test_that("n_factors", {
    set.seed(333)
    x <- n_factors(mtcars[, 1:4])
    expect_equal(ncol(x), 3)
  })
}
