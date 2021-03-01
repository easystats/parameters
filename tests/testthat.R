library(testthat)
library(parameters)
if (length(strsplit(packageDescription("parameters")$Version, "\\.")[[1]]) > 3) {
  Sys.setenv("RunAllparametersTests" = "yes")
} else {
  Sys.setenv("RunAllparametersTests" = "no")
}

osx <- tryCatch(
  {
    si <- Sys.info()
    if (!is.null(si["sysname"])) {
      si["sysname"] == "Darwin" || grepl("^darwin", R.version$os)
    } else {
      FALSE
    }
  },
  error = function(e) {
    FALSE
  }
)

if (!osx) {
  test_check("parameters")
}
