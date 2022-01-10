if (require("testthat")) {
  library(parameters)
  if (length(strsplit(packageDescription("parameters")$Version, "\\.")[[1]]) > 3) {
    Sys.setenv("RunAllparametersTests" = "yes")
  } else {
    Sys.setenv("RunAllparametersTests" = "no")
  }

  si <- Sys.info()

  osx <- tryCatch(
    {
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

  solaris <- tryCatch(
    {
      if (!is.null(si["sysname"])) {
        grepl("SunOS", si["sysname"], ignore.case = TRUE)
      } else {
        FALSE
      }
    },
    error = function(e) {
      FALSE
    }
  )

  # if (!osx && !solaris) {
  #   test_check("parameters")
  # }
  test_check("parameters")
}
