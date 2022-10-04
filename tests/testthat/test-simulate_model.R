.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

win_os <- tryCatch(
  {
    si <- Sys.info()
    if (!is.null(si["sysname"])) {
      si["sysname"] == "Windows" || grepl("^mingw", R.version$os)
    } else {
      FALSE
    }
  },
  error = function(e) {
    FALSE
  }
)

if (win_os && requiet("testthat") && requiet("parameters") && requiet("sandwich")) {
  mod <- lm(mpg ~ wt + cyl, data = mtcars)

  test_that("simulate_model, lm", {
    set.seed(123)
    s <- simulate_model(mod, iterations = 100)
    expect_equal(dim(s), c(100L, 3L))
    expect_equal(
      head(s$wt),
      c(-2.6182, -3.3655, -3.33661, -2.96915, -2.51617, -3.51031),
      tolerance = 1e-2
    )
    expect_equal(mean(s$cyl), -1.568975, tolerance = 1e-3)
  })

  test_that("simulate_model, vcov", {
    set.seed(123)
    s <- simulate_model(mod, iterations = 100, vcov = "HC1")
    expect_equal(dim(s), c(100L, 3L))
    expect_equal(
      head(s$wt),
      c(-2.65336, -3.31709, -3.45303, -3.00861, -2.63087, -3.61437),
      tolerance = 1e-2
    )
    expect_equal(mean(s$cyl), -1.562347, tolerance = 1e-3)
  })

  if (requiet("glmmTMB") && .runThisTest) {
    data(fish)
    mod <- suppressWarnings(glmmTMB(
      count ~ child + camper + (1 | persons),
      ziformula = ~ child + camper + (1 | persons),
      data = fish,
      family = truncated_poisson()
    ))

    test_that("simulate_model, glmmTMB", {
      set.seed(123)
      s <- simulate_model(mod, iterations = 100)
      expect_equal(dim(s), c(100L, 6L))
      expect_equal(
        colnames(s),
        c(
          "(Intercept)", "child", "camper1", "(Intercept)_zi", "child_zi",
          "camper1_zi"
        )
      )
      expect_equal(
        head(s$child),
        c(-1.21946, -1.23724, -1.10968, -1.14867, -1.04882, -1.11192),
        tolerance = 1e-2
      )
      expect_equal(mean(s$camper1), 0.717259, tolerance = 1e-3)      
    })

    test_that("simulate_model, glmmTMB, conditional only", {
      set.seed(123)
      s <- simulate_model(mod, component = "conditional", iterations = 100)
      expect_equal(dim(s), c(100L, 3L))
      expect_equal(colnames(s), c("(Intercept)", "child", "camper1"))
      expect_equal(
        head(s$child),
        c(-1.21946, -1.23724, -1.10968, -1.14867, -1.04882, -1.11192),
        tolerance = 1e-2
      )
      expect_equal(mean(s$camper1), 0.717259, tolerance = 1e-3)
    })
  }
}
