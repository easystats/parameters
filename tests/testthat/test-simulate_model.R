.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

win_os <- tryCatch(
  {
    si <- Sys.info()
    if (!is.null(si["sysname"])) {
      si["sysname"] == "Windows" || startsWith(R.version$os, "mingw")
    } else {
      FALSE
    }
  },
  error = function(e) {
    FALSE
  }
)

if (win_os && getRversion() >= "4.0.0" && requiet("sandwich")) {
  mod <- lm(mpg ~ wt + cyl, data = mtcars)

  test_that("simulate_model, lm", {
    set.seed(123)
    s1 <- simulate_model(mod, iterations = 100)
    set.seed(123)
    s2 <- simulate_model(mod, iterations = 100, vcov = "HC1")
    expect_identical(dim(s1), c(100L, 3L))
    expect_identical(dim(s2), c(100L, 3L))
    expect_false(isTRUE(all.equal(head(s1$wt), head(s2$wt), tolerance = 1e-5)))
    expect_false(isTRUE(all.equal(mean(s1$cyl), mean(s2$cyl), tolerance = 1e-5)))
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
      expect_identical(dim(s), c(100L, 6L))
      expect_identical(
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
      expect_equal(mean(s$camper1), 0.717259, tolerance = 1e-1)
    })

    test_that("simulate_model, glmmTMB, conditional only", {
      set.seed(123)
      s <- simulate_model(mod, component = "conditional", iterations = 100)
      expect_identical(dim(s), c(100L, 3L))
      expect_identical(colnames(s), c("(Intercept)", "child", "camper1"))
      expect_equal(
        head(s$child),
        c(-1.21946, -1.23724, -1.10968, -1.14867, -1.04882, -1.11192),
        tolerance = 1e-2
      )
      expect_equal(mean(s$camper1), 0.717259, tolerance = 1e-1)
    })
  }
}
