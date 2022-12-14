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

  test_that("simulate_parameters, lm", {
    set.seed(123)
    s1 <- simulate_parameters(mod)
    set.seed(123)
    s2 <- simulate_parameters(mod, vcov = "HC1")
    expect_equal(dim(s1), c(3L, 5L))
    expect_equal(dim(s2), c(3L, 5L))
    expect_false(isTRUE(all.equal(s1$Coefficient, s2$Coefficient, tolerance = 1e-5)))
    expect_false(isTRUE(all.equal(s1$Coefficient, s2$CI_low, tolerance = 1e-5)))
  })

  if (requiet("glmmTMB") && .runThisTest) {
    data(fish)
    mod <- suppressWarnings(glmmTMB(
      count ~ child + camper + (1 | persons),
      ziformula = ~ child + camper + (1 | persons),
      data = fish,
      family = truncated_poisson()
    ))

    test_that("simulate_parameters, glmmTMB", {
      set.seed(123)
      s <- simulate_parameters(mod)
      expect_equal(dim(s), c(6L, 6L))
      expect_equal(s$Coefficient, c(1.26979, -1.14433, 0.73637, -0.39618, 2.05839, -1.01957), tolerance = 1e-1)
      expect_equal(s$CI_low, c(0.33767, -1.33193, 0.55914, -1.65328, 1.44539, -1.65345), tolerance = 1e-1)
    })

    test_that("simulate_parameters, glmmTMB, conditional only", {
      set.seed(123)
      s <- simulate_parameters(mod, component = "conditional")
      expect_equal(dim(s), c(3L, 5L))
      expect_equal(s$Coefficient, c(1.26979, -1.14433, 0.73637), tolerance = 1e-1)
      expect_equal(s$CI_low, c(0.33767, -1.33193, 0.55914), tolerance = 1e-1)
    })
  }
}
