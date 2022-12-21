# Test Setup --------------------------------

# only run for dev-version, not on CRAN
.runThisTest <- length(strsplit(packageDescription("parameters")$Version, ".", fixed = TRUE)[[1]]) > 3

# only run on windows - there are some minor rounding issues on other OS
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



# tests --------------------------------

## TODO also check messages for profiled CI

if (.runThisTest && win_os &&


  requiet("glmmTMB") &&
  requiet("lme4") &&
  packageVersion("glmmTMB") >= "1.1.5") {
  data(sleepstudy)
  data(cake)
  set.seed(123)
  sleepstudy$Months <- sample(1:4, nrow(sleepstudy), TRUE)

  set.seed(123)
  m1 <- suppressWarnings(glmmTMB(angle ~ temperature + (temperature | recipe) + (temperature | replicate), data = cake))
  m2 <- glmmTMB(Reaction ~ Days + (Days | Subject), data = sleepstudy)
  m3 <- suppressWarnings(glmmTMB(angle ~ temperature + (temperature | recipe), data = cake))
  m4 <- suppressWarnings(glmmTMB(angle ~ temperature + (temperature | replicate), data = cake))
  m5 <- suppressWarnings(glmmTMB(Reaction ~ Days + (Days + Months | Subject), data = sleepstudy))

  set.seed(123)
  expect_message(mp1 <- model_parameters(m1, ci_random = TRUE), "singularity")
  mp2 <- model_parameters(m2, ci_random = TRUE) # works
  expect_message(mp3 <- model_parameters(m3, ci_random = TRUE), "singularity") # no SE/CI
  mp4 <- model_parameters(m4, ci_random = TRUE)
  expect_message(mp5 <- model_parameters(m5, ci_random = TRUE), "singularity") # no SE/CI

  test_that("random effects CIs, two slopes, categorical", {
    ## FIXME: Results differ across R versions, no idea why...
    # expect_equal(
    #   mp1$CI_low,
    #   c(
    #     28.91277, 5.03129, -1.87302, -2.42033, -3.2702, -2.57721, 0.2157,
    #     4.20738, NaN, NaN, 0.26244, 0.34083, 0.02479, 0.66487, 0.40589,
    #     0.15295, 0.01405, 0.62939, -0.99996, -0.41209, NaN, NaN, NaN,
    #     -0.40223, NaN, NaN, NaN, NaN, NA, NA, NA, NA, NA, NA, NA, NA,
    #     NA, NaN, NA, NA, NA, NA, NA, NA, NA, NA, NA, NaN, 4.12596
    #   ),
    #   tolerance = 1e-2,
    #   ignore_attr = TRUE
    # )

    expect_identical(
      mp1$Parameter,
      c(
        "(Intercept)", "temperature.L", "temperature.Q", "temperature.C",
        "temperature^4", "temperature^5", "SD (Intercept)", "SD (Intercept)",
        "SD (temperature.L)", "SD (temperature.Q)", "SD (temperature.C)",
        "SD (temperature^4)", "SD (temperature^5)", "SD (temperature.L)",
        "SD (temperature.Q)", "SD (temperature.C)", "SD (temperature^4)",
        "SD (temperature^5)", "Cor (Intercept~temperature.L)", "Cor (Intercept~temperature.Q)",
        "Cor (Intercept~temperature.C)", "Cor (Intercept~temperature^4)",
        "Cor (Intercept~temperature^5)", "Cor (Intercept~temperature.L)",
        "Cor (Intercept~temperature.Q)", "Cor (Intercept~temperature.C)",
        "Cor (Intercept~temperature^4)", "Cor (Intercept~temperature^5)",
        "Cor (temperature.L~temperature.Q)", "Cor (temperature.L~temperature.C)",
        "Cor (temperature.L~temperature^4)", "Cor (temperature.L~temperature^5)",
        "Cor (temperature.Q~temperature.C)", "Cor (temperature.Q~temperature^4)",
        "Cor (temperature.Q~temperature^5)", "Cor (temperature.C~temperature^4)",
        "Cor (temperature.C~temperature^5)", "Cor (temperature^4~temperature^5)",
        "Cor (temperature.L~temperature.Q)", "Cor (temperature.L~temperature.C)",
        "Cor (temperature.L~temperature^4)", "Cor (temperature.L~temperature^5)",
        "Cor (temperature.Q~temperature.C)", "Cor (temperature.Q~temperature^4)",
        "Cor (temperature.Q~temperature^5)", "Cor (temperature.C~temperature^4)",
        "Cor (temperature.C~temperature^5)", "Cor (temperature^4~temperature^5)",
        "SD (Observations)"
      )
    )

    expect_identical(
      mp1$Group,
      c(
        "", "", "", "", "", "", "recipe", "replicate", "recipe", "recipe",
        "recipe", "recipe", "recipe", "replicate", "replicate", "replicate",
        "replicate", "replicate", "recipe", "recipe", "recipe", "recipe",
        "recipe", "replicate", "replicate", "replicate", "replicate",
        "replicate", "recipe", "recipe", "recipe", "recipe", "recipe",
        "recipe", "recipe", "recipe", "recipe", "recipe", "replicate",
        "replicate", "replicate", "replicate", "replicate", "replicate",
        "replicate", "replicate", "replicate", "replicate", "Residual"
      )
    )
  })


  test_that("random effects CIs, simple slope", {
    expect_equal(
      mp2$CI_low,
      c(238.40611, 7.52295, 15.01709, 3.80546, -0.48781, 22.80047),
      tolerance = 1e-2,
      ignore_attr = TRUE
    )

    expect_identical(
      mp2$Parameter,
      c(
        "(Intercept)", "Days", "SD (Intercept)", "SD (Days)", "Cor (Intercept~Days)",
        "SD (Observations)"
      )
    )
  })


  test_that("random effects CIs, categorical slope-1", {
    ## FIXME: Results differ across R versions, no idea why...
    # expect_equal(
    #   mp3$CI_low,
    #   c(
    #     31.20278, 4.35879, -2.63767, -2.80041, -3.54983, -3.16627,
    #     0, 0, NaN, NaN, 0, 0, -1, NaN, NaN, NaN, NaN, NA, NA, NA, NA,
    #     NA, NA, NA, NA, NA, NaN, 7.08478
    #   ),
    #   tolerance = 1e-2,
    #   ignore_attr = TRUE
    # )

    expect_identical(
      mp3$Parameter,
      c(
        "(Intercept)", "temperature.L", "temperature.Q", "temperature.C",
        "temperature^4", "temperature^5", "SD (Intercept)", "SD (temperature.L)",
        "SD (temperature.Q)", "SD (temperature.C)", "SD (temperature^4)",
        "SD (temperature^5)", "Cor (Intercept~temperature.L)", "Cor (Intercept~temperature.Q)",
        "Cor (Intercept~temperature.C)", "Cor (Intercept~temperature^4)",
        "Cor (Intercept~temperature^5)", "Cor (temperature.L~temperature.Q)",
        "Cor (temperature.L~temperature.C)", "Cor (temperature.L~temperature^4)",
        "Cor (temperature.L~temperature^5)", "Cor (temperature.Q~temperature.C)",
        "Cor (temperature.Q~temperature^4)", "Cor (temperature.Q~temperature^5)",
        "Cor (temperature.C~temperature^4)", "Cor (temperature.C~temperature^5)",
        "Cor (temperature^4~temperature^5)", "SD (Observations)"
      )
    )

    expect_identical(
      mp3$Group,
      c(
        "", "", "", "", "", "", "recipe", "recipe", "recipe", "recipe",
        "recipe", "recipe", "recipe", "recipe", "recipe", "recipe", "recipe",
        "recipe", "recipe", "recipe", "recipe", "recipe", "recipe", "recipe",
        "recipe", "recipe", "recipe", "Residual"
      )
    )
  })


  test_that("random effects CIs, categorical slope-2", {
    ## FIXME: Results differ across R versions, no idea why...
    # expect_equal(
    #   mp4$CI_low,
    #   c(
    #     29.01106, 5.01248, -1.89447, -1.96271, -2.66795, -2.50896,
    #     4.23401, 0.62943, 0.36949, 0.13979, 0.01129, 0.6074, 0.50155,
    #     -0.30497, -0.94063, -0.13156, -0.32484, NA, NA, NA, NA, NA, NA,
    #     NA, NA, NA, 0.42465, 4.2358
    #   )
    #   ,
    #   tolerance = 1e-2,
    #   ignore_attr = TRUE
    # )

    expect_identical(
      mp4$Parameter,
      c(
        "(Intercept)", "temperature.L", "temperature.Q", "temperature.C",
        "temperature^4", "temperature^5", "SD (Intercept)", "SD (temperature.L)",
        "SD (temperature.Q)", "SD (temperature.C)", "SD (temperature^4)",
        "SD (temperature^5)", "Cor (Intercept~temperature.L)", "Cor (Intercept~temperature.Q)",
        "Cor (Intercept~temperature.C)", "Cor (Intercept~temperature^4)",
        "Cor (Intercept~temperature^5)", "Cor (temperature.L~temperature.Q)",
        "Cor (temperature.L~temperature.C)", "Cor (temperature.L~temperature^4)",
        "Cor (temperature.L~temperature^5)", "Cor (temperature.Q~temperature.C)",
        "Cor (temperature.Q~temperature^4)", "Cor (temperature.Q~temperature^5)",
        "Cor (temperature.C~temperature^4)", "Cor (temperature.C~temperature^5)",
        "Cor (temperature^4~temperature^5)", "SD (Observations)"
      )
    )

    expect_identical(
      mp4$Group,
      c(
        "", "", "", "", "", "", "replicate", "replicate", "replicate",
        "replicate", "replicate", "replicate", "replicate", "replicate",
        "replicate", "replicate", "replicate", "replicate", "replicate",
        "replicate", "replicate", "replicate", "replicate", "replicate",
        "replicate", "replicate", "replicate", "Residual"
      )
    )
  })


  test_that("random effects CIs, double slope", {
    expect_equal(
      mp5$CI_low,
      c(
        238.40607, 7.52296, 15.01708, 3.80547, NaN, -0.48781, NaN,
        NaN, 22.80046
      ),
      tolerance = 1e-2,
      ignore_attr = TRUE
    )

    expect_identical(
      mp5$Parameter,
      c(
        "(Intercept)", "Days", "SD (Intercept)", "SD (Days)", "SD (Months)",
        "Cor (Intercept~Days)", "Cor (Intercept~Months)",
        "Cor (Days~Months)", "SD (Observations)"
      )
    )
  })


  data(sleepstudy)
  set.seed(123)
  sleepstudy$Months <- sample(1:4, nrow(sleepstudy), TRUE)

  set.seed(123)
  m2 <- glmmTMB(Reaction ~ Days + (0 + Days | Subject), data = sleepstudy)
  m5 <- suppressWarnings(glmmTMB(Reaction ~ Days + (0 + Days + Months | Subject), data = sleepstudy))

  set.seed(123)
  mp2 <- model_parameters(m2, ci_random = TRUE)
  expect_message(mp5 <- model_parameters(m5, ci_random = TRUE), "singularity") # no SE/CI

  test_that("random effects CIs, simple slope", {
    expect_equal(
      mp2$CI_low,
      c(243.55046, 6.89554, 4.98429, 25.94359),
      tolerance = 1e-2,
      ignore_attr = TRUE
    )

    expect_identical(
      mp2$Parameter,
      c("(Intercept)", "Days", "SD (Days)", "SD (Observations)")
    )
  })

  test_that("random effects CIs, simple slope", {
    ## FIXME: Results differ across R versions, no idea why...
    # expect_equal(
    #   mp5$CI_low,
    #   c(237.03695, 9.04139, NaN, 8.95755, NaN, 30.67054),
    #   tolerance = 1e-2,
    #   ignore_attr = TRUE
    # )

    expect_identical(
      mp5$Parameter,
      c(
        "(Intercept)", "Days", "SD (Days)", "SD (Months)", "Cor (Days~Months)",
        "SD (Observations)"
      )
    )
  })


  # messages for profiled CI
  test_that("profiled CI messages", {
    mp2 <- model_parameters(m2, ci_method = "profile")
    expect_message(print(mp2), regexp = "(.*)profile-likelihood(.*)z-distribution(.*)")
  })
}
