# Test Setup --------------------------------

# only run for dev-version, not on CRAN
.runThisTest <- length(strsplit(packageDescription("parameters")$Version, "\\.")[[1]]) > 3

# only run on windows - there are some minor rounding issues on other OS
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



# tests --------------------------------

## TODO also check messages for profiled CI

if (.runThisTest && win_os &&
  requiet("testthat") &&
  requiet("parameters") &&
  requiet("glmmTMB") &&
  requiet("lme4") &&
  packageVersion("glmmTMB") > "1.1.3") {
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
  expect_message(mp4 <- model_parameters(m4, ci_random = TRUE), "singularity") # no SE/CI
  expect_message(mp5 <- model_parameters(m5, ci_random = TRUE), "singularity") # no SE/CI

  test_that("random effects CIs, two slopes, categorical", {
    expect_equal(
      mp1$CI_low,
      c(
        28.9123, 5.03115, -1.87304, -2.42081, -3.2708, -2.57695, 0.21571,
        4.17466, NaN, 0, 0.26247, 0.34089, 0.02477, 0.65731, 0.3902,
        0.14685, 0.01322, 0.62182, 0.99915, NaN, NaN, NaN, -0.31609,
        -0.48806, NaN, -0.8346, NaN, -0.60153, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NaN, NA, NA, NA, NA, NA, NA, NA, NA, NA, NaN, 4.12529
      ),
      tolerance = 1e-2,
      ignore_attr = TRUE
    )

    expect_equal(
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

    expect_equal(
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

    expect_equal(
      mp2$Parameter,
      c(
        "(Intercept)", "Days", "SD (Intercept)", "SD (Days)", "Cor (Intercept~Days)",
        "SD (Observations)"
      )
    )
  })


  test_that("random effects CIs, categorical slope-1", {
    expect_equal(
      mp3$CI_low,
      c(
        31.20278, 4.35879, -2.63767, -2.80041, -3.54983, -3.16627,
        0, NaN, NaN, 0, NaN, NaN, NaN, NaN, -0.49203, -0.41167, NaN,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NaN, 7.08478
      ),
      tolerance = 1e-2,
      ignore_attr = TRUE
    )

    expect_equal(
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

    expect_equal(
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
    expect_equal(
      mp4$CI_low,
      c(
        29.01131, 5.01247, -1.89444, -1.96275, -2.66798, -2.50892,
        4.23497, 0.62985, 0.36934, 0.1398, 0.01133, 0.60758, 0.56678,
        0.26866, NaN, NaN, NaN, NA, NA, NA, NA, NA, NA, NA, NA, NA, NaN,
        4.23582
      ),
      tolerance = 1e-2,
      ignore_attr = TRUE
    )

    expect_equal(
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

    expect_equal(
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

    expect_equal(
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

    expect_equal(
      mp2$Parameter,
      c("(Intercept)", "Days", "SD (Days)", "SD (Observations)")
    )
  })

  test_that("random effects CIs, simple slope", {
    expect_equal(
      mp5$CI_low,
      c(237.03695, 9.04139, NaN, 8.95755, NaN, 30.67054),
      tolerance = 1e-2,
      ignore_attr = TRUE
    )

    expect_equal(
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
