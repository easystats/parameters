.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

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

## TODO also check messages for profiled CI

if (.runThisTest && !osx &&
    requiet("testthat") &&
    requiet("parameters") &&
    requiet("glmmTMB") &&
    requiet("lme4")) {

  data(sleepstudy)
  data(cake)
  set.seed(123)
  sleepstudy$Months <- sample(1:4, nrow(sleepstudy), TRUE)

  m1 <- suppressWarnings(glmmTMB(angle ~ temperature + (temperature | recipe) + (temperature | replicate), data = cake))
  m2 <- glmmTMB(Reaction ~ Days + (Days | Subject), data = sleepstudy)
  m3 <- suppressWarnings(glmmTMB(angle ~ temperature + (temperature | recipe), data = cake))
  m4 <- suppressWarnings(glmmTMB(angle ~ temperature + (temperature | replicate), data = cake))
  m5 <- suppressWarnings(glmmTMB(Reaction ~ Days + (Days + Months | Subject), data = sleepstudy))

  expect_message(expect_message(mp1 <- model_parameters(m1), "singularity"), "singularity")
  mp2 <- model_parameters(m2) # works
  expect_message(mp3 <- model_parameters(m3), "singularity") # no SE/CI
  expect_message(mp4 <- model_parameters(m4), "singularity") # no SE/CI
  expect_message(mp5 <- model_parameters(m5), "singularity") # no SE/CI

  test_that("random effects CIs, two slopes, categorical", {
    expect_equal(
      mp1$CI_low,
      c(28.9123, 5.03115, -1.87304, -2.42081, -3.2708, -2.57695, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
      tolerance = 1e-3,
      ignore_attr = TRUE
    )

    expect_equal(
      mp1$Parameter,
      c("(Intercept)", "temperature.L", "temperature.Q", "temperature.C",
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
        "SD (Observations)")
    )

    expect_equal(
      mp1$Group,
      c("", "", "", "", "", "", "recipe", "replicate", "recipe", "recipe",
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
      tolerance = 1e-3,
      ignore_attr = TRUE
    )

    expect_equal(
      mp2$Parameter,
      c("(Intercept)", "Days", "SD (Intercept)", "SD (Days)", "Cor (Intercept~Days)",
        "SD (Observations)")
    )
  })


  test_that("random effects CIs, categorical slope-1", {
    expect_equal(
      mp3$CI_low,
      c(31.20278, 4.35879, -2.63767, -2.80041, -3.54983, -3.16627,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA),
      tolerance = 1e-3,
      ignore_attr = TRUE
    )

    expect_equal(
      mp3$Parameter,
      c("(Intercept)", "temperature.L", "temperature.Q", "temperature.C",
        "temperature^4", "temperature^5", "SD (Intercept)", "SD (temperature.L)",
        "SD (temperature.Q)", "SD (temperature.C)", "SD (temperature^4)",
        "SD (temperature^5)", "Cor (Intercept~temperature.L)", "Cor (Intercept~temperature.Q)",
        "Cor (Intercept~temperature.C)", "Cor (Intercept~temperature^4)",
        "Cor (Intercept~temperature^5)", "Cor (temperature.L~temperature.Q)",
        "Cor (temperature.L~temperature.C)", "Cor (temperature.L~temperature^4)",
        "Cor (temperature.L~temperature^5)", "Cor (temperature.Q~temperature.C)",
        "Cor (temperature.Q~temperature^4)", "Cor (temperature.Q~temperature^5)",
        "Cor (temperature.C~temperature^4)", "Cor (temperature.C~temperature^5)",
        "Cor (temperature^4~temperature^5)", "SD (Observations)")
    )

    expect_equal(
      mp3$Group,
      c("", "", "", "", "", "", "recipe", "recipe", "recipe", "recipe",
        "recipe", "recipe", "recipe", "recipe", "recipe", "recipe", "recipe",
        "recipe", "recipe", "recipe", "recipe", "recipe", "recipe", "recipe",
        "recipe", "recipe", "recipe", "Residual")
    )
  })


  test_that("random effects CIs, categorical slope-2", {
    expect_equal(
      mp4$CI_low,
      c(29.01131, 5.01247, -1.89444, -1.96275, -2.66798, -2.50892,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA),
      tolerance = 1e-3,
      ignore_attr = TRUE
    )

    expect_equal(
      mp4$Parameter,
      c("(Intercept)", "temperature.L", "temperature.Q", "temperature.C",
        "temperature^4", "temperature^5", "SD (Intercept)", "SD (temperature.L)",
        "SD (temperature.Q)", "SD (temperature.C)", "SD (temperature^4)",
        "SD (temperature^5)", "Cor (Intercept~temperature.L)", "Cor (Intercept~temperature.Q)",
        "Cor (Intercept~temperature.C)", "Cor (Intercept~temperature^4)",
        "Cor (Intercept~temperature^5)", "Cor (temperature.L~temperature.Q)",
        "Cor (temperature.L~temperature.C)", "Cor (temperature.L~temperature^4)",
        "Cor (temperature.L~temperature^5)", "Cor (temperature.Q~temperature.C)",
        "Cor (temperature.Q~temperature^4)", "Cor (temperature.Q~temperature^5)",
        "Cor (temperature.C~temperature^4)", "Cor (temperature.C~temperature^5)",
        "Cor (temperature^4~temperature^5)", "SD (Observations)")
    )

    expect_equal(
      mp4$Group,
      c("", "", "", "", "", "", "replicate", "replicate", "replicate",
        "replicate", "replicate", "replicate", "replicate", "replicate",
        "replicate", "replicate", "replicate", "replicate", "replicate",
        "replicate", "replicate", "replicate", "replicate", "replicate",
        "replicate", "replicate", "replicate", "Residual")
    )
  })


  test_that("random effects CIs, double slope", {
    expect_equal(
      mp5$CI_low,
      c(238.40607, 7.52296, NA, NA, NA, NA, NA, NA, NA),
      tolerance = 1e-3,
      ignore_attr = TRUE
    )

    expect_equal(
      mp5$Parameter,
      c("(Intercept)", "Days", "SD (Intercept)", "SD (Days)", "SD (Months)",
        "Cor (Intercept~Days)", "Cor (Intercept~Months)",
        "Cor (Days~Months)", "SD (Observations)")
    )
  })






  data(sleepstudy)
  set.seed(123)
  sleepstudy$Months <- sample(1:4, nrow(sleepstudy), TRUE)

  m2 <- glmmTMB(Reaction ~ Days + (0 + Days | Subject), data = sleepstudy)
  m5 <- suppressWarnings(glmmTMB(Reaction ~ Days + (0 + Days + Months | Subject), data = sleepstudy))

  mp2 <- model_parameters(m2)
  expect_message(mp5 <- model_parameters(m5), "singularity") # no SE/CI

  test_that("random effects CIs, simple slope", {
    expect_equal(
      mp2$CI_low,
      c(243.55046, 6.89554, 4.98429, 25.94359),
      tolerance = 1e-3,
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
      c(237.03695, 9.04139, NA, NA, NA, NA),
      tolerance = 1e-3,
      ignore_attr = TRUE
    )

    expect_equal(
      mp5$Parameter,
      c("(Intercept)", "Days", "SD (Days)", "SD (Months)", "Cor (Days~Months)",
        "SD (Observations)")
    )
  })


  # messages for profiled CI
  test_that("profiled CI messages", {
    mp2 <- model_parameters(m2, ci_method = "profile")
    expect_message(print(mp2), regexp = "(.*)profile-likelihood(.*)z-distribution(.*)")
  })

}