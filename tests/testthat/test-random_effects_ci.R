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

if (.runThisTest && !osx &&
    requiet("testthat") &&
    requiet("parameters") &&
    requiet("lme4")) {

  data(sleepstudy)
  data(cake)
  set.seed(123)
  sleepstudy$Months <- sample(1:4, nrow(sleepstudy), TRUE)

  m1 <- lmer(angle ~ temperature + (temperature | recipe) + (temperature | replicate), data = cake)
  m2 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
  m3 <- lmer(angle ~ temperature + (temperature | recipe), data = cake)
  m4 <- lmer(angle ~ temperature + (temperature | replicate), data = cake)
  m5 <- lmer(Reaction ~ Days + (Days + Months | Subject), data = sleepstudy)

  mp1 <- model_parameters(m1)
  mp2 <- model_parameters(m2)
  mp3 <- model_parameters(m3)
  mp4 <- model_parameters(m4)
  mp5 <- model_parameters(m5)

  test_that("random effects CIs, two slopes, categorical", {
    expect_equal(
      mp1$CI_low,
      c(28.75568, 4.97893, -1.95002, -2.69995, -3.62201, -2.69102,
        4.28558, 0.21474, 0.40062, 0.10169, 0.04953, 1e-05, 0.55398,
        0, 2e-05, 0.6333, 1.09851, 0.00944, -1, -1, -0.65406, -1, -0.69103,
        -1, -0.95271, -1, -0.90617, -1, -0.99802, -0.99836, -1, -1, -1,
        -1, -0.75274, -0.96895, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, 4.07985),
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
        "SD (temperature^5)", "Cor (Intercept~temperature.C: replicate)",
        "Cor (Intercept~temperature.C: recipe)", "Cor (Intercept~temperature.L: replicate)",
        "Cor (Intercept~temperature.L: recipe)", "Cor (Intercept~temperature.Q: replicate)",
        "Cor (Intercept~temperature.Q: recipe)", "Cor (Intercept~temperature^4: replicate)",
        "Cor (Intercept~temperature^4: recipe)", "Cor (Intercept~temperature^5: replicate)",
        "Cor (Intercept~temperature^5: recipe)", "Cor (temperature.L~temperature.C: replicate)",
        "Cor (temperature.Q~temperature.C: replicate)", "Cor (temperature.L~temperature.Q: replicate)",
        "Cor (temperature.L~temperature^4: replicate)", "Cor (temperature.Q~temperature^4: replicate)",
        "Cor (temperature.C~temperature^4: replicate)", "Cor (temperature.L~temperature^5: replicate)",
        "Cor (temperature.Q~temperature^5: replicate)", "Cor (temperature.C~temperature^5: replicate)",
        "Cor (temperature^4~temperature^5: replicate)", "Cor (temperature.L~temperature.C: recipe)",
        "Cor (temperature.Q~temperature.C: recipe)", "Cor (temperature.L~temperature.Q: recipe)",
        "Cor (temperature.L~temperature^4: recipe)", "Cor (temperature.Q~temperature^4: recipe)",
        "Cor (temperature.C~temperature^4: recipe)", "Cor (temperature.L~temperature^5: recipe)",
        "Cor (temperature.Q~temperature^5: recipe)", "Cor (temperature.C~temperature^5: recipe)",
        "Cor (temperature^4~temperature^5: recipe)", "SD (Observations)"
      )
    )
  })


  test_that("random effects CIs, simple slope", {
    expect_equal(
      mp2$CI_low,
      c(237.93546, 7.41637, 15.5817, 3.91828, -0.50907, 22.80044),
      tolerance = 1e-3,
      ignore_attr = TRUE
    )

    expect_equal(
      mp2$Parameter,
      c("(Intercept)", "Days", "SD (Intercept)", "SD (Days)", "Cor (Intercept~Days: Subject)",
        "SD (Observations)")
    )
  })


  test_that("random effects CIs, categorical slope-1", {
    expect_equal(
      mp3$CI_low,
      c(30.91139, 4.33247, -2.6798, -3.20703, -4.07681, -3.27237, 0.06301,
        0, 0, 0.1192, 0.32213, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, 7.09933),
      tolerance = 1e-3,
      ignore_attr = TRUE
    )

    expect_equal(
      mp3$Parameter,
      c("(Intercept)", "temperature.L", "temperature.Q", "temperature.C",
        "temperature^4", "temperature^5", "SD (Intercept)", "SD (temperature.L)",
        "SD (temperature.Q)", "SD (temperature.C)", "SD (temperature^4)",
        "SD (temperature^5)", "Cor (Intercept~temperature.L: recipe)",
        "Cor (Intercept~temperature.Q: recipe)", "Cor (Intercept~temperature.C: recipe)",
        "Cor (Intercept~temperature^4: recipe)", "Cor (Intercept~temperature^5: recipe)",
        "Cor (temperature.L~temperature.C: recipe)", "Cor (temperature.Q~temperature.C: recipe)",
        "Cor (temperature.L~temperature.Q: recipe)", "Cor (temperature.L~temperature^4: recipe)",
        "Cor (temperature.Q~temperature^4: recipe)", "Cor (temperature.C~temperature^4: recipe)",
        "Cor (temperature.L~temperature^5: recipe)", "Cor (temperature.Q~temperature^5: recipe)",
        "Cor (temperature.C~temperature^5: recipe)", "Cor (temperature^4~temperature^5: recipe)",
        "SD (Observations)")
    )
  })


  test_that("random effects CIs, categorical slope-2", {
    expect_equal(
      mp4$CI_low,
      c(28.88523, 4.96796, -1.93239, -1.98597, -2.68858, -2.5524, 4.27899,
        0.35378, 0.08109, 0.03419, 0, 0.49982, -0.68893, -0.71984, -1,
        -0.96725, -0.92158, -0.99894, -0.99924, -1, -1, -1, -1, -0.80378,
        -0.9778, -1, -1, 4.21143),
      tolerance = 1e-3,
      ignore_attr = TRUE
    )

    expect_equal(
      mp4$Parameter,
      c("(Intercept)", "temperature.L", "temperature.Q", "temperature.C",
        "temperature^4", "temperature^5", "SD (Intercept)", "SD (temperature.L)",
        "SD (temperature.Q)", "SD (temperature.C)", "SD (temperature^4)",
        "SD (temperature^5)", "Cor (Intercept~temperature.L: replicate)",
        "Cor (Intercept~temperature.Q: replicate)", "Cor (Intercept~temperature.C: replicate)",
        "Cor (Intercept~temperature^4: replicate)", "Cor (Intercept~temperature^5: replicate)",
        "Cor (temperature.L~temperature.C: replicate)", "Cor (temperature.Q~temperature.C: replicate)",
        "Cor (temperature.L~temperature.Q: replicate)", "Cor (temperature.L~temperature^4: replicate)",
        "Cor (temperature.Q~temperature^4: replicate)", "Cor (temperature.C~temperature^4: replicate)",
        "Cor (temperature.L~temperature^5: replicate)", "Cor (temperature.Q~temperature^5: replicate)",
        "Cor (temperature.C~temperature^5: replicate)", "Cor (temperature^4~temperature^5: replicate)",
        "SD (Observations)")
    )
  })

  test_that("random effects CIs, double slope", {
    expect_equal(
      mp5$CI_low,
      c(237.99863, 7.4022, 12.63814, 0.58664, 0, -0.58599, -1, -1, 22.65226),
      tolerance = 1e-3,
      ignore_attr = TRUE
    )

    expect_equal(
      mp5$Parameter,
      c("(Intercept)", "Days", "SD (Intercept)", "SD (Days)", "SD (Months)",
        "Cor (Intercept~Days: Subject)", "Cor (Intercept~Months: Subject)",
        "Cor (Days~Months: Subject)", "SD (Observations)")
    )
  })







  m2 <- lmer(Reaction ~ Days + (0 + Days | Subject), data = sleepstudy)

  ## TODO: fix me!
  m5 <- lmer(Reaction ~ Days + (0 + Days + Months | Subject), data = sleepstudy)

  mp2 <- model_parameters(m2)
  mp5 <- model_parameters(m5)

  test_that("random effects CIs, simple slope", {
    expect_equal(
      mp2$CI_low,
      c(243.47155, 6.77765, 5.09041, 26.01525),
      tolerance = 1e-3,
      ignore_attr = TRUE
    )

    expect_equal(
      mp2$Parameter,
      c("(Intercept)", "Days", "SD (Days)", "SD (Observations)")
    )
  })

}