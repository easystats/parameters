if (requiet("utils") && requiet("brglm2")) {
  data("stemcell")
  levels(stemcell$research) <- c("definitly", "alterly", "probably not", "definitely not")
  m1 <- bracl(research ~ as.numeric(religion) + gender, weights = frequency, data = stemcell, type = "ML")

  test_that("model_parameters", {
    params <- model_parameters(m1, verbose = FALSE)
    expect_identical(
      params$Response,
      c("definitly", "alterly", "probably not", "definitly", "alterly", "probably not", "definitly", "alterly", "probably not")
    )
    expect_identical(
      params$Parameter,
      c(
        "definitly:(Intercept)", "alterly:(Intercept)", "probably not:(Intercept)",
        "definitly:as.numeric(religion)", "alterly:as.numeric(religion)",
        "probably not:as.numeric(religion)", "definitly:genderfemale",
        "alterly:genderfemale", "probably not:genderfemale"
      )
    )
    expect_equal(
      params$Coefficient,
      c(-1.24836, 0.47098, 0.42741, 0.4382, 0.25962, 0.01192, -0.13683, 0.18707, -0.16093),
      tolerance = 1e-3
    )
  })

  # check vcov args
  test_that("model_parameters", {
    expect_warning(out <- model_parameters(m1, vcov = "vcovHAC"))
    expect_equal(out$SE, unname(coef(summary(m1))[, 2]), tolerance = 1e-3)
  })

  # check order of response levels
  test_that("print model_parameters", {
    out <- utils::capture.output(print(model_parameters(m1, verbose = FALSE)))
    expect_identical(out[1], "# Response level: definitly")
    expect_identical(out[9], "# Response level: alterly")
    expect_identical(out[17], "# Response level: probably not")
  })
}
