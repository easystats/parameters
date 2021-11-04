.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest &&
  requiet("testthat") &&
  requiet("parameters") &&
  requiet("ordinal")) {
  d <- data.frame(
    Stim = c(
      "New", "New", "New", "New", "New", "New",
      "Old", "Old", "Old", "Old", "Old", "Old"
    ),
    Response = c(
      "Confidence1", "Confidence2", "Confidence3", "Confidence4",
      "Confidence5", "Confidence6", "Confidence1", "Confidence2",
      "Confidence3", "Confidence4", "Confidence5", "Confidence6"
    ),
    w = c(320, 295, 243, 206, 174, 159, 136, 188, 208, 256, 302, 333)
  )

  m1 <- clm(ordered(Response) ~ Stim,
    scale = ~Stim,
    link = "probit",
    data = d, weights = w
  )

  m2 <- clm2(ordered(Response) ~ Stim,
    scale = ~Stim,
    link = "probit",
    data = d, weights = w
  )


  test_that("model_parameters.clm", {
    mp <- model_parameters(m1)
    expect_equal(
      mp$Parameter,
      c(
        "Confidence1|Confidence2", "Confidence2|Confidence3", "Confidence3|Confidence4",
        "Confidence4|Confidence5", "Confidence5|Confidence6", "StimOld",
        "StimOld"
      ),
      tolerance = 1e-4
    )
    expect_equal(
      mp$Component,
      c("intercept", "intercept", "intercept", "intercept", "intercept", "location", "scale"),
      tolerance = 1e-4
    )
    expect_equal(
      mp$Coefficient,
      c(-0.72845, -0.15862, 0.26583, 0.69614, 1.23477, 0.55237, -0.04069),
      tolerance = 1e-4
    )
  })

  test_that("model_parameters.clm2", {
    mp <- model_parameters(m2)
    expect_equal(
      mp$Parameter,
      c(
        "Confidence1|Confidence2", "Confidence2|Confidence3", "Confidence3|Confidence4",
        "Confidence4|Confidence5", "Confidence5|Confidence6", "StimOld",
        "StimOld"
      ),
      tolerance = 1e-4
    )
    expect_equal(
      mp$Component,
      c("intercept", "intercept", "intercept", "intercept", "intercept", "location", "scale"),
      tolerance = 1e-4
    )
    expect_equal(
      mp$Coefficient,
      c(-0.72845, -0.15862, 0.26583, 0.69614, 1.23477, 0.55237, -0.04069),
      tolerance = 1e-4
    )
  })
}
