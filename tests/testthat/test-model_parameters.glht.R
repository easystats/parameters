if (requiet("multcomp")) {
  set.seed(123)
  lmod <- lm(Fertility ~ ., data = swiss)
  model <- glht(
    model = lmod,
    linfct = c(
      "Agriculture = 0",
      "Examination = 0",
      "Education = 0",
      "Catholic = 0",
      "Infant.Mortality = 0"
    )
  )

  test_that("model_parameters.glht", {
    params <- model_parameters(model)
    expect_equal(params$Coefficient, c(-0.1721, -0.258, -0.8709, 0.1041, 1.077), tolerance = 1e-2)
    expect_equal(params$SE, c(0.0703, 0.2539, 0.183, 0.0353, 0.3817), tolerance = 1e-2)
    expect_equal(
      params$Parameter,
      c("Agriculture == 0", "Examination == 0", "Education == 0", "Catholic == 0", "Infant.Mortality == 0")
    )
  })
}
