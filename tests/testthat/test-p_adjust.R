.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest) {
  data(mtcars)
  model <- lm(mpg ~ wt * cyl + am + log(hp), data = mtcars)

  test_that("model_parameters, p-adjust", {
    mp <- model_parameters(model)
    expect_equal(mp$p, c(0, 0.00304, 0.02765, 0.65851, 0.01068, 0.02312), tolerance = 1e-3)
    mp <- model_parameters(model, p_adjust = "BH")
    expect_equal(mp$p, c(0, 0.00912, 0.03318, 0.65851, 0.02137, 0.03318), tolerance = 1e-3)
    mp <- model_parameters(model, p_adjust = "bonferroni")
    expect_equal(mp$p, c(0, 0.01824, 0.16588, 1, 0.06411, 0.13869), tolerance = 1e-3)
  })


  test_that("model_parameters, p-adjust after keep/drop", {
    model <- lm(mpg ~ wt + cyl + gear + hp, data = mtcars)
    mp <- model_parameters(model, p_adjust = "bonferroni")
    expect_equal(
      mp[["p"]],
      p.adjust(coef(summary(model))[, 4], "bonferroni"),
      tolerance = 1e-4,
      ignore_attr = TRUE
    )

    mp <- model_parameters(model, summary = TRUE, keep = c("wt", "hp"), p_adjust = "bonferroni")
    expect_equal(
      mp[["p"]],
      p.adjust(coef(summary(model))[c(2, 5), 4], "bonferroni"),
      tolerance = 1e-4,
      ignore_attr = TRUE
    )

    mp <- model_parameters(model, summary = TRUE, keep = c("cyl", "gear"), p_adjust = "bonferroni")
    expect_equal(
      mp[["p"]],
      p.adjust(coef(summary(model))[3:4, 4], "bonferroni"),
      tolerance = 1e-4,
      ignore_attr = TRUE
    )
  })


  if (requiet("emmeans")) {
    data(iris)
    m <- pairs(emmeans(aov(Sepal.Width ~ Species, data = iris), ~Species))
    test_that("model_parameters, emmeans, p-adjust", {
      mp <- model_parameters(m)
      expect_equal(mp$p, as.data.frame(m)$p.value, tolerance = 1e-4)
    })
    m <- pairs(emmeans(aov(Sepal.Width ~ Species, data = iris), ~Species), adjust = "scheffe")
    test_that("model_parameters, emmeans, p-adjust", {
      mp <- model_parameters(m, p_adjust = "scheffe")
      expect_equal(mp$p, as.data.frame(m)$p.value, tolerance = 1e-4)
    })
  }
}
