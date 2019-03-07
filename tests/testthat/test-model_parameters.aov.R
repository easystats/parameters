context("model_parameters.aov")

test_that("model_parameters.aov", {
  df <- iris
  df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")

  model <- aov(Sepal.Length ~ Sepal.Big, data=df)
  testthat::expect_equal(sum(model_parameters(model)$DoF), 149)

  model <- anova(lm(Sepal.Length ~ Sepal.Big, data=df))
  testthat::expect_equal(sum(model_parameters(model)$DoF), 149)

  model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data=df)
  testthat::expect_equal(sum(model_parameters(model)$DoF), 149)

  library(lme4)
  model <- anova(lme4::lmer(Sepal.Length ~ Sepal.Big + (1|Species), data=df))
  testthat::expect_equal(sum(model_parameters(model)$DoF), 1)
})
