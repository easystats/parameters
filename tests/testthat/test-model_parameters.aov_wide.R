if (require("insight") && require("effectsize") && require("gam")) {
  test_that("anova wide table - aov", {
    set.seed(123)
    model <- aov(Sepal.Width ~ Species, data = iris)

    set.seed(123)
    expect_snapshot(
      model_parameters(
        model,
        omega_squared = "partial",
        eta_squared = "partial",
        epsilon_squared = TRUE,
        ci = .9,
        table_wide = TRUE,
        verbose = FALSE
      )
    )
  })


  test_that("anova wide table - aovlist", {
    set.seed(123)
    mod <- stats::aov(yield ~ N * P * K + Error(block), npk)

    set.seed(123)
    expect_snapshot(
      model_parameters(
        mod,
        omega_squared = "partial",
        eta_squared = "partial",
        table_wide = TRUE,
        verbose = FALSE
      )
    )
  })


  test_that("anova wide table - manova", {
    set.seed(123)
    npk2 <- within(npk, foo <- rnorm(24))
    m_manova <- manova(cbind(yield, foo) ~ block + N * P * K, npk2)

    set.seed(123)
    expect_snapshot(
      model_parameters(
        m_manova,
        omega_squared = "raw",
        eta_squared = "partial",
        epsilon_squared = "raw",
        ci = 0.99,
        table_wide = TRUE,
        verbose = FALSE
      )
    )
  })

  test_that("anova wide table - Gam", {
    set.seed(123)
    mod_gam <- gam::gam(
      formula = mpg ~ s(hp, 4) + am + qsec,
      data = mtcars
    )

    set.seed(123)
    expect_snapshot(
      model_parameters(
        mod_gam,
        eta_squared = "raw",
        epsilon_squared = "partial",
        ci = 0.90,
        table_wide = TRUE,
        verbose = FALSE
      )
    )
  })

  test_that("anova wide table - maov", {
    set.seed(123)
    fit <- lm(cbind(mpg, disp, hp) ~ factor(cyl), data = mtcars)
    m_maov <- aov(fit)

    set.seed(123)
    expect_snapshot(
      model_parameters(
        m_maov,
        omega_squared = "raw",
        eta_squared = "partial",
        epsilon_squared = "raw",
        ci = 0.50,
        table_wide = TRUE,
        verbose = FALSE
      )
    )
  })
}
