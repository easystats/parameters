if (requiet("insight") && requiet("effectsize") && requiet("testthat") && requiet("lme4") && requiet("parameters")) {
  unloadNamespace("afex")
  unloadNamespace("lmerTest")
  data(iris)
  iris$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(iris))
  iris$Cat2 <- rep(c("A", "B"), length.out = nrow(iris))

  # aov ----------------------------------

  test_that("model_parameters.aov", {
    skip_if_not_installed("effectsize", minimum_version = "0.5.0")
    model <- aov(Sepal.Width ~ Species, data = iris)
    mp <- suppressMessages(model_parameters(model, omega_squared = "partial", eta_squared = "partial", epsilon_squared = TRUE, ci = .9))
    es <- suppressMessages(effectsize::omega_squared(model, partial = TRUE, ci = .9))
    expect_equal(na.omit(mp$Omega2_CI_low), es$CI_low, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(mp$Omega2_CI_low, c(0.3122, NA), tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(na.omit(mp$Omega2_CI_high), es$CI_high, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(na.omit(mp$Omega2_CI_high), 1, tolerance = 1e-3, ignore_attr = TRUE)

    expect_equal(colnames(mp), c(
      "Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
      "Omega2", "Omega2_CI_low", "Omega2_CI_high", "Eta2",
      "Eta2_CI_low", "Eta2_CI_high", "Epsilon2", "Epsilon2_CI_low",
      "Epsilon2_CI_high"
    ))

    model <- aov(Sepal.Length ~ Species * Cat1 * Cat2, data = iris)
    mp <- model_parameters(model, eta_squared = "raw", ci = .9)
    es <- effectsize::eta_squared(model, partial = FALSE, ci = .9)
    expect_equal(na.omit(mp$Eta2_CI_low), es$CI_low, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(mp$Eta2_CI_low, c(0.5572, 0, 0, 0, 0, 0, 0, NA), tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(na.omit(mp$Eta2_CI_high), es$CI_high, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(na.omit(mp$Eta2_CI_high), rep(1, 7), tolerance = 1e-3, ignore_attr = TRUE)

    expect_equal(colnames(mp), c(
      "Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
      "Eta2", "Eta2_CI_low", "Eta2_CI_high"
    ))
  })


  # anova ---------------------

  data(mtcars)
  test_that("model_parameters.anova", {
    skip_if_not_installed("effectsize", minimum_version = "0.5.0")
    model <- anova(lm(Sepal.Length ~ Species * Cat1 * Cat2, data = iris))
    mp <- model_parameters(model, omega_squared = "partial", eta_squared = "partial", epsilon_squared = TRUE, ci = .9)
    es <- effectsize::omega_squared(model, partial = TRUE, ci = .9)
    expect_equal(na.omit(mp$Omega2_CI_low), es$CI_low, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(na.omit(mp$Omega2_CI_high), es$CI_high, tolerance = 1e-3, ignore_attr = TRUE)

    expect_equal(colnames(mp), c(
      "Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
      "Omega2_partial", "Omega2_CI_low", "Omega2_CI_high", "Eta2_partial",
      "Eta2_CI_low", "Eta2_CI_high", "Epsilon2_partial", "Epsilon2_CI_low",
      "Epsilon2_CI_high"
    ))
  })

  data(mtcars)
  test_that("model_parameters.anova", {
    skip_if_not_installed("effectsize", minimum_version = "0.5.0")
    model <- aov(wt ~ cyl + Error(gear), data = mtcars)
    mp <- model_parameters(model, omega_squared = "partial", eta_squared = "partial", epsilon_squared = TRUE, ci = .9)
    es <- effectsize::omega_squared(model, partial = TRUE, ci = .9, alternative = "two", verbose = FALSE)
    expect_equal(na.omit(mp$Omega2_CI_low), es$CI_low, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(na.omit(mp$Omega2_CI_high), es$CI_high, tolerance = 1e-3, ignore_attr = TRUE)

    expect_equal(colnames(mp), c(
      "Group", "Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
      "Omega2_partial", "Omega2_CI_low", "Omega2_CI_high", "Eta2_partial",
      "Eta2_CI_low", "Eta2_CI_high", "Epsilon2_partial", "Epsilon2_CI_low",
      "Epsilon2_CI_high"
    ))
  })


  # car anova ---------------------------------

  if (requiet("car")) {
    set.seed(123)
    data(Moore)
    model <-
      car::Anova(stats::lm(
        formula = conformity ~ fcategory * partner.status,
        data = Moore,
        contrasts = list(fcategory = contr.sum, partner.status = contr.sum)
      ))
    test_that("model_parameters.car-anova", {
      skip_if_not_installed("effectsize", minimum_version = "0.5.0")
      mp <- model_parameters(model, omega_squared = "partial", eta_squared = "partial", epsilon_squared = TRUE, ci = .9)
      es <- effectsize::omega_squared(model, partial = TRUE, ci = .9)
      expect_equal(na.omit(mp$Omega2_CI_low), es$CI_low, tolerance = 1e-3, ignore_attr = TRUE)
      expect_equal(mp$Omega2_CI_low, c(0, 0.05110, 0.00666, NA), tolerance = 1e-3, ignore_attr = TRUE)
      expect_equal(na.omit(mp$Omega2_CI_high), es$CI_high, tolerance = 1e-3, ignore_attr = TRUE)
      expect_equal(na.omit(mp$Omega2_CI_high), rep(1, 3), tolerance = 1e-3, ignore_attr = TRUE)

      expect_equal(colnames(mp), c(
        "Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
        "Omega2_partial", "Omega2_CI_low", "Omega2_CI_high", "Eta2_partial",
        "Eta2_CI_low", "Eta2_CI_high", "Epsilon2_partial", "Epsilon2_CI_low",
        "Epsilon2_CI_high"
      ))
    })
  }


  # maov ----------------------------------

  set.seed(123)
  fit <- lm(cbind(mpg, disp, hp) ~ factor(cyl), data = mtcars)
  model <- aov(fit)

  test_that("model_parameters.maov", {
    skip_if_not_installed("effectsize", minimum_version = "0.5.0")
    mp <- suppressMessages(model_parameters(model, omega_squared = "partial", eta_squared = "partial", epsilon_squared = TRUE, ci = .9))
    es <- suppressMessages(effectsize::omega_squared(model, partial = TRUE, ci = .9))
    expect_equal(na.omit(mp$Omega2_CI_low), es$CI_low, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(mp$Omega2_CI_low, c(0.74092, NA, 0.55331, NA, 0.58067, NA), tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(na.omit(mp$Omega2_CI_high), es$CI_high, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(na.omit(mp$Omega2_CI_high), rep(1, 3), tolerance = 1e-3, ignore_attr = TRUE)

    expect_equal(colnames(mp), c(
      "Response", "Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
      "Omega2", "Omega2_CI_low", "Omega2_CI_high", "Eta2",
      "Eta2_CI_low", "Eta2_CI_high", "Epsilon2", "Epsilon2_CI_low",
      "Epsilon2_CI_high"
    ))
  })

  # stricter tests ---------------------------------------------------------

  if (requiet("car") && requiet("gam")) {

    # aov ------------------------------------------------

    test_that("works with aov", {
      skip_on_cran()
      skip_if_not_installed("effectsize", minimum_version = "0.5.0")

      set.seed(123)
      npk.aov <- aov(yield ~ block + N * P, npk)

      set.seed(123)
      df_aov <-
        as.data.frame(parameters::model_parameters(npk.aov,
          ci = 0.95,
          eta_squared = "partial",
          omega_squared = "raw"
        ))

      expect_equal(
        df_aov,
        structure(
          list(
            Parameter = c("block", "N", "P", "N:P", "Residuals"),
            Sum_Squares = c(343.29, 189.28, 8.4, 21.28, 314.1),
            df = c(5, 1, 1, 1, 15),
            Mean_Square = c(68.66, 189.28, 8.4, 21.28, 20.94),
            F = c(3.28, 9.04, 0.4, 1.02, NA),
            p = c(0.03, 0.01, 0.54, 0.33, NA),
            Omega2 = c(0.27, 0.19, -0.01, 0, NA),
            Omega2_CI_low = c(0, 0, 0, 0, NA),
            Omega2_CI_high = c(1, 1, 1, 1, NA),
            Eta2_partial = c(0.52, 0.38, 0.03, 0.06, NA),
            Eta2_CI_low = c(0.04258, 0.0733, 0, 0, NA),
            Eta2_CI_high = c(1, 1, 1, 1, NA)
          ),
          row.names = c(NA, 5L),
          class = "data.frame",
          ci = 0.95,
          model_class = c("aov", "lm"),
          digits = 2,
          ci_digits = 2,
          p_digits = 3
        ),
        tolerance = 0.1,
        ignore_attr = TRUE
      )
    })


    # aovlist ------------------------------------------------

    # test_that("works with aovlist", {
    #   skip_on_cran()
    #
    #   set.seed(123)
    #   npk.aovE <- aov(yield ~ N * P * K + Error(block), npk)
    #
    #   set.seed(123)
    #   df_aovE <-
    #     as.data.frame(model_parameters(npk.aovE,
    #       ci = 0.90,
    #       eta_squared = "raw",
    #       omega_squared = "partial"
    #     ))
    #
    #   expect_equal(
    #     df_aovE,
    #     structure(
    #       list(
    #         Group = c(
    #           "block",
    #           "block",
    #           "Within",
    #           "Within",
    #           "Within",
    #           "Within",
    #           "Within",
    #           "Within",
    #           "Within"
    #         ),
    #         Parameter = c(
    #           "N:P:K",
    #           "Residuals",
    #           "N",
    #           "P",
    #           "K",
    #           "N:P",
    #           "N:K",
    #           "P:K",
    #           "Residuals"
    #         ),
    #         Sum_Squares = c(37, 306.29, 189.28, 8.4, 95.2, 21.28, 33.14, 0.48, 185.29),
    #         df = c(1, 4, 1, 1, 1, 1, 1, 1, 12),
    #         Mean_Square = c(37, 76.57, 189.28, 8.4, 95.2, 21.28, 33.14, 0.48, 15.44),
    #         `F` = c(0.48, NA, 12.26, 0.54, 6.17, 1.38, 2.15, 0.03, NA),
    #         p = c(0.53, NA, 0, 0.47, 0.03, 0.26, 0.17, 0.86, NA),
    #         Omega2_partial = c(-0.09, NA, 0.23, -0.01, 0.12, 0.01, 0.03, -0.03, NA),
    #         Omega2_CI_low = c(0, NA, 0, 0, 0, 0, 0, 0, NA),
    #         Omega2_CI_high = c(0, NA, 0.52, 0, 0.42, 0.22, 0.29, 0, NA),
    #         Eta2 = c(0.04, NA, 0.22, 0.01, 0.11, 0.02, 0.04, 0, NA),
    #         Eta2_CI_low = c(0, NA, 0, 0, 0, 0, 0, 0, NA),
    #         Eta2_CI_high = c(0.49, NA, 0.51, 0.23, 0.41, 0.28, 0.31, 0.04, NA)
    #       ),
    #       row.names = c(NA, 9L),
    #       class = "data.frame",
    #       ci = 0.9,
    #       model_class = c("aovlist", "listof"),
    #       digits = 2,
    #       ci_digits = 2,
    #       p_digits = 3
    #     ),
    #     tolerance = 0.1,
    #     ignore_attr = TRUE
    #   )
    # })

    # manova ------------------------------------------------

    test_that("works with manova", {
      skip_on_cran()
      skip_if_not_installed("effectsize", minimum_version = "0.5.0")

      set.seed(123)
      # fake a 2nd response variable
      npk2 <- within(npk, foo <- rnorm(24))

      # model
      m <- manova(cbind(yield, foo) ~ block + N * P * K, npk2)

      set.seed(123)
      df_manova <-
        as.data.frame(model_parameters(m,
          ci = 0.99,
          eta_squared = NULL,
          omega_squared = "partial",
          epsilon_squared = "partial"
        ))

      expect_equal(
        df_manova,
        structure(
          list(
            Parameter = c("block", "N", "P", "K", "N:P", "N:K", "P:K", "Residuals"),
            Pillai = c(0.88, 0.61, 0.07, 0.39, 0.11, 0.17, 0, NA),
            df = c(5, 1, 1, 1, 1, 1, 1, 12),
            F = c(1.9, 8.52, 0.39, 3.49, 0.65, 1.16, 0.02, NA),
            p = c(0.1, 0.01, 0.69, 0.07, 0.54, 0.35, 0.98, NA),
            Omega2_partial = c(0.2, 0.52, -0.1, 0.26, -0.05, 0.02, -0.16, NA),
            Omega2_CI_low = c(0, 0, 0, 0, 0, 0, 0, NA),
            Omega2_CI_high = c(1, 1, 1, 1, 1, 1, 1, NA),
            Epsilon2_partial = c(0.21, 0.54, -0.1, 0.28, -0.06, 0.02, -0.18, NA),
            Epsilon2_CI_low = c(0, 0, 0, 0, 0, 0, 0, NA),
            Epsilon2_CI_high = c(1, 1, 1, 1, 1, 1, 1, NA)
          ),
          row.names = c(NA, 8L),
          class = "data.frame",
          ci = 0.99,
          model_class = c("manova", "maov", "aov", "mlm", "lm"),
          digits = 2,
          ci_digits = 2,
          p_digits = 3
        ),
        tolerance = 0.1,
        ignore_attr = TRUE
      )
    })

    # maov ------------------------------------------------

    test_that("works with maov", {
      skip_on_cran()
      skip_if_not_installed("effectsize", minimum_version = "0.5.0")

      set.seed(123)
      fit <- lm(cbind(mpg, disp, hp) ~ factor(cyl), data = mtcars)
      m <- aov(fit)

      set.seed(123)
      df_maov <-
        as.data.frame(model_parameters(m,
          ci = 0.95,
          eta_squared = "partial",
          omega_squared = "raw",
          epsilon_squared = "raw"
        ))

      expect_equal(
        df_maov,
        structure(
          list(
            Response = c("mpg", "mpg", "disp", "disp", "hp", "hp"),
            Parameter = c(
              "factor(cyl)",
              "Residuals",
              "factor(cyl)",
              "Residuals",
              "factor(cyl)",
              "Residuals"
            ),
            Sum_Squares = c(824.78, 301.26, 398890.96, 77293.83, 104030.54, 41696.33),
            df = c(2, 29, 2, 29, 2, 29),
            Mean_Square = c(412.39, 10.39, 199445.48, 2665.3, 52015.27, 1437.8),
            F = c(39.7, NA, 74.83, NA, 36.18, NA),
            p = c(0, NA, 0, NA, 0, NA),
            Omega2 = c(0.82, NA, 0.69, NA, 0.71, NA),
            Omega2_CI_low = c(0.68, NA, 0.47, NA, 0.5, NA),
            Omega2_CI_high = c(1, NA, 1, NA, 1, NA),
            Eta2 = c(0.84, NA, 0.71, NA, 0.73, NA),
            Eta2_CI_low = c(0.71, NA, 0.51, NA, 0.54, NA),
            Eta2_CI_high = c(1, NA, 1, NA, 1, NA),
            Epsilon2 = c(0.83, NA, 0.69, NA, 0.71, NA),
            Epsilon2_CI_low = c(0.69, NA, 0.48, NA, 0.51, NA),
            Epsilon2_CI_high = c(1, NA, 1, NA, 1, NA)
          ),
          row.names = c(NA, 6L),
          class = "data.frame",
          ci = 0.95,
          model_class = c("maov", "aov", "mlm", "lm"),
          digits = 2,
          ci_digits = 2,
          p_digits = 3
        ),
        tolerance = 0.1,
        ignore_attr = TRUE
      )
    })

    # Gam ------------------------------------------------

    test_that("works with Gam", {
      skip_on_cran()
      skip_if_not_installed("effectsize", minimum_version = "0.5.0")

      # setup
      set.seed(123)

      # model
      set.seed(123)
      g <- gam::gam(
        formula = mpg ~ gam::s(hp, 4) + am + qsec,
        data = mtcars
      )

      set.seed(123)
      df_Gam <-
        as.data.frame(model_parameters(g,
          ci = 0.50,
          omega_squared = "partial"
        ))

      expect_equal(
        df_Gam,
        structure(
          list(
            Parameter = c("gam::s(hp, 4)", "am", "qsec", "Residuals"),
            Sum_Squares = c(678.37287, 202.23503, 6.87905, 238.56023),
            df = c(1, 1, 1, 28),
            Mean_Square = c(678.37287, 202.23503, 6.87905, 8.52001),
            `F` = c(79.62115, 23.73648, 0.8074, NA),
            p = c(0, 4e-05, 0.37655, NA),
            Omega2_partial = c(0.71072, 0.41538, -0.00606, NA),
            Omega2_CI_low = c(0.70634, 0.41067, 0, NA),
            Omega2_CI_high = c(1, 1, 1, NA)
          ),
          row.names = c(NA, 4L),
          class = "data.frame",
          ci = 0.5,
          model_class = c("anova", "data.frame"),
          digits = 2,
          ci_digits = 2,
          p_digits = 3
        ),
        tolerance = 0.1,
        ignore_attr = TRUE
      )
    })


    # anova ------------------------------------------------

    test_that("works with anova", {
      skip_on_cran()
      skip_if_not_installed("effectsize", minimum_version = "0.5.0")

      set.seed(123)
      mod <-
        car::Anova(stats::lm(
          formula = conformity ~ fcategory * partner.status,
          data = Moore,
          contrasts = list(fcategory = contr.sum, partner.status = contr.sum)
        ))

      set.seed(123)
      df_car <-
        as.data.frame(model_parameters(mod,
          ci = 0.89,
          eta_squared = "raw",
          omega_squared = "partial",
          epsilon_squared = "raw"
        ))

      expect_equal(
        df_car,
        structure(
          list(
            Parameter = c(
              "fcategory",
              "partner.status",
              "fcategory:partner.status",
              "Residuals"
            ),
            Sum_Squares = c(11.61, 212.21, 175.49, 817.76),
            df = c(2, 1, 2, 39),
            Mean_Square = c(5.81, 212.21, 87.74, 20.97),
            F = c(0.28, 10.12, 4.18, NA),
            p = c(0.76, 0, 0.02, NA),
            Omega2_partial = c(-0.03, 0.17, 0.12, NA),
            Omega2_CI_low = c(0, 0.03, 0, NA),
            Omega2_CI_high = c(1, 1, 1, NA),
            Eta2 = c(0.01, 0.17, 0.14, NA),
            Eta2_CI_low = c(0, 0.03, 0, NA),
            Eta2_CI_high = c(1, 1, 1, NA),
            Epsilon2 = c(-0.02, 0.16, 0.11, NA),
            Epsilon2_CI_low = c(0, 0.03, 0, NA),
            Epsilon2_CI_high = c(1, 1, 1, NA)
          ),
          row.names = c(NA, 4L),
          class = "data.frame",
          ci = 0.89,
          model_class = c("anova", "data.frame"),
          digits = 2,
          ci_digits = 2,
          p_digits = 3
        ),
        tolerance = 0.1,
        ignore_attr = TRUE
      )
    })
  }
}
