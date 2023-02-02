if (requiet("lme4") && requiet("effectsize")) {
  unloadNamespace("afex")
  unloadNamespace("lmerTest")
  data(iris)
  iris$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(iris))
  iris$Cat2 <- rep(c("A", "B"), length.out = nrow(iris))

  # aov ----------------------------------

  test_that("model_parameters.aov", {
    skip_if_not_installed("effectsize", minimum_version = "0.5.1")
    model <- aov(Sepal.Width ~ Species, data = iris)
    mp <- suppressMessages(model_parameters(model, effectsize_type = c("omega", "eta", "epsilon"), ci = 0.9, alternative = "greater"))
    es <- suppressMessages(effectsize::omega_squared(model, partial = TRUE, ci = 0.9))
    expect_equal(na.omit(mp$Omega2_CI_low), es$CI_low, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(mp$Omega2_CI_low, c(0.3122, NA), tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(na.omit(mp$Omega2_CI_high), es$CI_high, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(na.omit(mp$Omega2_CI_high), 1, tolerance = 1e-3, ignore_attr = TRUE)

    expect_identical(colnames(mp), c(
      "Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
      "Omega2", "Omega2_CI_low", "Omega2_CI_high", "Eta2",
      "Eta2_CI_low", "Eta2_CI_high", "Epsilon2", "Epsilon2_CI_low",
      "Epsilon2_CI_high"
    ))

    model <- aov(Sepal.Length ~ Species * Cat1 * Cat2, data = iris)
    mp <- model_parameters(model, effectsize_type = "eta", ci = 0.9, partial = FALSE, alternative = "greater")
    es <- effectsize::eta_squared(model, partial = FALSE, ci = 0.9)
    expect_equal(na.omit(mp$Eta2_CI_low), es$CI_low, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(mp$Eta2_CI_low, c(0.5572, 0, 0, 0, 0, 0, 0, NA), tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(na.omit(mp$Eta2_CI_high), es$CI_high, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(na.omit(mp$Eta2_CI_high), rep(1, 7), tolerance = 1e-3, ignore_attr = TRUE)

    expect_identical(colnames(mp), c(
      "Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
      "Eta2", "Eta2_CI_low", "Eta2_CI_high"
    ))
  })


  # anova ---------------------

  data(mtcars)
  test_that("model_parameters.anova", {
    skip_if_not_installed("effectsize", minimum_version = "0.5.1")
    model <- anova(lm(Sepal.Length ~ Species * Cat1 * Cat2, data = iris))
    mp <- model_parameters(model, effectsize_type = c("omega", "eta", "epsilon"), partial = TRUE, ci = 0.9, alternative = "greater")
    es <- effectsize::omega_squared(model, partial = TRUE, ci = 0.9)
    expect_equal(na.omit(mp$Omega2_CI_low), es$CI_low, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(na.omit(mp$Omega2_CI_high), es$CI_high, tolerance = 1e-3, ignore_attr = TRUE)

    expect_identical(colnames(mp), c(
      "Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
      "Omega2_partial", "Omega2_CI_low", "Omega2_CI_high", "Eta2_partial",
      "Eta2_CI_low", "Eta2_CI_high", "Epsilon2_partial", "Epsilon2_CI_low",
      "Epsilon2_CI_high"
    ))
  })

  data(mtcars)
  test_that("model_parameters.anova", {
    skip_if_not_installed("effectsize", minimum_version = "0.5.1")
    model <- aov(wt ~ cyl + Error(gear), data = mtcars)
    suppressWarnings({
      mp <- model_parameters(model, effectsize_type = c("omega", "eta", "epsilon"), partial = TRUE, ci = 0.9)
      es <- effectsize::omega_squared(model, partial = TRUE, ci = 0.9, verbose = FALSE)
    })
    expect_equal(na.omit(mp$Omega2_CI_low), es$CI_low[2], tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(na.omit(mp$Omega2_CI_high), es$CI_high, tolerance = 1e-3, ignore_attr = TRUE)

    expect_identical(colnames(mp), c(
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
      skip_if_not_installed("effectsize", minimum_version = "0.5.1")
      mp <- model_parameters(model, effectsize_type = c("omega", "eta", "epsilon"), partial = TRUE, ci = 0.9)
      es <- effectsize::omega_squared(model, partial = TRUE, ci = 0.9)
      expect_equal(na.omit(mp$Omega2_CI_low), es$CI_low, tolerance = 1e-3, ignore_attr = TRUE)
      expect_equal(mp$Omega2_CI_low, c(0, 0.05110, 0.00666, NA), tolerance = 1e-3, ignore_attr = TRUE)
      expect_equal(na.omit(mp$Omega2_CI_high), es$CI_high, tolerance = 1e-3, ignore_attr = TRUE)
      expect_equal(na.omit(mp$Omega2_CI_high), rep(1, 3), tolerance = 1e-3, ignore_attr = TRUE)

      expect_identical(colnames(mp), c(
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
    skip_if_not_installed("effectsize", minimum_version = "0.5.1")
    mp <- suppressMessages(model_parameters(model, effectsize_type = c("omega", "eta", "epsilon"), partial = TRUE, ci = 0.9))
    es <- suppressMessages(effectsize::omega_squared(model, partial = TRUE, ci = 0.9))
    expect_equal(na.omit(mp$Omega2_CI_low), es$CI_low, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(mp$Omega2_CI_low, c(0.58067, NA, 0.74092, NA, 0.55331, NA), tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(na.omit(mp$Omega2_CI_high), es$CI_high, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(na.omit(mp$Omega2_CI_high), rep(1, 3), tolerance = 1e-3, ignore_attr = TRUE)

    expect_identical(colnames(mp), c(
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
      skip_if_not_installed("effectsize", minimum_version = "0.5.1")

      set.seed(123)
      npk.aov <- aov(yield ~ block + N * P, npk)

      set.seed(123)
      df_aov <-
        as.data.frame(parameters::model_parameters(npk.aov,
          ci = 0.95,
          effectsize_type = c("eta", "omega"),
          partial = FALSE
        ))

      expect_equal(
        df_aov,
        structure(
          list(
            Parameter = c("block", "N", "P", "N:P", "Residuals"),
            Sum_Squares = c(343.295, 189.28167, 8.40167, 21.28167, 314.105),
            df = c(5, 1, 1, 1, 15),
            Mean_Square = c(68.659, 189.28167, 8.40167, 21.28167, 20.94033),
            F = c(3.27879, 9.0391, 0.40122, 1.0163, NA),
            p = c(0.03371, 0.00885, 0.536, 0.32938, NA),
            Eta2 = c(0.39173, 0.21598, 0.00959, 0.02428, NA),
            Eta2_CI_low = c(0, 0, 0, 0, NA),
            Eta2_CI_high = c(1, 1, 1, 1, NA),
            Omega2 = c(0.2659, 0.18761, -0.01397, 0.00038, NA),
            Omega2_CI_low = c(0, 0, 0, 0, NA),
            Omega2_CI_high = c(1, 1, 1, 1, NA)
          ),
          row.names = c(NA, 5L), ci = 0.95, model_class = c("aov", "lm"),
          anova_type = 1, title = "", digits = 2, ci_digits = 2, p_digits = 3,
          object_name = "npk.aov", class = "data.frame"
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
      skip_if_not_installed("effectsize", minimum_version = "0.5.1")

      set.seed(123)
      # fake a 2nd response variable
      npk2 <- within(npk, foo <- rnorm(24))

      # model
      m <- manova(cbind(yield, foo) ~ block + N * P * K, npk2)

      set.seed(123)
      df_manova <-
        as.data.frame(model_parameters(m,
          ci = 0.99,
          effectsize_type = c("epsilon", "omega"),
          partial = TRUE
        ))

      expect_identical(
        df_manova$Parameter, c("block", "N", "P", "K", "N:P", "N:K", "P:K", "Residuals")
      )
      expect_identical(
        colnames(df_manova),
        c(
          "Parameter", "Statistic", "df", "df_num", "df_error", "F", "p",
          "Epsilon2_partial", "Epsilon2_CI_low", "Epsilon2_CI_high",
          "Omega2_partial", "Omega2_CI_low", "Omega2_CI_high"
        )
      )
      expect_equal(
        df_manova$Statistic,
        c(0.88, 0.61, 0.07, 0.39, 0.11, 0.17, 0, NA),
        tolerance = 0.1
      )
      expect_equal(
        df_manova$Omega2_CI_low,
        c(0, 0, 0, 0, 0, 0, 0, NA),
        tolerance = 0.1
      )
      expect_equal(
        df_manova$Omega2_partial,
        c(0.204, 0.518, 0, 0.262, 0, 0.022, 0, NA),
        tolerance = 0.1
      )
    })

    # Gam ------------------------------------------------

    test_that("works with Gam", {
      skip_on_cran()
      skip_if_not_installed("effectsize", minimum_version = "0.5.1")

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
          effectsize_type = "omega",
          partial = TRUE
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
      skip_if_not_installed("effectsize", minimum_version = "0.7.1")

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
          effectsize_type = c("eta", "epsilon"),
          partial = FALSE
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
