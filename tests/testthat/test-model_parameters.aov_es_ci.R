if (require("insight") && require("effectsize") && require("testthat") && require("lme4") && require("parameters")) {
  unloadNamespace("lmerTest")
  data(iris)
  iris$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(iris))
  iris$Cat2 <- rep(c("A", "B"), length.out = nrow(iris))

  # aov ----------------------------------

  test_that("model_parameters.aov", {
    model <- aov(Sepal.Width ~ Species, data = iris)
    mp <- suppressMessages(model_parameters(model, omega_squared = "partial", eta_squared = "partial", epsilon_squared = TRUE, ci = .9))
    es <- suppressMessages(effectsize::omega_squared(model, partial = TRUE, ci = .9))
    expect_equal(na.omit(mp$Omega2_CI_low), es$CI_low, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(mp$Omega2_CI_low, c(0.29018, NA), tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(na.omit(mp$Omega2_CI_high), es$CI_high, tolerance = 1e-3, ignore_attr = TRUE)

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
    expect_equal(mp$Eta2_CI_low, c(0.53866, 0, 0, 0, 0, 0, 0, NA), tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(na.omit(mp$Eta2_CI_high), es$CI_high, tolerance = 1e-3, ignore_attr = TRUE)

    expect_equal(colnames(mp), c(
      "Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
      "Eta2", "Eta2_CI_low", "Eta2_CI_high"
    ))
  })


  # anova ---------------------

  data(mtcars)
  test_that("model_parameters.anova", {
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
    model <- aov(wt ~ cyl + Error(gear), data = mtcars)
    mp <- model_parameters(model, omega_squared = "partial", eta_squared = "partial", epsilon_squared = TRUE, ci = .9)
    es <- effectsize::omega_squared(model, partial = TRUE, ci = .9)
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

  if (require("car")) {
    set.seed(123)
    data(Moore)
    model <-
      car::Anova(stats::lm(
        formula = conformity ~ fcategory * partner.status,
        data = Moore,
        contrasts = list(fcategory = contr.sum, partner.status = contr.sum)
      ))
    test_that("model_parameters.car-anova", {
      mp <- model_parameters(model, omega_squared = "partial", eta_squared = "partial", epsilon_squared = TRUE, ci = .9)
      es <- effectsize::omega_squared(model, partial = TRUE, ci = .9)
      expect_equal(na.omit(mp$Omega2_CI_low), es$CI_low, tolerance = 1e-3, ignore_attr = TRUE)
      expect_equal(mp$Omega2_CI_low, c(0, 0.0284, 0, NA), tolerance = 1e-3, ignore_attr = TRUE)
      expect_equal(na.omit(mp$Omega2_CI_high), es$CI_high, tolerance = 1e-3, ignore_attr = TRUE)

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
    mp <- suppressMessages(model_parameters(model, omega_squared = "partial", eta_squared = "partial", epsilon_squared = TRUE, ci = .9))
    es <- suppressMessages(effectsize::omega_squared(model, partial = TRUE, ci = .9))
    expect_equal(na.omit(mp$Omega2_CI_low), es$CI_low, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(mp$Omega2_CI_low, c(0.71218, NA, 0.50841, NA, 0.53774, NA), tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(na.omit(mp$Omega2_CI_high), es$CI_high, tolerance = 1e-3, ignore_attr = TRUE)

    expect_equal(colnames(mp), c(
      "Response", "Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
      "Omega2", "Omega2_CI_low", "Omega2_CI_high", "Eta2",
      "Eta2_CI_low", "Eta2_CI_high", "Epsilon2", "Epsilon2_CI_low",
      "Epsilon2_CI_high"
    ))
  })
}

# stricter tests ---------------------------------------------------------

if (FALSE) {
  if (require("insight") && require("effectsize") && require("testthat") && require("parameters")
  && require("car") && require("gam")) {

    # aov ------------------------------------------------

    test_that("works with aov", {
      testthat::skip_on_cran()

      set.seed(123)
      npk.aov <- aov(yield ~ block + N * P, npk)

      set.seed(123)
      df_aov <-
        as.data.frame(parameters::model_parameters(npk.aov,
          ci = 0.95,
          eta_squared = "partial",
          omega_squared = "raw"
        ))

      testthat::expect_equal(
        df_aov,
        structure(list(Parameter = c("block", "N", "P", "N:P", "Residuals"), Sum_Squares = c(
          343.295, 189.281666666666, 8.40166666666668,
          21.2816666666667, 314.105
        ), df = c(5, 1, 1, 1, 15), Mean_Square = c(
          68.659,
          189.281666666666, 8.40166666666668, 21.2816666666667, 20.9403333333333
        ), F = c(
          3.27879212365292, 9.03909520701675, 0.40121933748269,
          1.01630028175292, NA
        ), p = c(
          0.0337146802153708, 0.00885458998425757,
          0.535999422597354, 0.329384683205555, NA
        ), Omega2 = c(
          0.265899827483473,
          0.18760763708823, -0.0139736901151447, 0.000380398199646671,
          NA
        ), Omega2_CI_low = c(0, 0, 0, 0, NA), Omega2_CI_high = c(
          0.487758253395471,
          0.503695399849356, 0, 0.103325610903318, NA
        ), Eta2_partial = c(
          0.522201095223608,
          0.376016448588229, 0.0260511410616835, 0.0634541226047548, NA
        ), Eta2_CI_low = c(0, 0.0339723666586561, 0, 0, NA), Eta2_CI_high = c(
          0.710285425082251,
          0.643142321421895, 0.307961927646149, 0.372422768990859, NA
        )), row.names = c(
          NA,
          -5L
        ), class = "data.frame", ci = 0.95, model_class = c(
          "aov",
          "lm"
        ), digits = 2, ci_digits = 2, p_digits = 3),
        tolerance = 0.001
      )
    })


    # aovlist ------------------------------------------------

    test_that("works with aovlist", {
      testthat::skip_on_cran()

      set.seed(123)
      npk.aovE <- aov(yield ~ N * P * K + Error(block), npk)

      set.seed(123)
      df_aovE <-
        as.data.frame(parameters::model_parameters(npk.aovE,
          ci = 0.90,
          eta_squared = "raw",
          omega_squared = "partial"
        ))

      testthat::expect_equal(
        df_aovE,
        structure(
          list(
            Group = c(
              "block",
              "block",
              "Within",
              "Within",
              "Within",
              "Within",
              "Within",
              "Within",
              "Within"
            ),
            Parameter = c(
              "N:P:K",
              "Residuals",
              "N",
              "P",
              "K",
              "N:P",
              "N:K",
              "P:K",
              "Residuals"
            ),
            Sum_Squares = c(
              37.0016666666666,
              306.293333333333,
              189.281666666666,
              8.40166666666665,
              95.2016666666665,
              21.2816666666668,
              33.1350000000001,
              0.481666666666673,
              185.286666666667
            ),
            df = c(
              1, 4, 1, 1,
              1, 1, 1, 1, 12
            ),
            Mean_Square = c(
              37.0016666666666,
              76.5733333333334,
              189.281666666666,
              8.40166666666665,
              95.2016666666665,
              21.2816666666668,
              33.1350000000001,
              0.481666666666673,
              15.4405555555555
            ),
            F = c(
              0.483218701027336,
              NA,
              12.2587342136509,
              0.54412981686036,
              6.16568920231712,
              1.37829669341202,
              2.14597200733999,
              0.0311949051919552,
              NA
            ),
            p = c(
              0.525236141197408,
              NA,
              0.00437181182579937,
              0.474904092674435,
              0.0287950535002327,
              0.263165282877167,
              0.168647878500492,
              0.862752085685407,
              NA
            ),
            Omega2_partial = c(
              -0.0942477999055262,
              NA,
              0.229512910165375,
              -0.0122085297827422,
              0.120238953669017,
              0.00990967810040327,
              0.0294275532444688,
              -0.0263066622506333,
              NA
            ),
            Omega2_CI_low = c(0, NA, 0, 0, 0, 0, 0, 0, NA),
            Omega2_CI_high = c(
              0,
              NA,
              0.521347896232784,
              0,
              0.421019056126463,
              0.224904114251074,
              0.291675659579102,
              0,
              NA
            ),
            Eta2 = c(
              0.042221753112763,
              NA,
              0.215984968211495,
              0.00958694912127555,
              0.108632438158377,
              0.0242840216880715,
              0.0378095884705575,
              0.00054961878517133,
              NA
            ),
            Eta2_CI_low = c(0, NA, 0, 0, 0, 0, 0, 0, NA),
            Eta2_CI_high = c(
              0.492138577275158,
              NA,
              0.510212540175927,
              0.222986106654371,
              0.408375708137789,
              0.278952597512264,
              0.309385191526979,
              0.0383730915537892,
              NA
            )
          ),
          row.names = c(NA, -9L),
          class = "data.frame",
          ci = 0.9,
          model_class = c(
            "aovlist",
            "listof"
          ),
          digits = 2,
          ci_digits = 2,
          p_digits = 3
        ),
        tolerance = 0.001
      )
    })

    # manova ------------------------------------------------

    test_that("works with manova", {
      testthat::skip_on_cran()

      set.seed(123)
      # fake a 2nd response variable
      npk2 <- within(npk, foo <- rnorm(24))

      # model
      m <- manova(cbind(yield, foo) ~ block + N * P * K, npk2)

      set.seed(123)
      df_manova <-
        as.data.frame(parameters::model_parameters(m,
          ci = 0.99,
          eta_squared = NULL,
          omega_squared = "partial",
          epsilon_squared = "partial"
        ))

      testthat::expect_equal(
        df_manova,
        structure(list(Parameter = c(
          "block", "N", "P", "K", "N:P", "N:K",
          "P:K", "Residuals"
        ), Pillai = c(
          0.883067422762204, 0.607644075860196,
          0.0661452322660012, 0.387877219894175, 0.105732910542443, 0.173682169988875,
          0.00319108713280279, NA
        ), df = c(5, 1, 1, 1, 1, 1, 1, 12), F = c(
          1.89748410765359,
          8.5178844299551, 0.389566761377431, 3.4851254989222, 0.650287833287238,
          1.1560345187347, 0.0176071652288221, NA
        ), p = c(
          0.096377338151606,
          0.00582426569911999, 0.686335356549494, 0.0672375656314355, 0.540841321783317,
          0.350194387588424, 0.982574567642381, NA
        ), Omega2_partial = c(
          -0.0878988423640142,
          0.00646103797636744, -0.0406933499272959, -0.0413301468834696,
          -0.0409171937691718, -0.0434027067555957, -0.0434677958886055,
          NA
        ), Omega2_CI_low = c(0, 0, 0, 0, 0, 0, 0, NA), Omega2_CI_high = c(
          0,
          0.391981215238936, 0, 0, 0, 0, 0, NA
        ), Epsilon2_partial = c(
          -0.128752505288263,
          0.0118632135405797, -0.0778052394174465, -0.0790669109761701,
          -0.0782485756868515, -0.0831830002436285, -0.0833125095450078,
          NA
        ), Epsilon2_CI_low = c(0, 0, 0, 0, 0, 0, 0, NA), Epsilon2_CI_high = c(
          0,
          0.415582890766113, 0, 0, 0, 0, 0, NA
        )), row.names = c(NA, -8L), class = "data.frame", ci = 0.99, model_class = c(
          "manova",
          "maov", "aov", "mlm", "lm"
        ), digits = 2, ci_digits = 2, p_digits = 3)
      )
    })

    # maov ------------------------------------------------

    test_that("works with maov", {
      testthat::skip_on_cran()

      set.seed(123)
      fit <- lm(cbind(mpg, disp, hp) ~ factor(cyl), data = mtcars)
      m <- aov(fit)

      set.seed(123)
      df_maov <-
        as.data.frame(parameters::model_parameters(m,
          ci = 0.95,
          eta_squared = "partial",
          omega_squared = "raw",
          epsilon_squared = "raw"
        ))

      testthat::expect_equal(
        df_maov,
        structure(list(Response = c(
          "mpg", "mpg", "disp", "disp", "hp",
          "hp"
        ), Parameter = c(
          "factor(cyl)", "Residuals", "factor(cyl)",
          "Residuals", "factor(cyl)", "Residuals"
        ), Sum_Squares = c(
          824.784590097402,
          301.262597402597, 398890.960661526, 77293.834025974, 104030.543831169,
          41696.3311688312
        ), df = c(2, 29, 2, 29, 2, 29), Mean_Square = c(
          412.392295048701,
          10.3883654276758, 199445.480330763, 2665.30462158531, 52015.2719155844,
          1437.80452306314
        ), F = c(
          39.697515255869, NA, 74.8302759525227,
          NA, 36.1768731988473, NA
        ), p = c(
          4.97891917440023e-09, NA, 3.55103731907367e-12,
          NA, 1.31854141876143e-08, NA
        ), Omega2 = c(
          0.821886331413962,
          NA, 0.687358781420034, NA, 0.707482142010405, NA
        ), Omega2_CI_low = c(
          0.684104105003535,
          NA, 0.465971885477729, NA, 0.496911764314726, NA
        ), Omega2_CI_high = c(
          0.886539588773566,
          NA, 0.79891511526372, NA, 0.812202863035015, NA
        ), Eta2_partial = c(
          0.837681011892245,
          NA, 0.713873428159143, NA, 0.732460059625523, NA
        ), Eta2_CI_low = c(
          0.711214302249152,
          NA, 0.506876191339265, NA, 0.536217503688617, NA
        ), Eta2_CI_high = c(
          0.896678487239901,
          NA, 0.816407518666423, NA, 0.828595012041054, NA
        ), Epsilon2 = c(
          0.826486598919296,
          NA, 0.694140561135635, NA, 0.71400902925487, NA
        ), Epsilon2_CI_low = c(
          0.691973446569125,
          NA, 0.476324018419977, NA, 0.507088546983586, NA
        ), Epsilon2_CI_high = c(
          0.889495005680589,
          NA, 0.803401832697648, NA, 0.816496626351566, NA
        )), row.names = c(
          NA,
          -6L
        ), class = "data.frame", ci = 0.95, model_class = c(
          "maov",
          "aov", "mlm", "lm"
        ), digits = 2, ci_digits = 2, p_digits = 3),
        tolerance = 0.001
      )
    })

    # Gam ------------------------------------------------

    test_that("works with Gam", {
      testthat::skip_on_cran()

      # setup
      set.seed(123)
      library(gam)

      # model
      set.seed(123)
      g <- gam::gam(
        formula = mpg ~ s(hp, 4) + am + qsec,
        data = mtcars
      )

      set.seed(123)
      df_Gam <-
        as.data.frame(parameters::model_parameters(g,
          ci = 0.50,
          omega_squared = "partial"
        ))

      testthat::expect_equal(
        df_Gam,
        structure(list(Parameter = c("s(hp, 4)", "am", "qsec", "Residuals"), Sum_Squares = c(
          678.372873955398, 112.579787021897, 0.0263219666149212,
          179.682435975258
        ), df = c(1, 1, 1, 24.9999682012449), Mean_Square = c(
          678.372873955398,
          112.579787021897, 0.0263219666149212, 7.18730658090639
        ), F = c(
          94.3848528400814,
          15.6636962337148, 0.00366228521332977, NA
        ), p = c(
          5.72629386092921e-10,
          0.000552204052352258, 0.952225214872875, NA
        ), Omega2_partial = c(
          0.763042769891763,
          0.335832927068168, -0.0355788766484571, NA
        ), Omega2_CI_low = c(
          0.708024043833256,
          0.234134455850255, 0, NA
        ), Omega2_CI_high = c(
          0.79870598524304,
          0.422072113697509, 0, NA
        )), row.names = c(NA, -4L), class = "data.frame", ci = 0.5, model_class = c(
          "anova",
          "data.frame"
        ), digits = 2, ci_digits = 2, p_digits = 3),
        tolerance = 0.001
      )
    })


    # anova ------------------------------------------------

    test_that("works with anova", {
      testthat::skip_on_cran()

      set.seed(123)
      library(car)
      mod <-
        car::Anova(stats::lm(
          formula = conformity ~ fcategory * partner.status,
          data = Moore,
          contrasts = list(fcategory = contr.sum, partner.status = contr.sum)
        ))

      set.seed(123)
      df_car <-
        as.data.frame(parameters::model_parameters(mod,
          ci = 0.89,
          eta_squared = "raw",
          omega_squared = "partial",
          epsilon_squared = "raw"
        ))

      testthat::expect_equal(
        df_car,
        structure(
          list(
            Parameter = c(
              "fcategory",
              "partner.status",
              "fcategory:partner.status",
              "Residuals"
            ),
            Sum_Squares = c(
              11.6147000439174,
              212.213777777778,
              175.488927849928,
              817.763961038961
            ),
            df = c(2, 1, 2, 39),
            Mean_Square = c(
              5.80735002195871,
              212.213777777778,
              87.744463924964,
              20.9683066933067
            ),
            F = c(
              0.276958464357662,
              10.1206921894899, 4.18462326063615, NA
            ),
            p = c(
              0.759564473545402,
              0.00287422991075648,
              0.0225724417916786,
              NA
            ),
            Omega2_partial = c(
              -0.0332021359560262,
              0.16852504689992, 0.123989486951785, NA
            ),
            Omega2_CI_low = c(
              0,
              0.0309899250644307, 0, NA
            ),
            Omega2_CI_high = c(
              0, 0.338882655432455,
              0.272531060676589, NA
            ),
            Eta2 = c(
              0.00954307605193938, 0.174362851640174,
              0.14418832844695, NA
            ),
            Eta2_CI_low = c(
              0, 0.0340186024875703,
              0.00261502892178969, NA
            ),
            Eta2_CI_high = c(
              0.0574165319729867,
              0.345007422402552, 0.272956223959914, NA
            ),
            Epsilon2 = c(
              -0.024913628761442,
              0.157134499233483, 0.109731623633568, NA
            ),
            Epsilon2_CI_low = c(
              0,
              0.0253657240863739, 0, NA
            ),
            Epsilon2_CI_high = c(
              0, 0.326739989608251,
              0.254500512500744, NA
            )
          ),
          row.names = c(NA, -4L),
          class = "data.frame",
          ci = 0.89,
          model_class = c(
            "anova",
            "data.frame"
          ),
          digits = 2,
          ci_digits = 2,
          p_digits = 3
        ),
        tolerance = 0.001
      )
    })
  }
}
