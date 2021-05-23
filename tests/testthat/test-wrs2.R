.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest && require("testthat") && require("parameters") && require("WRS2") && getRversion() >= "3.6.0") {

  # model_parameters.t1way ---------------------------------------------------

  test_that("model_parameters.t1way", {
    set.seed(123)
    df_b <- as.data.frame(model_parameters(t1way(libido ~ dose, data = viagra)))

    set.seed(123)
    df_w <-
      as.data.frame(model_parameters(rmanova(WineTasting$Taste, WineTasting$Wine, WineTasting$Taster)))

    # between-subjects
    expect_equal(
      df_b,
      structure(
        list(
          `F` = 3,
          df = 2,
          df_error = 4,
          p = 0.16,
          Estimate = 0.789441283518576,
          CI = .95,
          CI_low = 0.419393210047738,
          CI_high = 1.32908145281133,
          Effectsize = "Explanatory measure of effect size",
          Method = "A heteroscedastic one-way ANOVA for trimmed means"
        ),
        class = "data.frame",
        row.names = c(
          NA,
          -1L
        ),
        title = "A heteroscedastic one-way ANOVA for trimmed means",
        model_class = "t1way",
        digits = 2,
        ci_digits = 2,
        p_digits = 3,
        ci = 0.95
      ),
      tolerance = 0.001
    )

    # within-subjects
    expect_equal(
      df_w,
      structure(
        list(
          `F` = 3.26140350877192,
          df = 1.60923075707166,
          df_error = 20.9199998419316,
          p = 0.0676086468201853,
          Method = "A heteroscedastic one-way repeated measures ANOVA for trimmed means"
        ),
        class = "data.frame",
        row.names = c(
          NA,
          -1L
        ),
        title = "A heteroscedastic one-way repeated measures ANOVA for trimmed means",
        model_class = "t1way",
        digits = 2,
        ci_digits = 2,
        p_digits = 3,
        ci = 0.95
      ),
      tolerance = 0.001
    )
  })

  # model_parameters.yuen ---------------------------------------------------

  test_that("model_parameters.yuen", {
    set.seed(123)
    df_b <- as.data.frame(model_parameters(yuen(Anxiety ~ Group, data = spider)))

    before <- c(190, 210, 300, 240, 280, 170, 280, 250, 240, 220)
    after <- c(210, 210, 340, 190, 260, 180, 200, 220, 230, 200)
    set.seed(123)
    df_w <- as.data.frame(model_parameters(yuend(before, after)))

    # between-subjects
    expect_equal(
      df_b,
      structure(
        list(
          Difference = -6.75,
          CI = .95,
          Difference_CI_low = -17.9293607102093,
          Difference_CI_high = 4.42936071020934,
          t = 1.29575716084179,
          df_error = 13.9137233889032,
          p = 0.216143324927692,
          Estimate = 0.378416737055775,
          Effectsize = "Explanatory measure of effect size",
          Method = "Yuen's test on trimmed means for independent samples"
        ),
        class = "data.frame",
        row.names = c(
          NA,
          -1L
        ),
        title = "Yuen's test on trimmed means for independent samples",
        model_class = "yuen",
        digits = 2,
        ci_digits = 2,
        p_digits = 3,
        ci = 0.95
      ),
      tolerance = 0.001
    )

    # within-subjects
    expect_equal(
      df_w,
      structure(
        list(
          Difference = 28.3333333333333,
          CI = .95,
          Difference_CI_low = -8.29182014978235,
          Difference_CI_high = 64.958486816449,
          t = 1.98861015130686,
          df_error = 5,
          p = 0.103433529948121,
          Estimate = 0.519275279669972,
          Effectsize = "Explanatory measure of effect size",
          Method = "Yuen's test on trimmed means for dependent samples"
        ),
        class = "data.frame",
        row.names = c(
          NA,
          -1L
        ),
        title = "Yuen's test on trimmed means for dependent samples",
        model_class = "yuen",
        digits = 2,
        ci_digits = 2,
        p_digits = 3,
        ci = 0.95
      ),
      tolerance = 0.001
    )
  })

  # model_parameters.mcp and robtab ---------------------------------------

  test_that("model_parameters.mcp and robtab", {
    set.seed(123)
    df_b <- as.data.frame(model_parameters(lincon(libido ~ dose, data = viagra)))

    set.seed(123)
    df_w <- as.data.frame(model_parameters(rmmcp(WineTasting$Taste, WineTasting$Wine, WineTasting$Taster)))

    set.seed(123)
    df <- as.data.frame(model_parameters(discmcp(libido ~ dose, viagra, nboot = 100)))

    # between-subjects
    expect_equal(
      df_b,
      structure(
        list(
          Group1 = c("placebo", "placebo", "low"),
          Group2 = c("low", "high", "high"),
          Psihat = c(-1, -3, -2),
          CI = c(.95, .95, .95),
          CI_low = c(-5.3185800135384, -7.3185800135384, -6.3185800135384),
          CI_high = c(3.3185800135384, 1.3185800135384, 2.3185800135384),
          p = c(0.435330942514376, 0.180509539510735, 0.316604846750915)
        ),
        class = "data.frame",
        row.names = c(NA, -3L),
        model_class = "mcp1",
        digits = 2,
        ci_digits = 2,
        p_digits = 3,
        ci = 0.95
      ),
      tolerance = 0.001
    )

    # within-subjects
    expect_equal(
      df_w,
      structure(
        list(
          Group1 = c("Wine A", "Wine A", "Wine B"),
          Group2 = c("Wine B", "Wine C", "Wine C"),
          Psihat = c(
            0.0214285714285715,
            0.114285714285714, 0.0821428571428571
          ),
          CI = c(.95, .95, .95),
          CI_low = c(
            -0.0216368317742901,
            0.0214755794006548, 0.00891056424896097
          ),
          CI_high = c(
            0.0644939746314331,
            0.207095849170774, 0.155375150036753
          ),
          p = c(
            0.195004531096295,
            0.00491556623286327, 0.00877739635342234
          ),
          p.crit = c(
            0.05, 0.0169,
            0.025
          )
        ),
        class = "data.frame",
        row.names = c(NA, -3L),
        model_class = "mcp2",
        digits = 2,
        ci_digits = 2,
        p_digits = 3,
        ci = 0.95
      ),
      tolerance = 0.001
    )

    expect_equal(
      df,
      structure(
        list(
          Group1 = c("placebo", "placebo", "low"),
          Group2 = c("low", "high", "high"),
          p = c(0.811881188118812, 0.851485148514851, 0.861386138613861),
          p.crit = c(0.0166666666666667, 0.025, 0.05)
        ),
        row.names = c(NA, -3L),
        class = "data.frame",
        model_class = "robtab",
        digits = 2,
        ci_digits = 2,
        p_digits = 3,
        ci = 0.95
      ),
      tolerance = 0.001
    )
  })

  # model_parameters.akp.effect -----------------------------------------------

  test_that("model_parameters.AKP", {
    set.seed(123)
    mod <-
      WRS2::akp.effect(
        formula = wt ~ am,
        data = mtcars,
        EQVAR = FALSE
      )

    expect_equal(
      as.data.frame(model_parameters(mod)),
      structure(
        list(
          Estimate = 2.48169367327709,
          CI = 0.95,
          CI_low = 0.791129191725663,
          CI_high = 5.09573917444489,
          Effectsize = "Algina-Keselman-Penfield robust standardized difference"
        ),
        class = "data.frame",
        row.names = c(
          NA,
          -1L
        ),
        model_class = "AKP",
        digits = 2,
        ci_digits = 2,
        p_digits = 3,
        ci = 0.95
      ),
      tolerance = 0.002
    )
  })


  # model_parameters.onesampb ---------------------------------------------------

  test_that("model_parameters.onesampb", {
    set.seed(123)
    x <- rnorm(30)

    set.seed(123)
    mod <- onesampb(x, nboot = 100)

    expect_equal(
      as.data.frame(model_parameters(mod)),
      structure(
        list(
          Estimate = -0.0811399751842395,
          CI = .95,
          CI_low = -0.414663225939919,
          CI_high = 0.241710493090677,
          p = 0.7,
          n_Obs = 30L,
          Effectsize = "Robust location measure",
          Method = "One-sample percentile bootstrap"
        ),
        class = "data.frame",
        row.names = c(
          NA,
          -1L
        ),
        title = "One-sample percentile bootstrap",
        model_class = "onesampb",
        digits = 2,
        ci_digits = 2,
        p_digits = 3,
        ci = 0.95
      ),
      tolerance = 0.001
    )
  })
}
