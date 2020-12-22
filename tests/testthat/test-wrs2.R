if (require("testthat") && require("parameters") && require("WRS2")) {
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
          F = 3,
          df = 2,
          df_error = 4,
          p = 0.16,
          Effsize = 0.789441283518576,
          CI_low = 0.419393210047738,
          CI_high = 1.32908145281133,
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
          F = 3.26140350877192,
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
          Difference_CI_low = -17.9293607102093,
          Difference_CI_high = 4.42936071020934,
          t = 1.29575716084179,
          df_error = 13.9137233889032,
          p = 0.216143324927692,
          Effsize = 0.378416737055775,
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
          Difference_CI_low = -8.29182014978235,
          Difference_CI_high = 64.958486816449,
          t = 1.98861015130686,
          df_error = 5,
          p = 0.103433529948121,
          Effsize = 0.519275279669972,
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
}
