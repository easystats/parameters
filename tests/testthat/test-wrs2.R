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
        )
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
        )
      ),
      tolerance = 0.001
    )
  })
}
