skip_if_not_installed("curl")
skip_if_offline()
skip_on_cran()
skip_if_not_installed("brms")
skip_if_not_installed("insight")
skip_if_not_installed("withr")

withr::with_options(
  list(parameters_exponentiate = FALSE),
  {
    test_that("print brms", {
      m1 <- insight::download_model("brms_1")
      mp1 <- model_parameters(m1, effects = "all", component = "all", centrality = "mean")
      expect_snapshot(mp1)

      m2 <- insight::download_model("brms_mixed_1")
      mp2 <- model_parameters(m2, effects = "all", component = "all", centrality = "mean")
      expect_snapshot(mp2)

      m3 <- insight::download_model("brms_mixed_2")
      mp3 <- model_parameters(m3, effects = "all", component = "all", centrality = "mean")
      expect_snapshot(mp3)

      m4 <- insight::download_model("brms_mixed_3")
      mp4 <- model_parameters(m4, effects = "all", component = "all", centrality = "mean")
      expect_snapshot(mp4)

      m5 <- insight::download_model("brms_mixed_4")
      mp5 <- model_parameters(m5, effects = "all", component = "all", centrality = "mean")
      expect_snapshot(mp5)

      m6 <- insight::download_model("brms_mixed_7")
      mp6 <- model_parameters(m6, effects = "all", component = "all", centrality = "mean")
      expect_snapshot(mp6)

      m7 <- insight::download_model("brms_zi_1")
      mp7 <- model_parameters(m7, effects = "all", component = "all", centrality = "mean", verbose = FALSE)
      expect_snapshot(mp7)

      m8 <- insight::download_model("brms_zi_3")
      mp8 <- model_parameters(m8, effects = "all", component = "all", centrality = "mean", verbose = FALSE)
      expect_snapshot(mp8)

      m9 <- insight::download_model("brms_ordinal_1")
      mp9 <- model_parameters(m9, effects = "all", component = "all", centrality = "mean")
      expect_snapshot(mp9)
    })
  }
)
