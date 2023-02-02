requiet("brms")
skip_if_offline()
skip_on_cran()

options("parameters_exponentiate" = FALSE)

test_that("print brms", {
  m1 <- download_model("brms_1")
  mp1 <- model_parameters(m1, effects = "all", component = "all", centrality = "mean")
  expect_snapshot(print(mp1))

  m2 <- download_model("brms_mixed_1")
  mp2 <- model_parameters(m2, effects = "all", component = "all", centrality = "mean")
  expect_snapshot(print(mp2))

  m3 <- download_model("brms_mixed_2")
  mp3 <- model_parameters(m3, effects = "all", component = "all", centrality = "mean")
  expect_snapshot(print(mp3))

  m4 <- download_model("brms_mixed_3")
  mp4 <- model_parameters(m4, effects = "all", component = "all", centrality = "mean")
  expect_snapshot(print(mp4))

  m5 <- download_model("brms_mixed_4")
  mp5 <- model_parameters(m5, effects = "all", component = "all", centrality = "mean")
  expect_snapshot(print(mp5))

  m6 <- download_model("brms_mixed_7")
  mp6 <- model_parameters(m6, effects = "all", component = "all", centrality = "mean")
  expect_snapshot(print(mp6))

  m7 <- download_model("brms_zi_1")
  mp7 <- model_parameters(m7, effects = "all", component = "all", centrality = "mean")
  expect_snapshot(print(mp7))

  m8 <- download_model("brms_zi_3")
  mp8 <- model_parameters(m8, effects = "all", component = "all", centrality = "mean")
  expect_snapshot(print(mp8))

  m9 <- download_model("brms_ordinal_1")
  mp9 <- model_parameters(m9, effects = "all", component = "all", centrality = "mean")
  expect_snapshot(print(mp9))
})

options("parameters_exponentiate" = NULL)
