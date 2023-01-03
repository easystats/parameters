requiet("brms")
requiet("parameters")
requiet("insight")
skip_if_offline()

options("parameters_exponentiate" = FALSE)

.runThisTest <- length(strsplit(packageDescription("parameters")$Version, ".", fixed = TRUE)[[1]]) > 3

if (.runThisTest) {
  test_that("print brms", {
    m <- download_model("brms_1")
    mp <- model_parameters(m, effects = "all", component = "all", centrality = "mean")
    expect_snapshot(print(mp))
  })

  test_that("print brms", {
    m <- download_model("brms_mixed_1")
    mp <- model_parameters(m, effects = "all", component = "all", centrality = "mean")
    expect_snapshot(print(mp))
  })

  test_that("print brms", {
    m <- download_model("brms_mixed_2")
    mp <- model_parameters(m, effects = "all", component = "all", centrality = "mean")
    expect_snapshot(print(mp))
  })

  test_that("print brms", {
    m <- download_model("brms_mixed_3")
    mp <- model_parameters(m, effects = "all", component = "all", centrality = "mean")
    expect_snapshot(print(mp))
  })

  test_that("print brms", {
    m <- download_model("brms_mixed_4")
    mp <- model_parameters(m, effects = "all", component = "all", centrality = "mean")
    expect_snapshot(print(mp))
  })

  test_that("print brms", {
    m <- download_model("brms_mixed_7")
    mp <- model_parameters(m, effects = "all", component = "all", centrality = "mean")
    expect_snapshot(print(mp))
  })

  test_that("print brms", {
    m <- download_model("brms_zi_1")
    mp <- model_parameters(m, effects = "all", component = "all", centrality = "mean")
    expect_snapshot(print(mp))
  })

  test_that("print brms", {
    m <- download_model("brms_zi_3")
    mp <- model_parameters(m, effects = "all", component = "all", centrality = "mean")
    expect_snapshot(print(mp))
  })

  test_that("print brms", {
    m <- download_model("brms_ordinal_1")
    mp <- model_parameters(m, effects = "all", component = "all", centrality = "mean")
    expect_snapshot(print(mp))
  })
}

options("parameters_exponentiate" = NULL)
