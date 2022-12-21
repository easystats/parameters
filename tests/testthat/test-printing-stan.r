requiet("brms")
requiet("parameters")
requiet("insight")

.runThisTest <- length(strsplit(packageDescription("parameters")$Version, "\\.")[[1]]) > 3
.runThisTest <- TRUE

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
