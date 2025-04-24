skip_if_not_installed("WRS2")
data(viagra, package = "WRS2")
data(WineTasting, package = "WRS2")
data(spider, package = "WRS2")

# model_parameters.t1way ---------------------------------------------------

test_that("model_parameters.t1way", {
  set.seed(123)
  df_b <- model_parameters(WRS2::t1way(libido ~ dose, data = viagra))
  expect_named(
    df_b,
    c(
      "F", "df", "df_error", "p", "Method", "Estimate", "CI", "CI_low",
      "CI_high", "Effectsize"
    )
  )

  set.seed(123)
  df_w <- model_parameters(WRS2::rmanova(WineTasting$Taste, WineTasting$Wine, WineTasting$Taster))
  expect_named(df_w, c("F", "df", "df_error", "p", "Method"))
})

# model_parameters.yuen ---------------------------------------------------

test_that("model_parameters.yuen", {
  set.seed(123)
  df_b <- model_parameters(WRS2::yuen(Anxiety ~ Group, data = spider))
  expect_named(
    df_b,
    c(
      "t", "df_error", "p", "Method", "Difference", "CI", "Difference_CI_low",
      "Difference_CI_high", "Estimate", "Effectsize"
    )
  )

  before <- c(190, 210, 300, 240, 280, 170, 280, 250, 240, 220)
  after <- c(210, 210, 340, 190, 260, 180, 200, 220, 230, 200)
  set.seed(123)
  df_w <- model_parameters(WRS2::yuend(before, after))

  set.seed(123)
  df_bt <- model_parameters(WRS2::yuenbt(Anxiety ~ Group, data = spider))
  expect_named(
    df_bt,
    c(
      "t", "df_error", "p", "Method", "Difference", "CI", "Difference_CI_low",
      "Difference_CI_high"
    )
  )
})

# model_parameters.mcp and robtab ---------------------------------------

test_that("model_parameters.mcp and robtab", {
  set.seed(123)
  df_b <- model_parameters(WRS2::lincon(libido ~ dose, data = viagra))
  expect_snapshot(print(df_b, table_width = Inf))

  set.seed(123)
  df_w <- model_parameters(WRS2::rmmcp(WineTasting$Taste, WineTasting$Wine, WineTasting$Taster))

  set.seed(123)
  df <- model_parameters(WRS2::discmcp(libido ~ dose, viagra, nboot = 100))
})

# model_parameters.akp.effect -----------------------------------------------

test_that("model_parameters.AKP", {
  set.seed(123)
  mod <- WRS2::akp.effect(
    formula = wt ~ am,
    data = mtcars,
    EQVAR = FALSE
  )
})


# model_parameters.onesampb ---------------------------------------------------

test_that("model_parameters.onesampb", {
  set.seed(123)
  x <- rnorm(30)

  set.seed(123)
  mod <- WRS2::onesampb(x, nboot = 100)
})
