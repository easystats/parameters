if (require("testthat") && packageVersion("WRS2") >= "1.1.3" && getRversion() >= "3.6.0") {

  # model_parameters.t1way ---------------------------------------------------

  test_that("model_parameters.t1way", {
    set.seed(123)
    df_b <- model_parameters(t1way(libido ~ dose, data = viagra))

    set.seed(123)
    df_w <- model_parameters(rmanova(WineTasting$Taste, WineTasting$Wine, WineTasting$Taster))
  })

  # model_parameters.yuen ---------------------------------------------------

  test_that("model_parameters.yuen", {
    set.seed(123)
    df_b <- model_parameters(yuen(Anxiety ~ Group, data = spider))

    before <- c(190, 210, 300, 240, 280, 170, 280, 250, 240, 220)
    after <- c(210, 210, 340, 190, 260, 180, 200, 220, 230, 200)
    set.seed(123)
    df_w <- model_parameters(yuend(before, after))
  })

  # model_parameters.mcp and robtab ---------------------------------------

  test_that("model_parameters.mcp and robtab", {
    set.seed(123)
    df_b <- model_parameters(lincon(libido ~ dose, data = viagra))

    set.seed(123)
    df_w <- model_parameters(rmmcp(WineTasting$Taste, WineTasting$Wine, WineTasting$Taster))

    set.seed(123)
    df <- model_parameters(discmcp(libido ~ dose, viagra, nboot = 100))
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
    mod <- onesampb(x, nboot = 100)
  })
}
