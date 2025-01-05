skip_on_cran()
skip_if_not_installed("logitr")

test_that("model_parameters.logitr", {
  data(yogurt, package = "logitr")
  m <- logitr::logitr(
    data = yogurt,
    outcome = "choice",
    obsID = "obsID",
    pars = c("feat", "brand"),
    scalePar = "price",
    numMultiStarts = 5
  )
  params <- model_parameters(m, verbose = FALSE)
  expect_snapshot(params, variant = "windows")
})
