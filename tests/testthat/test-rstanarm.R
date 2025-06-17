skip_on_os("mac")
skip_on_cran()
skip_if_not_installed("rstanarm")

test_that("mp", {
  set.seed(123)
  model <- rstanarm::stan_glm(
    vs ~ mpg + cyl,
    data = mtcars,
    refresh = 0,
    family = "binomial",
    seed = 123
  )

  mp <- model_parameters(model, centrality = "mean")
  s <- summary(model)
  expect_equal(mp$Mean, unname(s[1:3, 1]), tolerance = 1e-2, ignore_attr = TRUE)
  expect_equal(mp$Prior_Scale, c(2.5, 0.4148, 1.39984), tolerance = 1e-2)
})


test_that("mp2", {
  data(pbcLong, package = "rstanarm")
  pbcLong$ybern <- as.integer(pbcLong$logBili >= mean(pbcLong$logBili))
  set.seed(123)
  invisible(capture.output({
    model <- rstanarm::stan_mvmer(
      formula = list(
        ybern ~ year + (1 | id),
        albumin ~ sex + year + (year | id)
      ),
      data = pbcLong,
      refresh = 0,
      seed = 123
    )
  }))

  mp <- suppressWarnings(model_parameters(model, centrality = "mean"))
  s <- summary(model)
  expect_equal(
    mp$Mean,
    unname(s[c("y1|(Intercept)", "y1|year", "y2|(Intercept)", "y2|sexf", "y2|year"), 1]),
    tolerance = 1e-2,
    ignore_attr = TRUE
  )
  expect_identical(mp$Response, c("y1", "y1", "y2", "y2", "y2"))
  expect_equal(mp$Prior_Scale, c(4.9647, 0.3465, 5.57448, 1.39362, 0.38906), tolerance = 1e-2)
})


test_that("mp3", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  m <- insight::download_model("stanreg_merMod_1")
  skip_if(is.null(m))
  out <- model_parameters(m, effects = "grouplevel")
  expect_identical(dim(out), c(3L, 9L))
  expect_equal(out$Median, c(0.21861, 0.17629, -0.68358), tolerance = 1e-3)
  expect_identical(out$Parameter, c("b[(Intercept) gear:3]", "b[(Intercept) gear:4]", "b[(Intercept) gear:5]"))
})
