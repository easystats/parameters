skip_on_os("mac")

skip_on_cran()

  test_that("mp", {
    skip_if_not_installed("rstanarm")
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
  skip_if_not_installed("rstanarm")
  data(pbcLong, package = "rstanarm")
  pbcLong$ybern <- as.integer(pbcLong$logBili >= mean(pbcLong$logBili))
  set.seed(123)
  model <- rstanarm::stan_mvmer(
    formula = list(
      ybern ~ year + (1 | id),
      albumin ~ sex + year + (year | id)
    ),
    data = pbcLong,
    refresh = 0,
    seed = 123
  )

  mp <- suppressWarnings(model_parameters(model, centrality = "mean"))
  s <- summary(model)
    expect_equal(mp$Mean, unname(s[c("y1|(Intercept)", "y1|year", "y2|(Intercept)", "y2|sexf", "y2|year"), 1]), tolerance = 1e-2, ignore_attr = TRUE)
    expect_identical(mp$Response, c("y1", "y1", "y2", "y2", "y2"))
    expect_equal(mp$Prior_Scale, c(4.9647, 0.3465, 5.57448, 1.39362, 0.38906), tolerance = 1e-2)
  })
