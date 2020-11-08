.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest && require("testthat") && require("parameters") && require("rstanarm")) {
  data(mtcars)
  set.seed(123)
  model <- stan_glm(
    vs ~ mpg + cyl,
    data = mtcars,
    refresh = 0,
    family = "binomial"
  )

  mp <- model_parameters(model)

  test_that("mp", {
    expect_equal(mp$Median, c(9.50343, 0.00294, -1.65762), tolerance = 1e-2)
    expect_equal(mp$Prior_Scale, c(2.5, 0.4148, 1.39984), tolerance = 1e-2)
  })


  pbcLong$ybern <- as.integer(pbcLong$logBili >= mean(pbcLong$logBili))
  set.seed(123)
  model <- stan_mvmer(
    formula = list(
      ybern ~ year + (1 | id),
      albumin ~ sex + year + (year | id)
    ),
    data = pbcLong,
    refresh = 0,
    seed = 123
  )

  mp <- suppressWarnings(model_parameters(model))

  test_that("mp2", {
    expect_equal(mp$Median, c(0.48125, 0.03037, 3.44494, 0.07625, -0.12925), tolerance = 1e-2)
    expect_equal(mp$Response, c("y1", "y1", "y2", "y2", "y2"))
    expect_equal(mp$Prior_Scale, c(4.9647, 0.3465, 5.57448, 1.39362, 0.38906), tolerance = 1e-2)
  })
}
