.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest) {
  if (require("testthat") && require("parameters") && require("rstanarm")) {

    data(mtcars)
    model <- stan_glm(
      vs ~ mpg + cyl,
      data = mtcars,
      refresh = 0,
      family = "binomial"
    )

    mp <- model_parameters(model)

    test_that("mp", {
      expect_equal(mp$Median, c(9.1346, 0.0135, -1.61359), tolerance = 1e-3)
      expect_equal(mp$Prior_Scale, c(2.5, 0.4148, 1.39984), tolerance = 1e-3)
    })


    set.seed(123)
    pbcLong$ybern <- as.integer(pbcLong$logBili >= mean(pbcLong$logBili))

    model <- stan_mvmer(
      formula = list(
        ybern ~ year + (1 | id),
        albumin ~ sex + year + (year | id)
      ),
      data = pbcLong,
      refresh = 0
    )

    mp <- model_parameters(model)

    test_that("mp2", {
      expect_equal(mp$Median, c(0.48209, 0.0304, 3.45047, 0.07639, -0.12918), tolerance = 1e-3)
      expect_equal(mp$Response, c("y1", "y1", "y2", "y2", "y2"), tolerance = 1e-3)
      expect_equal(mp$Prior_Scale, c(4.9647, 0.3465, 5.57448, 1.39362, 0.38906), tolerance = 1e-3)
    })

  }
}