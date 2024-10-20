test_that("pretty_names", {
  data(mtcars)
  attr(mtcars$hp, "label") <- "Gross horsepower"
  mod <- lm(mpg ~ hp + factor(cyl), mtcars)

  p <- parameters::parameters(mod, pretty_names = "labels", include_reference = TRUE)
  expect_identical(
    attr(p, "pretty_labels"),
    c(
      `(Intercept)` = "(Intercept)", hp = "Gross horsepower", `factor(cyl)4` = "cyl [4]",
      `factor(cyl)6` = "cyl [6]", `factor(cyl)8` = "cyl [8]"
    )
  )

  p <- parameters::parameters(mod, pretty_names = "labels")
  expect_identical(
    attr(p, "pretty_labels"),
    c(
      `(Intercept)` = "(Intercept)", hp = "Gross horsepower", `factor(cyl)6` = "cyl [6]",
      `factor(cyl)8` = "cyl [8]"
    )
  )

  mtcars2 <- transform(mtcars, cyl = as.factor(cyl))
  attr(mtcars2$cyl, "label") <- "Cylinders"
  model <- lm(mpg ~ wt + cyl, data = mtcars2)
  p <- model_parameters(model, pretty_names = "labels", include_reference = TRUE)
  expect_identical(
    attr(p, "pretty_labels"),
    c(
      `(Intercept)` = "(Intercept)", wt = "wt", cyl4 = "Cylinders [4]",
      cyl6 = "Cylinders [6]", cyl8 = "Cylinders [8]"
    )
  )
})

skip_if_not_installed("withr")

# make sure we have the correct interaction mark for tests
withr::with_options(
  list(parameters_interaction = "*"),
  test_that("pretty_labels", {
    set.seed(1024)
    N <- 5000
    X <- rbinom(N, 1, .5)
    M <- sample(c("a", "b", "c"), N, replace = TRUE)
    b <- runif(8, -1, 1)
    Y <- rbinom(N, 1, prob = plogis(
      b[1] + b[2] * X +
      b[3] * (M == "b") + b[4] * (M == "b") + b[5] * (M == "c") +
      b[6] * X * (M == "a") + b[7] * X + (M == "b") +
      b[8] * X * (M == "c")
    ))
    dat <- data.frame(Y, X, M, stringsAsFactors = FALSE)
    mod <- glm(Y ~ X * M, data = dat, family = binomial)

    p <- parameters(mod)
    expect_identical(
      attr(p, "pretty_labels"),
      c(
        `(Intercept)` = "(Intercept)", X = "X", Mb = "M [b]", Mc = "M [c]",
        `X:Mb` = "X * M [b]", `X:Mc` = "X * M [c]"
      )
    )
    expect_snapshot(print(p))
  })
)
