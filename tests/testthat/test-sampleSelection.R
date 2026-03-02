skip_on_os("mac")
skip_on_cran()
skip_if_not_installed("sampleSelection")
skip_if_not_installed("mvtnorm")

test_that("model_parameters", {
  set.seed(0)
  vc <- diag(3)
  vc[lower.tri(vc)] <- c(0.9, 0.5, 0.1)
  vc[upper.tri(vc)] <- vc[lower.tri(vc)]
  eps <- mvtnorm::rmvnorm(500, c(0, 0, 0), vc)
  xs <- runif(500)
  ys <- xs + eps[, 1] > 0
  xo1 <- runif(500)
  yo1 <- xo1 + eps[, 2]
  xo2 <- runif(500)
  yo2 <- xo2 + eps[, 3]
  yo <- ifelse(ys, yo2, yo1)
  ys <- as.numeric(ys) + 1
  dat_sel <<- data.frame(ys, yo, yo1, yo2, xs, xo1, xo2)
  m1 <- sampleSelection::selection(ys ~ xs, list(yo1 ~ xo1, yo2 ~ xo2), data = dat_sel)

  data(Mroz87, package = "sampleSelection")
  Mroz87$kids <- (Mroz87$kids5 + Mroz87$kids618 > 0)
  m2 <- sampleSelection::selection(
    lfp ~ age + I(age^2) + faminc + kids + educ,
    wage ~ exper + I(exper^2) + educ + city,
    data = Mroz87
  )

  expect_snapshot(print(model_parameters(m1), zap_small = TRUE, table_width = Inf))
  expect_snapshot(print(model_parameters(m2), zap_small = TRUE, table_width = Inf))
})
