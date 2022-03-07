.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest &&
    requiet("testthat") &&
    requiet("parameters") &&
    requiet("MASS") &&
    requiet("pscl") &&
    requiet("survival") &&
    requiet("sandwich")) {

  # standard errors -------------------------------------

  data(iris)
  m <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)

  test_that("robust-se lm", {
    se1 <- standard_error(m, vcov = "HC")
    se2 <- sqrt(diag(sandwich::vcovHC(m)))
    expect_equal(se1$SE, se2, tolerance = 1e-4, ignore_attr = TRUE)
  })

  data(housing)
  m <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

  test_that("robust-se polr", {
    se1 <- standard_error(m, vcov = "vcovCL")
    se2 <- sqrt(diag(sandwich::vcovCL(m)))
    expect_equal(se1$SE, se2, tolerance = 1e-4, ignore_attr = TRUE)

    se1 <- standard_error(m, vcov = "vcovOPG")
    se2 <- sqrt(diag(sandwich::vcovOPG(m)))
    expect_equal(se1$SE, se2, tolerance = 1e-4, ignore_attr = TRUE)
  })

  data("bioChemists")
  m <- zeroinfl(art ~ fem + mar + kid5 + ment | kid5 + phd, data = bioChemists)

  test_that("robust-se zeroinfl", {
    se1 <- standard_error(m, vcov = "vcovCL")
    se2 <- sqrt(diag(sandwich::vcovCL(m)))
    expect_equal(se1$SE, se2, tolerance = 1e-4, ignore_attr = TRUE)

    set.seed(123)
    se1 <- standard_error(m, vcov = "vcovBS", vcov_args = list(R = 20))
    set.seed(123)
    se2 <- sqrt(diag(sandwich::vcovBS(m, R = 20)))
    expect_equal(se1$SE, se2, tolerance = 1e-4, ignore_attr = TRUE)

    se1 <- standard_error(m, vcov = "vcovOPG")
    se2 <- sqrt(diag(sandwich::vcovOPG(m)))
    expect_equal(se1$SE, se2, tolerance = 1e-4, ignore_attr = TRUE)
  })

  set.seed(123)
  m <- survreg(
    formula = Surv(futime, fustat) ~ ecog.ps + rx,
    data = ovarian,
    dist = "logistic"
  )

  test_that("robust-se survival", {
    set.seed(123)
    se1 <- standard_error(m, vcov = "vcovBS")
    set.seed(123)
    se2 <- sqrt(diag(sandwich::vcovBS(m)))
    expect_equal(se1$SE, se2, tolerance = 1e-4, ignore_attr = TRUE)

    se1 <- standard_error(m, vcov = "vcovOPG")
    se2 <- sqrt(diag(sandwich::vcovOPG(m)))
    expect_equal(se1$SE, se2, tolerance = 1e-4, ignore_attr = TRUE)
  })




  # p-values -------------------------------------

  data(iris)
  m <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)

  test_that("robust-p lm", {
    p1 <- p_value(m, vcov = "HC")
    # robust p manually
    se <- sqrt(diag(sandwich::vcovHC(m)))
    dof <- degrees_of_freedom(m, method = "wald", verbose = FALSE)
    stat <- coef(m) / se
    p2 <- 2 * pt(abs(stat), df = dof, lower.tail = FALSE)
    expect_equal(p1$p, p2, tolerance = 1e-4, ignore_attr = TRUE)
  })

  data(housing)
  m <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

  test_that("robust-p polr", {
    p1 <- p_value(m, vcov = "vcovCL")
    # robust p manually
    se <- sqrt(diag(sandwich::vcovCL(m)))
    dof <- degrees_of_freedom(m, method = "wald", verbose = FALSE)
    stat <- c(m$coefficients, m$zeta) / se
    p2 <- 2 * pt(abs(stat), df = dof, lower.tail = FALSE)
    expect_equal(p1$p, p2, tolerance = 1e-4, ignore_attr = TRUE)

    p1 <- p_value(m, vcov = "vcovOPG")
    # robust p manually
    se <- sqrt(diag(sandwich::vcovOPG(m)))
    dof <- degrees_of_freedom(m, method = "wald", verbose = FALSE)
    stat <- c(m$coefficients, m$zeta) / se
    p2 <- 2 * pt(abs(stat), df = dof, lower.tail = FALSE)
    expect_equal(p1$p, p2, tolerance = 1e-4, ignore_attr = TRUE)
  })

  data("bioChemists")
  m <- zeroinfl(art ~ fem + mar + kid5 + ment | kid5 + phd, data = bioChemists)

  test_that("robust-p zeroinfl", {
    p1 <- p_value(m, vcov = "vcovCL")
    # robust p manually
    se <- sqrt(diag(sandwich::vcovCL(m)))
    dof <- degrees_of_freedom(m, method = "wald", verbose = FALSE)
    stat <- coef(m) / se
    p2 <- 2 * pt(abs(stat), df = dof, lower.tail = FALSE)
    expect_equal(p1$p, p2, tolerance = 1e-4, ignore_attr = TRUE)

    set.seed(123)
    p1 <- p_value(m, vcov = "vcovBS", vcov_args = list(R = 20))
    set.seed(123)
    se <- sqrt(diag(sandwich::vcovBS(m, R = 20)))
    dof <- degrees_of_freedom(m, method = "wald", verbose = FALSE)
    stat <- coef(m) / se
    p2 <- 2 * pt(abs(stat), df = dof, lower.tail = FALSE)
    expect_equal(p1$p, p2, tolerance = 1e-4, ignore_attr = TRUE)

    p1 <- p_value(m, vcov = "vcovOPG")
    # robust p manually
    se <- sqrt(diag(sandwich::vcovOPG(m)))
    dof <- degrees_of_freedom(m, method = "wald", verbose = FALSE)
    stat <- coef(m) / se
    p2 <- 2 * pt(abs(stat), df = dof, lower.tail = FALSE)
    expect_equal(p1$p, p2, tolerance = 1e-4, ignore_attr = TRUE)
  })

  set.seed(123)
  m <- survreg(
    formula = Surv(futime, fustat) ~ ecog.ps + rx,
    data = ovarian,
    dist = "logistic"
  )

  test_that("robust-p survival", {
    set.seed(123)
    p1 <- p_value(m, vcov = "vcovBS")
    set.seed(123)
    se <- sqrt(diag(sandwich::vcovBS(m)))
    dof <- degrees_of_freedom(m, method = "wald", verbose = FALSE)
    stat <- coef(m) / se
    p2 <- 2 * pt(abs(stat), df = dof, lower.tail = FALSE)
    expect_equal(p1$p, p2, tolerance = 1e-4, ignore_attr = TRUE)

    p1 <- p_value(m, vcov = "vcovOPG")
    # robust p manually
    se <- sqrt(diag(sandwich::vcovOPG(m)))
    dof <- degrees_of_freedom(m, method = "wald", verbose = FALSE)
    stat <- insight::get_parameters(m)$Estimate / se
    p2 <- 2 * pt(abs(stat), df = dof, lower.tail = FALSE)
    expect_equal(p1$p, p2, tolerance = 1e-4, ignore_attr = TRUE)
  })

}

