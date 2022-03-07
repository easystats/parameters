.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest &&
    requiet("testthat") &&
    requiet("parameters") &&
    requiet("MASS") &&
    requiet("pscl") &&
    requiet("survial") &&
    requiet("sandwich")) {

  data(iris)
  m <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)

  test_that("robust-se lm", {
    se1 <- standard_error(m, vcov = "HC")
    se2 <- sqrt(diag(sandwich::vcovHC(m)))
    expect_equal(se1$SE, se2, ignore_attr = TRUE)
  })

  data(housing)
  m <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

  test_that("robust-se polr", {
    se1 <- standard_error(m, vcov = "vcovCL")
    se2 <- sqrt(diag(sandwich::vcovCL(m)))
    expect_equal(se1$SE, se2, ignore_attr = TRUE)

    se1 <- standard_error(m, vcov = "vcovOPG")
    se2 <- sqrt(diag(sandwich::vcovOPG(m)))
    expect_equal(se1$SE, se2, ignore_attr = TRUE)
  })

  data("bioChemists")
  m <- zeroinfl(art ~ fem + mar + kid5 + ment | kid5 + phd, data = bioChemists)

  test_that("robust-se zeroinfl", {
    se1 <- standard_error(m, vcov = "vcovCL")
    se2 <- sqrt(diag(sandwich::vcovCL(m)))
    expect_equal(se1$SE, se2, ignore_attr = TRUE)

    set.seed(123)
    se1 <- standard_error(m, vcov = "vcovBS", vcov_args = list(R = 20))
    set.seed(123)
    se2 <- sqrt(diag(sandwich::vcovBS(m, R = 20)))
    expect_equal(se1$SE, se2, ignore_attr = TRUE)

    se1 <- standard_error(m, vcov = "vcovOPG")
    se2 <- sqrt(diag(sandwich::vcovOPG(m)))
    expect_equal(se1$SE, se2, ignore_attr = TRUE)
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
    expect_equal(se1$SE, se2, ignore_attr = TRUE)
  })
}
