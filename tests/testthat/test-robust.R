.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest &&
    requiet("testthat") &&
    requiet("parameters") &&
    requiet("MASS") &&
    requiet("pscl") &&
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
    # MASS::polr doesn't work with sandwich?
  })

  data("bioChemists")
  m <- zeroinfl(art ~ fem + mar + kid5 + ment | kid5 + phd, data = bioChemists)

  test_that("robust-se zeroinfl", {
    # pscl::zeroinfl doesn't work with sandwich?
  })
}
