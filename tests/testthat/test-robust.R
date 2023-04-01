skip_if_not_installed("sandwich")

.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest) {
  # standard errors -------------------------------------
  test_that("robust-se lm", {
    m <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)
    se1 <- standard_error(m, vcov = "HC")
    se2 <- sqrt(diag(sandwich::vcovHC(m)))
    expect_equal(se1$SE, se2, tolerance = 1e-4, ignore_attr = TRUE)
  })

  test_that("robust-se polr", {
    skip_if_not_installed("MASS")
    data(housing, package = "MASS")
    m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
    se1 <- standard_error(m, vcov = "vcovCL")
    se2 <- sqrt(diag(sandwich::vcovCL(m)))
    expect_equal(se1$SE, se2, tolerance = 1e-4, ignore_attr = TRUE)

    se1 <- standard_error(m, vcov = "vcovOPG")
    se2 <- sqrt(diag(sandwich::vcovOPG(m)))
    expect_equal(se1$SE, se2, tolerance = 1e-4, ignore_attr = TRUE)
  })


  test_that("robust-se zeroinfl", {
    skip_if_not_installed("pscl")

    skip_if_not_installed("clubSandwich")
    data("bioChemists", package = "pscl")
    m <- pscl::zeroinfl(art ~ fem + mar + kid5 + ment | kid5 + phd, data = bioChemists)
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

  test_that("robust-se ivreg", {
    skip_if_not_installed("AER")

    skip_if_not_installed("clubSandwich")
    data(CigarettesSW, package = "AER")
    CigarettesSW$rprice <- with(CigarettesSW, price / cpi)
    CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)
    CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax) / cpi)

    m <- AER::ivreg(
      log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax / cpi),
      data = CigarettesSW,
      subset = year == "1995"
    )

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


  test_that("robust-se survival", {
    skip_if_not_installed("survival")
    set.seed(123)
    m <- survival::survreg(
      formula = survival::Surv(futime, fustat) ~ ecog.ps + rx,
      data = survival::ovarian,
      dist = "logistic"
    )

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

  test_that("robust-p lm", {
    m <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)
    p1 <- p_value(m, vcov = "HC")
    # robust p manually
    se <- sqrt(diag(sandwich::vcovHC(m)))
    dof <- degrees_of_freedom(m, method = "wald", verbose = FALSE)
    stat <- coef(m) / se
    p2 <- 2 * pt(abs(stat), df = dof, lower.tail = FALSE)
    expect_equal(p1$p, p2, tolerance = 1e-4, ignore_attr = TRUE)
  })

  test_that("robust-p polr", {
    skip_if_not_installed("MASS")
    data(housing, package = "MASS")
    m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
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


  data(CigarettesSW, package = "AER")
  CigarettesSW$rprice <- with(CigarettesSW, price / cpi)
  CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)
  CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax) / cpi)

  m <- AER::ivreg(
    log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax / cpi),
    data = CigarettesSW,
    subset = year == "1995"
  )

  test_that("robust-p ivreg", {
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




  test_that("robust-p zeroinfl", {
    skip_if_not_installed("pscl")

    data("bioChemists", package = "pscl")
    m <- pscl::zeroinfl(art ~ fem + mar + kid5 + ment | kid5 + phd, data = bioChemists)

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

  test_that("robust-p survival", {
    skip_if_not_installed("survival")

    set.seed(123)
    m <- survival::survreg(
      formula = survival::Surv(futime, fustat) ~ ecog.ps + rx,
      data = survival::ovarian,
      dist = "logistic"
    )
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




  # CI -------------------------------------


  test_that("robust-ci lm", {
    data(iris)
    m <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)
    ci1 <- ci(m, vcov = "HC")
    # robust CI manually
    params <- insight::get_parameters(m)
    se <- sqrt(diag(sandwich::vcovHC(m)))
    dof <- degrees_of_freedom(m, method = "wald", verbose = FALSE)
    fac <- suppressWarnings(stats::qt(.975, df = dof))
    ci2 <- as.data.frame(cbind(
      CI_low = params$Estimate - se * fac,
      CI_high = params$Estimate + se * fac
    ))
    expect_equal(ci1$CI_low, ci2$CI_low, tolerance = 1e-4, ignore_attr = TRUE)
    expect_equal(ci1$CI_high, ci2$CI_high, tolerance = 1e-4, ignore_attr = TRUE)
  })


  data(housing)
  m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

  test_that("robust-ci polr", {
    ci1 <- ci(m, vcov = "vcovCL")
    # robust CI manually
    se <- sqrt(diag(sandwich::vcovCL(m)))
    dof <- degrees_of_freedom(m, method = "wald", verbose = FALSE)
    fac <- suppressWarnings(stats::qt(.975, df = dof))
    ci2 <- as.data.frame(cbind(
      CI_low = c(m$coefficients, m$zeta) - se * fac,
      CI_high = c(m$coefficients, m$zeta) + se * fac
    ))
    expect_equal(ci1$CI_low, ci2$CI_low, tolerance = 1e-4, ignore_attr = TRUE)
    expect_equal(ci1$CI_high, ci2$CI_high, tolerance = 1e-4, ignore_attr = TRUE)

    ci1 <- ci(m, vcov = "vcovOPG")
    # robust CI manually
    se <- sqrt(diag(sandwich::vcovOPG(m)))
    dof <- degrees_of_freedom(m, method = "wald", verbose = FALSE)
    fac <- suppressWarnings(stats::qt(.975, df = dof))
    ci2 <- as.data.frame(cbind(
      CI_low = c(m$coefficients, m$zeta) - se * fac,
      CI_high = c(m$coefficients, m$zeta) + se * fac
    ))
    expect_equal(ci1$CI_low, ci2$CI_low, tolerance = 1e-4, ignore_attr = TRUE)
    expect_equal(ci1$CI_high, ci2$CI_high, tolerance = 1e-4, ignore_attr = TRUE)
  })

  test_that("robust-ci ivreg", {
    skip_if_not_installed("AER")
    data(CigarettesSW, package = "AER")
    CigarettesSW$rprice <- with(CigarettesSW, price / cpi)
    CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)
    CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax) / cpi)

    m <- AER::ivreg(
      log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax / cpi),
      data = CigarettesSW,
      subset = year == "1995"
    )
    ci1 <- ci(m, vcov = "vcovCL")
    # robust CI manually
    se <- sqrt(diag(sandwich::vcovCL(m)))
    dof <- degrees_of_freedom(m, method = "wald", verbose = FALSE)
    fac <- suppressWarnings(stats::qt(.975, df = dof))
    ci2 <- as.data.frame(cbind(
      CI_low = coef(m) - se * fac,
      CI_high = coef(m) + se * fac
    ))
    expect_equal(ci1$CI_low, ci2$CI_low, tolerance = 1e-4, ignore_attr = TRUE)
    expect_equal(ci1$CI_high, ci2$CI_high, tolerance = 1e-4, ignore_attr = TRUE)

    ci1 <- ci(m, vcov = "vcovOPG")
    # robust CI manually
    se <- sqrt(diag(sandwich::vcovOPG(m)))
    dof <- degrees_of_freedom(m, method = "wald", verbose = FALSE)
    fac <- suppressWarnings(stats::qt(.975, df = dof))
    ci2 <- as.data.frame(cbind(
      CI_low = coef(m) - se * fac,
      CI_high = coef(m) + se * fac
    ))
    expect_equal(ci1$CI_low, ci2$CI_low, tolerance = 1e-4, ignore_attr = TRUE)
    expect_equal(ci1$CI_high, ci2$CI_high, tolerance = 1e-4, ignore_attr = TRUE)
  })

  test_that("robust-ci zeroinfl", {
    skip_if_not_installed("pscl")
    data("bioChemists", package = "pscl")
    m <- pscl::zeroinfl(art ~ fem + mar + kid5 + ment | kid5 + phd, data = bioChemists)
    ci1 <- ci(m, vcov = "vcovCL")
    # robust CI manually
    se <- sqrt(diag(sandwich::vcovCL(m)))
    dof <- degrees_of_freedom(m, method = "wald", verbose = FALSE)
    fac <- suppressWarnings(stats::qt(.975, df = dof))
    ci2 <- as.data.frame(cbind(
      CI_low = coef(m) - se * fac,
      CI_high = coef(m) + se * fac
    ))
    expect_equal(ci1$CI_low, ci2$CI_low, tolerance = 1e-4, ignore_attr = TRUE)
    expect_equal(ci1$CI_high, ci2$CI_high, tolerance = 1e-4, ignore_attr = TRUE)

    ci1 <- ci(m, vcov = "vcovOPG")
    # robust CI manually
    se <- sqrt(diag(sandwich::vcovOPG(m)))
    dof <- degrees_of_freedom(m, method = "wald", verbose = FALSE)
    fac <- suppressWarnings(stats::qt(.975, df = dof))
    ci2 <- as.data.frame(cbind(
      CI_low = coef(m) - se * fac,
      CI_high = coef(m) + se * fac
    ))
    expect_equal(ci1$CI_low, ci2$CI_low, tolerance = 1e-4, ignore_attr = TRUE)
    expect_equal(ci1$CI_high, ci2$CI_high, tolerance = 1e-4, ignore_attr = TRUE)
  })


  test_that("robust-ci survival", {
    skip_if_not_installed("survival")
    set.seed(123)
    m <- survival::survreg(
      formula = survival::Surv(futime, fustat) ~ ecog.ps + rx,
      data = survival::ovarian,
      dist = "logistic"
    )

    set.seed(123)
    ci1 <- ci(m, vcov = "vcovBS")
    # robust CI manually
    set.seed(123)
    se <- sqrt(diag(sandwich::vcovBS(m)))
    dof <- degrees_of_freedom(m, method = "wald", verbose = FALSE)
    fac <- suppressWarnings(stats::qt(.975, df = dof))
    ci2 <- as.data.frame(cbind(
      CI_low = coef(m) - se * fac,
      CI_high = coef(m) + se * fac
    ))
    expect_equal(ci1$CI_low, ci2$CI_low, tolerance = 1e-4, ignore_attr = TRUE)
    expect_equal(ci1$CI_high, ci2$CI_high, tolerance = 1e-4, ignore_attr = TRUE)

    ci1 <- ci(m, vcov = "vcovOPG")
    # robust CI manually
    se <- sqrt(diag(sandwich::vcovOPG(m)))
    dof <- degrees_of_freedom(m, method = "wald", verbose = FALSE)
    fac <- suppressWarnings(stats::qt(.975, df = dof))
    ci2 <- as.data.frame(cbind(
      CI_low = insight::get_parameters(m)$Estimate - se * fac,
      CI_high = insight::get_parameters(m)$Estimate + se * fac
    ))
    expect_equal(ci1$CI_low, ci2$CI_low, tolerance = 1e-4, ignore_attr = TRUE)
    expect_equal(ci1$CI_high, ci2$CI_high, tolerance = 1e-4, ignore_attr = TRUE)
  })




  # mixed models ----------------------

  skip_if_not_installed("clubSandwich")
  skip_if_not_installed("lme4")


  test_that("robust-se lmer", {
    set.seed(1234)
    iris$grp <- as.factor(sample(1:3, nrow(iris), replace = TRUE))

    m <- lme4::lmer(
      Sepal.Length ~ Species * Sepal.Width + Petal.Length + (1 | grp),
      data = iris
    )
    se1 <- standard_error(m, vcov = "vcovCR", vcov_args = list(type = "CR1", cluster = iris$grp))
    se2 <- sqrt(diag(clubSandwich::vcovCR(m, type = "CR1", cluster = iris$grp)))
    expect_equal(se1$SE, se2, tolerance = 1e-4, ignore_attr = TRUE)
  })

  test_that("robust-p lmer", {
    p1 <- p_value(m, vcov = "vcovCR", vcov_args = list(type = "CR1", cluster = iris$grp))
    se <- sqrt(diag(clubSandwich::vcovCR(m, type = "CR1", cluster = iris$grp)))
    dof <- degrees_of_freedom(m, method = "wald", verbose = FALSE)
    stat <- fixef(m) / se
    p2 <- 2 * pt(abs(stat), df = dof, lower.tail = FALSE)
    expect_equal(p1$p, p2, tolerance = 1e-4, ignore_attr = TRUE)
  })

  test_that("robust-ci lmer", {
    ci1 <- ci(m, vcov = "vcovCR", vcov_args = list(type = "CR1", cluster = iris$grp))
    # robust CI manually
    params <- insight::get_parameters(m)
    se <- sqrt(diag(clubSandwich::vcovCR(m, type = "CR1", cluster = iris$grp)))
    dof <- degrees_of_freedom(m, method = "wald", verbose = FALSE)
    fac <- suppressWarnings(stats::qt(.975, df = dof))
    ci2 <- as.data.frame(cbind(
      CI_low = params$Estimate - se * fac,
      CI_high = params$Estimate + se * fac
    ))
    expect_equal(ci1$CI_low, ci2$CI_low, tolerance = 1e-4, ignore_attr = TRUE)
    expect_equal(ci1$CI_high, ci2$CI_high, tolerance = 1e-4, ignore_attr = TRUE)
  })
}
