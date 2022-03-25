.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest &&
  requiet("testthat") &&
  requiet("parameters") &&
  requiet("MASS") &&
  requiet("pscl") &&
  requiet("survival") &&
  requiet("ivreg") &&
  requiet("AER") &&
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


  data(CigarettesSW)
  CigarettesSW$rprice <- with(CigarettesSW, price / cpi)
  CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)
  CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax) / cpi)

  m <- ivreg(
    log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax / cpi),
    data = CigarettesSW,
    subset = year == "1995"
  )

  test_that("robust-se ivreg", {
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


  data(CigarettesSW)
  CigarettesSW$rprice <- with(CigarettesSW, price / cpi)
  CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)
  CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax) / cpi)

  m <- ivreg(
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




  # CI -------------------------------------

  data(iris)
  m <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)

  test_that("robust-ci lm", {
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
  m <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

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


  data(CigarettesSW)
  CigarettesSW$rprice <- with(CigarettesSW, price / cpi)
  CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)
  CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax) / cpi)

  m <- ivreg(
    log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax / cpi),
    data = CigarettesSW,
    subset = year == "1995"
  )

  test_that("robust-ci ivreg", {
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


  data("bioChemists")
  m <- zeroinfl(art ~ fem + mar + kid5 + ment | kid5 + phd, data = bioChemists)

  test_that("robust-ci zeroinfl", {
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


  set.seed(123)
  m <- survreg(
    formula = Surv(futime, fustat) ~ ecog.ps + rx,
    data = ovarian,
    dist = "logistic"
  )

  test_that("robust-ci survival", {
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

  if (requiet("clubSandwich") && requiet("lme4")) {
    data(iris)
    set.seed(1234)
    iris$grp <- as.factor(sample(1:3, nrow(iris), replace = TRUE))

    m <- lme4::lmer(
      Sepal.Length ~ Species * Sepal.Width + Petal.Length + (1 | grp),
      data = iris
    )

    test_that("robust-se lmer", {
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
}
