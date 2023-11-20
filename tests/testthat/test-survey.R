skip_if_not_installed("withr")
skip_if_not_installed("survey")

withr::with_environment(
  new.env(),
  test_that("model_parameters svytable", {
    # svychisq is called in model_parameters
    svychisq <- survey::svychisq

    data(api, package = "survey")
    dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
    m <- survey::svytable(~ sch.wide + stype, dclus1)
    mp <- model_parameters(m)
    expect_named(mp, c("F", "df", "df_error", "p", "Method"))
    expect_equal(mp$p, 0.02174746, tolerance = 1e-3)
  })
)

withr::with_environment(
  new.env(),
  test_that("model_parameters, bootstrap svyglm", {
    data(api, package = "survey")
    dstrat <- survey::svydesign(
      id = ~1,
      strata = ~stype,
      weights = ~pw,
      data = apistrat,
      fpc = ~fpc
    )

    model_svyglm <- suppressWarnings(survey::svyglm(sch.wide ~ ell + meals + mobility,
      design = dstrat,
      family = binomial(link = "logit")
    ))
    expect_message(parameters(model_svyglm, bootstrap = TRUE), regex = "arguments are not supported")
  })
)
