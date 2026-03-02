skip_on_cran()
skip_on_os(c("mac", "linux", "solaris"))

skip_if_not_installed("withr")
skip_if_not_installed("survey")
skip_if_not_installed("lme4")
skip_if_not_installed("svylme")

withr::with_environment(
  new.env(),
  test_that("model_parameters svylme", {
    data(api, package = "survey")
    # two-stage cluster sample
    dclus2 <- survey::svydesign(
      id = ~ dnum + snum,
      fpc = ~ fpc1 + fpc2,
      data = apiclus2
    )
    m <- svylme::svy2lme(
      api00 ~ ell + mobility + api99 + (1 + api99 | dnum),
      design = dclus2,
      method = "nested"
    )
    mp <- model_parameters(m)
    expect_snapshot(print(mp))
  })
)
