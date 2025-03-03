skip_on_cran()
skip_if_not_installed("glmmTMB")
skip_if_not_installed("lme4")

test_that("group_level_total", {
  data("fish", package = "insight")

  m1 <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ child + camper + (1 | ID),
    data = fish,
    family = poisson()
  ))

  m2 <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ child + camper + (1 + xb | persons) + (1 + zg | ID),
    ziformula = ~ child + livebait + (1 + zg + nofish | ID),
    dispformula = ~xb,
    data = fish,
    family = glmmTMB::truncated_poisson()
  ))

  m3 <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ child + camper,
    ziformula = ~ child + livebait + (1 | ID),
    data = fish,
    family = glmmTMB::truncated_poisson()
  ))

  m4 <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ child + camper + (1 + xb | persons),
    ziformula = ~ child + livebait,
    dispformula = ~xb,
    data = fish,
    family = glmmTMB::truncated_poisson()
  ))

  m5 <- suppressWarnings(lme4::glmer(
    count ~ child + camper + (1 | ID),
    data = fish,
    family = poisson()
  ))

  m6 <- suppressWarnings(lme4::lmer(
    Reaction ~ Days + (1 + Days | Subject),
    data = lme4::sleepstudy
  ))

  out <- model_parameters(m1, effects = "total")
  expect_identical(dim(out), c(4L, 6L))

  out <- model_parameters(m2, effects = "total")
  expect_identical(dim(out), c(28L, 6L))

  out <- model_parameters(m3, effects = "total")
  expect_identical(dim(out), c(4L, 6L))

  out <- model_parameters(m4, effects = "total")
  expect_identical(dim(out), c(8L, 6L))

  out <- model_parameters(m5, effects = "total")
  expect_identical(dim(out), c(4L, 5L))

  out <- model_parameters(m6, effects = "total")
  expect_identical(dim(out), c(36L, 5L))
})


test_that("group_level_total, brms", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_installed("brms")

  m1 <- insight::download_model("brms_zi_4")
  m2 <- insight::download_model("brms_sigma_3")
  skip_if(is.null(m1) || is.null(m2))

  out <- model_parameters(m1, effects = "total")
  expect_identical(dim(out), c(28L, 10L))

  out <- model_parameters(m2, effects = "total")
  expect_identical(dim(out), c(12L, 10L))
})
