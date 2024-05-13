skip_on_cran()
skip_if_not_installed("coxme")
skip_if_not_installed("survival")

# modelparameters ----------------------------------

## TODO: works only interactively

# test_that("model_parameters.coxme", {
#   data(eortc, package = "coxme")
#   d <- coxme::eortc
#   d$surv <- survival::Surv(d$y, d$uncens)
#   m1 <- coxme::coxme(surv ~ trt + (1 | center), data = d)
#   out <- model_parameters(m1)
#   expect_named(
#     out,
#     c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "z", "df_error", "p")
#   )
#   expect_equal(out$Coefficient, 0.7086127, tolerance = 1e-4)
# })
