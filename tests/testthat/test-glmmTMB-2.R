.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest) {

  if (require("testthat") && require("parameters") && require("glmmTMB")) {

    data(Salamanders)
    model <- glmmTMB(
      count ~ spp + mined + spp * mined,
      ziformula = ~ spp + mined + spp * mined,
      family = truncated_poisson,
      data = Salamanders
    )

    mp <- model_parameters(model, component = "conditional")
    test_that("model_parameters", {
      expect_equal(mp$Coefficient, as.vector(fixef(model)[[1]]), tolerance = 1e-3)
      expect_equal(mp$Parameter, names(fixef(model)[[1]]))
    })

    mp <- model_parameters(model, component = "all")
    test_that("model_parameters", {
      expect_equal(mp$Coefficient, as.vector(unlist(fixef(model))), tolerance = 1e-3)
      expect_equal(mp$Parameter, gsub("^(cond\\.|zi\\.)", "", names(unlist(fixef(model)))))
    })

  }

}