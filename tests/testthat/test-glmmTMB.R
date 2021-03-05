.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest) {
  if (require("testthat") &&
    require("parameters") &&
    require("glmmTMB")) {
    data("fish")
    data("Salamanders")

    m1 <- glmmTMB(
      count ~ child + camper + (1 | persons),
      ziformula = ~ child + camper + (1 | persons),
      data = fish,
      family = truncated_poisson()
    )

    m2 <- glmmTMB(
      count ~ child + camper + (1 | persons),
      data = fish,
      family = poisson()
    )

    m3 <- glmmTMB(
      count ~ spp + mined + (1 | site),
      ziformula =  ~ spp + mined,
      family = nbinom2,
      data = Salamanders
    )

    test_that("ci", {
      expect_equal(
        ci(m1)$CI_low,
        c(0.33067, -1.32402, 0.55037, -1.66786, 1.44667, -1.64177),
        tolerance = 1e-3
      )
      expect_equal(
        ci(m1, component = "cond")$CI_low,
        c(0.33067, -1.32402, 0.55037),
        tolerance = 1e-3
      )
      expect_equal(
        ci(m1, component = "zi")$CI_low,
        c(-1.66786, 1.44667, -1.64177),
        tolerance = 1e-3
      )

      expect_equal(
        ci(m2)$CI_low,
        c(-0.47982, -1.85096, 0.76044),
        tolerance = 1e-3
      )
      expect_equal(
        ci(m2, component = "cond")$CI_low,
        c(-0.47982, -1.85096, 0.76044),
        tolerance = 1e-3
      )

      expect_null(ci(m2, component = "zi"))
    })



    test_that("se", {
      expect_equal(
        standard_error(m1)$SE,
        c(0.47559, 0.09305, 0.09346, 0.65229, 0.3099, 0.32324),
        tolerance = 1e-3
      )
      expect_equal(
        standard_error(m1, component = "cond")$SE,
        c(0.47559, 0.09305, 0.09346),
        tolerance = 1e-3
      )
      expect_equal(
        standard_error(m1, component = "zi")$SE,
        c(0.65229, 0.3099, 0.32324),
        tolerance = 1e-3
      )

      expect_equal(
        standard_error(m2)$SE,
        c(0.62127, 0.08128, 0.08915),
        tolerance = 1e-3
      )
      expect_equal(
        standard_error(m2, component = "cond")$SE,
        c(0.62127, 0.08128, 0.08915),
        tolerance = 1e-3
      )

      expect_null(standard_error(m2, component = "zi"))
    })


    test_that("p_value", {
      expect_equal(
        p_value(m1)$p,
        c(0.00792, 0, 0, 0.55054, 0, 0.00181),
        tolerance = 1e-3
      )
      expect_equal(
        p_value(m1, component = "cond")$p,
        c(0.00792, 0, 0),
        tolerance = 1e-3
      )
      expect_equal(
        p_value(m1, component = "zi")$p,
        c(0.55054, 0, 0.00181),
        tolerance = 1e-3
      )

      expect_equal(
        p_value(m2)$p,
        c(0.23497, 0, 0),
        tolerance = 1e-3
      )
      expect_equal(
        p_value(m2, component = "cond")$p,
        c(0.23497, 0, 0),
        tolerance = 1e-3
      )

      expect_null(p_value(m2, component = "zi"))
    })


    test_that("model_parameters", {
      expect_equal(
        model_parameters(m1)$Coefficient,
        c(1.2628, -1.14165, 0.73354, -0.38939, 2.05407, -1.00823),
        tolerance = 1e-3
      )
      expect_equal(
        model_parameters(m2)$Coefficient,
        c(0.73785, -1.69166, 0.93516),
        tolerance = 1e-3
      )
      expect_equal(
        model_parameters(m3)$Coefficient,
        c(
          -0.6104, -0.9637, 0.1707, -0.3871, 0.4879, 0.5895, -0.1133,
          1.4294, 0.91, 1.1614, -0.9393, 1.0424, -0.5623, -0.893, -2.5398,
          -2.563, 0.4132
        ),
        tolerance = 1e-2
      )
      expect_equal(
        model_parameters(m1)$Component,
        c(
          "conditional", "conditional", "conditional", "zero_inflated",
          "zero_inflated", "zero_inflated"
        )
      )
      expect_null(model_parameters(m2)$Component)
      expect_equal(
        model_parameters(m3)$Component,
        c(
          "conditional", "conditional", "conditional", "conditional",
          "conditional", "conditional", "conditional", "conditional", "zero_inflated",
          "zero_inflated", "zero_inflated", "zero_inflated", "zero_inflated",
          "zero_inflated", "zero_inflated", "zero_inflated", "dispersion"
        )
      )
      expect_equal(
        model_parameters(m3)$SE,
        c(
          0.4052, 0.6436, 0.2353, 0.3424, 0.2383, 0.2278, 0.2439, 0.3666,
          0.6279, 1.3346, 0.8005, 0.714, 0.7263, 0.7535, 2.1817, 0.6045,
          NA
        ),
        tolerance = 1e-2
      )
    })

    test_that("model_parameters.mixed-random", {
      params <- model_parameters(m1, effects = "random", group_level = TRUE)
      expect_equal(c(nrow(params), ncol(params)), c(8, 10))
      expect_equal(
        colnames(params),
        c(
          "Parameter", "Level", "Coefficient", "SE", "CI", "CI_low",
          "CI_high", "Component", "Effects", "Group"
        )
      )
      expect_equal(
        as.vector(params$Parameter),
        c(
          "(Intercept)", "(Intercept)", "(Intercept)", "(Intercept)",
          "(Intercept)", "(Intercept)", "(Intercept)", "(Intercept)"
        )
      )
      expect_equal(
        params$Component,
        c(
          "conditional", "conditional", "conditional", "conditional",
          "zero_inflated", "zero_inflated", "zero_inflated", "zero_inflated"
        )
      )
      expect_equal(
        params$Coefficient,
        c(-1.24, -0.3456, 0.3617, 1.2553, 1.5719, 0.3013, -0.3176, -1.5665),
        tolerance = 1e-2
      )
    })

    test_that("model_parameters.mixed-ran_pars", {
      params <- model_parameters(m1, effects = "random")
      expect_equal(c(nrow(params), ncol(params)), c(4, 9))
      expect_equal(
        colnames(params),
        c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high","Effects", "Group", "Component")
      )
      expect_equal(
        params$Parameter,
        c("SD (Intercept)", "SD (Observations)", "SD (Intercept)", "SD (Observations)")
      )
      expect_equal(
        params$Component,
        c("conditional", "conditional", "zero_inflated", "zero_inflated")
      )
      expect_equal(
        params$Coefficient,
        c(0.9312, 1, 1.17399, 1),
        tolerance = 1e-2
      )
    })

    test_that("model_parameters.mixed-all_pars", {
      params <- model_parameters(m1, effects = "all")
      expect_equal(c(nrow(params), ncol(params)), c(10, 12))
      expect_equal(
        colnames(params),
        c("Parameter", "Coefficient", "SE", "CI", "CI_low",
          "CI_high", "z", "df_error", "p", "Effects", "Group",  "Component")
      )
      expect_equal(
        params$Parameter,
        c("(Intercept)", "child", "camper1", "(Intercept)", "child",
          "camper1", "SD (Intercept)", "SD (Observations)", "SD (Intercept)",
          "SD (Observations)")
      )
      expect_equal(
        params$Component,
        c("conditional", "conditional", "conditional", "zero_inflated",
          "zero_inflated", "zero_inflated", "conditional", "conditional",
          "zero_inflated", "zero_inflated")
      )
      expect_equal(
        params$Coefficient,
        c(1.2628, -1.14165, 0.73354, -0.38939, 2.05407, -1.00823, 0.9312, 1, 1.17399, 1),
        tolerance = 1e-2
      )
    })

    test_that("model_parameters.mixed-all", {
      params <- model_parameters(m1, effects = "all", group_level = TRUE)
      expect_equal(c(nrow(params), ncol(params)), c(14, 13))
      expect_equal(
        colnames(params),
        c(
          "Parameter", "Level", "Coefficient", "SE", "CI", "CI_low",
          "CI_high", "z", "df_error", "p", "Component", "Effects",
          "Group"
        )
      )
      expect_equal(
        params$Parameter,
        c(
          "(Intercept)", "child", "camper1", "(Intercept)", "child",
          "camper1", "(Intercept)", "(Intercept)", "(Intercept)", "(Intercept)",
          "(Intercept)", "(Intercept)", "(Intercept)", "(Intercept)"
        )
      )
      expect_equal(
        params$Component,
        c(
          "conditional", "conditional", "conditional", "zero_inflated",
          "zero_inflated", "zero_inflated", "conditional", "conditional",
          "conditional", "conditional", "zero_inflated", "zero_inflated",
          "zero_inflated", "zero_inflated"
        )
      )
      expect_equal(
        params$Coefficient,
        c(
          1.2628, -1.1417, 0.7335, -0.3894, 2.0541, -1.0082, -1.24, -0.3456,
          0.3617, 1.2553, 1.5719, 0.3013, -0.3176, -1.5665
        ),
        tolerance = 1e-2
      )
    })


    m4 <- suppressWarnings(glmmTMB(
      count ~ child + camper + (1 + xb | persons),
      ziformula = ~ child + camper + (1 + zg| persons),
      data = fish,
      family = truncated_poisson()
    ))

    test_that("model_parameters.mixed-ran_pars", {
      params <- model_parameters(m4, effects = "random")
      expect_equal(c(nrow(params), ncol(params)), c(8, 9))
      expect_equal(
        colnames(params),
        c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high","Effects", "Group", "Component")
      )
      expect_equal(
        params$Parameter,
        c("SD (Intercept)", "SD (xb)", "Cor (Intercept~persons)", "SD (Observations)",
          "SD (Intercept)", "SD (zg)", "Cor (Intercept~persons)", "SD (Observations)")
      )
      expect_equal(
        params$Component,
        c("conditional", "conditional", "conditional", "conditional",
          "zero_inflated", "zero_inflated", "zero_inflated", "zero_inflated")
      )
      expect_equal(
        params$Coefficient,
        c(3.40563, 1.21316, -1, 1, 2.73583, 1.56833, 1, 1),
        tolerance = 1e-2
      )
    })

    test_that("model_parameters.mixed-all", {
      params <- model_parameters(m4, effects = "all")
      expect_equal(c(nrow(params), ncol(params)), c(14, 12))
      expect_equal(
        colnames(params),
        c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high",
          "z", "df_error", "p", "Effects", "Group", "Component")
      )
      expect_equal(
        params$Parameter,
        c("(Intercept)", "child", "camper1", "(Intercept)", "child",
          "camper1", "SD (Intercept)", "SD (xb)", "Cor (Intercept~persons)",
          "SD (Observations)", "SD (Intercept)", "SD (zg)", "Cor (Intercept~persons)",
          "SD (Observations)")
      )
      expect_equal(
        params$Component,
        c("conditional", "conditional", "conditional", "zero_inflated",
          "zero_inflated", "zero_inflated", "conditional", "conditional",
          "conditional", "conditional", "zero_inflated", "zero_inflated",
          "zero_inflated", "zero_inflated")
      )
      expect_equal(
        params$Coefficient,
        c(2.54713, -1.08747, 0.2723, 1.88964, 0.15712, -0.17007, 3.40563,
          1.21316, -1, 1, 2.73583, 1.56833, 1, 1),
        tolerance = 1e-2
      )
    })
  }
}
