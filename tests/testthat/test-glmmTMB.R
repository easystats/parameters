.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest &&
  require("testthat") &&
  require("parameters") &&
  require("glmmTMB")) {
  data("fish")
  data("Salamanders")

  m1 <- suppressWarnings(glmmTMB(
    count ~ child + camper + (1 | persons),
    ziformula = ~ child + camper + (1 | persons),
    data = fish,
    family = truncated_poisson()
  ))

  m2 <- suppressWarnings(glmmTMB(
    count ~ child + camper + (1 | persons),
    data = fish,
    family = poisson()
  ))

  m3 <- suppressWarnings(glmmTMB(
    count ~ spp + mined + (1 | site),
    ziformula =  ~ spp + mined,
    family = nbinom2,
    data = Salamanders
  ))

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
      model_parameters(m1, effects = "fixed")$Coefficient,
      c(1.2628, -1.14165, 0.73354, -0.38939, 2.05407, -1.00823),
      tolerance = 1e-3
    )
    expect_equal(
      model_parameters(m1, effects = "all")$Coefficient,
      c(
        1.2628, -1.14165, 0.73354, -0.38939, 2.05407, -1.00823, 0.9312,
        1, 1.17399, 1
      ),
      tolerance = 1e-3
    )
    expect_equal(
      model_parameters(m2, effects = "fixed")$Coefficient,
      c(0.73785, -1.69166, 0.93516),
      tolerance = 1e-3
    )
    expect_equal(
      model_parameters(m3, effects = "fixed")$Coefficient,
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
        "zero_inflated", "zero_inflated", "conditional", "conditional",
        "zero_inflated", "zero_inflated"
      )
    )
    expect_null(model_parameters(m2, effects = "fixed")$Component)
    expect_equal(
      model_parameters(m2)$Component,
      c(
        "conditional", "conditional", "conditional", "conditional",
        "conditional"
      )
    )
    expect_equal(
      model_parameters(m3, effects = "fixed")$Component,
      c(
        "conditional", "conditional", "conditional", "conditional",
        "conditional", "conditional", "conditional", "conditional", "zero_inflated",
        "zero_inflated", "zero_inflated", "zero_inflated", "zero_inflated",
        "zero_inflated", "zero_inflated", "zero_inflated", "dispersion"
      )
    )
    expect_equal(
      model_parameters(m3, effects = "fixed")$SE,
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
      c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "Effects", "Group", "Component")
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
      c(
        "Parameter", "Coefficient", "SE", "CI", "CI_low",
        "CI_high", "z", "df_error", "p", "Effects", "Group", "Component"
      )
    )
    expect_equal(
      params$Parameter,
      c(
        "(Intercept)", "child", "camper1", "(Intercept)", "child",
        "camper1", "SD (Intercept)", "SD (Observations)", "SD (Intercept)",
        "SD (Observations)"
      )
    )
    expect_equal(
      params$Component,
      c(
        "conditional", "conditional", "conditional", "zero_inflated",
        "zero_inflated", "zero_inflated", "conditional", "conditional",
        "zero_inflated", "zero_inflated"
      )
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
    ziformula = ~ child + camper + (1 + zg | persons),
    data = fish,
    family = truncated_poisson()
  ))

  test_that("model_parameters.mixed-ran_pars", {
    params <- model_parameters(m4, effects = "random")
    expect_equal(c(nrow(params), ncol(params)), c(8, 9))
    expect_equal(
      colnames(params),
      c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "Effects", "Group", "Component")
    )
    expect_equal(
      params$Parameter,
      c(
        "SD (Intercept)", "SD (xb)", "Cor (Intercept~persons)", "SD (Observations)",
        "SD (Intercept)", "SD (zg)", "Cor (Intercept~persons)", "SD (Observations)"
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
      c(3.40563, 1.21316, -1, 1, 2.73583, 1.56833, 1, 1),
      tolerance = 1e-2
    )
  })


  # proper printing ---------------------

  test_that("print-model_parameters glmmTMB", {
    mp <- model_parameters(m4, effects = "fixed", component = "conditional")
    out <- utils::capture.output(print(mp))
    expect_equal(
      out[-5],
      c(
        "# Fixed Effects",
        "",
        "Parameter   | Log-Mean |   SE |         95% CI |      z |      p",
        "----------------------------------------------------------------",
        "child       |    -1.09 | 0.10 | [-1.28, -0.90] | -11.09 | < .001",
        "camper [1]  |     0.27 | 0.10 | [ 0.07,  0.47] |   2.70 | 0.007 "
      )
    )

    mp <- model_parameters(m4, effects = "random", component = "conditional")
    out <- utils::capture.output(print(mp))
    expect_equal(
      out,
      c(
        "# Random Effects",
        "",
        "Parameter               | Coefficient",
        "-------------------------------------",
        "SD (Intercept: persons) |        3.41",
        "SD (xb: persons)        |        1.21",
        "Cor (Intercept~persons) |       -1.00",
        "SD (Residual)           |        1.00"
      )
    )

    mp <- model_parameters(m4, effects = "fixed", component = "zero_inflated")
    out <- utils::capture.output(print(mp))
    expect_equal(
      out[-6],
      c(
        "# Fixed Effects (Zero-Inflated Model)",
        "",
        "Parameter   | Log-Mean |   SE |        95% CI |     z |     p",
        "-------------------------------------------------------------",
        "(Intercept) |     1.89 | 0.66 | [ 0.59, 3.19] |  2.85 | 0.004",
        "camper [1]  |    -0.17 | 0.39 | [-0.93, 0.59] | -0.44 | 0.660"
      )
    )

    mp <- model_parameters(m4, effects = "random", component = "zero_inflated")
    out <- utils::capture.output(print(mp))
    expect_equal(
      out,
      c(
        "# Random Effects (Zero-Inflated Model)",
        "",
        "Parameter               | Coefficient",
        "-------------------------------------",
        "SD (Intercept: persons) |        2.74",
        "SD (zg: persons)        |        1.57",
        "Cor (Intercept~persons) |        1.00",
        "SD (Residual)           |        1.00"
      )
    )

    mp <- model_parameters(m4, effects = "all", component = "conditional")
    out <- utils::capture.output(print(mp))
    expect_equal(
      out[-5],
      c(
        "# Fixed Effects",
        "",
        "Parameter   | Log-Mean |   SE |         95% CI |      z |      p",
        "----------------------------------------------------------------",
        "child       |    -1.09 | 0.10 | [-1.28, -0.90] | -11.09 | < .001",
        "camper [1]  |     0.27 | 0.10 | [ 0.07,  0.47] |   2.70 | 0.007 ",
        "",
        "# Random Effects",
        "",
        "Parameter               | Coefficient",
        "-------------------------------------",
        "SD (Intercept: persons) |        3.41",
        "SD (xb: persons)        |        1.21",
        "Cor (Intercept~persons) |       -1.00",
        "SD (Residual)           |        1.00"
      )
    )

    mp <- model_parameters(m4, effects = "all", component = "zero_inflated")
    out <- utils::capture.output(print(mp))
    expect_equal(
      out[-6],
      c(
        "# Fixed Effects (Zero-Inflated Model)",
        "",
        "Parameter   | Log-Mean |   SE |        95% CI |     z |     p",
        "-------------------------------------------------------------",
        "(Intercept) |     1.89 | 0.66 | [ 0.59, 3.19] |  2.85 | 0.004",
        "camper [1]  |    -0.17 | 0.39 | [-0.93, 0.59] | -0.44 | 0.660",
        "",
        "# Random Effects (Zero-Inflated Model)",
        "",
        "Parameter               | Coefficient",
        "-------------------------------------",
        "SD (Intercept: persons) |        2.74",
        "SD (zg: persons)        |        1.57",
        "Cor (Intercept~persons) |        1.00",
        "SD (Residual)           |        1.00"
      )
    )

    mp <- model_parameters(m4, effects = "all", component = "all")
    out <- utils::capture.output(print(mp))
    expect_equal(
      out[-c(5, 14)],
      c(
        "# Fixed Effects (Count Model)",
        "",
        "Parameter   | Log-Mean |   SE |         95% CI |      z |      p",
        "----------------------------------------------------------------",
        "child       |    -1.09 | 0.10 | [-1.28, -0.90] | -11.09 | < .001",
        "camper [1]  |     0.27 | 0.10 | [ 0.07,  0.47] |   2.70 | 0.007 ",
        "",
        "# Fixed Effects (Zero-Inflated Model)",
        "",
        "Parameter   | Log-Odds |   SE |        95% CI |     z |     p",
        "-------------------------------------------------------------",
        "(Intercept) |     1.89 | 0.66 | [ 0.59, 3.19] |  2.85 | 0.004",
        "camper [1]  |    -0.17 | 0.39 | [-0.93, 0.59] | -0.44 | 0.660",
        "",
        "# Random Effects Variances",
        "",
        "Parameter               | Coefficient",
        "-------------------------------------",
        "SD (Intercept: persons) |        3.41",
        "SD (xb: persons)        |        1.21",
        "Cor (Intercept~persons) |       -1.00",
        "SD (Residual)           |        1.00",
        "",
        "# Random Effects (Zero-Inflated Model)",
        "",
        "Parameter               | Coefficient",
        "-------------------------------------",
        "SD (Intercept: persons) |        2.74",
        "SD (zg: persons)        |        1.57",
        "Cor (Intercept~persons) |        1.00",
        "SD (Residual)           |        1.00"
      )
    )
  })


  # proper printing of digits ---------------------

  win <- tryCatch(
    {
      si <- Sys.info()
      if (!is.null(si["sysname"])) {
        si["sysname"] == "Windows" || grepl("^mingw", R.version$os)
      } else {
        FALSE
      }
    },
    error = function(e) {
      FALSE
    }
  )

  if (win) {
    test_that("print-model_parameters glmmTMB digits", {
      mp <- model_parameters(m4, effects = "all", component = "all")
      out <- utils::capture.output(print(mp, digits = 4, ci_digits = 5))
      expect_equal(
        out[-c(5, 14)],
        c(
          "# Fixed Effects (Count Model)",
          "",
          "Parameter   | Log-Mean |     SE |               95% CI |        z |      p",
          "--------------------------------------------------------------------------",
          "child       |  -1.0875 | 0.0981 | [-1.27966, -0.89529] | -11.0903 | < .001",
          "camper [1]  |   0.2723 | 0.1009 | [ 0.07462,  0.46998] |   2.6997 | 0.007 ",
          "",
          "# Fixed Effects (Zero-Inflated Model)",
          "",
          "Parameter   | Log-Odds |     SE |              95% CI |       z |     p",
          "-----------------------------------------------------------------------",
          "(Intercept) |   1.8896 | 0.6642 | [ 0.58790, 3.19137] |  2.8451 | 0.004",
          "camper [1]  |  -0.1701 | 0.3868 | [-0.92810, 0.58796] | -0.4397 | 0.660",
          "",
          "# Random Effects Variances",
          "",
          "Parameter               | Coefficient",
          "-------------------------------------",
          "SD (Intercept: persons) |      3.4056",
          "SD (xb: persons)        |      1.2132",
          "Cor (Intercept~persons) |     -1.0000",
          "SD (Residual)           |      1.0000",
          "",
          "# Random Effects (Zero-Inflated Model)",
          "",
          "Parameter               | Coefficient",
          "-------------------------------------",
          "SD (Intercept: persons) |      2.7358",
          "SD (zg: persons)        |      1.5683",
          "Cor (Intercept~persons) |      1.0000",
          "SD (Residual)           |      1.0000"
        )
      )

      mp <- model_parameters(m4, effects = "all", component = "all", digits = 4, ci_digits = 5)
      out <- utils::capture.output(print(mp))
      expect_equal(
        out[-c(5, 14)],
        c(
          "# Fixed Effects (Count Model)",
          "",
          "Parameter   | Log-Mean |     SE |               95% CI |        z |      p",
          "--------------------------------------------------------------------------",
          "child       |  -1.0875 | 0.0981 | [-1.27966, -0.89529] | -11.0903 | < .001",
          "camper [1]  |   0.2723 | 0.1009 | [ 0.07462,  0.46998] |   2.6997 | 0.007 ",
          "",
          "# Fixed Effects (Zero-Inflated Model)",
          "",
          "Parameter   | Log-Odds |     SE |              95% CI |       z |     p",
          "-----------------------------------------------------------------------",
          "(Intercept) |   1.8896 | 0.6642 | [ 0.58790, 3.19137] |  2.8451 | 0.004",
          "camper [1]  |  -0.1701 | 0.3868 | [-0.92810, 0.58796] | -0.4397 | 0.660",
          "",
          "# Random Effects Variances",
          "",
          "Parameter               | Coefficient",
          "-------------------------------------",
          "SD (Intercept: persons) |      3.4056",
          "SD (xb: persons)        |      1.2132",
          "Cor (Intercept~persons) |     -1.0000",
          "SD (Residual)           |      1.0000",
          "",
          "# Random Effects (Zero-Inflated Model)",
          "",
          "Parameter               | Coefficient",
          "-------------------------------------",
          "SD (Intercept: persons) |      2.7358",
          "SD (zg: persons)        |      1.5683",
          "Cor (Intercept~persons) |      1.0000",
          "SD (Residual)           |      1.0000"
        )
      )
    })
  }



  test_that("model_parameters.mixed-all", {
    params <- model_parameters(m4, effects = "all")
    expect_equal(c(nrow(params), ncol(params)), c(14, 12))
    expect_equal(
      colnames(params),
      c(
        "Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high",
        "z", "df_error", "p", "Effects", "Group", "Component"
      )
    )
    expect_equal(
      params$Parameter,
      c(
        "(Intercept)", "child", "camper1", "(Intercept)", "child",
        "camper1", "SD (Intercept)", "SD (xb)", "Cor (Intercept~persons)",
        "SD (Observations)", "SD (Intercept)", "SD (zg)", "Cor (Intercept~persons)",
        "SD (Observations)"
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
        2.54713, -1.08747, 0.2723, 1.88964, 0.15712, -0.17007, 3.40563,
        1.21316, -1, 1, 2.73583, 1.56833, 1, 1
      ),
      tolerance = 1e-2
    )
  })

  test_that("print-model_parameters", {
    out <- utils::capture.output(print(model_parameters(m1, effects = "fixed")))
    expect_equal(
      out,
      c(
        "# Fixed Effects",
        "",
        "Parameter   | Log-Mean |   SE |         95% CI |      z |      p",
        "----------------------------------------------------------------",
        "(Intercept) |     1.26 | 0.48 | [ 0.33,  2.19] |   2.66 | 0.008 ",
        "child       |    -1.14 | 0.09 | [-1.32, -0.96] | -12.27 | < .001",
        "camper [1]  |     0.73 | 0.09 | [ 0.55,  0.92] |   7.85 | < .001",
        "",
        "# Zero-Inflated",
        "",
        "Parameter   | Log-Odds |   SE |         95% CI |     z |      p",
        "---------------------------------------------------------------",
        "(Intercept) |    -0.39 | 0.65 | [-1.67,  0.89] | -0.60 | 0.551 ",
        "child       |     2.05 | 0.31 | [ 1.45,  2.66] |  6.63 | < .001",
        "camper [1]  |    -1.01 | 0.32 | [-1.64, -0.37] | -3.12 | 0.002 "
      )
    )

    out <- utils::capture.output(print(model_parameters(m1, effects = "fixed", exponentiate = TRUE)))
    expect_equal(
      out,
      c(
        "# Fixed Effects",
        "",
        "Parameter   |  IRR |   SE |       95% CI |      z |      p",
        "----------------------------------------------------------",
        "(Intercept) | 3.54 | 1.68 | [1.39, 8.98] |   2.66 | 0.008 ",
        "child       | 0.32 | 0.03 | [0.27, 0.38] | -12.27 | < .001",
        "camper [1]  | 2.08 | 0.19 | [1.73, 2.50] |   7.85 | < .001",
        "",
        "# Zero-Inflated",
        "",
        "Parameter   | Odds Ratio |   SE |        95% CI |     z |      p",
        "----------------------------------------------------------------",
        "(Intercept) |       0.68 | 0.44 | [0.19,  2.43] | -0.60 | 0.551 ",
        "child       |       7.80 | 2.42 | [4.25, 14.32] |  6.63 | < .001",
        "camper [1]  |       0.36 | 0.12 | [0.19,  0.69] | -3.12 | 0.002 "
      )
    )

    out <- utils::capture.output(print(model_parameters(m1, effects = "all")))
    expect_equal(
      out,
      c(
        "# Fixed Effects (Count Model)",
        "",
        "Parameter   | Log-Mean |   SE |         95% CI |      z |      p",
        "----------------------------------------------------------------",
        "(Intercept) |     1.26 | 0.48 | [ 0.33,  2.19] |   2.66 | 0.008 ",
        "child       |    -1.14 | 0.09 | [-1.32, -0.96] | -12.27 | < .001",
        "camper [1]  |     0.73 | 0.09 | [ 0.55,  0.92] |   7.85 | < .001",
        "",
        "# Fixed Effects (Zero-Inflated Model)",
        "",
        "Parameter   | Log-Odds |   SE |         95% CI |     z |      p",
        "---------------------------------------------------------------",
        "(Intercept) |    -0.39 | 0.65 | [-1.67,  0.89] | -0.60 | 0.551 ",
        "child       |     2.05 | 0.31 | [ 1.45,  2.66] |  6.63 | < .001",
        "camper [1]  |    -1.01 | 0.32 | [-1.64, -0.37] | -3.12 | 0.002 ",
        "",
        "# Random Effects Variances",
        "",
        "Parameter               | Coefficient",
        "-------------------------------------",
        "SD (Intercept: persons) |        0.93",
        "SD (Residual)           |        1.00",
        "",
        "# Random Effects (Zero-Inflated Model)",
        "",
        "Parameter               | Coefficient",
        "-------------------------------------",
        "SD (Intercept: persons) |        1.17",
        "SD (Residual)           |        1.00"
      )
    )
  })
}
