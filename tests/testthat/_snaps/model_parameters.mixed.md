# print-model_parameters

    Code
      model_parameters(model, effects = "fixed")
    Output
      Parameter   | Coefficient |   SE |         95% CI | t(558) |      p
      -------------------------------------------------------------------
      (Intercept) |       71.53 | 1.56 | [68.48, 74.59] |  45.98 | < .001
      time        |        1.09 | 0.64 | [-0.17,  2.34] |   1.70 | 0.089 
      
      # Within-Effects
      
      Parameter   | Coefficient |   SE |         95% CI | t(558) |      p
      -------------------------------------------------------------------
      phq4 within |       -3.66 | 0.41 | [-4.46, -2.86] |  -8.95 | < .001
      
      # Between-Effects
      
      Parameter    | Coefficient |   SE |         95% CI | t(558) |      p
      --------------------------------------------------------------------
      phq4 between |       -6.28 | 0.50 | [-7.27, -5.30] | -12.53 | < .001
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      model_parameters(m1, effects = "all")
    Output
      # Fixed Effects
      
      Parameter   | Coefficient |   SE |        95% CI | t(28) |      p
      -----------------------------------------------------------------
      (Intercept) |        0.65 | 0.50 | [-0.38, 1.68] |  1.29 | 0.206 
      cyl         |        0.40 | 0.08 | [ 0.25, 0.56] |  5.29 | < .001
      
      # Random Effects
      
      Parameter            | Coefficient |   SE |       95% CI
      --------------------------------------------------------
      SD (Intercept: gear) |        0.27 | 0.24 | [0.05, 1.54]
      SD (Residual)        |        0.59 | 0.08 | [0.46, 0.77]
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation. Uncertainty intervals for
        random effect variances computed using a Wald z-distribution
        approximation.

---

    Code
      model_parameters(m1, effects = "fixed", include_info = TRUE)
    Output
      # Fixed Effects
      
      Parameter   | Coefficient |   SE |        95% CI | t(28) |      p
      -----------------------------------------------------------------
      (Intercept) |        0.65 | 0.50 | [-0.38, 1.68] |  1.29 | 0.206 
      cyl         |        0.40 | 0.08 | [ 0.25, 0.56] |  5.29 | < .001
      
      Model: wt ~ cyl (32 Observations)
      Sigma: 0.594 (df = 28)
      RMSE : 0.564
      Conditional R2: 0.628; Marginal R2: 0.550
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

