# robust SE/VCOV, using FPC

    Code
      print(out, table_width = Inf)
    Output
      # Fixed Effects
      
      Parameter   | Coefficient |   SE |           95% CI | t(17.00) |      p
      -----------------------------------------------------------------------
      (Intercept) |      251.41 | 6.48 | [237.01, 265.80] |    38.77 | < .001
      Days        |       10.47 | 1.49 | [  7.21,  13.73] |     7.01 | < .001
      
      # Random Effects
      
      Parameter                     | Coefficient |   SE |         95% CI
      -------------------------------------------------------------------
      SD (Intercept: Subject)       |       24.74 | 5.84 | [15.58, 39.28]
      SD (Days: Subject)            |        5.92 | 1.25 | [ 3.92,  8.95]
      Cor (Intercept~Days: Subject) |        0.07 | 0.32 | [-0.51,  0.60]
      SD (Residual)                 |       25.59 | 1.51 | [22.80, 28.72]
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution with Satterthwaite approximation.
        Uncertainty intervals for random effect variances computed using a Wald
        z-distribution approximation.

