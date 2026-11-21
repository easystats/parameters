# model_parameters, simultaenous confidence intervals

    Code
      print(out, zap_small = TRUE)
    Output
      Parameter   | Coefficient |   SE |         95% CI | t(29) |      p
      ------------------------------------------------------------------
      (Intercept) |       37.23 | 1.60 | [33.32, 41.14] | 23.28 | < .001
      wt          |       -3.88 | 0.63 | [-5.42, -2.33] | -6.13 | < .001
      hp          |       -0.03 | 0.01 | [-0.05, -0.01] | -3.52 | 0.003 
      
      p-value adjustment method: Simultaneous confidence bands
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

