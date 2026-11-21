# print-model_parameters

    Code
      tmp
    Output
      # Fixed Effects
      
      Parameter     | Coefficient |   SE |         95% CI | t(45) |      p
      --------------------------------------------------------------------
      (Intercept)   |        9.89 | 1.06 | [ 7.76, 12.03] |  9.35 | < .001
      rprice [log]  |       -1.28 | 0.26 | [-1.81, -0.75] | -4.85 | < .001
      rincome [log] |        0.28 | 0.24 | [-0.20,  0.76] |  1.18 | 0.246 
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.
      
      The model has a log-transformed response variable. Consider using
        `exponentiate = TRUE` to interpret coefficients as ratios.

