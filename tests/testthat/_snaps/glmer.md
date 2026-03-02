# print model_parameters

    Code
      params
    Output
      # Fixed Effects
      
      Parameter   | Log-Odds |   SE |         95% CI |     z |      p
      ---------------------------------------------------------------
      (Intercept) |    -1.36 | 0.23 | [-1.81, -0.91] | -5.98 | < .001
      period [2]  |    -0.98 | 0.30 | [-1.57, -0.38] | -3.22 | 0.001 
      period [3]  |    -1.11 | 0.32 | [-1.75, -0.48] | -3.43 | < .001
      period [4]  |    -1.56 | 0.42 | [-2.39, -0.73] | -3.67 | < .001
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.
      
      The model has a log- or logit-link. Consider using `exponentiate =
        TRUE` to interpret coefficients as ratios.

---

    Code
      mp
    Output
      # Fixed Effects
      
      Parameter   | Odds Ratio |   SE |       95% CI |     z |      p
      ---------------------------------------------------------------
      (Intercept) |       0.26 | 0.06 | [0.16, 0.40] | -5.98 | < .001
      period [2]  |       0.38 | 0.11 | [0.21, 0.68] | -3.22 | 0.001 
      period [3]  |       0.33 | 0.11 | [0.17, 0.62] | -3.43 | < .001
      period [4]  |       0.21 | 0.09 | [0.09, 0.48] | -3.67 | < .001
      
      # Random Effects
      
      Parameter            | Coefficient
      ----------------------------------
      SD (Intercept: herd) |        0.64
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

---

    Code
      mp
    Output
      # Fixed Effects
      
      Parameter   | Log-Odds |   SE |         95% CI |     z |      p
      ---------------------------------------------------------------
      (Intercept) |    -1.40 | 0.23 | [-1.85, -0.94] | -6.02 | < .001
      period [2]  |    -0.99 | 0.31 | [-1.59, -0.39] | -3.24 | 0.001 
      period [3]  |    -1.13 | 0.33 | [-1.77, -0.49] | -3.46 | < .001
      period [4]  |    -1.58 | 0.43 | [-2.42, -0.74] | -3.70 | < .001
      
      # Random Effects
      
      Parameter            | Coefficient |   SE |       95% CI
      --------------------------------------------------------
      SD (Intercept: herd) |        0.64 | 0.18 | [0.37, 1.11]
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

