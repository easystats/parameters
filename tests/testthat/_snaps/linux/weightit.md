# weightit, multinom

    Code
      print(model_parameters(fit4, exponentiate = TRUE), zap_small = TRUE)
    Output
      # Response level: 2
      
      Parameter   | Odds Ratio |   SE |       95% CI |     z |     p
      --------------------------------------------------------------
      (Intercept) |       1.00 | 0.62 | [0.30, 3.39] |  0.00 | 0.998
      treat       |       1.08 | 0.25 | [0.68, 1.71] |  0.31 | 0.755
      age         |       0.97 | 0.01 | [0.95, 0.99] | -2.38 | 0.018
      educ        |       0.98 | 0.05 | [0.89, 1.08] | -0.33 | 0.738
      
      # Response level: 3
      
      Parameter   | Odds Ratio |   SE |       95% CI |     z |      p
      ---------------------------------------------------------------
      (Intercept) |       0.05 | 0.04 | [0.01, 0.20] | -4.23 | < .001
      treat       |       1.18 | 0.29 | [0.73, 1.91] |  0.67 | 0.502 
      age         |       1.00 | 0.01 | [0.98, 1.02] | -0.01 | 0.989 
      educ        |       1.20 | 0.06 | [1.08, 1.33] |  3.51 | < .001
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

# weightit, ordinal

    Code
      print(model_parameters(fit5, exponentiate = TRUE), zap_small = TRUE)
    Output
      # Fixed Effects
      
      Parameter | Odds Ratio |   SE |       95% CI |     z |     p
      ------------------------------------------------------------
      treat     |       1.12 | 0.21 | [0.78, 1.61] |  0.60 | 0.549
      age       |       0.99 | 0.01 | [0.97, 1.01] | -0.78 | 0.436
      educ      |       1.11 | 0.04 | [1.03, 1.20] |  2.70 | 0.007
      
      # Intercept
      
      Parameter | Odds Ratio |   SE |        95% CI |    z |      p
      -------------------------------------------------------------
      1|2       |       3.28 | 1.70 | [1.19,  9.04] | 2.30 | 0.022 
      2|3       |       9.84 | 5.03 | [3.61, 26.82] | 4.47 | < .001
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

