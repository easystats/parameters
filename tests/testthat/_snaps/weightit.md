# weightit, multinom

    Code
      print(model_parameters(fit4))
    Output
      # Response level: 2
      
      Parameter   | Log-Odds |   SE |         95% CI |        z |     p
      -----------------------------------------------------------------
      (Intercept) | 1.68e-03 | 0.62 | [-1.22,  1.22] | 2.71e-03 | 0.998
      treat       |     0.07 | 0.24 | [-0.39,  0.54] |     0.31 | 0.755
      age         |    -0.03 | 0.01 | [-0.05, -0.01] |    -2.38 | 0.018
      educ        |    -0.02 | 0.05 | [-0.11,  0.08] |    -0.33 | 0.738
      
      # Response level: 3
      
      Parameter   |  Log-Odds |   SE |         95% CI |     z |      p
      ----------------------------------------------------------------
      (Intercept) |     -3.01 | 0.71 | [-4.40, -1.61] | -4.23 | < .001
      treat       |      0.16 | 0.25 | [-0.32,  0.64] |  0.67 | 0.502 
      age         | -1.70e-04 | 0.01 | [-0.02,  0.02] | -0.01 | 0.989 
      educ        |      0.18 | 0.05 | [ 0.08,  0.29] |  3.51 | < .001
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.
      
      The model has a log- or logit-link. Consider using `exponentiate =
        TRUE` to interpret coefficients as ratios.

# weightit, ordinal

    Code
      print(model_parameters(fit5))
    Output
      # Fixed Effects
      
      Parameter |  Log-Odds |       SE |        95% CI |     z |     p
      ----------------------------------------------------------------
      treat     |      0.11 |     0.19 | [-0.25, 0.48] |  0.60 | 0.549
      age       | -7.77e-03 | 9.97e-03 | [-0.03, 0.01] | -0.78 | 0.436
      educ      |      0.11 |     0.04 | [ 0.03, 0.18] |  2.70 | 0.007
      
      # Intercept
      
      Parameter | Log-Odds |   SE |       95% CI |    z |      p
      ----------------------------------------------------------
      1|2       |     1.19 | 0.52 | [0.17, 2.20] | 2.30 | 0.022 
      2|3       |     2.29 | 0.51 | [1.28, 3.29] | 4.47 | < .001
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

