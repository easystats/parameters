# model_parameters

    Code
      print(model_parameters(m1), zap_small = TRUE, table_width = Inf)
    Output
      # selection
      
      Parameter   | Coefficient |   SE |        95% CI | t(490) |      p
      ------------------------------------------------------------------
      (Intercept) |       -0.15 | 0.11 | [-0.36, 0.05] |  -1.47 | 0.141 
      xs          |        1.14 | 0.18 | [ 0.79, 1.49] |   6.39 | < .001
      
      # outcome
      
      Parameter   | Coefficient |   SE |        95% CI | t(490) |      p
      ------------------------------------------------------------------
      (Intercept) |        0.03 | 0.16 | [-0.30, 0.35] |   0.17 | 0.869 
      xo1         |        0.84 | 0.15 | [ 0.55, 1.13] |   5.61 | < .001
      (Intercept) |        0.16 | 0.16 | [-0.30, 0.35] |   0.17 | 0.869 
      xo2         |        0.84 | 0.17 | [ 0.50, 1.17] |   4.91 | < .001
      
      # Auxiliary
      
      Parameter | Coefficient |   SE |        95% CI | t(490) |      p
      ----------------------------------------------------------------
      sigma1    |        0.93 | 0.09 | [ 0.75, 1.11] |  10.12 | < .001
      rho1      |        0.89 | 0.05 | [ 0.78, 1.00] |  16.62 | < .001
      sigma2    |        0.91 | 0.04 | [ 0.82, 0.99] |  20.45 | < .001
      rho2      |        0.18 | 0.33 | [-0.47, 0.83] |   0.53 | 0.594 
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(model_parameters(m2), zap_small = TRUE, table_width = Inf)
    Output
      # selection
      
      Parameter   | Coefficient |   SE |         95% CI | t(740) |      p
      -------------------------------------------------------------------
      (Intercept) |       -4.12 | 1.40 | [-6.87, -1.37] |  -2.94 | 0.003 
      age         |        0.18 | 0.07 | [ 0.05,  0.31] |   2.79 | 0.005 
      age^2       |        0.00 | 0.00 | [ 0.00,  0.00] |  -3.12 | 0.002 
      faminc      |        0.00 | 0.00 | [ 0.00,  0.00] |   1.29 | 0.199 
      kidsTRUE    |       -0.45 | 0.13 | [-0.71, -0.20] |  -3.46 | < .001
      educ        |        0.10 | 0.02 | [ 0.05,  0.14] |   4.12 | < .001
      
      # outcome
      
      Parameter   | Coefficient |   SE |        95% CI | t(740) |      p
      ------------------------------------------------------------------
      (Intercept) |       -1.96 | 1.20 | [-4.32, 0.39] |  -1.64 | 0.102 
      exper       |        0.03 | 0.06 | [-0.09, 0.15] |   0.45 | 0.651 
      exper^2     |        0.00 | 0.00 | [ 0.00, 0.00] |  -0.06 | 0.955 
      educ        |        0.46 | 0.07 | [ 0.31, 0.60] |   6.24 | < .001
      city        |        0.45 | 0.32 | [-0.17, 1.07] |   1.41 | 0.158 
      
      # Auxiliary
      
      Parameter | Coefficient |   SE |        95% CI | t(740) |      p
      ----------------------------------------------------------------
      sigma     |        3.11 | 0.11 | [ 2.88, 3.33] |  27.31 | < .001
      rho       |       -0.13 | 0.17 | [-0.46, 0.19] |  -0.80 | 0.424 
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

