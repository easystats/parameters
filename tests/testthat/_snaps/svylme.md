# model_parameters svylme

    Code
      print(mp)
    Output
      # Fixed Effects
      
      Parameter   | Coefficient |    SE |          95% CI |     t |      p
      --------------------------------------------------------------------
      (Intercept) |      -60.98 | 34.48 | [-128.57, 6.61] | -1.77 | 0.077 
      ell         |        0.92 |  0.26 | [   0.41, 1.42] |  3.56 | < .001
      mobility    |       -0.38 |  0.24 | [  -0.85, 0.08] | -1.60 | 0.109 
      api99       |        1.10 |  0.03 | [   1.03, 1.17] | 31.44 | < .001
      
      # Random Effects
      
      Parameter             | Coefficient
      -----------------------------------
      SD (Intercept: dnum1) |        1.19
      SD (api99: dnum2)     |    1.39e-03
      SD (Residual)         |       20.00
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

