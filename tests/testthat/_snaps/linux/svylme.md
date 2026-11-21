# model_parameters svylme

    Code
      print(mp)
    Output
      # Fixed Effects
      
      Parameter   | Coefficient |    SE |          95% CI |     t |      p
      --------------------------------------------------------------------
      (Intercept) |      -61.91 | 34.97 | [-130.45, 6.63] | -1.77 | 0.077 
      ell         |        0.92 |  0.26 | [   0.41, 1.43] |  3.54 | < .001
      mobility    |       -0.37 |  0.23 | [  -0.83, 0.08] | -1.60 | 0.110 
      api99       |        1.10 |  0.04 | [   1.03, 1.17] | 30.82 | < .001
      
      # Random Effects
      
      Parameter             | Coefficient
      -----------------------------------
      SD (Intercept: dnum1) |       12.71
      SD (api99: dnum2)     |        0.02
      SD (Residual)         |       19.71
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

