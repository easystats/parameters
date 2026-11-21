# vcov standard errors

    Code
      print(model_parameters(ran))
    Output
      # Fixed Effects
      
      Parameter   | Coefficient |     SE |            95% CI |     z |  df |      p
      -----------------------------------------------------------------------------
      (Intercept) |      817.10 | 188.26 | [445.84, 1188.36] |  4.34 | 197 | < .001
      capital     |       -0.58 |   0.15 | [ -0.88,   -0.29] | -3.95 | 197 | < .001
      inv         |        2.92 |   0.30 | [  2.33,    3.51] |  9.80 | 197 | < .001
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

---

    Code
      print(model_parameters(ran, vcov = "HC1"))
    Output
      # Fixed Effects
      
      Parameter   | Coefficient |     SE |            95% CI |     z |  df |     p
      ----------------------------------------------------------------------------
      (Intercept) |      817.10 | 274.75 | [275.28, 1358.92] |  2.97 | 197 | 0.003
      capital     |       -0.58 |   0.43 | [ -1.43,    0.26] | -1.37 | 197 | 0.173
      inv         |        2.92 |   0.89 | [  1.16,    4.67] |  3.28 | 197 | 0.001
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

