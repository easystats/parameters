# no more message about interpretation of log-resoponse

    Code
      print(out)
    Output
      Parameter   | Coefficient |   SE |        95% CI | t(30) |      p
      -----------------------------------------------------------------
      (Intercept) |        9.29 | 2.24 | [5.67, 15.21] |  9.23 | < .001
      gear        |        1.22 | 0.08 | [1.07,  1.39] |  3.08 | 0.004 
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

# no fail for mgcv-binomial

    Code
      print(out)
    Output
      # Fixed Effects
      
      Parameter   | Log-Odds |   SE |        95% CI |     z |    df |     p
      ---------------------------------------------------------------------
      (Intercept) |    -0.20 | 0.50 | [-1.18, 0.79] | -0.39 | 29.98 | 0.695
      
      # Smooth Terms
      
      Parameter         |    z |   df |     p
      ---------------------------------------
      Smooth term (mpg) | 7.24 | 1.02 | 0.007
    Message
      
      The model has a log- or logit-link. Consider using `exponentiate =
        TRUE` to interpret coefficients as ratios.

