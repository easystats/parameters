# MuMIn link functions

    Code
      print(mp)
    Output
      Parameter     | Log-Odds |   SE |         95% CI |    z |      p
      ----------------------------------------------------------------
      (Intercept)   |    -1.01 | 0.26 | [-1.51, -0.50] | 3.91 | < .001
      var cont      |    -0.42 | 0.25 | [-0.90,  0.07] | 1.70 | 0.090 
      var binom [1] |    -0.71 | 0.62 | [-1.92,  0.50] | 1.15 | 0.250 
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.
      
      The model has a log- or logit-link. Consider using `exponentiate =
        TRUE` to interpret coefficients as ratios.

# ggpredict, glmmTMB averaging

    Code
      print(mp)
    Output
      Parameter    | Coefficient |       SE |                95% CI |        z |      p
      ---------------------------------------------------------------------------------
      cond((Int))  |       -0.11 |     0.22 | [    -0.55,     0.32] |     0.52 | 0.605 
      cond(income) |       -0.01 | 3.20e-03 | [    -0.02,    -0.01] |     4.07 | < .001
      zi((Int))    |      -23.11 | 17557.33 | [-34434.85, 34388.63] | 1.32e-03 | 0.999 
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

# ggpredict, poly averaging

    Code
      print(mp)
    Output
      Parameter   | Coefficient |     SE |            95% CI |    z |      p
      ----------------------------------------------------------------------
      (Intercept) |      954.50 | 123.60 | [712.26, 1196.75] | 7.72 | < .001
      gear        |      -24.81 |  18.54 | [-61.14,   11.52] | 1.34 | 0.181 
      mpg         |      -51.21 |  11.60 | [-73.96,  -28.47] | 4.41 | < .001
      mpg^2       |        0.79 |   0.26 | [  0.29,    1.30] | 3.07 | 0.002 
      am [1]      |      -30.80 |  32.30 | [-94.11,   32.52] | 0.95 | 0.340 
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

