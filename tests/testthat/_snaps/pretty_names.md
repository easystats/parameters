# pretty_labels

    Code
      print(p)
    Output
      Parameter   | Log-Odds |   SE |       95% CI |    z |      p
      ------------------------------------------------------------
      (Intercept) |     0.44 | 0.07 | [0.30, 0.58] | 6.07 | < .001
      X           |     0.26 | 0.10 | [0.06, 0.46] | 2.52 | 0.012 
      M [b]       |     0.57 | 0.11 | [0.36, 0.78] | 5.29 | < .001
      M [c]       |     0.97 | 0.11 | [0.75, 1.19] | 8.75 | < .001
      X * M [b]   |     0.89 | 0.17 | [0.56, 1.24] | 5.17 | < .001
      X * M [c]   |     1.41 | 0.21 | [1.00, 1.84] | 6.58 | < .001
    Message
      
      Uncertainty intervals (profile-likelihood) and p-values (two-tailed)
        computed using a Wald z-distribution approximation.
      
      The model has a log- or logit-link. Consider using `exponentiate =
        TRUE` to interpret coefficients as ratios.

