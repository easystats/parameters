# glmmTMB profiled and uniroot CI work

    Code
      print(mp1)
    Output
      # Fixed Effects
      
      Parameter   | Coefficient |   SE |           95% CI |     z |      p
      --------------------------------------------------------------------
      (Intercept) |      251.40 | 6.63 | [237.68, 265.13] | 37.91 | < .001
      Days        |       10.47 | 1.50 | [  7.36,  13.58] |  6.97 | < .001
      
      # Random Effects
      
      Parameter                     | Coefficient |         95% CI
      ------------------------------------------------------------
      SD (Intercept: Subject)       |       23.78 | [15.02, 37.66]
      SD (Days: Subject)            |        5.72 | [ 3.81,  8.59]
      Cor (Intercept~Days: Subject) |        0.08 | [-0.49,  0.59]
      SD (Residual)                 |       25.59 | [22.80, 28.72]
    Message
      
      Uncertainty intervals (profile-likelihood) and p-values (two-tailed)
        computed using a Wald z-distribution approximation. Uncertainty
        intervals for random effect variances computed using a Wald
        z-distribution approximation.

---

    Code
      print(mp2)
    Output
      # Fixed Effects
      
      Parameter   | Coefficient |   SE |           95% CI |     z |      p
      --------------------------------------------------------------------
      (Intercept) |      251.40 | 6.63 | [237.68, 265.13] | 37.91 | < .001
      Days        |       10.47 | 1.50 | [  7.36,  13.58] |  6.97 | < .001
      
      # Random Effects
      
      Parameter                     | Coefficient |         95% CI
      ------------------------------------------------------------
      SD (Intercept: Subject)       |       23.78 | [15.02, 37.66]
      SD (Days: Subject)            |        5.72 | [ 3.81,  8.59]
      Cor (Intercept~Days: Subject) |        0.08 | [-0.49,  0.59]
      SD (Residual)                 |       25.59 | [22.80, 28.72]
    Message
      
      Uncertainty intervals (profile-likelihood) and p-values (two-tailed)
        computed using a Wald z-distribution approximation. Uncertainty
        intervals for random effect variances computed using a Wald
        z-distribution approximation.

