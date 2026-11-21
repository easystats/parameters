# print digits model_parameters.lm

    Code
      params
    Output
      Parameter   | Coefficient |   SE |         95% CI | t(30) |      p
      ------------------------------------------------------------------
      (Intercept) |       37.29 | 1.88 | [33.45, 41.12] | 19.86 | < .001
      wt          |       -5.34 | 0.56 | [-6.49, -4.20] | -9.56 | < .001
      
      Model: mpg ~ wt (32 Observations)
      Sigma: 3.046 (df = 30)
      RMSE : 2.949
      R2: 0.753; adjusted R2: 0.745

---

    Code
      params
    Output
      Parameter   | Coefficient |   SE |         95% CI | t(30) |      p
      ------------------------------------------------------------------
      (Intercept) |       37.29 | 1.88 | [33.45, 41.12] | 19.86 | < .001
      wt          |       -5.34 | 0.56 | [-6.49, -4.20] | -9.56 | < .001

# model_parameters.glm - Gamma - print

    Code
      mp
    Output
      Parameter   | Prevalence Ratio |    SE |           95% CI |   t(7) |      p
      ---------------------------------------------------------------------------
      (Intercept) |           245.48 | 46.72 | [173.66, 351.67] |  28.92 | < .001
      u [log]     |             0.55 |  0.03 | [  0.49,   0.61] | -10.88 | < .001
    Message
      
      Uncertainty intervals (profile-likelihood) and p-values (two-tailed)
        computed using a Wald t-distribution approximation.

