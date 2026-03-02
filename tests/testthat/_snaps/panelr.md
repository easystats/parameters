# model_parameters, asym

    Code
      print(model_parameters(m4))
    Output
      Parameter   | Coefficient |   SE |         95% CI | t(3447) |      p
      --------------------------------------------------------------------
      (Intercept) |        5.08 | 1.36 | [ 2.41,  7.75] |    3.73 | < .001
      +lag(pov)   |       -0.70 | 0.73 | [-2.14,  0.74] |   -0.95 | 0.344 
      -lag(pov)   |        2.74 | 0.79 | [ 1.20,  4.29] |    3.48 | < .001
      +spouse     |       -3.00 | 1.32 | [-5.58, -0.41] |   -2.27 | 0.023 
      -spouse     |       -0.40 | 2.49 | [-5.28,  4.48] |   -0.16 | 0.872 
      wave        |       -0.81 | 0.34 | [-1.48, -0.14] |   -2.38 | 0.018 
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

