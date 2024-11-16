# model_parameters coxph-panel

    Code
      print(model_parameters(mod))
    Output
      Parameter         | Coefficient |       SE |        95% CI | Chi2(1) |      p
      -----------------------------------------------------------------------------
      ph ecog [ok]      |        0.36 |     0.20 | [-0.03, 0.75] |    3.19 | 0.074 
      ph ecog [limited] |        0.87 |     0.23 | [ 0.41, 1.33] |   13.87 | < .001
      age, linear       |        0.01 | 9.36e-03 | [-0.01, 0.03] |    1.30 | 0.253 
      age, nonlin       |             |          |               |    2.83 | 0.093 
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

