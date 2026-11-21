# model_parameters.fixest_multi

    Code
      print(model_parameters(mod))
    Output
      # Petal.Width ~ x
      
      Parameter | Coefficient |   SE |        95% CI | t(146) |     p
      ---------------------------------------------------------------
      x         |        0.02 | 0.02 | [-0.02, 0.05] |   0.90 | 0.369
      
      # Sepal.Width ~ x
      
      Parameter | Coefficient |   SE |        95% CI | t(146) |     p
      ---------------------------------------------------------------
      x         |        0.05 | 0.03 | [-0.01, 0.10] |   1.56 | 0.120
      
      # Petal.Width ~ x + Petal.Length
      
      Parameter    | Coefficient |   SE |        95% CI | t(145) |      p
      -------------------------------------------------------------------
      x            |        0.01 | 0.02 | [-0.02, 0.05] |   0.89 | 0.376 
      Petal.Length |        0.23 | 0.03 | [ 0.16, 0.30] |   6.67 | < .001
      
      # Sepal.Width ~ x + Petal.Length
      
      Parameter    | Coefficient |   SE |        95% CI | t(145) |      p
      -------------------------------------------------------------------
      x            |        0.04 | 0.03 | [-0.01, 0.10] |   1.58 | 0.116 
      Petal.Length |        0.30 | 0.06 | [ 0.18, 0.42] |   4.92 | < .001
      
      # Petal.Width ~ x + Petal.Length + Sepal.Length
      
      Parameter    | Coefficient |   SE |        95% CI | t(144) |      p
      -------------------------------------------------------------------
      x            |        0.01 | 0.02 | [-0.02, 0.05] |   0.89 | 0.375 
      Petal.Length |        0.23 | 0.05 | [ 0.13, 0.34] |   4.42 | < .001
      Sepal.Length |   -4.24e-03 | 0.04 | [-0.09, 0.08] |  -0.10 | 0.924 
      
      # Sepal.Width ~ x + Petal.Length + Sepal.Length
      
      Parameter    | Coefficient |   SE |        95% CI | t(144) |      p
      -------------------------------------------------------------------
      x            |        0.03 | 0.03 | [-0.02, 0.09] |   1.37 | 0.171 
      Petal.Length |       -0.04 | 0.08 | [-0.21, 0.13] |  -0.45 | 0.651 
      Sepal.Length |        0.37 | 0.07 | [ 0.23, 0.51] |   5.23 | < .001
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(ci(mod))
    Output
            Parameter   CI      CI_low    CI_high    Response
      1             x 0.95 -0.01929491 0.05165987 Petal.Width
      2             x 0.95 -0.01715341 0.04515343 Petal.Width
      3  Petal.Length 0.95  0.16163902 0.29786840 Petal.Width
      4             x 0.95 -0.01722960 0.04542622 Petal.Width
      5  Petal.Length 0.95  0.12908917 0.33808245 Petal.Width
      6  Sepal.Length 0.95 -0.09172321 0.08324053 Petal.Width
      7             x 0.95 -0.01227870 0.10484812 Sepal.Width
      8             x 0.95 -0.01093910 0.09787861 Sepal.Width
      9  Petal.Length 0.95  0.17737594 0.41529802 Sepal.Width
      10            x 0.95 -0.01527889 0.08504860 Sepal.Width
      11 Petal.Length 0.95 -0.20565718 0.12899281 Sepal.Width
      12 Sepal.Length 0.95  0.23032902 0.51048930 Sepal.Width
                                   Group
      1                                x
      2                 x + Petal.Length
      3                 x + Petal.Length
      4  x + Petal.Length + Sepal.Length
      5  x + Petal.Length + Sepal.Length
      6  x + Petal.Length + Sepal.Length
      7                                x
      8                 x + Petal.Length
      9                 x + Petal.Length
      10 x + Petal.Length + Sepal.Length
      11 x + Petal.Length + Sepal.Length
      12 x + Petal.Length + Sepal.Length

---

    Code
      print(model_parameters(mod))
    Output
      # Petal.Width response
      
      Parameter    | Coefficient |   SE |        95% CI | t(145) |      p
      -------------------------------------------------------------------
      x            |        0.01 | 0.02 | [-0.02, 0.05] |   0.89 | 0.376 
      Petal Length |        0.23 | 0.03 | [ 0.16, 0.30] |   6.67 | < .001
      
      # Sepal.Width response
      
      Parameter    | Coefficient |   SE |        95% CI | t(145) |      p
      -------------------------------------------------------------------
      x            |        0.04 | 0.03 | [-0.01, 0.10] |   1.58 | 0.116 
      Petal Length |        0.30 | 0.06 | [ 0.18, 0.42] |   4.92 | < .001
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(ci(mod))
    Output
           Parameter   CI      CI_low    CI_high    Response
      1            x 0.95 -0.01715341 0.04515343 Petal.Width
      2 Petal.Length 0.95  0.16163902 0.29786840 Petal.Width
      3            x 0.95 -0.01093910 0.09787861 Sepal.Width
      4 Petal.Length 0.95  0.17737594 0.41529802 Sepal.Width

---

    Code
      print(model_parameters(mod))
    Output
      # x
      
      Parameter | Coefficient |   SE |        95% CI | t(146) |     p
      ---------------------------------------------------------------
      x         |        0.02 | 0.02 | [-0.02, 0.05] |   0.90 | 0.369
      
      # x + Petal.Length
      
      Parameter    | Coefficient |   SE |        95% CI | t(145) |      p
      -------------------------------------------------------------------
      x            |        0.01 | 0.02 | [-0.02, 0.05] |   0.89 | 0.376 
      Petal.Length |        0.23 | 0.03 | [ 0.16, 0.30] |   6.67 | < .001
      
      # x + Petal.Length + Sepal.Length
      
      Parameter    | Coefficient |   SE |        95% CI | t(144) |      p
      -------------------------------------------------------------------
      x            |        0.01 | 0.02 | [-0.02, 0.05] |   0.89 | 0.375 
      Petal.Length |        0.23 | 0.05 | [ 0.13, 0.34] |   4.42 | < .001
      Sepal.Length |   -4.24e-03 | 0.04 | [-0.09, 0.08] |  -0.10 | 0.924 
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(ci(mod))
    Output
           Parameter   CI      CI_low    CI_high                           Group
      1            x 0.95 -0.01929491 0.05165987                               x
      2            x 0.95 -0.01715341 0.04515343                x + Petal.Length
      3 Petal.Length 0.95  0.16163902 0.29786840                x + Petal.Length
      4            x 0.95 -0.01722960 0.04542622 x + Petal.Length + Sepal.Length
      5 Petal.Length 0.95  0.12908917 0.33808245 x + Petal.Length + Sepal.Length
      6 Sepal.Length 0.95 -0.09172321 0.08324053 x + Petal.Length + Sepal.Length

