# model_parameters.fixest_multi

    Code
      print(model_parameters(mod))
    Output
      # Petal.Width ~ x
      
      Parameter | Coefficient |   SE |        95% CI | t(2) |     p
      -------------------------------------------------------------
      x         |        0.02 | 0.02 | [-0.06, 0.09] | 0.91 | 0.459
      
      # Sepal.Width ~ x
      
      Parameter | Coefficient |   SE |        95% CI | t(2) |     p
      -------------------------------------------------------------
      x         |        0.05 | 0.06 | [-0.22, 0.31] | 0.74 | 0.534
      
      # Petal.Width ~ x + Petal.Length
      
      Parameter    | Coefficient |   SE |        95% CI | t(2) |     p
      ----------------------------------------------------------------
      x            |        0.01 | 0.02 | [-0.07, 0.10] | 0.73 | 0.541
      Petal.Length |        0.23 | 0.07 | [-0.07, 0.53] | 3.34 | 0.079
      
      # Sepal.Width ~ x + Petal.Length
      
      Parameter    | Coefficient |   SE |        95% CI | t(2) |     p
      ----------------------------------------------------------------
      x            |        0.04 | 0.06 | [-0.22, 0.31] | 0.71 | 0.553
      Petal.Length |        0.30 | 0.06 | [ 0.05, 0.54] | 5.15 | 0.036
      
      # Petal.Width ~ x + Petal.Length + Sepal.Length
      
      Parameter    | Coefficient |   SE |        95% CI |  t(2) |     p
      -----------------------------------------------------------------
      x            |        0.01 | 0.02 | [-0.07, 0.10] |  0.74 | 0.539
      Petal.Length |        0.23 | 0.08 | [-0.11, 0.58] |  2.93 | 0.099
      Sepal.Length |   -4.24e-03 | 0.03 | [-0.14, 0.13] | -0.13 | 0.906
      
      # Sepal.Width ~ x + Petal.Length + Sepal.Length
      
      Parameter    | Coefficient |   SE |        95% CI |  t(2) |     p
      -----------------------------------------------------------------
      x            |        0.03 | 0.04 | [-0.15, 0.22] |  0.81 | 0.502
      Petal.Length |       -0.04 | 0.19 | [-0.85, 0.78] | -0.20 | 0.858
      Sepal.Length |        0.37 | 0.20 | [-0.49, 1.23] |  1.86 | 0.205
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(ci(mod))
    Output
            Parameter   CI      CI_low    CI_high    Response
      1             x 0.95 -0.06044147 0.09280643 Petal.Width
      2             x 0.95 -0.06835071 0.09635073 Petal.Width
      3  Petal.Length 0.95 -0.06642354 0.52593096 Petal.Width
      4             x 0.95 -0.06835856 0.09655518 Petal.Width
      5  Petal.Length 0.95 -0.10895767 0.57612928 Petal.Width
      6  Sepal.Length 0.95 -0.14108092 0.13259825 Petal.Width
      7             x 0.95 -0.22113283 0.31370225 Sepal.Width
      8             x 0.95 -0.22124463 0.30818414 Sepal.Width
      9  Petal.Length 0.95  0.04898068 0.54369328 Sepal.Width
      10            x 0.95 -0.14997065 0.21974036 Sepal.Width
      11 Petal.Length 0.95 -0.85203579 0.77537142 Sepal.Width
      12 Sepal.Length 0.95 -0.48871075 1.22952907 Sepal.Width
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
      
      Parameter    | Coefficient |   SE |        95% CI | t(2) |     p
      ----------------------------------------------------------------
      x            |        0.01 | 0.02 | [-0.07, 0.10] | 0.73 | 0.541
      Petal Length |        0.23 | 0.07 | [-0.07, 0.53] | 3.34 | 0.079
      
      # Sepal.Width response
      
      Parameter    | Coefficient |   SE |        95% CI | t(2) |     p
      ----------------------------------------------------------------
      x            |        0.04 | 0.06 | [-0.22, 0.31] | 0.71 | 0.553
      Petal Length |        0.30 | 0.06 | [ 0.05, 0.54] | 5.15 | 0.036
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(ci(mod))
    Output
           Parameter   CI      CI_low    CI_high    Response
      1            x 0.95 -0.06835071 0.09635073 Petal.Width
      2 Petal.Length 0.95 -0.06642354 0.52593096 Petal.Width
      3            x 0.95 -0.22124463 0.30818414 Sepal.Width
      4 Petal.Length 0.95  0.04898068 0.54369328 Sepal.Width

---

    Code
      print(model_parameters(mod))
    Output
      # x
      
      Parameter | Coefficient |   SE |        95% CI | t(2) |     p
      -------------------------------------------------------------
      x         |        0.02 | 0.02 | [-0.06, 0.09] | 0.91 | 0.459
      
      # x + Petal.Length
      
      Parameter    | Coefficient |   SE |        95% CI | t(2) |     p
      ----------------------------------------------------------------
      x            |        0.01 | 0.02 | [-0.07, 0.10] | 0.73 | 0.541
      Petal.Length |        0.23 | 0.07 | [-0.07, 0.53] | 3.34 | 0.079
      
      # x + Petal.Length + Sepal.Length
      
      Parameter    | Coefficient |   SE |        95% CI |  t(2) |     p
      -----------------------------------------------------------------
      x            |        0.01 | 0.02 | [-0.07, 0.10] |  0.74 | 0.539
      Petal.Length |        0.23 | 0.08 | [-0.11, 0.58] |  2.93 | 0.099
      Sepal.Length |   -4.24e-03 | 0.03 | [-0.14, 0.13] | -0.13 | 0.906
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(ci(mod))
    Output
           Parameter   CI      CI_low    CI_high                           Group
      1            x 0.95 -0.06044147 0.09280643                               x
      2            x 0.95 -0.06835071 0.09635073                x + Petal.Length
      3 Petal.Length 0.95 -0.06642354 0.52593096                x + Petal.Length
      4            x 0.95 -0.06835856 0.09655518 x + Petal.Length + Sepal.Length
      5 Petal.Length 0.95 -0.10895767 0.57612928 x + Petal.Length + Sepal.Length
      6 Sepal.Length 0.95 -0.14108092 0.13259825 x + Petal.Length + Sepal.Length

