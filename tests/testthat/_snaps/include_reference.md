# include_reference, on-the-fly factors

    Code
      print(out1)
    Output
      Parameter   | Coefficient |   SE |         95% CI | t(27) |      p
      ------------------------------------------------------------------
      (Intercept) |       27.48 | 1.97 | [23.43, 31.53] | 13.92 | < .001
      gear [3]    |        0.00 |      |                |       |       
      gear [4]    |        0.08 | 1.83 | [-3.68,  3.83] |  0.04 | 0.967 
      gear [5]    |        2.39 | 2.38 | [-2.50,  7.29] |  1.00 | 0.324 
      am [0]      |        0.00 |      |                |       |       
      am [1]      |        4.14 | 1.81 | [ 0.42,  7.85] |  2.29 | 0.030 
      hp          |       -0.06 | 0.01 | [-0.09, -0.04] | -6.24 | < .001
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(out2)
    Output
      Parameter   | Coefficient |   SE |         95% CI | t(27) |      p
      ------------------------------------------------------------------
      (Intercept) |       27.48 | 1.97 | [23.43, 31.53] | 13.92 | < .001
      gear [3]    |        0.00 |      |                |       |       
      gear [4]    |        0.08 | 1.83 | [-3.68,  3.83] |  0.04 | 0.967 
      gear [5]    |        2.39 | 2.38 | [-2.50,  7.29] |  1.00 | 0.324 
      am [0]      |        0.00 |      |                |       |       
      am [1]      |        4.14 | 1.81 | [ 0.42,  7.85] |  2.29 | 0.030 
      hp          |       -0.06 | 0.01 | [-0.09, -0.04] | -6.24 | < .001
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print_md(out, engine = "tt")
    Output
      
      +--------------+----------------------+----------------------+
      | Parameter    | m1                   | m2                   |
      +==============+======================+======================+
      | (Intercept)  | 27.48 (23.43, 31.53) | 27.48 (23.43, 31.53) |
      +--------------+----------------------+----------------------+
      | gear (3)     | 0.00                 | 0.00                 |
      +--------------+----------------------+----------------------+
      | gear (4)     | 0.08 (-3.68, 3.83)   | 0.08 (-3.68, 3.83)   |
      +--------------+----------------------+----------------------+
      | gear (5)     | 2.39 (-2.50, 7.29)   | 2.39 (-2.50, 7.29)   |
      +--------------+----------------------+----------------------+
      | am (0)       | 0.00                 | 0.00                 |
      +--------------+----------------------+----------------------+
      | am (1)       | 4.14 (0.42, 7.85)    | 4.14 (0.42, 7.85)    |
      +--------------+----------------------+----------------------+
      | hp           | -0.06 (-0.09, -0.04) | -0.06 (-0.09, -0.04) |
      +--------------+----------------------+----------------------+
      |              |                      |                      |
      +--------------+----------------------+----------------------+
      | Observations | 32                   | 32                   |
      +--------------+----------------------+----------------------+ 
