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

# include_reference, different contrasts

    Code
      print(out)
    Output
      Parameter   | Coefficient |   SE |          95% CI | t(27) |      p
      -------------------------------------------------------------------
      (Intercept) |       19.70 | 1.18 | [ 17.28, 22.11] | 16.71 | < .001
      cyl [6]     |       -6.66 | 1.63 | [-10.00, -3.31] | -4.09 | < .001
      cyl [8]     |      -10.54 | 1.96 | [-14.56, -6.52] | -5.38 | < .001
      gear [3]    |        0.00 |      |                 |       |       
      gear [4]    |        1.32 | 1.93 | [ -2.63,  5.28] |  0.69 | 0.498 
      gear [5]    |        1.50 | 1.85 | [ -2.31,  5.31] |  0.81 | 0.426 
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(out)
    Output
      Parameter   | Coefficient |   SE |          95% CI | t(27) |      p
      -------------------------------------------------------------------
      (Intercept) |       25.43 | 1.88 | [ 21.57, 29.29] | 13.52 | < .001
      cyl [4]     |        0.00 |      |                 |       |       
      cyl [6]     |       -6.66 | 1.63 | [-10.00, -3.31] | -4.09 | < .001
      cyl [8]     |      -10.54 | 1.96 | [-14.56, -6.52] | -5.38 | < .001
      gear [3]    |        0.00 |      |                 |       |       
      gear [4]    |        1.32 | 1.93 | [ -2.63,  5.28] |  0.69 | 0.498 
      gear [5]    |        1.50 | 1.85 | [ -2.31,  5.31] |  0.81 | 0.426 
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(out)
    Output
      Parameter   | Coefficient |   SE |          95% CI | t(27) |      p
      -------------------------------------------------------------------
      (Intercept) |       20.64 | 0.67 | [ 19.26, 22.01] | 30.76 | < .001
      cyl [6]     |       -6.66 | 1.63 | [-10.00, -3.31] | -4.09 | < .001
      cyl [8]     |      -10.54 | 1.96 | [-14.56, -6.52] | -5.38 | < .001
      gear [1]    |       -0.94 | 1.09 | [ -3.18,  1.30] | -0.86 | 0.396 
      gear [2]    |        0.38 | 1.11 | [ -1.90,  2.67] |  0.34 | 0.734 
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(out)
    Output
      Parameter   | Coefficient |   SE |         95% CI | t(27) |      p
      ------------------------------------------------------------------
      (Intercept) |       15.83 | 1.24 | [13.28, 18.37] | 12.75 | < .001
      cyl [8]     |        0.00 |      |                |       |       
      cyl [4]     |       10.54 | 1.96 | [ 6.52, 14.56] |  5.38 | < .001
      cyl [6]     |        3.89 | 1.88 | [ 0.03,  7.75] |  2.07 | 0.049 
      gear [1]    |       -0.94 | 1.09 | [-3.18,  1.30] | -0.86 | 0.396 
      gear [2]    |        0.38 | 1.11 | [-1.90,  2.67] |  0.34 | 0.734 
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(out)
    Output
      Parameter   | Coefficient |   SE |         95% CI | t(27) |      p
      ------------------------------------------------------------------
      (Intercept) |       14.89 | 0.92 | [13.00, 16.77] | 16.19 | < .001
      cyl [8]     |        0.00 |      |                |       |       
      cyl [4]     |       10.54 | 1.96 | [ 6.52, 14.56] |  5.38 | < .001
      cyl [6]     |        3.89 | 1.88 | [ 0.03,  7.75] |  2.07 | 0.049 
      gear [3]    |        0.00 |      |                |       |       
      gear [4]    |        1.32 | 1.93 | [-2.63,  5.28] |  0.69 | 0.498 
      gear [5]    |        1.50 | 1.85 | [-2.31,  5.31] |  0.81 | 0.426 
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

# include_reference, random effects models

    Code
      print(out)
    Output
      # Fixed Effects
      
      Parameter          | Odds Ratio |       SE |       95% CI |     z |      p
      --------------------------------------------------------------------------
      (Intercept)        |   1.81e-06 | 4.52e-06 | [0.00, 0.00] | -5.29 | < .001
      flipper len        |       1.07 |     0.01 | [1.04, 1.09] |  5.33 | < .001
      island [Biscoe]    |       1.00 |          |              |       |       
      island [Dream]     |       2.84 |     0.93 | [1.49, 5.41] |  3.18 | 0.001 
      island [Torgersen] |       2.97 |     1.22 | [1.32, 6.65] |  2.64 | 0.008 
      
      # Random Effects
      
      Parameter            | Coefficient |      95% CI
      ------------------------------------------------
      SD (Intercept: year) |    3.05e-05 | [0.00, Inf]
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

---

    Code
      print(out, include_reference = TRUE)
    Output
      # Fixed Effects
      
      Parameter          | Odds Ratio |       SE |       95% CI |     z |      p
      --------------------------------------------------------------------------
      (Intercept)        |   1.81e-06 | 4.52e-06 | [0.00, 0.00] | -5.29 | < .001
      flipper len        |       1.07 |     0.01 | [1.04, 1.09] |  5.33 | < .001
      island [Biscoe]    |       1.00 |          |              |       |       
      island [Dream]     |       2.84 |     0.93 | [1.49, 5.41] |  3.18 | 0.001 
      island [Torgersen] |       2.97 |     1.22 | [1.32, 6.65] |  2.64 | 0.008 
      
      # Random Effects
      
      Parameter            | Coefficient |      95% CI
      ------------------------------------------------
      SD (Intercept: year) |    3.05e-05 | [0.00, Inf]
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

---

    Code
      print(out)
    Output
      # Fixed Effects
      
      Parameter          | Odds Ratio |       SE |       95% CI |     z |      p
      --------------------------------------------------------------------------
      (Intercept)        |   1.81e-06 | 4.52e-06 | [0.00, 0.00] | -5.29 | < .001
      flipper len        |       1.07 |     0.01 | [1.04, 1.09] |  5.33 | < .001
      island [Biscoe]    |       1.00 |          |              |       |       
      island [Dream]     |       2.84 |     0.93 | [1.49, 5.41] |  3.18 | 0.001 
      island [Torgersen] |       2.97 |     1.22 | [1.32, 6.65] |  2.64 | 0.008 
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

