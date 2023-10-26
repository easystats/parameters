# print model with multiple components

    Code
      print(out)
    Output
      # Fixed Effects (Count Model)
      
      Parameter   |  IRR |   SE |       95% CI |     z |      p
      ---------------------------------------------------------
      (Intercept) | 0.54 | 0.22 | [0.25, 1.20] | -1.51 | 0.132 
      spp [PR]    | 0.38 | 0.25 | [0.11, 1.35] | -1.50 | 0.134 
      spp [DM]    | 1.19 | 0.28 | [0.75, 1.88] |  0.73 | 0.468 
      spp [EC-A]  | 0.68 | 0.23 | [0.35, 1.33] | -1.13 | 0.258 
      spp [EC-L]  | 1.63 | 0.39 | [1.02, 2.60] |  2.05 | 0.041 
      spp [DES-L] | 1.80 | 0.41 | [1.15, 2.82] |  2.59 | 0.010 
      spp [DF]    | 0.89 | 0.22 | [0.55, 1.44] | -0.46 | 0.642 
      mined [no]  | 4.18 | 1.53 | [2.04, 8.57] |  3.90 | < .001
      
      # Fixed Effects (Zero-Inflation Component)
      
      Parameter   | Odds Ratio |   SE |        95% CI |     z |      p
      ----------------------------------------------------------------
      (Intercept) |       2.48 | 1.56 | [0.73,  8.51] |  1.45 | 0.147 
      spp [PR]    |       3.19 | 4.26 | [0.23, 43.70] |  0.87 | 0.384 
      spp [DM]    |       0.39 | 0.31 | [0.08,  1.88] | -1.17 | 0.241 
      spp [EC-A]  |       2.84 | 2.02 | [0.70, 11.49] |  1.46 | 0.144 
      spp [EC-L]  |       0.57 | 0.41 | [0.14,  2.37] | -0.77 | 0.439 
      spp [DES-L] |       0.41 | 0.31 | [0.09,  1.79] | -1.19 | 0.236 
      spp [DF]    |       0.08 | 0.17 | [0.00,  5.68] | -1.16 | 0.244 
      mined [no]  |       0.08 | 0.05 | [0.02,  0.25] | -4.24 | < .001
      
      # Dispersion
      
      Parameter   | Coefficient |       95% CI
      ----------------------------------------
      (Intercept) |        1.51 | [0.93, 2.46]
      
      # Random Effects Variances
      
      Parameter            | Coefficient |       95% CI
      -------------------------------------------------
      SD (Intercept: site) |        0.38 | [0.17, 0.87]
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

---

    Code
      print(out, split_component = FALSE)
    Output
      # Fixed Effects
      
      Parameter            | Coefficient |   SE |        95% CI |     z |      p | Effects |     Component
      ----------------------------------------------------------------------------------------------------
      (Intercept)          |        0.54 | 0.22 | [0.25,  1.20] | -1.51 | 0.132  |   fixed |   conditional
      spp [PR]             |        0.38 | 0.25 | [0.11,  1.35] | -1.50 | 0.134  |   fixed |   conditional
      spp [DM]             |        1.19 | 0.28 | [0.75,  1.88] |  0.73 | 0.468  |   fixed |   conditional
      spp [EC-A]           |        0.68 | 0.23 | [0.35,  1.33] | -1.13 | 0.258  |   fixed |   conditional
      spp [EC-L]           |        1.63 | 0.39 | [1.02,  2.60] |  2.05 | 0.041  |   fixed |   conditional
      spp [DES-L]          |        1.80 | 0.41 | [1.15,  2.82] |  2.59 | 0.010  |   fixed |   conditional
      spp [DF]             |        0.89 | 0.22 | [0.55,  1.44] | -0.46 | 0.642  |   fixed |   conditional
      mined [no]           |        4.18 | 1.53 | [2.04,  8.57] |  3.90 | < .001 |   fixed |   conditional
      (Intercept)          |        2.48 | 1.56 | [0.73,  8.51] |  1.45 | 0.147  |   fixed | zero_inflated
      sppPR                |        3.19 | 4.26 | [0.23, 43.70] |  0.87 | 0.384  |   fixed | zero_inflated
      sppDM                |        0.39 | 0.31 | [0.08,  1.88] | -1.17 | 0.241  |   fixed | zero_inflated
      sppEC-A              |        2.84 | 2.02 | [0.70, 11.49] |  1.46 | 0.144  |   fixed | zero_inflated
      sppEC-L              |        0.57 | 0.41 | [0.14,  2.37] | -0.77 | 0.439  |   fixed | zero_inflated
      sppDES-L             |        0.41 | 0.31 | [0.09,  1.79] | -1.19 | 0.236  |   fixed | zero_inflated
      sppDF                |        0.08 | 0.17 | [0.00,  5.68] | -1.16 | 0.244  |   fixed | zero_inflated
      minedno              |        0.08 | 0.05 | [0.02,  0.25] | -4.24 | < .001 |   fixed | zero_inflated
      (Intercept)          |        1.51 |      | [0.93,  2.46] |       |        |   fixed |    dispersion
      SD (Intercept: site) |        0.38 |      | [0.17,  0.87] |       |        |  random |   conditional
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

# adding model summaries

    Code
      print(out)
    Output
      Parameter                           | Coefficient |   SE |         95% CI | t(144) |      p
      -------------------------------------------------------------------------------------------
      (Intercept)                         |        4.21 | 0.41 | [ 3.41,  5.02] |  10.34 | < .001
      Species [versicolor]                |       -1.81 | 0.60 | [-2.99, -0.62] |  -3.02 | 0.003 
      Species [virginica]                 |       -3.15 | 0.63 | [-4.41, -1.90] |  -4.97 | < .001
      Petal Length                        |        0.54 | 0.28 | [ 0.00,  1.09] |   1.96 | 0.052 
      Species [versicolor] * Petal Length |        0.29 | 0.30 | [-0.30,  0.87] |   0.97 | 0.334 
      Species [virginica] * Petal Length  |        0.45 | 0.29 | [-0.12,  1.03] |   1.56 | 0.120 
      
      Model: Sepal.Length ~ Species * Petal.Length (150 Observations)
      Residual standard deviation: 0.336 (df = 144)
      R2: 0.840; adjusted R2: 0.835
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

# grouped parameters

    Code
      print(out, groups = list(Engine = c("cyl6", "cyl8", "vs", "hp"), Interactions = c(
        "gear4:vs", "gear5:vs"), Controls = c(2, 3, 7)))
    Output
      Parameter        | Coefficient |   SE |          95% CI | t(22) |     p
      -----------------------------------------------------------------------
      Engine           |             |      |                 |       |      
        cyl [6]        |       -2.47 | 2.21 | [ -7.05,  2.12] | -1.12 | 0.276
        cyl [8]        |        1.97 | 5.11 | [ -8.63, 12.58] |  0.39 | 0.703
        vs             |        3.18 | 3.79 | [ -4.68, 11.04] |  0.84 | 0.410
        hp             |       -0.06 | 0.02 | [ -0.11, -0.02] | -2.91 | 0.008
      Interactions     |             |      |                 |       |      
        gear [4] * vs  |       -2.90 | 4.67 | [-12.57,  6.78] | -0.62 | 0.541
        gear [5] * vs  |        2.59 | 4.54 | [ -6.82, 12.00] |  0.57 | 0.574
      Controls         |             |      |                 |       |      
        gear [4]       |        3.10 | 4.34 | [ -5.90, 12.10] |  0.71 | 0.482
        gear [5]       |        4.80 | 3.48 | [ -2.42, 12.01] |  1.38 | 0.182
        drat           |        2.70 | 2.03 | [ -1.52,  6.91] |  1.33 | 0.198
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(out, sep = "  ", groups = list(Engine = c("cyl6", "cyl8", "vs", "hp"),
      Interactions = c("gear4:vs", "gear5:vs"), Controls = c(2, 3, 7)))
    Output
      Parameter         Coefficient    SE           95% CI  t(22)      p
      ------------------------------------------------------------------
      Engine                                                            
        cyl [6]               -2.47  2.21  [ -7.05,  2.12]  -1.12  0.276
        cyl [8]                1.97  5.11  [ -8.63, 12.58]   0.39  0.703
        vs                     3.18  3.79  [ -4.68, 11.04]   0.84  0.410
        hp                    -0.06  0.02  [ -0.11, -0.02]  -2.91  0.008
      Interactions                                                      
        gear [4] * vs         -2.90  4.67  [-12.57,  6.78]  -0.62  0.541
        gear [5] * vs          2.59  4.54  [ -6.82, 12.00]   0.57  0.574
      Controls                                                          
        gear [4]               3.10  4.34  [ -5.90, 12.10]   0.71  0.482
        gear [5]               4.80  3.48  [ -2.42, 12.01]   1.38  0.182
        drat                   2.70  2.03  [ -1.52,  6.91]   1.33  0.198
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

# digits and ci_digits

    Code
      model_parameters(model, digits = 4)
    Output
      Parameter   | Coefficient |     SE |             95% CI |   t(24) |     p
      -------------------------------------------------------------------------
      (Intercept) |     18.9880 | 7.4728 | [ 3.5648, 34.4112] |  2.5409 | 0.018
      hp          |     -0.0627 | 0.0199 | [-0.1038, -0.0217] | -3.1541 | 0.004
      gear [4]    |      0.8223 | 2.2921 | [-3.9084,  5.5530] |  0.3587 | 0.723
      gear [5]    |      5.1839 | 2.6751 | [-0.3373, 10.7051] |  1.9378 | 0.064
      vs          |      1.9583 | 2.0920 | [-2.3593,  6.2759] |  0.9361 | 0.359
      cyl [6]     |     -2.3057 | 2.1418 | [-6.7262,  2.1148] | -1.0765 | 0.292
      cyl [8]     |      0.9279 | 4.3980 | [-8.1490, 10.0049] |  0.2110 | 0.835
      drat        |      2.3430 | 1.9741 | [-1.7313,  6.4172] |  1.1869 | 0.247
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      model_parameters(model, digits = 4, ci_digits = 1)
    Output
      Parameter   | Coefficient |     SE |       95% CI |   t(24) |     p
      -------------------------------------------------------------------
      (Intercept) |     18.9880 | 7.4728 | [ 3.6, 34.4] |  2.5409 | 0.018
      hp          |     -0.0627 | 0.0199 | [-0.1,  0.0] | -3.1541 | 0.004
      gear [4]    |      0.8223 | 2.2921 | [-3.9,  5.6] |  0.3587 | 0.723
      gear [5]    |      5.1839 | 2.6751 | [-0.3, 10.7] |  1.9378 | 0.064
      vs          |      1.9583 | 2.0920 | [-2.4,  6.3] |  0.9361 | 0.359
      cyl [6]     |     -2.3057 | 2.1418 | [-6.7,  2.1] | -1.0765 | 0.292
      cyl [8]     |      0.9279 | 4.3980 | [-8.1, 10.0] |  0.2110 | 0.835
      drat        |      2.3430 | 1.9741 | [-1.7,  6.4] |  1.1869 | 0.247
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(out, digits = 4)
    Output
      Parameter   | Coefficient |     SE |             95% CI |   t(24) |     p
      -------------------------------------------------------------------------
      (Intercept) |     18.9880 | 7.4728 | [ 3.5648, 34.4112] |  2.5409 | 0.018
      hp          |     -0.0627 | 0.0199 | [-0.1038, -0.0217] | -3.1541 | 0.004
      gear [4]    |      0.8223 | 2.2921 | [-3.9084,  5.5530] |  0.3587 | 0.723
      gear [5]    |      5.1839 | 2.6751 | [-0.3373, 10.7051] |  1.9378 | 0.064
      vs          |      1.9583 | 2.0920 | [-2.3593,  6.2759] |  0.9361 | 0.359
      cyl [6]     |     -2.3057 | 2.1418 | [-6.7262,  2.1148] | -1.0765 | 0.292
      cyl [8]     |      0.9279 | 4.3980 | [-8.1490, 10.0049] |  0.2110 | 0.835
      drat        |      2.3430 | 1.9741 | [-1.7313,  6.4172] |  1.1869 | 0.247
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(out, digits = 4, ci_digits = 1)
    Output
      Parameter   | Coefficient |     SE |       95% CI |   t(24) |     p
      -------------------------------------------------------------------
      (Intercept) |     18.9880 | 7.4728 | [ 3.6, 34.4] |  2.5409 | 0.018
      hp          |     -0.0627 | 0.0199 | [-0.1,  0.0] | -3.1541 | 0.004
      gear [4]    |      0.8223 | 2.2921 | [-3.9,  5.6] |  0.3587 | 0.723
      gear [5]    |      5.1839 | 2.6751 | [-0.3, 10.7] |  1.9378 | 0.064
      vs          |      1.9583 | 2.0920 | [-2.4,  6.3] |  0.9361 | 0.359
      cyl [6]     |     -2.3057 | 2.1418 | [-6.7,  2.1] | -1.0765 | 0.292
      cyl [8]     |      0.9279 | 4.3980 | [-8.1, 10.0] |  0.2110 | 0.835
      drat        |      2.3430 | 1.9741 | [-1.7,  6.4] |  1.1869 | 0.247
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

# select pattern

    Code
      print(out, groups = list(Engine = c("cyl6", "cyl8", "vs", "hp"), Interactions = c(
        "gear4:vs", "gear5:vs"), Controls = c(2, 3, 7)))
    Output
      Parameter        | Coefficient |   SE |          95% CI | t(22) |     p
      -----------------------------------------------------------------------
      Engine           |             |      |                 |       |      
        cyl [6]        |       -2.47 | 2.21 | [ -7.05,  2.12] | -1.12 | 0.276
        cyl [8]        |        1.97 | 5.11 | [ -8.63, 12.58] |  0.39 | 0.703
        vs             |        3.18 | 3.79 | [ -4.68, 11.04] |  0.84 | 0.410
        hp             |       -0.06 | 0.02 | [ -0.11, -0.02] | -2.91 | 0.008
      Interactions     |             |      |                 |       |      
        gear [4] * vs  |       -2.90 | 4.67 | [-12.57,  6.78] | -0.62 | 0.541
        gear [5] * vs  |        2.59 | 4.54 | [ -6.82, 12.00] |  0.57 | 0.574
      Controls         |             |      |                 |       |      
        gear [4]       |        3.10 | 4.34 | [ -5.90, 12.10] |  0.71 | 0.482
        gear [5]       |        4.80 | 3.48 | [ -2.42, 12.01] |  1.38 | 0.182
        drat           |        2.70 | 2.03 | [ -1.52,  6.91] |  1.33 | 0.198
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(out, select = "{coef} ({se})")
    Output
      Parameter     | Estimate (SE)
      -----------------------------
      hp            |  -0.06 (0.02)
      gear [4]      |   3.10 (4.34)
      gear [5]      |   4.80 (3.48)
      vs            |   3.18 (3.79)
      cyl [6]       |  -2.47 (2.21)
      cyl [8]       |   1.97 (5.11)
      drat          |   2.70 (2.03)
      gear [4] * vs |  -2.90 (4.67)
      gear [5] * vs |   2.59 (4.54)
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(out, select = "{coef}{stars}|[{ci}]")
    Output
      Parameter     | Estimate |            [ci]
      ------------------------------------------
      hp            |  -0.06** | [ -0.11, -0.02]
      gear [4]      |     3.10 | [ -5.90, 12.10]
      gear [5]      |     4.80 | [ -2.42, 12.01]
      vs            |     3.18 | [ -4.68, 11.04]
      cyl [6]       |    -2.47 | [ -7.05,  2.12]
      cyl [8]       |     1.97 | [ -8.63, 12.58]
      drat          |     2.70 | [ -1.52,  6.91]
      gear [4] * vs |    -2.90 | [-12.57,  6.78]
      gear [5] * vs |     2.59 | [ -6.82, 12.00]
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(out, groups = list(Engine = c("cyl6", "cyl8", "vs", "hp"), Interactions = c(
        "gear4:vs", "gear5:vs"), Controls = c(2, 3, 7)), select = "{coef}{stars}|[{ci}]")
    Output
      Parameter        | Estimate |            [ci]
      ---------------------------------------------
      Engine           |          |                
        cyl [6]        |    -2.47 | [ -7.05,  2.12]
        cyl [8]        |     1.97 | [ -8.63, 12.58]
        vs             |     3.18 | [ -4.68, 11.04]
        hp             |  -0.06** | [ -0.11, -0.02]
      Interactions     |          |                
        gear [4] * vs  |    -2.90 | [-12.57,  6.78]
        gear [5] * vs  |     2.59 | [ -6.82, 12.00]
      Controls         |          |                
        gear [4]       |     3.10 | [ -5.90, 12.10]
        gear [5]       |     4.80 | [ -2.42, 12.01]
        drat           |     2.70 | [ -1.52,  6.91]
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(out, sep = "  ", groups = list(Engine = c("cyl6", "cyl8", "vs", "hp"),
      Interactions = c("gear4:vs", "gear5:vs"), Controls = c(2, 3, 7)), select = "{coef}{stars}|[{ci}]")
    Output
      Parameter         Estimate             [ci]
      -------------------------------------------
      Engine                                     
        cyl [6]            -2.47  [ -7.05,  2.12]
        cyl [8]             1.97  [ -8.63, 12.58]
        vs                  3.18  [ -4.68, 11.04]
        hp               -0.06**  [ -0.11, -0.02]
      Interactions                               
        gear [4] * vs      -2.90  [-12.57,  6.78]
        gear [5] * vs       2.59  [ -6.82, 12.00]
      Controls                                   
        gear [4]            3.10  [ -5.90, 12.10]
        gear [5]            4.80  [ -2.42, 12.01]
        drat                2.70  [ -1.52,  6.91]
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

# message about interpretation of log-resoponse

    Code
      print(out)
    Output
      Parameter   | Coefficient |   SE |        95% CI | t(30) |      p
      -----------------------------------------------------------------
      (Intercept) |        9.29 | 2.24 | [5.67, 15.21] |  9.23 | < .001
      gear        |        1.22 | 0.08 | [1.07,  1.39] |  3.08 | 0.004 
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.
      
      This model has a log-transformed response variable, and exponentiated
        parameters are reported.
        
      A one-unit increase in the predictor is associated with multiplying the
        outcome by that predictor's coefficient.

