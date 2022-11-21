# print model with multiple components

    Code
      print(out)
    Output
      # Fixed Effects (Count Model)
      
      Parameter   | Log-Mean |   SE |        95% CI |     z |      p
      --------------------------------------------------------------
      (Intercept) |    -0.61 | 0.41 | [-1.40, 0.18] | -1.51 | 0.132 
      spp [PR]    |    -0.96 | 0.64 | [-2.23, 0.30] | -1.50 | 0.134 
      spp [DM]    |     0.17 | 0.24 | [-0.29, 0.63] |  0.73 | 0.468 
      spp [EC-A]  |    -0.39 | 0.34 | [-1.06, 0.28] | -1.13 | 0.258 
      spp [EC-L]  |     0.49 | 0.24 | [ 0.02, 0.96] |  2.05 | 0.041 
      spp [DES-L] |     0.59 | 0.23 | [ 0.14, 1.04] |  2.59 | 0.010 
      spp [DF]    |    -0.11 | 0.24 | [-0.59, 0.36] | -0.46 | 0.642 
      mined [no]  |     1.43 | 0.37 | [ 0.71, 2.15] |  3.90 | < .001
      
      # Fixed Effects (Zero-Inflation Component)
      
      Parameter   | Log-Odds |   SE |         95% CI |     z |      p
      ---------------------------------------------------------------
      (Intercept) |     0.91 | 0.63 | [-0.32,  2.14] |  1.45 | 0.147 
      spp [PR]    |     1.16 | 1.33 | [-1.45,  3.78] |  0.87 | 0.384 
      spp [DM]    |    -0.94 | 0.80 | [-2.51,  0.63] | -1.17 | 0.241 
      spp [EC-A]  |     1.04 | 0.71 | [-0.36,  2.44] |  1.46 | 0.144 
      spp [EC-L]  |    -0.56 | 0.73 | [-1.99,  0.86] | -0.77 | 0.439 
      spp [DES-L] |    -0.89 | 0.75 | [-2.37,  0.58] | -1.19 | 0.236 
      spp [DF]    |    -2.54 | 2.18 | [-6.82,  1.74] | -1.16 | 0.244 
      mined [no]  |    -2.56 | 0.60 | [-3.75, -1.38] | -4.24 | < .001
      
      # Dispersion
      
      Parameter   | Coefficient |       95% CI
      ----------------------------------------
      (Intercept) |        1.51 | [0.93, 2.46]
      
      # Random Effects Variances
      
      Parameter            | Coefficient |       95% CI
      -------------------------------------------------
      SD (Intercept: site) |        0.38 | [0.17, 0.87]
    Message <simpleMessage>
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.
      
      The model has a log- or logit-link. Consider using `exponentiate = TRUE`
        to interpret coefficients as ratios.

---

    Code
      print(out, split_component = FALSE)
    Output
      # Fixed Effects
      
      Parameter            | Coefficient |   SE |         95% CI |     z |      p | Effects |     Component
      -----------------------------------------------------------------------------------------------------
      (Intercept)          |       -0.61 | 0.41 | [-1.40,  0.18] | -1.51 | 0.132  |   fixed |   conditional
      spp [PR]             |       -0.96 | 0.64 | [-2.23,  0.30] | -1.50 | 0.134  |   fixed |   conditional
      spp [DM]             |        0.17 | 0.24 | [-0.29,  0.63] |  0.73 | 0.468  |   fixed |   conditional
      spp [EC-A]           |       -0.39 | 0.34 | [-1.06,  0.28] | -1.13 | 0.258  |   fixed |   conditional
      spp [EC-L]           |        0.49 | 0.24 | [ 0.02,  0.96] |  2.05 | 0.041  |   fixed |   conditional
      spp [DES-L]          |        0.59 | 0.23 | [ 0.14,  1.04] |  2.59 | 0.010  |   fixed |   conditional
      spp [DF]             |       -0.11 | 0.24 | [-0.59,  0.36] | -0.46 | 0.642  |   fixed |   conditional
      mined [no]           |        1.43 | 0.37 | [ 0.71,  2.15] |  3.90 | < .001 |   fixed |   conditional
      (Intercept)          |        0.91 | 0.63 | [-0.32,  2.14] |  1.45 | 0.147  |   fixed | zero_inflated
      sppPR                |        1.16 | 1.33 | [-1.45,  3.78] |  0.87 | 0.384  |   fixed | zero_inflated
      sppDM                |       -0.94 | 0.80 | [-2.51,  0.63] | -1.17 | 0.241  |   fixed | zero_inflated
      sppEC-A              |        1.04 | 0.71 | [-0.36,  2.44] |  1.46 | 0.144  |   fixed | zero_inflated
      sppEC-L              |       -0.56 | 0.73 | [-1.99,  0.86] | -0.77 | 0.439  |   fixed | zero_inflated
      sppDES-L             |       -0.89 | 0.75 | [-2.37,  0.58] | -1.19 | 0.236  |   fixed | zero_inflated
      sppDF                |       -2.54 | 2.18 | [-6.82,  1.74] | -1.16 | 0.244  |   fixed | zero_inflated
      minedno              |       -2.56 | 0.60 | [-3.75, -1.38] | -4.24 | < .001 |   fixed | zero_inflated
      (Intercept)          |        1.51 |      | [ 0.93,  2.46] |       |        |   fixed |    dispersion
      SD (Intercept: site) |        0.38 |      | [ 0.17,  0.87] |       |        |  random |   conditional
    Message <simpleMessage>
      
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
      Species [versicolor] × Petal Length |        0.29 | 0.30 | [-0.30,  0.87] |   0.97 | 0.334 
      Species [virginica] × Petal Length  |        0.45 | 0.29 | [-0.12,  1.03] |   1.56 | 0.120 
      
      Model: Sepal.Length ~ Species * Petal.Length (150 Observations)
      Residual standard deviation: 0.336 (df = 144)
      R2: 0.840; adjusted R2: 0.835
    Message <simpleMessage>
      
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
        gear [4] × vs  |       -2.90 | 4.67 | [-12.57,  6.78] | -0.62 | 0.541
        gear [5] × vs  |        2.59 | 4.54 | [ -6.82, 12.00] |  0.57 | 0.574
      Controls         |             |      |                 |       |      
        gear [4]       |        3.10 | 4.34 | [ -5.90, 12.10] |  0.71 | 0.482
        gear [5]       |        4.80 | 3.48 | [ -2.42, 12.01] |  1.38 | 0.182
        drat           |        2.70 | 2.03 | [ -1.52,  6.91] |  1.33 | 0.198
    Message <simpleMessage>
      
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
        gear [4] × vs         -2.90  4.67  [-12.57,  6.78]  -0.62  0.541
        gear [5] × vs          2.59  4.54  [ -6.82, 12.00]   0.57  0.574
      Controls                                                          
        gear [4]               3.10  4.34  [ -5.90, 12.10]   0.71  0.482
        gear [5]               4.80  3.48  [ -2.42, 12.01]   1.38  0.182
        drat                   2.70  2.03  [ -1.52,  6.91]   1.33  0.198
    Message <simpleMessage>
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

# style pattern

    Code
      print(out, style = "{coef} ({se})")
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
      gear [4] × vs |  -2.90 (4.67)
      gear [5] × vs |   2.59 (4.54)
    Message <simpleMessage>
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(out, style = "{coef}{stars}|[{ci}]")
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
      gear [4] × vs |    -2.90 | [-12.57,  6.78]
      gear [5] × vs |     2.59 | [ -6.82, 12.00]
    Message <simpleMessage>
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(out, groups = list(Engine = c("cyl6", "cyl8", "vs", "hp"), Interactions = c(
        "gear4:vs", "gear5:vs"), Controls = c(2, 3, 7)), style = "{coef}{stars}|[{ci}]")
    Output
      Parameter        | Estimate |            [ci]
      ---------------------------------------------
      Engine           |          |                
        cyl [6]        |    -2.47 | [ -7.05,  2.12]
        cyl [8]        |     1.97 | [ -8.63, 12.58]
        vs             |     3.18 | [ -4.68, 11.04]
        hp             |  -0.06** | [ -0.11, -0.02]
      Interactions     |          |                
        gear [4] × vs  |    -2.90 | [-12.57,  6.78]
        gear [5] × vs  |     2.59 | [ -6.82, 12.00]
      Controls         |          |                
        gear [4]       |     3.10 | [ -5.90, 12.10]
        gear [5]       |     4.80 | [ -2.42, 12.01]
        drat           |     2.70 | [ -1.52,  6.91]
    Message <simpleMessage>
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(out, sep = "  ", groups = list(Engine = c("cyl6", "cyl8", "vs", "hp"),
      Interactions = c("gear4:vs", "gear5:vs"), Controls = c(2, 3, 7)), style = "{coef}{stars}|[{ci}]")
    Output
      Parameter         Estimate             [ci]
      -------------------------------------------
      Engine                                     
        cyl [6]            -2.47  [ -7.05,  2.12]
        cyl [8]             1.97  [ -8.63, 12.58]
        vs                  3.18  [ -4.68, 11.04]
        hp               -0.06**  [ -0.11, -0.02]
      Interactions                               
        gear [4] × vs      -2.90  [-12.57,  6.78]
        gear [5] × vs       2.59  [ -6.82, 12.00]
      Controls                                   
        gear [4]            3.10  [ -5.90, 12.10]
        gear [5]            4.80  [ -2.42, 12.01]
        drat                2.70  [ -1.52,  6.91]
    Message <simpleMessage>
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

