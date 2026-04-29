# print brms

    Code
      mp1
    Output
      # Fixed Effects
      
      Parameter   |  Mean |         95% CI |     pd |  Rhat | ESS (tail)
      ------------------------------------------------------------------
      (Intercept) | 39.68 | [36.12, 43.27] |   100% | 1.000 |       3255
      wt          | -3.21 | [-4.79, -1.65] | 99.95% | 1.002 |       2003
      cyl         | -1.50 | [-2.36, -0.64] | 99.95% | 1.002 |       2227
      
      # Sigma
      
      Parameter | Mean |       95% CI |   pd |  Rhat | ESS (tail)
      -----------------------------------------------------------
      sigma     | 2.67 | [2.06, 3.51] | 100% | 1.001 |       2415
    Message
      
      Uncertainty intervals (equal-tailed) computed using a MCMC distribution
        approximation.

---

    Code
      mp2
    Output
      # Fixed Effects
      
      Parameter   |  Mean |         95% CI |   pd |  Rhat | ESS (tail)
      ----------------------------------------------------------------
      (Intercept) | 33.55 | [24.17, 40.87] | 100% | 1.090 |        157
      wt          | -4.49 | [-6.95, -1.68] | 100% | 1.168 |         81
      
      # Sigma
      
      Parameter | Mean |       95% CI |   pd |  Rhat | ESS (tail)
      -----------------------------------------------------------
      sigma     | 2.56 | [1.95, 3.48] | 100% | 1.294 |        541
      
      # Random Effects Variances
      
      Parameter                |  Mean |         95% CI |     pd |  Rhat | ESS (tail)
      -------------------------------------------------------------------------------
      SD (Intercept: cyl)      |  3.00 | [ 0.39,  9.19] |   100% | 1.109 |        575
      SD (Intercept: gear)     |  3.88 | [ 0.21, 10.30] |   100% | 1.285 |        916
      SD (wt: gear)            |  1.96 | [ 0.06,  5.06] |   100% | 1.219 |        186
      Cor (Intercept~wt: gear) | -0.25 | [-0.99,  0.83] | 62.48% | 1.077 |         85
    Message
      
      Uncertainty intervals (equal-tailed) computed using a MCMC distribution
        approximation.

---

    Code
      mp3
    Output
      # Fixed Effects
      
      Parameter   |   Mean |           95% CI |   pd |  Rhat | ESS (tail)
      -------------------------------------------------------------------
      (Intercept) | 251.32 | [237.00, 265.98] | 100% | 1.001 |       2415
      Days        |  10.44 | [  6.84,  13.91] | 100% | 1.004 |       1641
      
      # Sigma
      
      Parameter |  Mean |         95% CI |   pd |  Rhat | ESS (tail)
      --------------------------------------------------------------
      sigma     | 25.94 | [23.05, 29.38] | 100% | 1.002 |       2781
      
      # Random Effects Variances
      
      Parameter                     |  Mean |         95% CI |     pd |  Rhat | ESS (tail)
      ------------------------------------------------------------------------------------
      SD (Intercept: Subject)       | 26.63 | [15.46, 42.36] |   100% | 1.002 |       2683
      SD (Days: Subject)            |  6.58 | [ 4.12, 10.16] |   100% | 1.001 |       1961
      Cor (Intercept~Days: Subject) |  0.09 | [-0.47,  0.67] | 60.42% | 1.003 |       1625
    Message
      
      Uncertainty intervals (equal-tailed) computed using a MCMC distribution
        approximation.

---

    Code
      mp4
    Output
      # Fixed Effects
      
      Parameter   |   Mean |           95% CI |   pd |  Rhat | ESS (tail)
      -------------------------------------------------------------------
      (Intercept) | 250.84 | [228.76, 272.54] | 100% | 1.002 |       1607
      Days        |  10.37 | [  8.77,  11.96] | 100% | 0.999 |       2848
      
      # Sigma
      
      Parameter |  Mean |         95% CI |   pd |  Rhat | ESS (tail)
      --------------------------------------------------------------
      sigma     | 30.03 | [26.27, 34.03] | 100% | 1.000 |       2289
      
      # Random Effects Variances
      
      Parameter                  |  Mean |         95% CI |   pd |  Rhat | ESS (tail)
      -------------------------------------------------------------------------------
      SD (Intercept: grp)        |  8.22 | [ 0.44, 25.69] | 100% | 1.001 |       1788
      SD (Intercept: grp:subgrp) |  7.41 | [ 0.44, 16.87] | 100% | 1.003 |       1480
      SD (Intercept: Subject)    | 38.51 | [26.89, 55.98] | 100% | 1.002 |       1893
    Message
      
      Uncertainty intervals (equal-tailed) computed using a MCMC distribution
        approximation.

---

    Code
      mp5
    Output
      # Fixed Effects
      
      Parameter   | Mean |       95% CI |     pd |  Rhat | ESS (tail)
      ---------------------------------------------------------------
      (Intercept) | 2.57 | [0.70, 4.84] | 99.42% | 1.009 |        197
      Petal.Width | 1.05 | [0.73, 1.37] |   100% | 1.004 |       2213
      
      # Sigma
      
      Parameter | Mean |       95% CI |   pd |  Rhat | ESS (tail)
      -----------------------------------------------------------
      sigma     | 0.38 | [0.34, 0.43] | 100% | 1.002 |       2011
      
      # Random Effects Variances
      
      Parameter               | Mean |       95% CI |   pd |  Rhat | ESS (tail)
      -------------------------------------------------------------------------
      SD (Intercept: Species) | 1.68 | [0.64, 3.64] | 100% | 1.007 |        774
    Message
      
      Uncertainty intervals (equal-tailed) computed using a MCMC distribution
        approximation.

---

    Code
      mp6
    Output
      # Fixed Effects
      
      Parameter   |  Mean |         95% CI |     pd |  Rhat | ESS (tail)
      ------------------------------------------------------------------
      (Intercept) | 33.06 | [24.70, 40.47] |   100% | 1.021 |        666
      wt          | -4.39 | [-6.94, -1.76] | 99.72% | 1.024 |        102
      
      # Sigma
      
      Parameter | Mean |       95% CI |   pd |  Rhat | ESS (tail)
      -----------------------------------------------------------
      sigma     | 2.59 | [1.96, 3.48] | 100% | 1.009 |        902
      
      # Random Effects Variances
      
      Parameter                |  Mean |         95% CI |     pd |  Rhat | ESS (tail)
      -------------------------------------------------------------------------------
      SD (Intercept: cyl)      |  3.19 | [ 0.49,  9.00] |   100% | 1.003 |        350
      SD (Intercept: gear)     |  3.76 | [ 0.14, 10.13] |   100% | 1.015 |       1264
      SD (wt: gear)            |  1.47 | [ 0.06,  3.96] |   100% | 1.034 |        660
      Cor (Intercept~wt: gear) | -0.38 | [-0.99,  0.82] | 76.85% | 1.022 |        893
    Message
      
      Uncertainty intervals (equal-tailed) computed using a MCMC distribution
        approximation.

---

    Code
      mp7
    Output
      # Fixed Effects
      
      Parameter   |  Mean |         95% CI |   pd |  Rhat | ESS (tail)
      ----------------------------------------------------------------
      (Intercept) | -1.07 | [-1.42, -0.73] | 100% | 1.001 |       2806
      persons     |  0.90 | [ 0.81,  0.99] | 100% | 1.001 |       3057
      child       | -1.17 | [-1.37, -0.99] | 100% | 1.002 |       2560
      camper      |  0.74 | [ 0.56,  0.94] | 100% | 1.000 |       2879
      
      # Zero-Inflation
      
      Parameter   |  Mean |        95% CI |     pd |  Rhat | ESS (tail)
      -----------------------------------------------------------------
      (Intercept) | -0.58 | [-1.27, 0.08] | 95.97% | 1.001 |       2969
      child       |  1.24 | [ 0.71, 1.82] |   100% | 1.001 |       2872
      camper      | -0.62 | [-1.38, 0.11] | 94.73% | 1.000 |       3040

---

    Code
      mp9
    Output
      Parameter    |   Mean |           95% CI |   pd |  Rhat | ESS (tail)
      --------------------------------------------------------------------
      Intercept[1] | -38.42 | [-67.76, -19.66] | 100% | 1.001 |       1121
      Intercept[2] | -33.26 | [-59.09, -16.53] | 100% | 1.001 |       1111
      mpg          |  -1.80 | [ -3.20,  -0.90] | 100% | 1.002 |       1172
    Message
      
      Uncertainty intervals (equal-tailed) computed using a MCMC distribution
        approximation.

# print-information

    Code
      out
    Output
      # Fixed Effects
      
      Parameter   | Median |         95% CI |     pd |  Rhat | ESS (tail)
      -------------------------------------------------------------------
      (Intercept) |  39.68 | [36.12, 43.27] |   100% | 1.000 |       3255
      wt          |  -3.20 | [-4.79, -1.65] | 99.95% | 1.002 |       2003
      cyl         |  -1.49 | [-2.36, -0.64] | 99.95% | 1.002 |       2227
      
      # Sigma
      
      Parameter | Median |       95% CI |   pd |  Rhat | ESS (tail)
      -------------------------------------------------------------
      sigma     |   2.63 | [2.06, 3.51] | 100% | 1.001 |       2415
    Message
      
      Uncertainty intervals (equal-tailed) computed using a MCMC distribution
        approximation.

---

    Code
      out
    Output
      # Fixed Effects
      
      Parameter   | Median |         95% CI |     pd |  Rhat | ESS (tail)
      -------------------------------------------------------------------
      (Intercept) |  39.68 | [36.27, 43.34] |   100% | 1.000 |       3255
      wt          |  -3.20 | [-4.70, -1.57] | 99.95% | 1.002 |       2003
      cyl         |  -1.49 | [-2.38, -0.68] | 99.95% | 1.002 |       2227
      
      # Sigma
      
      Parameter | Median |       95% CI |   pd |  Rhat | ESS (tail)
      -------------------------------------------------------------
      sigma     |   2.63 | [1.99, 3.39] | 100% | 1.001 |       2415
    Message
      
      Uncertainty intervals (highest-density) computed using a MCMC
        distribution approximation.

