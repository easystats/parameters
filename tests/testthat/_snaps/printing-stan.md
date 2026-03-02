# print brms

    Code
      mp1
    Output
      # Fixed Effects
      
      Parameter   |  Mean |         95% CI |     pd |  Rhat |  ESS
      ------------------------------------------------------------
      (Intercept) | 39.68 | [36.12, 43.27] |   100% | 1.000 | 5242
      wt          | -3.21 | [-4.79, -1.65] | 99.95% | 1.000 | 2071
      cyl         | -1.50 | [-2.36, -0.64] | 99.95% | 1.000 | 1951
      
      # Sigma
      
      Parameter | Mean |       95% CI |   pd |  Rhat |  ESS
      -----------------------------------------------------
      sigma     | 2.67 | [2.06, 3.51] | 100% | 1.000 | 2390
    Message
      
      Uncertainty intervals (equal-tailed) computed using a MCMC distribution
        approximation.

---

    Code
      mp2
    Output
      # Fixed Effects
      
      Parameter   |  Mean |         95% CI |   pd |  Rhat | ESS
      ---------------------------------------------------------
      (Intercept) | 33.55 | [24.17, 40.87] | 100% | 1.091 |  24
      wt          | -4.49 | [-6.95, -1.68] | 100% | 1.192 |  10
      
      # Sigma
      
      Parameter | Mean |       95% CI |   pd |  Rhat | ESS
      ----------------------------------------------------
      sigma     | 2.56 | [1.95, 3.48] | 100% | 1.015 | 454
      
      # Random Effects Variances
      
      Parameter                |  Mean |         95% CI |     pd |  Rhat | ESS
      ------------------------------------------------------------------------
      SD (Intercept: cyl)      |  3.00 | [ 0.39,  9.19] |   100% | 1.080 |  32
      SD (Intercept: gear)     |  3.88 | [ 0.21, 10.30] |   100% | 1.010 | 424
      SD (wt: gear)            |  1.96 | [ 0.06,  5.06] |   100% | 1.385 |   9
      Cor (Intercept~wt: gear) | -0.25 | [-0.99,  0.83] | 62.48% | 1.106 |  36
    Message
      
      Uncertainty intervals (equal-tailed) computed using a MCMC distribution
        approximation.

---

    Code
      mp3
    Output
      # Fixed Effects
      
      Parameter   |   Mean |           95% CI |   pd |  Rhat |  ESS
      -------------------------------------------------------------
      (Intercept) | 251.32 | [237.00, 265.98] | 100% | 1.001 | 1621
      Days        |  10.44 | [  6.84,  13.91] | 100% | 1.004 | 1161
      
      # Sigma
      
      Parameter |  Mean |         95% CI |   pd |  Rhat |  ESS
      --------------------------------------------------------
      sigma     | 25.94 | [23.05, 29.38] | 100% | 1.000 | 3672
      
      # Random Effects Variances
      
      Parameter                     |  Mean |         95% CI |     pd |  Rhat |  ESS
      ------------------------------------------------------------------------------
      SD (Intercept: Subject)       | 26.63 | [15.46, 42.36] |   100% | 1.002 | 1823
      SD (Days: Subject)            |  6.58 | [ 4.12, 10.16] |   100% | 1.000 | 1228
      Cor (Intercept~Days: Subject) |  0.09 | [-0.47,  0.67] | 60.42% | 1.003 |  899
    Message
      
      Uncertainty intervals (equal-tailed) computed using a MCMC distribution
        approximation.

---

    Code
      mp4
    Output
      # Fixed Effects
      
      Parameter   |   Mean |           95% CI |   pd |  Rhat |  ESS
      -------------------------------------------------------------
      (Intercept) | 250.84 | [228.76, 272.54] | 100% | 1.003 |  786
      Days        |  10.37 | [  8.77,  11.96] | 100% | 0.999 | 6026
      
      # Sigma
      
      Parameter |  Mean |         95% CI |   pd |  Rhat |  ESS
      --------------------------------------------------------
      sigma     | 30.03 | [26.27, 34.03] | 100% | 0.999 | 2102
      
      # Random Effects Variances
      
      Parameter                  |  Mean |         95% CI |   pd |  Rhat |  ESS
      -------------------------------------------------------------------------
      SD (Intercept: grp)        |  8.22 | [ 0.44, 25.69] | 100% | 1.000 | 1604
      SD (Intercept: grp:subgrp) |  7.41 | [ 0.44, 16.87] | 100% | 1.003 |  770
      SD (Intercept: Subject)    | 38.51 | [26.89, 55.98] | 100% | 1.003 | 1254
    Message
      
      Uncertainty intervals (equal-tailed) computed using a MCMC distribution
        approximation.

---

    Code
      mp5
    Output
      # Fixed Effects
      
      Parameter   | Mean |       95% CI |     pd |  Rhat |  ESS
      ---------------------------------------------------------
      (Intercept) | 2.57 | [0.70, 4.84] | 99.42% | 1.012 |  292
      Petal.Width | 1.05 | [0.73, 1.37] |   100% | 1.002 | 2150
      
      # Sigma
      
      Parameter | Mean |       95% CI |   pd |  Rhat |  ESS
      -----------------------------------------------------
      sigma     | 0.38 | [0.34, 0.43] | 100% | 1.001 | 2642
      
      # Random Effects Variances
      
      Parameter               | Mean |       95% CI |   pd |  Rhat | ESS
      ------------------------------------------------------------------
      SD (Intercept: Species) | 1.68 | [0.64, 3.64] | 100% | 1.003 | 796
    Message
      
      Uncertainty intervals (equal-tailed) computed using a MCMC distribution
        approximation.

---

    Code
      mp6
    Output
      # Fixed Effects
      
      Parameter   |  Mean |         95% CI |     pd |  Rhat | ESS
      -----------------------------------------------------------
      (Intercept) | 33.06 | [24.70, 40.47] |   100% | 1.006 | 744
      wt          | -4.39 | [-6.94, -1.76] | 99.72% | 1.025 |  83
      
      # Sigma
      
      Parameter | Mean |       95% CI |   pd |  Rhat | ESS
      ----------------------------------------------------
      sigma     | 2.59 | [1.96, 3.48] | 100% | 1.010 | 608
      
      # Random Effects Variances
      
      Parameter                |  Mean |         95% CI |     pd |  Rhat | ESS
      ------------------------------------------------------------------------
      SD (Intercept: cyl)      |  3.19 | [ 0.49,  9.00] |   100% | 1.001 | 651
      SD (Intercept: gear)     |  3.76 | [ 0.14, 10.13] |   100% | 1.015 | 643
      SD (wt: gear)            |  1.47 | [ 0.06,  3.96] |   100% | 1.039 |  94
      Cor (Intercept~wt: gear) | -0.38 | [-0.99,  0.82] | 76.85% | 1.003 | 854
    Message
      
      Uncertainty intervals (equal-tailed) computed using a MCMC distribution
        approximation.

---

    Code
      mp7
    Output
      # Fixed Effects
      
      Parameter   |  Mean |         95% CI |   pd |  Rhat |  ESS
      ----------------------------------------------------------
      (Intercept) | -1.07 | [-1.42, -0.73] | 100% | 1.000 | 3259
      persons     |  0.90 | [ 0.81,  0.99] | 100% | 1.000 | 3305
      child       | -1.17 | [-1.37, -0.99] | 100% | 1.000 | 3224
      camper      |  0.74 | [ 0.56,  0.94] | 100% | 1.000 | 4166
      
      # Zero-Inflation
      
      Parameter   |  Mean |        95% CI |     pd |  Rhat |  ESS
      -----------------------------------------------------------
      (Intercept) | -0.58 | [-1.27, 0.08] | 95.97% | 1.000 | 4494
      child       |  1.24 | [ 0.71, 1.82] |   100% | 1.000 | 4195
      camper      | -0.62 | [-1.38, 0.11] | 94.73% | 1.000 | 4427

---

    Code
      mp9
    Output
      Parameter    |   Mean |           95% CI |   pd |  Rhat |  ESS
      --------------------------------------------------------------
      Intercept[1] | -38.42 | [-67.76, -19.66] | 100% | 1.002 |  992
      Intercept[2] | -33.26 | [-59.09, -16.53] | 100% | 1.001 | 1039
      mpg          |  -1.80 | [ -3.20,  -0.90] | 100% | 1.002 | 1021
    Message
      
      Uncertainty intervals (equal-tailed) computed using a MCMC distribution
        approximation.

# print-information

    Code
      out
    Output
      # Fixed Effects
      
      Parameter   | Median |         95% CI |     pd |  Rhat |  ESS
      -------------------------------------------------------------
      (Intercept) |  39.68 | [36.12, 43.27] |   100% | 1.000 | 5242
      wt          |  -3.20 | [-4.79, -1.65] | 99.95% | 1.000 | 2071
      cyl         |  -1.49 | [-2.36, -0.64] | 99.95% | 1.000 | 1951
      
      # Sigma
      
      Parameter | Median |       95% CI |   pd |  Rhat |  ESS
      -------------------------------------------------------
      sigma     |   2.63 | [2.06, 3.51] | 100% | 1.000 | 2390
    Message
      
      Uncertainty intervals (equal-tailed) computed using a MCMC distribution
        approximation.

---

    Code
      out
    Output
      # Fixed Effects
      
      Parameter   | Median |         95% CI |     pd |  Rhat |  ESS
      -------------------------------------------------------------
      (Intercept) |  39.68 | [36.27, 43.34] |   100% | 1.000 | 5242
      wt          |  -3.20 | [-4.70, -1.57] | 99.95% | 1.000 | 2071
      cyl         |  -1.49 | [-2.38, -0.68] | 99.95% | 1.000 | 1951
      
      # Sigma
      
      Parameter | Median |       95% CI |   pd |  Rhat |  ESS
      -------------------------------------------------------
      sigma     |   2.63 | [1.99, 3.39] | 100% | 1.000 | 2390
    Message
      
      Uncertainty intervals (highest-density) computed using a MCMC
        distribution approximation.

