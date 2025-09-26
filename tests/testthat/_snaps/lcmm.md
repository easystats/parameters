# model_parameters lcmm

    Code
      model_parameters(out$mx_linear)
    Output
      Parameter        | Log-Odds |      SE |              95% CI |        z |     p
      ------------------------------------------------------------------------------
      intercept class1 |     1.12 |    0.90 | [   -0.64,    2.89] |     1.25 | 0.211
      intercept class2 |     1.19 |    0.74 | [   -0.26,    2.65] |     1.61 | 0.107
      X1 class1        |     1.85 |   13.74 | [  -25.08,   28.77] |     0.13 | 0.893
      X1 class2        |     4.12 |   12.32 | [  -20.03,   28.26] |     0.33 | 0.738
      X2 class1        |    10.50 | 2079.25 | [-4064.75, 4085.74] | 5.05e-03 | 0.996
      X2 class2        |     9.64 | 2078.80 | [-4064.73, 4084.02] | 4.64e-03 | 0.996
      X3 class1        |    -0.60 |    0.90 | [   -2.36,    1.17] |    -0.66 | 0.508
      X3 class2        |     0.12 |    0.55 | [   -0.95,    1.19] |     0.23 | 0.822
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.
      
      The model has a log- or logit-link. Consider using `exponentiate =
        TRUE` to interpret coefficients as ratios.
        
      Some coefficients are very large, which may indicate issues with
        complete separation.

---

    Code
      model_parameters(out$mx_beta)
    Output
      Parameter        | Log-Odds |    SE |            95% CI |        z |     p
      --------------------------------------------------------------------------
      intercept class1 |     0.12 | 67.86 | [-132.89, 133.13] | 1.76e-03 | 0.999
      intercept class2 |     1.55 | 35.86 | [ -68.73,  71.83] |     0.04 | 0.966
      X1 class1        |     0.51 | 24.20 | [ -46.92,  47.94] |     0.02 | 0.983
      X1 class2        |    -1.02 | 26.22 | [ -52.41,  50.36] |    -0.04 | 0.969
      X2 class1        |    15.95 | 14.97 | [ -13.39,  45.28] |     1.07 | 0.287
      X2 class2        |    15.44 |  9.01 | [  -2.22,  33.09] |     1.71 | 0.087
      X3 class1        |     0.44 | 14.56 | [ -28.09,  28.97] |     0.03 | 0.976
      X3 class2        |    -0.35 | 18.61 | [ -36.81,  36.12] |    -0.02 | 0.985
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

---

    Code
      model_parameters(out$mx_splines)
    Output
      Parameter        | Log-Odds |      SE |              95% CI |     z |     p
      ---------------------------------------------------------------------------
      intercept class1 |    -1.46 |    0.83 | [   -3.09,    0.17] | -1.75 | 0.080
      intercept class2 |    -1.53 |    0.83 | [   -3.16,    0.11] | -1.83 | 0.067
      X1 class1        |     1.58 |    0.71 | [    0.18,    2.97] |  2.22 | 0.027
      X1 class2        |     0.86 |    1.83 | [   -2.73,    4.45] |  0.47 | 0.639
      X2 class1        |     0.47 |    0.98 | [   -1.44,    2.38] |  0.48 | 0.631
      X2 class2        |   -21.31 | 1194.95 | [-2363.38, 2320.75] | -0.02 | 0.986
      X3 class1        |     0.80 |    0.38 | [    0.05,    1.54] |  2.10 | 0.036
      X3 class2        |     0.30 |    0.63 | [   -0.95,    1.54] |  0.47 | 0.638
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

---

    Code
      model_parameters(out$m1_linear)
    Output
      # longitudinal
      
      Parameter | Coefficient |   SE |         95% CI |     z |      p
      ----------------------------------------------------------------
      Time      |       -0.78 | 0.19 | [-1.15, -0.40] | -4.09 | < .001
      I(Time^2) |       -0.18 | 0.08 | [-0.33, -0.04] | -2.44 | 0.015 
      
      # linear
      
      Parameter            | Coefficient |   SE |         95% CI |      z |      p
      ----------------------------------------------------------------------------
      Linear 1 (intercept) |       25.40 | 0.23 | [24.96, 25.85] | 112.34 | < .001
      Linear 2 (std err)   |        2.24 | 0.04 | [ 2.15,  2.33] |  50.37 | < .001
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

---

    Code
      model_parameters(out$m1_beta)
    Output
      # longitudinal
      
      Parameter | Coefficient |   SE |         95% CI |     z |      p
      ----------------------------------------------------------------
      Time      |       -0.88 | 0.19 | [-1.25, -0.51] | -4.62 | < .001
      I(Time^2) |       -0.10 | 0.08 | [-0.25,  0.05] | -1.34 | 0.180 
      
      # beta
      
      Parameter | Coefficient |       SE |         95% CI |     z |      p
      --------------------------------------------------------------------
      Beta1     |        0.54 |     0.07 | [ 0.41,  0.68] |  7.97 | < .001
      Beta2     |       -0.76 |     0.09 | [-0.94, -0.58] | -8.36 | < .001
      Beta3     |        0.70 |     0.02 | [ 0.67,  0.74] | 42.20 | < .001
      Beta4     |        0.09 | 4.36e-03 | [ 0.08,  0.10] | 20.53 | < .001
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

---

    Code
      model_parameters(out$m1_splines)
    Output
      # longitudinal
      
      Parameter | Coefficient |   SE |         95% CI |     z |      p
      ----------------------------------------------------------------
      Time      |       -0.89 | 0.19 | [-1.26, -0.52] | -4.68 | < .001
      I(Time^2) |       -0.10 | 0.08 | [-0.25,  0.05] | -1.28 | 0.200 
      
      # splines
      
      Parameter  | Coefficient |   SE |         95% CI |      z |      p
      ------------------------------------------------------------------
      I-splines1 |       -7.76 | 0.59 | [-8.92, -6.59] | -13.04 | < .001
      I-splines2 |        0.76 | 0.28 | [ 0.21,  1.31] |   2.71 | 0.007 
      I-splines3 |        0.81 | 0.41 | [ 0.00,  1.63] |   1.96 | 0.049 
      I-splines4 |        1.44 | 0.13 | [ 1.18,  1.70] |  10.85 | < .001
      I-splines5 |       -1.67 | 0.06 | [-1.80, -1.54] | -25.86 | < .001
      I-splines6 |        1.62 | 0.05 | [ 1.52,  1.72] |  32.56 | < .001
      I-splines7 |        1.30 | 0.05 | [ 1.20,  1.40] |  25.33 | < .001
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

---

    Code
      model_parameters(out$m2_linear)
    Output
      # membership
      
      Parameter        | Coefficient |   SE |        95% CI |    z |     p
      --------------------------------------------------------------------
      intercept class1 |        1.69 | 0.95 | [-0.18, 3.56] | 1.77 | 0.076
      intercept class2 |        2.47 | 0.79 | [ 0.91, 4.02] | 3.11 | 0.002
      
      # longitudinal
      
      Parameter        | Coefficient |   SE |         95% CI |     z |     p
      ----------------------------------------------------------------------
      intercept class2 |        0.68 | 0.33 | [ 0.03,  1.32] |  2.06 | 0.039
      intercept class3 |       -0.63 | 0.68 | [-1.97,  0.70] | -0.93 | 0.353
      Time class1      |       -1.12 | 0.56 | [-2.21, -0.03] | -2.01 | 0.045
      Time class2      |       -0.37 | 0.25 | [-0.86,  0.13] | -1.44 | 0.151
      Time class3      |       -2.90 | 1.48 | [-5.80,  0.00] | -1.96 | 0.050
      I(Time^2) class1 |       -0.52 | 0.24 | [-0.98, -0.06] | -2.21 | 0.027
      I(Time^2) class2 |       -0.15 | 0.09 | [-0.34,  0.03] | -1.67 | 0.094
      I(Time^2) class3 |        0.79 | 0.70 | [-0.59,  2.18] |  1.13 | 0.260
      
      # linear
      
      Parameter            | Coefficient |   SE |         95% CI |     z |      p
      ---------------------------------------------------------------------------
      Linear 1 (intercept) |       24.46 | 0.60 | [23.28, 25.64] | 40.56 | < .001
      Linear 2 (std err)   |        2.23 | 0.04 | [ 2.14,  2.32] | 50.22 | < .001
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

---

    Code
      model_parameters(out$m2_beta)
    Output
      # membership
      
      Parameter        | Coefficient |   SE |        95% CI |    z |     p
      --------------------------------------------------------------------
      intercept class1 |        0.93 | 1.48 | [-1.97, 3.83] | 0.63 | 0.531
      intercept class2 |        1.36 | 1.10 | [-0.79, 3.51] | 1.24 | 0.214
      
      # longitudinal
      
      Parameter        | Coefficient |   SE |         95% CI |     z |      p
      -----------------------------------------------------------------------
      intercept class2 |       -0.54 | 0.44 | [-1.41,  0.33] | -1.22 | 0.221 
      intercept class3 |       -0.99 | 0.64 | [-2.25,  0.27] | -1.54 | 0.123 
      Time class1      |       -0.52 | 0.67 | [-1.83,  0.79] | -0.77 | 0.438 
      Time class2      |       -1.68 | 0.33 | [-2.32, -1.04] | -5.14 | < .001
      Time class3      |        1.61 | 1.09 | [-0.52,  3.74] |  1.48 | 0.138 
      I(Time^2) class1 |        0.02 | 0.23 | [-0.43,  0.47] |  0.07 | 0.943 
      I(Time^2) class2 |       -0.07 | 0.13 | [-0.33,  0.20] | -0.50 | 0.618 
      I(Time^2) class3 |       -0.69 | 0.39 | [-1.46,  0.09] | -1.74 | 0.082 
      
      # beta
      
      Parameter | Coefficient |       SE |         95% CI |     z |      p
      --------------------------------------------------------------------
      Beta1     |        0.60 |     0.06 | [ 0.48,  0.73] |  9.34 | < .001
      Beta2     |       -0.83 |     0.09 | [-1.01, -0.66] | -9.39 | < .001
      Beta3     |        0.73 |     0.03 | [ 0.67,  0.80] | 22.27 | < .001
      Beta4     |        0.09 | 4.25e-03 | [ 0.08,  0.10] | 21.88 | < .001
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

---

    Code
      model_parameters(out$m2_splines)
    Output
      # membership
      
      Parameter        | Coefficient |   SE |        95% CI |     z |     p
      ---------------------------------------------------------------------
      intercept class1 |       -0.46 | 0.45 | [-1.33, 0.42] | -1.01 | 0.310
      intercept class2 |       -1.40 | 1.06 | [-3.48, 0.68] | -1.32 | 0.186
      
      # longitudinal
      
      Parameter        | Coefficient |   SE |         95% CI |     z |      p
      -----------------------------------------------------------------------
      intercept class2 |       -1.03 | 0.67 | [-2.34,  0.27] | -1.55 | 0.121 
      intercept class3 |       -0.55 | 0.41 | [-1.36,  0.26] | -1.34 | 0.182 
      Time class1      |       -0.48 | 0.59 | [-1.63,  0.67] | -0.81 | 0.415 
      Time class2      |        1.65 | 1.10 | [-0.50,  3.80] |  1.50 | 0.133 
      Time class3      |       -1.72 | 0.32 | [-2.35, -1.09] | -5.36 | < .001
      I(Time^2) class1 |    6.95e-03 | 0.20 | [-0.38,  0.40] |  0.03 | 0.972 
      I(Time^2) class2 |       -0.70 | 0.40 | [-1.49,  0.08] | -1.76 | 0.078 
      I(Time^2) class3 |       -0.04 | 0.13 | [-0.30,  0.22] | -0.33 | 0.743 
      
      # splines
      
      Parameter  | Coefficient |   SE |         95% CI |      z |      p
      ------------------------------------------------------------------
      I-splines1 |       -7.87 | 0.62 | [-9.09, -6.66] | -12.67 | < .001
      I-splines2 |        0.72 | 0.27 | [ 0.20,  1.24] |   2.70 | 0.007 
      I-splines3 |        0.77 | 0.41 | [-0.03,  1.57] |   1.88 | 0.060 
      I-splines4 |        1.37 | 0.13 | [ 1.11,  1.63] |  10.35 | < .001
      I-splines5 |       -1.66 | 0.06 | [-1.79, -1.54] | -25.77 | < .001
      I-splines6 |        1.64 | 0.05 | [ 1.54,  1.73] |  33.01 | < .001
      I-splines7 |        1.28 | 0.05 | [ 1.18,  1.39] |  24.94 | < .001
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

