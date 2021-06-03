# anova wide table - aov

    Code
      model_parameters(model, omega_squared = "partial", eta_squared = "partial",
        epsilon_squared = TRUE, ci = 0.9, table_wide = TRUE, verbose = FALSE)
    Output
      Parameter | Sum_Squares | df | Mean_Square |     F |      p | Omega2 | Omega2 90% CI | Eta2 |  Eta2 90% CI | Epsilon2 | Epsilon2 90% CI | Sum_Squares_Error
      -----------------------------------------------------------------------------------------------------------------------------------------------------------
      Species   |       11.34 |  2 |        5.67 | 49.16 | < .001 |   0.39 |  [0.29, 0.48] | 0.40 | [0.30, 0.48] |     0.39 |    [0.29, 0.48] |             16.96
      
      Anova Table (Type 1 tests)

# anova wide table - aovlist

    Code
      model_parameters(mod, omega_squared = "partial", eta_squared = "partial",
        table_wide = TRUE, verbose = FALSE)
    Output
      # Within
      
      Parameter | Sum_Squares | df | Mean_Square |     F |     p | Omega2 (partial) | Eta2 (partial) | Sum_Squares_Error
      ------------------------------------------------------------------------------------------------------------------
      N         |      189.28 |  1 |      189.28 | 12.26 | 0.004 |             0.23 |           0.51 |            185.29
      P         |        8.40 |  1 |        8.40 |  0.54 | 0.475 |            -0.01 |           0.04 |            185.29
      K         |       95.20 |  1 |       95.20 |  6.17 | 0.029 |             0.12 |           0.34 |            185.29
      N:P       |       21.28 |  1 |       21.28 |  1.38 | 0.263 |         9.91e-03 |           0.10 |            185.29
      N:K       |       33.14 |  1 |       33.14 |  2.15 | 0.169 |             0.03 |           0.15 |            185.29
      P:K       |        0.48 |  1 |        0.48 |  0.03 | 0.863 |            -0.03 |       2.59e-03 |            185.29
      
      # block
      
      Parameter | Sum_Squares | df | Mean_Square |    F |     p | Omega2 (partial) | Eta2 (partial) | Sum_Squares_Error
      -----------------------------------------------------------------------------------------------------------------
      N:P:K     |       37.00 |  1 |       37.00 | 0.48 | 0.525 |            -0.09 |           0.11 |            306.29
      
      Anova Table (Type 1 tests)

# anova wide table - manova

    Code
      model_parameters(m_manova, omega_squared = "raw", eta_squared = "partial",
        epsilon_squared = "raw", ci = 0.99, table_wide = TRUE, verbose = FALSE)
    Output
      Parameter |   Pillai | df |    F |     p | Omega2 (partial) | Omega2 99% CI | Eta2 (partial) |  Eta2 99% CI | Epsilon2 (partial) | Epsilon2 99% CI
      --------------------------------------------------------------------------------------------------------------------------------------------------
      block     |     0.88 |  5 | 1.90 | 0.096 |             0.20 |  [0.00, 0.38] |           0.44 | [0.00, 0.64] |               0.21 |    [0.00, 0.37]
      N         |     0.61 |  1 | 8.52 | 0.006 |             0.52 |  [0.00, 0.79] |           0.61 | [0.00, 0.83] |               0.54 |    [0.00, 0.80]
      P         |     0.07 |  1 | 0.39 | 0.686 |            -0.10 |  [0.00, 0.00] |           0.07 | [0.00, 0.48] |              -0.10 |    [0.00, 0.00]
      K         |     0.39 |  1 | 3.49 | 0.067 |             0.26 |  [0.00, 0.66] |           0.39 | [0.00, 0.73] |               0.28 |    [0.00, 0.67]
      N:P       |     0.11 |  1 | 0.65 | 0.541 |            -0.05 |  [0.00, 0.00] |           0.11 | [0.00, 0.53] |              -0.06 |    [0.00, 0.00]
      N:K       |     0.17 |  1 | 1.16 | 0.350 |             0.02 |  [0.00, 0.38] |           0.17 | [0.00, 0.59] |               0.02 |    [0.00, 0.39]
      P:K       | 3.19e-03 |  1 | 0.02 | 0.983 |            -0.16 |  [0.00, 0.00] |       3.19e-03 | [0.00, 0.19] |              -0.18 |    [0.00, 0.00]
      
      Anova Table (Type 1 tests)

# anova wide table - Gam

    Code
      model_parameters(mod_gam, eta_squared = "raw", epsilon_squared = "partial", ci = 0.9,
        table_wide = TRUE, verbose = FALSE)
    Output
      Parameter | Sum_Squares | df | Mean_Square |        F |      p |     Eta2 |  Eta2 90% CI | Epsilon2 (partial) | Epsilon2 90% CI | Sum_Squares_Error
      ---------------------------------------------------------------------------------------------------------------------------------------------------
      s(hp, 4)  |      678.37 |  1 |      678.37 |    94.38 | < .001 |     0.70 | [0.52, 0.80] |               0.78 |    [0.64, 0.86] |            179.68
      am        |      112.58 |  1 |      112.58 |    15.66 | < .001 |     0.12 | [0.00, 0.33] |               0.36 |    [0.12, 0.55] |            179.68
      qsec      |        0.03 |  1 |        0.03 | 3.66e-03 | 0.952  | 2.71e-05 | [0.00, 0.00] |              -0.04 |    [0.00, 0.00] |            179.68
      
      Anova Table (Type 1 tests)

# anova wide table - maov

    Code
      model_parameters(m_maov, omega_squared = "raw", eta_squared = "partial",
        epsilon_squared = "raw", ci = 0.5, table_wide = TRUE, verbose = FALSE)
    Output
      # mpg response
      
      Parameter   | Sum_Squares | df | Mean_Square |     F |      p | Omega2 | Omega2 50% CI | Eta2 |  Eta2 50% CI | Epsilon2 | Epsilon2 50% CI | Sum_Squares_Error
      -------------------------------------------------------------------------------------------------------------------------------------------------------------
      factor(cyl) |      824.78 |  2 |      412.39 | 39.70 | < .001 |   0.82 |  [0.78, 0.85] | 0.84 | [0.80, 0.86] |     0.83 |    [0.79, 0.85] |            301.26
      
      # disp response
      
      Parameter   | Sum_Squares | df | Mean_Square |     F |      p | Omega2 | Omega2 50% CI | Eta2 |  Eta2 50% CI | Epsilon2 | Epsilon2 50% CI | Sum_Squares_Error
      -------------------------------------------------------------------------------------------------------------------------------------------------------------
      factor(cyl) |    3.99e+05 |  2 |    1.99e+05 | 74.83 | < .001 |   0.69 |  [0.62, 0.73] | 0.71 | [0.65, 0.75] |     0.69 |    [0.63, 0.73] |          41696.33
      
      # hp response
      
      Parameter   | Sum_Squares | df | Mean_Square |     F |      p | Omega2 | Omega2 50% CI | Eta2 |  Eta2 50% CI | Epsilon2 | Epsilon2 50% CI | Sum_Squares_Error
      -------------------------------------------------------------------------------------------------------------------------------------------------------------
      factor(cyl) |    1.04e+05 |  2 |    52015.27 | 36.18 | < .001 |   0.71 |  [0.64, 0.75] | 0.73 | [0.67, 0.77] |     0.71 |    [0.65, 0.75] |          77293.83
      
      Anova Table (Type 1 tests)

