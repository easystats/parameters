# anova wide table - aov

    Code
      model_parameters(model, omega_squared = "partial", eta_squared = "partial",
        epsilon_squared = TRUE, ci = 0.9, table_wide = TRUE, verbose = FALSE)
    Output
      Parameter |     F | df | df (error) |      p | Sum_Squares | Mean_Square | Omega2 | Omega2 90% CI | Eta2 |  Eta2 90% CI | Epsilon2 | Epsilon2 90% CI | Sum_Squares_Error | Mean_Square_Error
      --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      Species   | 49.16 |  2 |        147 | < .001 |       11.34 |        5.67 |   0.39 |  [0.29, 0.48] | 0.40 | [0.30, 0.48] |     0.39 |    [0.29, 0.48] |             16.96 |             16.96
      
      Anova Table (Type 1 tests)

# anova wide table - aovlist

    Code
      model_parameters(mod, omega_squared = "partial", eta_squared = "partial",
        table_wide = TRUE, verbose = FALSE)
    Output
      # Within
      
      Parameter |     F | df | df (error) |     p | Sum_Squares | Mean_Square | Omega2 (partial) | Eta2 (partial) | Sum_Squares_Error | Mean_Square_Error
      ---------------------------------------------------------------------------------------------------------------------------------------------------
      N         | 12.26 |  1 |         12 | 0.004 |      189.28 |      189.28 |             0.23 |           0.51 |            185.29 |            185.29
      P         |  0.54 |  1 |         12 | 0.475 |        8.40 |        8.40 |            -0.01 |           0.04 |            185.29 |            185.29
      K         |  6.17 |  1 |         12 | 0.029 |       95.20 |       95.20 |             0.12 |           0.34 |            185.29 |            185.29
      N:P       |  1.38 |  1 |         12 | 0.263 |       21.28 |       21.28 |         9.91e-03 |           0.10 |            185.29 |            185.29
      N:K       |  2.15 |  1 |         12 | 0.169 |       33.14 |       33.14 |             0.03 |           0.15 |            185.29 |            185.29
      P:K       |  0.03 |  1 |         12 | 0.863 |        0.48 |        0.48 |            -0.03 |       2.59e-03 |            185.29 |            185.29
      
      # block
      
      Parameter |    F | df | df (error) |     p | Sum_Squares | Mean_Square | Omega2 (partial) | Eta2 (partial) | Sum_Squares_Error | Mean_Square_Error
      --------------------------------------------------------------------------------------------------------------------------------------------------
      N:P:K     | 0.48 |  1 |          4 | 0.525 |       37.00 |       37.00 |            -0.09 |           0.11 |            306.29 |            306.29
      
      Anova Table (Type 1 tests)

# anova wide table - manova

    Code
      model_parameters(m_manova, omega_squared = "raw", eta_squared = "partial",
        epsilon_squared = "raw", ci = 0.99, table_wide = TRUE, verbose = FALSE)
    Output
      Parameter |    F | df | df (error) |     p |   Pillai | Omega2 (partial) | Omega2 99% CI | Eta2 (partial) |  Eta2 99% CI | Epsilon2 (partial) | Epsilon2 99% CI
      ---------------------------------------------------------------------------------------------------------------------------------------------------------------
      block     | 1.90 |  5 |         12 | 0.096 |     0.88 |             0.20 |  [0.00, 0.38] |           0.44 | [0.00, 0.64] |               0.21 |    [0.00, 0.37]
      N         | 8.52 |  1 |         12 | 0.006 |     0.61 |             0.52 |  [0.00, 0.79] |           0.61 | [0.00, 0.83] |               0.54 |    [0.00, 0.80]
      P         | 0.39 |  1 |         12 | 0.686 |     0.07 |            -0.10 |  [0.00, 0.00] |           0.07 | [0.00, 0.48] |              -0.10 |    [0.00, 0.00]
      K         | 3.49 |  1 |         12 | 0.067 |     0.39 |             0.26 |  [0.00, 0.66] |           0.39 | [0.00, 0.73] |               0.28 |    [0.00, 0.67]
      N:P       | 0.65 |  1 |         12 | 0.541 |     0.11 |            -0.05 |  [0.00, 0.00] |           0.11 | [0.00, 0.53] |              -0.06 |    [0.00, 0.00]
      N:K       | 1.16 |  1 |         12 | 0.350 |     0.17 |             0.02 |  [0.00, 0.38] |           0.17 | [0.00, 0.59] |               0.02 |    [0.00, 0.39]
      P:K       | 0.02 |  1 |         12 | 0.983 | 3.19e-03 |            -0.16 |  [0.00, 0.00] |       3.19e-03 | [0.00, 0.19] |              -0.18 |    [0.00, 0.00]
      
      Anova Table (Type 1 tests)

# anova wide table - Gam

    Code
      model_parameters(mod_gam, eta_squared = "raw", epsilon_squared = "partial", ci = 0.9,
        table_wide = TRUE, verbose = FALSE)
    Output
      Parameter |        F | df | df (error) |      p | Sum_Squares | Mean_Square |     Eta2 |  Eta2 90% CI | Epsilon2 (partial) | Epsilon2 90% CI | Sum_Squares_Error | Mean_Square_Error
      ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      s(hp, 4)  |    94.38 |  1 |      25.00 | < .001 |      678.37 |      678.37 |     0.70 | [0.52, 0.80] |               0.78 |    [0.64, 0.86] |            179.68 |            179.68
      am        |    15.66 |  1 |      25.00 | < .001 |      112.58 |      112.58 |     0.12 | [0.00, 0.33] |               0.36 |    [0.12, 0.55] |            179.68 |            179.68
      qsec      | 3.66e-03 |  1 |      25.00 | 0.952  |        0.03 |        0.03 | 2.71e-05 | [0.00, 0.00] |              -0.04 |    [0.00, 0.00] |            179.68 |            179.68
      
      Anova Table (Type 1 tests)

# anova wide table - maov

    Code
      model_parameters(m_maov, omega_squared = "raw", eta_squared = "partial",
        epsilon_squared = "raw", ci = 0.5, table_wide = TRUE, verbose = FALSE)
    Output
      # mpg response
      
      Parameter   |     F | df | df (error) |      p | Sum_Squares | Mean_Square | Omega2 | Omega2 50% CI | Eta2 |  Eta2 50% CI | Epsilon2 | Epsilon2 50% CI | Sum_Squares_Error | Mean_Square_Error
      ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      factor(cyl) | 39.70 |  2 |         29 | < .001 |      824.78 |      412.39 |   0.82 |  [0.78, 0.85] | 0.84 | [0.80, 0.86] |     0.83 |    [0.79, 0.85] |            301.26 |            301.26
      
      # disp response
      
      Parameter   |     F | df | df (error) |      p | Sum_Squares | Mean_Square | Omega2 | Omega2 50% CI | Eta2 |  Eta2 50% CI | Epsilon2 | Epsilon2 50% CI | Sum_Squares_Error | Mean_Square_Error
      ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      factor(cyl) | 74.83 |  2 |         29 | < .001 |    3.99e+05 |    1.99e+05 |   0.69 |  [0.62, 0.73] | 0.71 | [0.65, 0.75] |     0.69 |    [0.63, 0.73] |          41696.33 |          41696.33
      
      # hp response
      
      Parameter   |     F | df | df (error) |      p | Sum_Squares | Mean_Square | Omega2 | Omega2 50% CI | Eta2 |  Eta2 50% CI | Epsilon2 | Epsilon2 50% CI | Sum_Squares_Error | Mean_Square_Error
      ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      factor(cyl) | 36.18 |  2 |         29 | < .001 |    1.04e+05 |    52015.27 |   0.71 |  [0.64, 0.75] | 0.73 | [0.67, 0.77] |     0.71 |    [0.65, 0.75] |          77293.83 |          77293.83
      
      Anova Table (Type 1 tests)

