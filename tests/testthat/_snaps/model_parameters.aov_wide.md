# anova wide table - aov

    Code
      model_parameters(model, omega_squared = "partial", eta_squared = "partial",
        epsilon_squared = TRUE, ci = 0.9, table_wide = TRUE, verbose = FALSE)
    Output
      Parameter |     F | df | df (error) |      p | Sum_Squares | Sum_Squares_Error | Mean_Square | Mean_Square_Error | Omega2 | Omega2 90% CI | Eta2 |  Eta2 90% CI | Epsilon2 | Epsilon2 90% CI
      --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      Species   | 49.16 |  2 |        147 | < .001 |       11.34 |             16.96 |        5.67 |             16.96 |   0.39 |  [0.29, 0.48] | 0.40 | [0.30, 0.48] |     0.39 |    [0.29, 0.48]
      
      Anova Table (Type 1 tests)

# anova wide table - aovlist

    Code
      model_parameters(mod, omega_squared = "partial", eta_squared = "partial",
        table_wide = TRUE, verbose = FALSE)
    Output
      # Within
      
      Parameter |     F | df | df (error) |     p | Sum_Squares | Sum_Squares_Error | Mean_Square | Mean_Square_Error | Omega2 (partial) | Eta2 (partial)
      ---------------------------------------------------------------------------------------------------------------------------------------------------
      N         | 12.26 |  1 |         12 | 0.004 |      189.28 |            185.29 |      189.28 |            185.29 |             0.23 |           0.51
      P         |  0.54 |  1 |         12 | 0.475 |        8.40 |            185.29 |        8.40 |            185.29 |            -0.01 |           0.04
      K         |  6.17 |  1 |         12 | 0.029 |       95.20 |            185.29 |       95.20 |            185.29 |             0.12 |           0.34
      N:P       |  1.38 |  1 |         12 | 0.263 |       21.28 |            185.29 |       21.28 |            185.29 |         9.91e-03 |           0.10
      N:K       |  2.15 |  1 |         12 | 0.169 |       33.14 |            185.29 |       33.14 |            185.29 |             0.03 |           0.15
      P:K       |  0.03 |  1 |         12 | 0.863 |        0.48 |            185.29 |        0.48 |            185.29 |            -0.03 |       2.59e-03
      
      # block
      
      Parameter |    F | df | df (error) |     p | Sum_Squares | Sum_Squares_Error | Mean_Square | Mean_Square_Error | Omega2 (partial) | Eta2 (partial)
      --------------------------------------------------------------------------------------------------------------------------------------------------
      N:P:K     | 0.48 |  1 |          4 | 0.525 |       37.00 |            306.29 |       37.00 |            306.29 |            -0.09 |           0.11
      
      Anova Table (Type 1 tests)

# anova wide table - manova

    Code
      model_parameters(m_manova, omega_squared = "raw", eta_squared = "partial",
        epsilon_squared = "raw", ci = 0.99, table_wide = TRUE, verbose = FALSE)
    Error <simpleError>
      undefined columns selected

# anova wide table - Gam

    Code
      model_parameters(mod_gam, eta_squared = "raw", epsilon_squared = "partial", ci = 0.9,
        table_wide = TRUE, verbose = FALSE)
    Output
      Parameter |        F | df | df (error) |      p | Sum_Squares | Sum_Squares_Error | Mean_Square | Mean_Square_Error |     Eta2 |  Eta2 90% CI | Epsilon2 (partial) | Epsilon2 90% CI
      ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      s(hp, 4)  |    94.38 |  1 |      25.00 | < .001 |      678.37 |            179.68 |      678.37 |            179.68 |     0.70 | [0.52, 0.80] |               0.78 |    [0.64, 0.86]
      am        |    15.66 |  1 |      25.00 | < .001 |      112.58 |            179.68 |      112.58 |            179.68 |     0.12 | [0.00, 0.33] |               0.36 |    [0.12, 0.55]
      qsec      | 3.66e-03 |  1 |      25.00 | 0.952  |        0.03 |            179.68 |        0.03 |            179.68 | 2.71e-05 | [0.00, 0.00] |              -0.04 |    [0.00, 0.00]
      
      Anova Table (Type 1 tests)

# anova wide table - maov

    Code
      model_parameters(m_maov, omega_squared = "raw", eta_squared = "partial",
        epsilon_squared = "raw", ci = 0.5, table_wide = TRUE, verbose = FALSE)
    Output
      # mpg response
      
      Parameter   |     F | df | df (error) |      p | Sum_Squares | Sum_Squares_Error | Mean_Square | Mean_Square_Error | Omega2 | Omega2 50% CI | Eta2 |  Eta2 50% CI | Epsilon2 | Epsilon2 50% CI
      ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      factor(cyl) | 39.70 |  2 |         29 | < .001 |      824.78 |            301.26 |      412.39 |            301.26 |   0.82 |  [0.78, 0.85] | 0.84 | [0.80, 0.86] |     0.83 |    [0.79, 0.85]
      
      # disp response
      
      Parameter   |     F | df | df (error) |      p | Sum_Squares | Sum_Squares_Error | Mean_Square | Mean_Square_Error | Omega2 | Omega2 50% CI | Eta2 |  Eta2 50% CI | Epsilon2 | Epsilon2 50% CI
      ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      factor(cyl) | 74.83 |  2 |         29 | < .001 |    3.99e+05 |          41696.33 |    1.99e+05 |          41696.33 |   0.69 |  [0.62, 0.73] | 0.71 | [0.65, 0.75] |     0.69 |    [0.63, 0.73]
      
      # hp response
      
      Parameter   |     F | df | df (error) |      p | Sum_Squares | Sum_Squares_Error | Mean_Square | Mean_Square_Error | Omega2 | Omega2 50% CI | Eta2 |  Eta2 50% CI | Epsilon2 | Epsilon2 50% CI
      ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      factor(cyl) | 36.18 |  2 |         29 | < .001 |    1.04e+05 |          77293.83 |    52015.27 |          77293.83 |   0.71 |  [0.64, 0.75] | 0.73 | [0.67, 0.77] |     0.71 |    [0.65, 0.75]
      
      Anova Table (Type 1 tests)

