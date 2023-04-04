# compare_parameters, proper printing for CI=NULL #820

    Code
      compare_parameters(m1, m2, ci = NULL)
    Output
      Parameter    |    m1 |    m2
      ----------------------------
      (Intercept)  | 30.10 | 30.10
      hp           | -0.07 | -0.07
      ----------------------------
      Observations |    32 |    32

# compare_parameters, correct random effects

    Code
      cp
    Output
      # Fixed Effects
      
      Parameter   |                   m0 |                   m1 |                   m2
      --------------------------------------------------------------------------------
      (Intercept) |  0.91 ( 0.75,  1.07) |  0.68 (-0.54,  1.91) |  1.41 ( 1.06,  1.75)
      child       | -1.23 (-1.39, -1.08) | -1.67 (-1.84, -1.51) | -0.53 (-0.77, -0.29)
      camper (1)  |  1.05 ( 0.88,  1.23) |  0.94 ( 0.77,  1.12) |  0.58 ( 0.39,  0.78)
      zg          |                      |                      |  0.13 ( 0.05,  0.21)
      
      # Fixed Effects (Zero-Inflation Component)
      
      Parameter   | m0 | m1 |                   m2
      --------------------------------------------
      (Intercept) |    |    | -0.92 (-2.07,  0.22)
      child       |    |    |  1.96 ( 1.38,  2.54)
      
      # Random Effects
      
      Parameter               | m0 |                   m1 |                   m2
      --------------------------------------------------------------------------
      SD (Intercept: ID)      |    |  0.27 ( 0.11,  0.63) |  0.28 ( 0.13,  0.60)
      SD (Intercept: persons) |    |  1.21 ( 0.60,  2.43) |                     
      
      # Random Effects (Zero-Inflation Component)
      
      Parameter               | m0 | m1 |                   m2
      --------------------------------------------------------
      SD (Intercept: persons) |    |    |  1.08 ( 0.49,  2.37)

