# multiple model

    Code
      print(out)
    Output
      Parameter                           |               lm1 |                  lm2 |                  lm3
      -----------------------------------------------------------------------------------------------------
      (Intercept)                         | 5.01 (4.86, 5.15) |  3.68 ( 3.47,  3.89) |  4.21 ( 3.41,  5.02)
      Species (versicolor)                | 0.93 (0.73, 1.13) | -1.60 (-1.98, -1.22) | -1.81 (-2.99, -0.62)
      Species (virginica)                 | 1.58 (1.38, 1.79) | -2.12 (-2.66, -1.58) | -3.15 (-4.41, -1.90)
      Petal Length                        |                   |  0.90 ( 0.78,  1.03) |  0.54 ( 0.00,  1.09)
      Species (versicolor) * Petal Length |                   |                      |  0.29 (-0.30,  0.87)
      Species (virginica) * Petal Length  |                   |                      |  0.45 (-0.12,  1.03)
      -----------------------------------------------------------------------------------------------------
      Observations                        |               150 |                  150 |                  150

# templates

    Code
      print(out)
    Output
      Parameter                           |            lm1 |             lm2 |             lm3
      ----------------------------------------------------------------------------------------
      (Intercept)                         | 5.01*** (0.07) |  3.68*** (0.11) |  4.21*** (0.41)
      Species (versicolor)                | 0.93*** (0.10) | -1.60*** (0.19) | -1.81 ** (0.60)
      Species (virginica)                 | 1.58*** (0.10) | -2.12*** (0.27) | -3.15*** (0.63)
      Petal Length                        |                |  0.90*** (0.06) |     0.54 (0.28)
      Species (versicolor) * Petal Length |                |                 |     0.29 (0.30)
      Species (virginica) * Petal Length  |                |                 |     0.45 (0.29)
      ----------------------------------------------------------------------------------------
      Observations                        |            150 |             150 |             150

# templates, glue-1

    Code
      print(out)
    Output
      Parameter                           |            lm1 |             lm2 |             lm3
      ----------------------------------------------------------------------------------------
      (Intercept)                         | 5.01*** (0.07) |  3.68*** (0.11) |  4.21*** (0.41)
      Species (versicolor)                | 0.93*** (0.10) | -1.60*** (0.19) | -1.81 ** (0.60)
      Species (virginica)                 | 1.58*** (0.10) | -2.12*** (0.27) | -3.15*** (0.63)
      Petal Length                        |                |  0.90*** (0.06) |     0.54 (0.28)
      Species (versicolor) * Petal Length |                |                 |     0.29 (0.30)
      Species (virginica) * Petal Length  |                |                 |     0.45 (0.29)
      ----------------------------------------------------------------------------------------
      Observations                        |            150 |             150 |             150

# templates, glue-2

    Code
      print(out)
    Output
      Parameter                           |                           lm1 |                              lm2 |                              lm3
      -----------------------------------------------------------------------------------------------------------------------------------------
      (Intercept)                         | 5.01 (4.86, 5.15), p<0.001*** |  3.68 ( 3.47,  3.89), p<0.001*** |  4.21 ( 3.41,  5.02), p<0.001***
      Species (versicolor)                | 0.93 (0.73, 1.13), p<0.001*** | -1.60 (-1.98, -1.22), p<0.001*** | -1.81 (-2.99, -0.62), p=0.003 **
      Species (virginica)                 | 1.58 (1.38, 1.79), p<0.001*** | -2.12 (-2.66, -1.58), p<0.001*** | -3.15 (-4.41, -1.90), p<0.001***
      Petal Length                        |                               |  0.90 ( 0.78,  1.03), p<0.001*** |     0.54 ( 0.00,  1.09), p=0.052
      Species (versicolor) * Petal Length |                               |                                  |     0.29 (-0.30,  0.87), p=0.334
      Species (virginica) * Petal Length  |                               |                                  |     0.45 (-0.12,  1.03), p=0.120
      -----------------------------------------------------------------------------------------------------------------------------------------
      Observations                        |                           150 |                              150 |                              150

# templates, glue-3, separate columnns

    Code
      print(out)
    Output
      Parameter                           | Estimate (SE) (lm1) | p (lm1) | Estimate (SE) (lm2) | p (lm2) | Estimate (SE) (lm3) | p (lm3)
      -----------------------------------------------------------------------------------------------------------------------------------
      (Intercept)                         |         5.01 (0.07) |  <0.001 |         3.68 (0.11) |  <0.001 |         4.21 (0.41) |  <0.001
      Species (versicolor)                |         0.93 (0.10) |  <0.001 |        -1.60 (0.19) |  <0.001 |        -1.81 (0.60) |   0.003
      Species (virginica)                 |         1.58 (0.10) |  <0.001 |        -2.12 (0.27) |  <0.001 |        -3.15 (0.63) |  <0.001
      Petal Length                        |                     |         |         0.90 (0.06) |  <0.001 |         0.54 (0.28) |   0.052
      Species (versicolor) * Petal Length |                     |         |                     |         |         0.29 (0.30) |   0.334
      Species (virginica) * Petal Length  |                     |         |                     |         |         0.45 (0.29) |   0.120
      -----------------------------------------------------------------------------------------------------------------------------------
      Observations                        |                 150 |         |                 150 |         |                 150 |        

---

    Code
      print(out, groups = list(Species = c("Species (versicolor)",
        "Species (virginica)"), Interactions = c(
        "Species (versicolor) * Petal Length", "Species (virginica) * Petal Length"),
      Controls = "Petal Length"))
    Output
      Parameter                             |                  lm1 |                  lm2
      -----------------------------------------------------------------------------------
      Species                               |                      |                     
        Species (versicolor)                | -1.60 (-1.98, -1.22) | -1.69 (-2.80, -0.57)
        Species (virginica)                 | -2.12 (-2.66, -1.58) | -1.19 (-2.37, -0.01)
      Interactions                          |                      |                     
        Species (versicolor) * Petal Length |                      | -0.01 (-0.56,  0.53)
        Species (virginica) * Petal Length  |                      | -0.15 (-0.69,  0.39)
      Controls                              |                      |                     
        Petal Length                        |  0.90 ( 0.78,  1.03) |  0.39 (-0.13,  0.90)
      -----------------------------------------------------------------------------------
        Observations                        |                  150 |                  150

---

    Code
      print(out, groups = list(Species = c("Species (versicolor)",
        "Species (virginica)"), Interactions = c(
        "Species (versicolor) * Petal Length", "Species (virginica) * Petal Length"),
      Controls = "Petal Length"), select = "{estimate}{stars}")
    Output
      Parameter                             |      lm1 |     lm2
      ----------------------------------------------------------
      Species                               |          |        
        Species (versicolor)                | -1.60*** | -1.69**
        Species (virginica)                 | -2.12*** | -1.19 *
      Interactions                          |          |        
        Species (versicolor) * Petal Length |          |   -0.01
        Species (virginica) * Petal Length  |          |   -0.15
      Controls                              |          |        
        Petal Length                        |  0.90*** |    0.39
      ----------------------------------------------------------
        Observations                        |      150 |     150

---

    Code
      print(out, groups = list(Species = c("Species (versicolor)",
        "Species (virginica)"), Interactions = c(
        "Species (versicolor) * Petal Length", "Species (virginica) * Petal Length"),
      Controls = "Petal Length"), select = "{estimate}|{p}")
    Output
      Parameter                             | Estimate (lm1) | p (lm1) | Estimate (lm2) | p (lm2)
      -------------------------------------------------------------------------------------------
      Species                               |                |         |                |        
        Species (versicolor)                |          -1.60 |  <0.001 |          -1.69 |   0.003
        Species (virginica)                 |          -2.12 |  <0.001 |          -1.19 |   0.048
      Interactions                          |                |         |                |        
        Species (versicolor) * Petal Length |                |         |          -0.01 |   0.961
        Species (virginica) * Petal Length  |                |         |          -0.15 |   0.574
      Controls                              |                |         |                |        
        Petal Length                        |           0.90 |  <0.001 |           0.39 |   0.138
      -------------------------------------------------------------------------------------------
        Observations                        |            150 |         |            150 |        

# combination of different models

    Code
      print(cp)
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

