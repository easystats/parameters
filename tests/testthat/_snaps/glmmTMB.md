# print-model_parameters glmmTMB

    Code
      out[-5]
    Output
      [1] "# Fixed Effects"                                                 
      [2] ""                                                                
      [3] "Parameter   | Log-Mean |   SE |         95% CI |      z |      p"
      [4] "----------------------------------------------------------------"
      [5] "child       |    -1.04 | 0.09 | [-1.22, -0.87] | -11.60 | < .001"
      [6] "camper [1]  |     0.27 | 0.09 | [ 0.08,  0.45] |   2.83 | 0.005 "

# print-model_parameters glmmTMB digits

    Code
      out[-c(5, 14)]
    Output
       [1] "# Fixed Effects (Count Model)"                                                
       [2] ""                                                                             
       [3] "Parameter   | Log-Mean |     SE |               95% CI |        z |      p"   
       [4] "--------------------------------------------------------------------------"   
       [5] "child       |  -1.0426 | 0.0899 | [-1.21878, -0.86645] | -11.5996 | < .001"   
       [6] "camper [1]  |   0.2684 | 0.0948 | [ 0.08262,  0.45421] |   2.8315 | 0.005 "   
       [7] ""                                                                             
       [8] "# Fixed Effects (Zero-Inflation Component)"                                   
       [9] ""                                                                             
      [10] "Parameter   | Log-Odds |     SE |               95% CI |           z |      p"
      [11] "-----------------------------------------------------------------------------"
      [12] "(Intercept) |  -4.0420 | 0.2038 | [-4.44154, -3.64250] |    -19.8292 | < .001"
      [13] "camper [1]  |  -4.8290 | 0.0002 | [-4.82949, -4.82861] | -21474.3719 | < .001"
      [14] ""                                                                             
      [15] "# Random Effects Variances"                                                   
      [16] ""                                                                             
      [17] "Parameter                   | Coefficient |             95% CI"               
      [18] "--------------------------------------------------------------"               
      [19] "SD (Intercept: persons)     |      2.1800 | [1.15990, 4.09711]"               
      [20] "SD (xb: persons)            |      1.0592 | [0.59457, 1.88686]"               
      [21] "Cor (Intercept~xb: persons) |     -0.9883 |                   "               
      [22] ""                                                                             
      [23] "# Random Effects (Zero-Inflation Component)"                                  
      [24] ""                                                                             
      [25] "Parameter                   | Coefficient |               95% CI"             
      [26] "----------------------------------------------------------------"             
      [27] "SD (Intercept: persons)     |      5.8635 | [ 3.83067,  8.97520]"             
      [28] "SD (zg: persons)            |     12.5180 | [10.94956, 14.31115]"             
      [29] "Cor (Intercept~zg: persons) |      0.9790 |                     "             

---

    Code
      out[-c(5, 14)]
    Output
       [1] "# Fixed Effects (Count Model)"                                                
       [2] ""                                                                             
       [3] "Parameter   | Log-Mean |     SE |               95% CI |        z |      p"   
       [4] "--------------------------------------------------------------------------"   
       [5] "child       |  -1.0426 | 0.0899 | [-1.21878, -0.86645] | -11.5996 | < .001"   
       [6] "camper [1]  |   0.2684 | 0.0948 | [ 0.08262,  0.45421] |   2.8315 | 0.005 "   
       [7] ""                                                                             
       [8] "# Fixed Effects (Zero-Inflation Component)"                                   
       [9] ""                                                                             
      [10] "Parameter   | Log-Odds |     SE |               95% CI |           z |      p"
      [11] "-----------------------------------------------------------------------------"
      [12] "(Intercept) |  -4.0420 | 0.2038 | [-4.44154, -3.64250] |    -19.8292 | < .001"
      [13] "camper [1]  |  -4.8290 | 0.0002 | [-4.82949, -4.82861] | -21474.3719 | < .001"
      [14] ""                                                                             
      [15] "# Random Effects Variances"                                                   
      [16] ""                                                                             
      [17] "Parameter                   | Coefficient |             95% CI"               
      [18] "--------------------------------------------------------------"               
      [19] "SD (Intercept: persons)     |      2.1800 | [1.15990, 4.09711]"               
      [20] "SD (xb: persons)            |      1.0592 | [0.59457, 1.88686]"               
      [21] "Cor (Intercept~xb: persons) |     -0.9883 |                   "               
      [22] ""                                                                             
      [23] "# Random Effects (Zero-Inflation Component)"                                  
      [24] ""                                                                             
      [25] "Parameter                   | Coefficient |               95% CI"             
      [26] "----------------------------------------------------------------"             
      [27] "SD (Intercept: persons)     |      5.8635 | [ 3.83067,  8.97520]"             
      [28] "SD (zg: persons)            |     12.5180 | [10.94956, 14.31115]"             
      [29] "Cor (Intercept~zg: persons) |      0.9790 |                     "             

# print-model_parameters glmmTMB CI alignment

    Code
      print(mp)
    Output
      # Random Effects: conditional
      
      Parameter                           | Coefficient |       95% CI
      ----------------------------------------------------------------
      SD (Intercept: Session:Participant) |        0.27 | [0.08, 0.87]
      SD (Intercept: Participant)         |        0.38 | [0.16, 0.92]
      
      # Random Effects: zero_inflated
      
      Parameter                           | Coefficient |       95% CI
      ----------------------------------------------------------------
      SD (Intercept: Session:Participant) |        0.69 | [0.40, 1.19]
      SD (Intercept: Participant)         |        2.39 | [1.25, 4.57]
    Message
      
       Uncertainty intervals for random effect variances computed using a
        Wald z-distribution approximation.

---

    Code
      print(mp)
    Output
      # Fixed Effects
      
      Parameter          | Log-Mean |   SE |        95% CI |     z |      p
      ---------------------------------------------------------------------
      (Intercept)        |     2.12 | 0.30 | [ 1.53, 2.71] |  7.05 | < .001
      Surface [Lingual]  |     0.01 | 0.29 | [-0.56, 0.58] |  0.04 | 0.971 
      Surface [Occlusal] |     0.54 | 0.22 | [ 0.10, 0.98] |  2.43 | 0.015 
      Side [Anterior]    |     0.04 | 0.32 | [-0.58, 0.66] |  0.14 | 0.889 
      Side [Left]        |    -0.04 | 0.20 | [-0.44, 0.37] | -0.17 | 0.862 
      Jaw [Maxillar]     |    -0.10 | 0.21 | [-0.51, 0.30] | -0.51 | 0.612 
      
      # Zero-Inflation
      
      Parameter          | Log-Odds |   SE |         95% CI |     z |      p
      ----------------------------------------------------------------------
      (Intercept)        |     4.87 | 0.93 | [ 3.04,  6.69] |  5.23 | < .001
      Surface [Lingual]  |     0.93 | 0.34 | [ 0.27,  1.60] |  2.75 | 0.006 
      Surface [Occlusal] |    -1.01 | 0.29 | [-1.59, -0.44] | -3.45 | < .001
      Side [Anterior]    |    -0.20 | 0.37 | [-0.93,  0.52] | -0.55 | 0.583 
      Side [Left]        |    -0.38 | 0.27 | [-0.91,  0.14] | -1.44 | 0.151 
      Jaw [Maxillar]     |     0.59 | 0.24 | [ 0.11,  1.07] |  2.42 | 0.016 
      
      # Dispersion
      
      Parameter   | Coefficient |       95% CI
      ----------------------------------------
      (Intercept) |        2.06 | [1.30, 3.27]
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

# print-model_parameters

    Code
      mp
    Output
      # Fixed Effects
      
      Parameter   | Log-Mean |   SE |         95% CI |      z |      p
      ----------------------------------------------------------------
      (Intercept) |     1.26 | 0.48 | [ 0.33,  2.19] |   2.66 | 0.008 
      child       |    -1.14 | 0.09 | [-1.32, -0.96] | -12.27 | < .001
      camper [1]  |     0.73 | 0.09 | [ 0.55,  0.92] |   7.85 | < .001
      
      # Zero-Inflation
      
      Parameter   | Log-Odds |   SE |         95% CI |     z |      p
      ---------------------------------------------------------------
      (Intercept) |    -0.39 | 0.65 | [-1.67,  0.89] | -0.60 | 0.551 
      child       |     2.05 | 0.31 | [ 1.45,  2.66] |  6.63 | < .001
      camper [1]  |    -1.01 | 0.32 | [-1.64, -0.37] | -3.12 | 0.002 

---

    Code
      mp
    Output
      # Fixed Effects
      
      Parameter   |  IRR |   SE |       95% CI |      z |      p
      ----------------------------------------------------------
      (Intercept) | 3.54 | 1.68 | [1.39, 8.98] |   2.66 | 0.008 
      child       | 0.32 | 0.03 | [0.27, 0.38] | -12.27 | < .001
      camper [1]  | 2.08 | 0.19 | [1.73, 2.50] |   7.85 | < .001
      
      # Zero-Inflation
      
      Parameter   | Odds Ratio |   SE |        95% CI |     z |      p
      ----------------------------------------------------------------
      (Intercept) |       0.68 | 0.44 | [0.19,  2.43] | -0.60 | 0.551 
      child       |       7.80 | 2.42 | [4.25, 14.32] |  6.63 | < .001
      camper [1]  |       0.36 | 0.12 | [0.19,  0.69] | -3.12 | 0.002 

---

    Code
      mp
    Output
      # Fixed Effects (Count Model)
      
      Parameter   | Log-Mean |   SE |         95% CI |      z |      p
      ----------------------------------------------------------------
      (Intercept) |     1.26 | 0.48 | [ 0.33,  2.19] |   2.66 | 0.008 
      child       |    -1.14 | 0.09 | [-1.32, -0.96] | -12.27 | < .001
      camper [1]  |     0.73 | 0.09 | [ 0.55,  0.92] |   7.85 | < .001
      
      # Fixed Effects (Zero-Inflation Component)
      
      Parameter   | Log-Odds |   SE |         95% CI |     z |      p
      ---------------------------------------------------------------
      (Intercept) |    -0.39 | 0.65 | [-1.67,  0.89] | -0.60 | 0.551 
      child       |     2.05 | 0.31 | [ 1.45,  2.66] |  6.63 | < .001
      camper [1]  |    -1.01 | 0.32 | [-1.64, -0.37] | -3.12 | 0.002 
      
      # Random Effects Variances
      
      Parameter               | Coefficient |       95% CI
      ----------------------------------------------------
      SD (Intercept: persons) |        0.93 | [0.46, 1.89]
      
      # Random Effects (Zero-Inflation Component)
      
      Parameter               | Coefficient |       95% CI
      ----------------------------------------------------
      SD (Intercept: persons) |        1.17 | [0.54, 2.57]

# robust SE/VCOV

    Code
      print(out, table_width = Inf)
    Output
      # Fixed Effects (Count Model)
      
      Parameter   | Log-Mean |   SE |         95% CI |     z |      p
      ---------------------------------------------------------------
      (Intercept) |     1.26 | 0.46 | [ 0.36,  2.17] |  2.74 | 0.006 
      child       |    -1.14 | 0.30 | [-1.73, -0.55] | -3.80 | < .001
      camper [1]  |     0.73 | 0.41 | [-0.06,  1.53] |  1.80 | 0.072 
      
      # Fixed Effects (Zero-Inflation Component)
      
      Parameter   | Log-Odds |   SE |         95% CI |     z |      p
      ---------------------------------------------------------------
      (Intercept) |    -0.39 | 0.58 | [-1.52,  0.75] | -0.67 | 0.501 
      child       |     2.05 | 0.39 | [ 1.30,  2.81] |  5.33 | < .001
      camper [1]  |    -1.01 | 0.39 | [-1.77, -0.24] | -2.59 | 0.010 
      
      # Random Effects Variances
      
      Parameter               | Coefficient |       95% CI
      ----------------------------------------------------
      SD (Intercept: persons) |        0.93 | [0.46, 1.89]
      
      # Random Effects (Zero-Inflation Component)
      
      Parameter               | Coefficient |       95% CI
      ----------------------------------------------------
      SD (Intercept: persons) |        1.17 | [0.54, 2.57]
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald z-distribution approximation.

