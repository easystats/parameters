# print-model_parameters glmmTMB

    Code
      out[-5]
    Output
      [1] "# Fixed Effects"                                                 
      [2] ""                                                                
      [3] "Parameter   | Log-Mean |   SE |         95% CI |      z |      p"
      [4] "----------------------------------------------------------------"
      [5] "child       |    -1.09 | 0.10 | [-1.28, -0.90] | -11.09 | < .001"
      [6] "camper [1]  |     0.27 | 0.10 | [ 0.07,  0.47] |   2.70 | 0.007 "

---

    Code
      print(mp)
    Output
      # Random Effects
      
      Parameter                   | Coefficient |        95% CI
      ---------------------------------------------------------
      SD (Intercept: persons)     |        3.41 | [ 1.68, 6.92]
      SD (xb: persons)            |        1.21 | [ 0.60, 2.44]
      Cor (Intercept~xb: persons) |       -1.00 | [-1.00, 1.00]
    Message <simpleMessage>
      
       Uncertainty intervals for random effect variances computed using a
        Wald z-distribution approximation.

---

    Code
      out[-6]
    Output
      [1] "# Fixed Effects (Zero-Inflation Component)"                   
      [2] ""                                                             
      [3] "Parameter   | Log-Mean |   SE |        95% CI |     z |     p"
      [4] "-------------------------------------------------------------"
      [5] "(Intercept) |     1.89 | 0.66 | [ 0.59, 3.19] |  2.85 | 0.004"
      [6] "camper [1]  |    -0.17 | 0.39 | [-0.93, 0.59] | -0.44 | 0.660"

---

    Code
      print(mp)
    Output
      # Random Effects (Zero-Inflation Component)
      
      Parameter                   | Coefficient |       95% CI
      --------------------------------------------------------
      SD (Intercept: persons)     |        2.74 | [1.17, 6.40]
      SD (zg: persons)            |        1.57 | [0.65, 3.81]
      Cor (Intercept~zg: persons) |        1.00 |             
    Message <simpleMessage>
      
       Uncertainty intervals for random effect variances computed using a
        Wald z-distribution approximation.

---

    Code
      out[-5]
    Output
       [1] "# Fixed Effects"                                                 
       [2] ""                                                                
       [3] "Parameter   | Log-Mean |   SE |         95% CI |      z |      p"
       [4] "----------------------------------------------------------------"
       [5] "child       |    -1.09 | 0.10 | [-1.28, -0.90] | -11.09 | < .001"
       [6] "camper [1]  |     0.27 | 0.10 | [ 0.07,  0.47] |   2.70 | 0.007 "
       [7] ""                                                                
       [8] "# Random Effects"                                                
       [9] ""                                                                
      [10] "Parameter                   | Coefficient |        95% CI"       
      [11] "---------------------------------------------------------"       
      [12] "SD (Intercept: persons)     |        3.41 | [ 1.68, 6.92]"       
      [13] "SD (xb: persons)            |        1.21 | [ 0.60, 2.44]"       
      [14] "Cor (Intercept~xb: persons) |       -1.00 | [-1.00, 1.00]"       

---

    Code
      out[-6]
    Output
       [1] "# Fixed Effects (Zero-Inflation Component)"                   
       [2] ""                                                             
       [3] "Parameter   | Log-Mean |   SE |        95% CI |     z |     p"
       [4] "-------------------------------------------------------------"
       [5] "(Intercept) |     1.89 | 0.66 | [ 0.59, 3.19] |  2.85 | 0.004"
       [6] "camper [1]  |    -0.17 | 0.39 | [-0.93, 0.59] | -0.44 | 0.660"
       [7] ""                                                             
       [8] "# Random Effects (Zero-Inflation Component)"                  
       [9] ""                                                             
      [10] "Parameter                   | Coefficient |       95% CI"     
      [11] "--------------------------------------------------------"     
      [12] "SD (Intercept: persons)     |        2.74 | [1.17, 6.40]"     
      [13] "SD (zg: persons)            |        1.57 | [0.65, 3.81]"     
      [14] "Cor (Intercept~zg: persons) |        1.00 |             "     

---

    Code
      out[-c(5, 14)]
    Output
       [1] "# Fixed Effects (Count Model)"                                   
       [2] ""                                                                
       [3] "Parameter   | Log-Mean |   SE |         95% CI |      z |      p"
       [4] "----------------------------------------------------------------"
       [5] "child       |    -1.09 | 0.10 | [-1.28, -0.90] | -11.09 | < .001"
       [6] "camper [1]  |     0.27 | 0.10 | [ 0.07,  0.47] |   2.70 | 0.007 "
       [7] ""                                                                
       [8] "# Fixed Effects (Zero-Inflation Component)"                      
       [9] ""                                                                
      [10] "Parameter   | Log-Odds |   SE |        95% CI |     z |     p"   
      [11] "-------------------------------------------------------------"   
      [12] "(Intercept) |     1.89 | 0.66 | [ 0.59, 3.19] |  2.85 | 0.004"   
      [13] "camper [1]  |    -0.17 | 0.39 | [-0.93, 0.59] | -0.44 | 0.660"   
      [14] ""                                                                
      [15] "# Random Effects Variances"                                      
      [16] ""                                                                
      [17] "Parameter                   | Coefficient |        95% CI"       
      [18] "---------------------------------------------------------"       
      [19] "SD (Intercept: persons)     |        3.41 | [ 1.68, 6.92]"       
      [20] "SD (xb: persons)            |        1.21 | [ 0.60, 2.44]"       
      [21] "Cor (Intercept~xb: persons) |       -1.00 | [-1.00, 1.00]"       
      [22] ""                                                                
      [23] "# Random Effects (Zero-Inflation Component)"                     
      [24] ""                                                                
      [25] "Parameter                   | Coefficient |       95% CI"        
      [26] "--------------------------------------------------------"        
      [27] "SD (Intercept: persons)     |        2.74 | [1.17, 6.40]"        
      [28] "SD (zg: persons)            |        1.57 | [0.65, 3.81]"        
      [29] "Cor (Intercept~zg: persons) |        1.00 |             "        

# print-model_parameters glmmTMB digits

    Code
      out[-c(5, 14)]
    Output
       [1] "# Fixed Effects (Count Model)"                                             
       [2] ""                                                                          
       [3] "Parameter   | Log-Mean |     SE |               95% CI |        z |      p"
       [4] "--------------------------------------------------------------------------"
       [5] "child       |  -1.0875 | 0.0981 | [-1.27966, -0.89529] | -11.0903 | < .001"
       [6] "camper [1]  |   0.2723 | 0.1009 | [ 0.07462,  0.46998] |   2.6997 | 0.007 "
       [7] ""                                                                          
       [8] "# Fixed Effects (Zero-Inflation Component)"                                
       [9] ""                                                                          
      [10] "Parameter   | Log-Odds |     SE |              95% CI |       z |     p"   
      [11] "-----------------------------------------------------------------------"   
      [12] "(Intercept) |   1.8896 | 0.6642 | [ 0.58788, 3.19139] |  2.8451 | 0.004"   
      [13] "camper [1]  |  -0.1701 | 0.3868 | [-0.92809, 0.58796] | -0.4397 | 0.660"   
      [14] ""                                                                          
      [15] "# Random Effects Variances"                                                
      [16] ""                                                                          
      [17] "Parameter                   | Coefficient |              95% CI"           
      [18] "---------------------------------------------------------------"           
      [19] "SD (Intercept: persons)     |      3.4056 | [ 1.67675, 6.91715]"           
      [20] "SD (xb: persons)            |      1.2132 | [ 0.60312, 2.44025]"           
      [21] "Cor (Intercept~xb: persons) |     -1.0000 | [-1.00000, 1.00000]"           
      [22] ""                                                                          
      [23] "# Random Effects (Zero-Inflation Component)"                               
      [24] ""                                                                          
      [25] "Parameter                   | Coefficient |             95% CI"            
      [26] "--------------------------------------------------------------"            
      [27] "SD (Intercept: persons)     |      2.7358 | [1.16901, 6.40268]"            
      [28] "SD (zg: persons)            |      1.5683 | [0.64628, 3.80586]"            
      [29] "Cor (Intercept~zg: persons) |      1.0000 |                   "            

---

    Code
      out[-c(5, 14)]
    Output
       [1] "# Fixed Effects (Count Model)"                                             
       [2] ""                                                                          
       [3] "Parameter   | Log-Mean |     SE |               95% CI |        z |      p"
       [4] "--------------------------------------------------------------------------"
       [5] "child       |  -1.0875 | 0.0981 | [-1.27966, -0.89529] | -11.0903 | < .001"
       [6] "camper [1]  |   0.2723 | 0.1009 | [ 0.07462,  0.46998] |   2.6997 | 0.007 "
       [7] ""                                                                          
       [8] "# Fixed Effects (Zero-Inflation Component)"                                
       [9] ""                                                                          
      [10] "Parameter   | Log-Odds |     SE |              95% CI |       z |     p"   
      [11] "-----------------------------------------------------------------------"   
      [12] "(Intercept) |   1.8896 | 0.6642 | [ 0.58788, 3.19139] |  2.8451 | 0.004"   
      [13] "camper [1]  |  -0.1701 | 0.3868 | [-0.92809, 0.58796] | -0.4397 | 0.660"   
      [14] ""                                                                          
      [15] "# Random Effects Variances"                                                
      [16] ""                                                                          
      [17] "Parameter                   | Coefficient |              95% CI"           
      [18] "---------------------------------------------------------------"           
      [19] "SD (Intercept: persons)     |      3.4056 | [ 1.67675, 6.91715]"           
      [20] "SD (xb: persons)            |      1.2132 | [ 0.60312, 2.44025]"           
      [21] "Cor (Intercept~xb: persons) |     -1.0000 | [-1.00000, 1.00000]"           
      [22] ""                                                                          
      [23] "# Random Effects (Zero-Inflation Component)"                               
      [24] ""                                                                          
      [25] "Parameter                   | Coefficient |             95% CI"            
      [26] "--------------------------------------------------------------"            
      [27] "SD (Intercept: persons)     |      2.7358 | [1.16901, 6.40268]"            
      [28] "SD (zg: persons)            |      1.5683 | [0.64628, 3.80586]"            
      [29] "Cor (Intercept~zg: persons) |      1.0000 |                   "            

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
    Message <simpleMessage>
      
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
    Message <simpleMessage>
      
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

