# print-model_parameters

    Code
      print(mp)
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
      print(mp)
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
      print(mp)
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

