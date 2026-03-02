# model_parameters.anova

    Code
      mp
    Output
      Parameter   | df | Deviance | df (error) | Deviance (error) |      p
      --------------------------------------------------------------------
      NULL        |    |          |         31 |            43.23 |       
      mpg         |  1 |    13.55 |         30 |            29.68 | < .001
      hp          |  1 |    10.44 |         29 |            19.23 | 0.001 
      factor(cyl) |  2 |     8.75 |         27 |            10.49 | 0.013 
      
      Anova Table (Type 1 tests)

# print-model_parameters

    Code
      mp
    Output
      Parameter   | Sum_Squares | df | Mean_Square |     F |      p
      -------------------------------------------------------------
      mpg         |       16.72 |  1 |       16.72 | 53.40 | < .001
      hp          |       18.92 |  1 |       18.92 | 60.43 | < .001
      factor(cyl) |        8.75 |  2 |        4.37 | 13.97 | < .001
      Residuals   |        8.45 | 27 |        0.31 |       |       
      
      Anova Table (Type 3 tests)

# anova survey

    Code
      print(model_parameters(out))
    Output
      # Fixed Effects
      
      Parameter | DEff | Chi2(1) | df (error) |     p
      -----------------------------------------------
      ell       | 0.77 |    1.13 |         38 | 0.236
      meals     | 1.24 |    4.82 |         37 | 0.058
      ell:meals | 1.48 |   16.52 |         36 | 0.002
      
      Anova Table (Type 1 tests)

---

    Code
      print(model_parameters(out))
    Output
      # Fixed Effects
      
      Parameter | df | df (error) |    F |     p
      ------------------------------------------
      ell       |  1 |         38 | 1.47 | 0.234
      meals     |  1 |         37 | 3.54 | 0.068
      ell:meals |  1 |         36 | 9.10 | 0.005
      
      Anova Table (Type 1 tests)

