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

