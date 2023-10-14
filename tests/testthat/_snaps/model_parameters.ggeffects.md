# model_parameters.ggeffects

    Code
      print(params)
    Output
      # Species = setosa
      
      Petal.Length | Predicted |        95% CI
      ----------------------------------------
      1.00         |      4.79 | [4.36,  5.21]
      2.50         |      5.59 | [4.97,  6.21]
      4.00         |      6.40 | [4.99,  7.80]
      7.00         |      8.00 | [4.96, 11.05]
      
      # Species = versicolor
      
      Petal.Length | Predicted |       95% CI
      ---------------------------------------
      1.00         |      3.26 | [2.53, 4.00]
      2.50         |      4.49 | [4.10, 4.89]
      4.00         |      5.72 | [5.61, 5.83]
      7.00         |      8.17 | [7.52, 8.83]
      
      # Species = virginica
      
      Petal.Length | Predicted |       95% CI
      ---------------------------------------
      1.00         |      2.05 | [1.26, 2.85]
      2.50         |      3.54 | [2.99, 4.08]
      4.00         |      5.03 | [4.69, 5.36]
      7.00         |      8.00 | [7.57, 8.42]
      
      Adjusted for:
      * Petal.Width = 1.20

