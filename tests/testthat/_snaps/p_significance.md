# p_significance

    Code
      print(x)
    Output
      Practical Significance (threshold: -0.60, 0.60)
      
      Parameter   |         95% CI |     ps
      -------------------------------------
      (Intercept) | [24.44, 48.94] |   100%
      gear        | [-1.69,  2.41] | 39.83%
      wt          | [-4.77, -1.28] | 99.59%
      cyl         | [-2.17,  0.55] | 61.88%
      hp          | [-0.05,  0.01] |  0.00%

# p_significance, robust

    Code
      print(x)
    Output
      Practical Significance (threshold: -0.60, 0.60)
      
      Parameter   |         95% CI |     ps
      -------------------------------------
      (Intercept) | [20.32, 53.06] |   100%
      gear        | [-2.04,  2.77] | 41.23%
      wt          | [-4.91, -1.13] | 99.39%
      cyl         | [-2.53,  0.91] | 59.51%
      hp          | [-0.06,  0.01] |  0.00%

