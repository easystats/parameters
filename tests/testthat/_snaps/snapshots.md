# kurtosis

    Code
      parameters(lm(formula = wt ~ am * cyl, data = mtcars))
    Output
      Parameter   | Coefficient |   SE |        95% CI | t(28) |      p
      -----------------------------------------------------------------
      (Intercept) |        1.66 | 0.59 | [ 0.46, 2.86] |  2.82 | 0.009 
      am          |       -0.96 | 0.79 | [-2.58, 0.67] | -1.21 | 0.238 
      cyl         |        0.30 | 0.08 | [ 0.13, 0.47] |  3.68 | < .001
      am * cyl    |        0.03 | 0.13 | [-0.23, 0.30] |  0.25 | 0.803 

