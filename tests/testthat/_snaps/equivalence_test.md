# equivalence_test

    Code
      print(x)
    Output
      # TOST-test for Practical Equivalence
      
        ROPE: [-0.60 0.60]
      
      Parameter   |         90% CI |   SGPV | Equivalence |      p
      ------------------------------------------------------------
      (Intercept) | [26.52, 46.86] | < .001 |    Rejected | > .999
      gear        | [-1.34,  2.07] | 0.475  |   Undecided | 0.578 
      wt          | [-4.47, -1.57] | < .001 |    Rejected | 0.996 
      cyl         | [-1.94,  0.32] | 0.351  |   Undecided | 0.644 
      hp          | [-0.05,  0.01] | > .999 |    Accepted | < .001

# equivalence_test, robust

    Code
      print(x)
    Output
      # TOST-test for Practical Equivalence
      
        ROPE: [-0.60 0.60]
      
      Parameter   |         90% CI |   SGPV | Equivalence |      p
      ------------------------------------------------------------
      (Intercept) | [23.10, 50.28] | < .001 |    Rejected | > .999
      gear        | [-1.63,  2.36] | 0.421  |   Undecided | 0.628 
      wt          | [-4.59, -1.45] | 0.001  |    Rejected | 0.993 
      cyl         | [-2.24,  0.62] | 0.361  |   Undecided | 0.649 
      hp          | [-0.05,  0.01] | > .999 |    Accepted | < .001

