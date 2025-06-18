# factor_analysis

    Code
      print(summary(out))
    Output
      # (Explained) Variance of Components
      
      Parameter                       |   MR1 |   MR2
      -----------------------------------------------
      Eigenvalues                     | 4.947 | 1.062
      Variance Explained              | 0.638 | 0.220
      Variance Explained (Cumulative) | 0.638 | 0.858
      Variance Explained (Proportion) | 0.744 | 0.256
      
      # Factor Correlations
      
      Factor |    MR1 |    MR2
      ------------------------
      MR1    |  1.000 | -0.366
      MR2    | -0.366 |  1.000

---

    Code
      print_md(summary(out))
    Output
      
      
      Table: (Explained) Variance of Components
      
      |Parameter                       |   MR1 |   MR2 |
      |:-------------------------------|:-----:|:-----:|
      |Eigenvalues                     | 4.947 | 1.062 |
      |Variance Explained              | 0.638 | 0.220 |
      |Variance Explained (Cumulative) | 0.638 | 0.858 |
      |Variance Explained (Proportion) | 0.744 | 0.256 |
      
      Table: Factor Correlations
      
      |Factor |    MR1 |    MR2 |
      |:------|:------:|:------:|
      |MR1    |  1.000 | -0.366 |
      |MR2    | -0.366 |  1.000 |

