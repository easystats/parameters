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

# omega

    Code
      print(out)
    Output
      # Rotated loadings from Omega (oblimin-rotation)
      
      Variable |        g |   F1* |      F2* |       F3* |   h2 |   u2 |       p2 | Complexity
      ----------------------------------------------------------------------------------------
      mpg-     |     0.58 | -0.67 |     0.09 |      0.29 | 0.88 | 0.12 |     0.38 |       2.40
      cyl      |     0.70 | -0.61 |     0.28 |      0.07 | 0.96 | 0.04 |     0.52 |       2.33
      disp     |     0.59 | -0.71 |     0.18 |      0.11 | 0.89 | 0.11 |     0.39 |       2.13
      hp       |     0.77 | -0.31 |     0.23 |      0.36 | 0.87 | 0.13 |     0.68 |       2.00
      drat-    |     0.27 | -0.79 |     0.06 |     -0.07 | 0.71 | 0.29 |     0.10 |       1.26
      wt       |     0.43 | -0.79 |    -0.04 |      0.31 | 0.91 | 0.09 |     0.20 |       1.87
      qsec-    |     0.81 |  0.19 |     0.50 |      0.06 | 0.95 | 0.05 |     0.70 |       1.81
      vs-      |     0.74 | -0.27 |     0.38 |      0.05 | 0.77 | 0.23 |     0.71 |       1.81
      am-      | 8.38e-03 | -0.89 |    -0.15 | -9.51e-03 | 0.81 | 0.19 | 8.63e-05 |       1.06
      gear     |     0.03 |  0.87 | 9.01e-03 |      0.32 | 0.87 | 0.13 | 9.03e-04 |       1.27
      carb     |     0.68 |  0.06 |     0.10 |      0.63 | 0.87 | 0.13 |     0.53 |       2.06

---

    Code
      print_md(out)
    Output
      
      
      Table: Rotated loadings from Omega (oblimin-rotation)
      
      |Variable |        g|  F1* |      F2*|       F3*|  h2 |  u2 |       p2| Complexity|
      |:--------|--------:|:-----|--------:|---------:|:----|:----|--------:|----------:|
      |mpg-     |     0.58|-0.67 |     0.09|      0.29|0.88 |0.12 |     0.38|       2.40|
      |cyl      |     0.70|-0.61 |     0.28|      0.07|0.96 |0.04 |     0.52|       2.33|
      |disp     |     0.59|-0.71 |     0.18|      0.11|0.89 |0.11 |     0.39|       2.13|
      |hp       |     0.77|-0.31 |     0.23|      0.36|0.87 |0.13 |     0.68|       2.00|
      |drat-    |     0.27|-0.79 |     0.06|     -0.07|0.71 |0.29 |     0.10|       1.26|
      |wt       |     0.43|-0.79 |    -0.04|      0.31|0.91 |0.09 |     0.20|       1.87|
      |qsec-    |     0.81| 0.19 |     0.50|      0.06|0.95 |0.05 |     0.70|       1.81|
      |vs-      |     0.74|-0.27 |     0.38|      0.05|0.77 |0.23 |     0.71|       1.81|
      |am-      | 8.38e-03|-0.89 |    -0.15| -9.51e-03|0.81 |0.19 | 8.63e-05|       1.06|
      |gear     |     0.03| 0.87 | 9.01e-03|      0.32|0.87 |0.13 | 9.03e-04|       1.27|
      |carb     |     0.68| 0.06 |     0.10|      0.63|0.87 |0.13 |     0.53|       2.06|

---

    Code
      print(summary(out))
    Output
      # Omega Statistics
      
      Statistic            | Coefficient
      ----------------------------------
      Alpha                |        0.88
      G.6                  |        0.97
      Omega (hierarchical) |        0.57
      Omega (asymptotic H) |        0.58
      Omega (total)        |        0.97
      
      # Omega Coefficients
      
      Composite | Omega (total) | Omega (hierarchical) | Omega (group)
      ----------------------------------------------------------------
      g         |          0.97 |                 0.57 |          0.26
      F1*       |          0.90 |                 0.31 |          0.59
      F2*       |          0.91 |                 0.69 |          0.22
      F3*       |          0.87 |                 0.60 |          0.28
      
      # Variances
      
      Composite | Total (%) | General Factor (%) | Group Factor (%)
      -------------------------------------------------------------
      g         |     97.28 |              56.64 |            26.42
      F1*       |     90.12 |              31.07 |            59.05
      F2*       |     91.37 |              69.32 |            22.04
      F3*       |     87.36 |              59.65 |            27.71

---

    Code
      print_md(summary(out))
    Output
      
      
      Table: Omega Statistics
      
      |Statistic            | Coefficient|
      |:--------------------|-----------:|
      |Alpha                |        0.88|
      |G.6                  |        0.97|
      |Omega (hierarchical) |        0.57|
      |Omega (asymptotic H) |        0.58|
      |Omega (total)        |        0.97|
      
      Table: Omega Coefficients
      
      |Composite | Omega (total)| Omega (hierarchical)| Omega (group)|
      |:---------|-------------:|--------------------:|-------------:|
      |g         |          0.97|                 0.57|          0.26|
      |F1*       |          0.90|                 0.31|          0.59|
      |F2*       |          0.91|                 0.69|          0.22|
      |F3*       |          0.87|                 0.60|          0.28|
      
      Table: Variances
      
      |Composite | Total (%)| General Factor (%)| Group Factor (%)|
      |:---------|---------:|------------------:|----------------:|
      |g         |     97.28|              56.64|            26.42|
      |F1*       |     90.12|              31.07|            59.05|
      |F2*       |     91.37|              69.32|            22.04|
      |F3*       |     87.36|              59.65|            27.71|

