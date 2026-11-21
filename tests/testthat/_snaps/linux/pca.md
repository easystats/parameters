# print model_parameters pca

    Code
      print(principal_components(mtcars[, 1:4], n = "auto"))
    Output
      # Loadings from Principal Component Analysis (no rotation)
      
      Variable |   PC1 | Complexity
      -----------------------------
      mpg      | -0.93 |       1.00
      cyl      |  0.96 |       1.00
      disp     |  0.95 |       1.00
      hp       |  0.91 |       1.00
      
      The unique principal component accounted for 87.55% of the total variance of the original data.

---

    Code
      print(principal_components(mtcars[, 1:4], n = "auto"), labels = c(
        "Miles/(US) gallon", "Number of cylinders", "Displacement (cu.in.)",
        "Gross horsepower"))
    Output
      # Loadings from Principal Component Analysis (no rotation)
      
      Variable | Label                 |   PC1 | Complexity
      -----------------------------------------------------
      mpg      | Miles/(US) gallon     | -0.93 |       1.00
      cyl      | Number of cylinders   |  0.96 |       1.00
      disp     | Displacement (cu.in.) |  0.95 |       1.00
      hp       | Gross horsepower      |  0.91 |       1.00

