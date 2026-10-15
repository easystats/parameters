# print model_parameters.TukeyHSD, snapshots

    Code
      print(model_parameters(x1))
    Output
      Term    | Contrast | Coefficient |         95% CI |     p
      ---------------------------------------------------------
      tension |      M-H |        4.72 | [-4.63, 14.08] | 0.447
      tension |      L-H |       14.72 | [ 5.37, 24.08] | 0.001
      tension |      L-M |       10.00 | [ 0.65, 19.35] | 0.034
      
      p-value adjustment method: Tukey

---

    Code
      print(model_parameters(x2))
    Output
      Term         | Contrast | Coefficient |           90% CI |      p
      -----------------------------------------------------------------
      wool         |      B-A |       -5.78 | [-10.77,  -0.78] | 0.058 
      tension      |      M-L |      -10.00 | [-17.67,  -2.33] | 0.023 
      tension      |      H-L |      -14.72 | [-22.39,  -7.06] | < .001
      tension      |      H-M |       -4.72 | [-12.39,   2.94] | 0.405 
      wool:tension |  B:L-A:L |      -16.33 | [-30.11,  -2.55] | 0.030 
      wool:tension |  A:M-A:L |      -20.56 | [-34.33,  -6.78] | 0.003 
      wool:tension |  B:M-A:L |      -15.78 | [-29.56,  -2.00] | 0.040 
      wool:tension |  A:H-A:L |      -20.00 | [-33.78,  -6.22] | 0.004 
      wool:tension |  B:H-A:L |      -25.78 | [-39.56, -12.00] | < .001
      wool:tension |  A:M-B:L |       -4.22 | [-18.00,   9.56] | 0.963 
      wool:tension |  B:M-B:L |        0.56 | [-13.22,  14.33] | > .999
      wool:tension |  A:H-B:L |       -3.67 | [-17.45,  10.11] | 0.980 
      wool:tension |  B:H-B:L |       -9.44 | [-23.22,   4.33] | 0.456 
      wool:tension |  B:M-A:M |        4.78 | [ -9.00,  18.56] | 0.938 
      wool:tension |  A:H-A:M |        0.56 | [-13.22,  14.33] | > .999
      wool:tension |  B:H-A:M |       -5.22 | [-19.00,   8.56] | 0.911 
      wool:tension |  A:H-B:M |       -4.22 | [-18.00,   9.56] | 0.963 
      wool:tension |  B:H-B:M |      -10.00 | [-23.78,   3.78] | 0.392 
      wool:tension |  B:H-A:H |       -5.78 | [-19.56,   8.00] | 0.871 
      
      p-value adjustment method: Tukey

