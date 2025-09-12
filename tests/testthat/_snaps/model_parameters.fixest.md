# model_parameters.fixest-1

    Code
      model_parameters(m1, include_info = TRUE, verbose = FALSE)
    Output
      # Fixed Effects
      
      Parameter | Coefficient |   SE |         95% CI | t(374) |      p
      -----------------------------------------------------------------
      time      |        1.09 | 0.64 | [-0.17,  2.34] |   1.70 | 0.089 
      phq4      |       -3.66 | 0.41 | [-4.46, -2.86] |  -8.95 | < .001
      
      Model: QoL ~ time + phq4 (564 Observations)
      Sigma: 12.365 (df = 374)
      RMSE : 10.069
      r2: 0.743; ar2: 0.613; wr2: 0.180; war2: 0.175

# model_parameters works for fixest-negbin

    Code
      print(out)
    Output
      # Fixed Effects
      
      Parameter   | Log-Mean |   SE |         95% CI | t(636) |      p
      ----------------------------------------------------------------
      (Intercept) |    -1.46 | 0.21 | [-1.86, -1.06] |  -7.11 | < .001
      mined [no]  |     2.04 | 0.15 | [ 1.75,  2.33] |  13.72 | < .001
      spp [PR]    |    -1.23 | 0.29 | [-1.80, -0.65] |  -4.20 | < .001
      spp [DM]    |     0.40 | 0.23 | [-0.05,  0.86] |   1.75 | 0.080 
      spp [EC-A]  |    -0.67 | 0.26 | [-1.18, -0.16] |  -2.60 | 0.010 
      spp [EC-L]  |     0.64 | 0.22 | [ 0.20,  1.07] |   2.89 | 0.004 
      spp [DES-L] |     0.82 | 0.22 | [ 0.38,  1.26] |   3.69 | < .001
      spp [DF]    |     0.36 | 0.24 | [-0.10,  0.82] |   1.52 | 0.128 
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.
      
      The model has a log- or logit-link. Consider using `exponentiate =
        TRUE` to interpret coefficients as ratios.

