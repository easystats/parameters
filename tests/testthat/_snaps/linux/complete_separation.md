# print warning about complete separation

    Code
      print(out)
    Output
      Parameter   | Log-Odds |       SE |                95% CI |         z |      p
      ------------------------------------------------------------------------------
      (Intercept) |   -66.10 | 1.83e+05 | [-10644.72, 10512.52] | -3.60e-04 | > .999
      x1          |    15.29 | 27362.84 | [ -3122.69,         ] |  5.59e-04 | > .999
      x2          |     6.24 | 81543.72 | [-12797.28,         ] |  7.65e-05 | > .999
    Message
      
      Uncertainty intervals (profile-likelihood) and p-values (two-tailed)
        computed using a Wald z-distribution approximation.
      
      The model has a log- or logit-link. Consider using `exponentiate =
        TRUE` to interpret coefficients as ratios.
        
      Some coefficients are very large, which may indicate issues with
        complete separation.

---

    Code
      print(out)
    Output
      Parameter   | Log-Odds |       SE |            95% CI |         z |     p
      -------------------------------------------------------------------------
      (Intercept) |   -83.33 | 15505.03 | [       , 816.56] | -5.37e-03 | 0.996
      gear        |    21.01 |  3876.26 | [-248.93,       ] |  5.42e-03 | 0.996
    Message
      
      Uncertainty intervals (profile-likelihood) and p-values (two-tailed)
        computed using a Wald z-distribution approximation.
      
      The model has a log- or logit-link. Consider using `exponentiate =
        TRUE` to interpret coefficients as ratios.
        
      Some coefficients are very large, which may indicate issues with
        complete separation.

# print warning about quasi complete separation

    Code
      print(out)
    Output
      Parameter   | Log-Odds |    SE |            95% CI |     z |     p
      ------------------------------------------------------------------
      (Intercept) |   -51.19 | 19.62 | [-105.21, -22.47] | -2.61 | 0.009
      qsec        |     2.89 |  1.10 | [   1.28,   5.93] |  2.63 | 0.009
    Message
      
      Uncertainty intervals (profile-likelihood) and p-values (two-tailed)
        computed using a Wald z-distribution approximation.
      
      The model has a log- or logit-link. Consider using `exponentiate =
        TRUE` to interpret coefficients as ratios.
        
      Some coefficients seem to be rather large, which may indicate issues
        with (quasi) complete separation. Consider using bias-corrected or
        penalized regression models.

