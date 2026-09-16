# mp, footer exp

    Code
      print(out)
    Output
      # Fixed Effects
      
      Parameter          | Median |        95% CI |     pd |  Rhat | ESS (tail)
      -------------------------------------------------------------------------
      (Intercept)        |  -0.25 | [-1.28, 0.75] | 68.62% | 1.000 |       2983
      var_binom1         |  -0.64 | [-2.09, 0.64] | 83.20% | 1.001 |       2861
      groupsb            |  -0.22 | [-1.35, 0.87] | 64.75% | 1.001 |       2900
      var_cont           |  -0.06 | [-0.14, 0.00] | 96.65% | 1.000 |       2955
      var_binom1:groupsb |   0.53 | [-1.70, 2.69] | 69.25% | 1.001 |       2712
    Message
      
      Uncertainty intervals (equal-tailed) computed using a MCMC distribution
        approximation.
      
      The model has a log- or logit-link. Consider using `exponentiate =
        TRUE` to interpret coefficients as ratios.

---

    Code
      print(out)
    Output
      # Fixed Effects
      
      Parameter          | Median |        95% CI |     pd |  Rhat | ESS (tail)
      -------------------------------------------------------------------------
      (Intercept)        |   0.78 | [0.28,  2.11] | 68.62% | 1.000 |       2983
      var_binom1         |   0.53 | [0.12,  1.90] | 83.20% | 1.001 |       2861
      groupsb            |   0.80 | [0.26,  2.38] | 64.75% | 1.001 |       2900
      var_cont           |   0.94 | [0.87,  1.00] | 96.65% | 1.000 |       2955
      var_binom1:groupsb |   1.69 | [0.18, 14.80] | 69.25% | 1.001 |       2712
    Message
      
      Uncertainty intervals (equal-tailed) computed using a MCMC distribution
        approximation.

