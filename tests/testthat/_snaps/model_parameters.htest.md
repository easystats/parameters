# model_parameters-rank biserial

    Code
      model_parameters(mod_paired, rank_biserial = TRUE, ci = 0.9)
    Output
      # Wilcoxon signed rank exact test
      
      Parameter1 | Parameter2 |     W | r_rank_biserial | rank_biserial 90% CI |     p
      --------------------------------------------------------------------------------
      x          |          y | 40.00 |            0.33 |        [-0.20, 1.00] | 0.020

---

    Code
      model_parameters(mod_one, rank_biserial = TRUE)
    Output
      # Wilcoxon signed rank exact test
      
      Parameter |    W | r_rank_biserial | rank_biserial 95% CI |      p
      ------------------------------------------------------------------
      x         | 7.00 |           -0.07 |        [-1.00, 0.87] | > .999

---

    Code
      model_parameters(mod_unpaired, rank_biserial = TRUE, ci = 0.99)
    Output
      # Wilcoxon rank sum exact test
      
      Parameter1 | Parameter2 |     W | r_rank_biserial | rank_biserial 99% CI |     p
      --------------------------------------------------------------------------------
      m          |          n | 35.00 |            0.40 |        [-0.48, 0.92] | 0.127

