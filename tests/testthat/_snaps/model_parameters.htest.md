# model_parameters-rank biserial

    Code
      model_parameters(mod_paired)
    Output
      # Wilcoxon signed rank exact test
      
      Parameter1 | Parameter2 |     W |     p
      ---------------------------------------
      x          |          y | 40.00 | 0.020

---

    Code
      model_parameters(mod_one)
    Output
      # Wilcoxon signed rank exact test
      
      Parameter |    W |      p
      -------------------------
      x         | 7.00 | > .999

---

    Code
      model_parameters(mod_unpaired)
    Output
      # Wilcoxon rank sum exact test
      
      Parameter1 | Parameter2 |     W |     p
      ---------------------------------------
      m          |          n | 35.00 | 0.127

# model_parameters- Kendall's W

    Code
      suppressWarnings(model_parameters(mod))
    Output
      # Friedman rank sum test
      
      Parameter1 | Parameter2 | Chi2(1) |     p
      -----------------------------------------
      x          |          w |    0.33 | 0.564

