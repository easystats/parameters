# model_parameters.t1way

    Code
      as.data.frame(df_b)
    Output
        F df df_error    p                                            Method
      1 3  2        4 0.16 A heteroscedastic one-way ANOVA for trimmed means
         Estimate   CI    CI_low  CI_high                         Effectsize
      1 0.7894413 0.95 0.4193932 1.329081 Explanatory measure of effect size

---

    Code
      as.data.frame(df_w)
    Output
               F       df df_error          p
      1 3.261404 1.609231    20.92 0.06760865
                                                                     Method
      1 A heteroscedastic one-way repeated measures ANOVA for trimmed means

# model_parameters.yuen

    Code
      as.data.frame(df_b)
    Output
               t df_error         p
      1 1.295757 13.91372 0.2161433
                                                      Method Difference   CI
      1 Yuen's test on trimmed means for independent samples      -6.75 0.95
        Difference_CI_low Difference_CI_high  Estimate
      1         -17.92936           4.429361 0.3784167
                                Effectsize
      1 Explanatory measure of effect size

---

    Code
      as.data.frame(df_w)
    Output
              t df_error         p                                             Method
      1 1.98861        5 0.1034335 Yuen's test on trimmed means for dependent samples
        Difference   CI Difference_CI_low Difference_CI_high  Estimate
      1   28.33333 0.95          -8.29182           64.95849 0.5192753
                                Effectsize
      1 Explanatory measure of effect size

# model_parameters.mcp and robtab

    Code
      as.data.frame(df_b)
    Output
         Group1 Group2 Psihat   CI   CI_low CI_high         p
      1 placebo    low     -1 0.95 -5.31858 3.31858 0.4353309
      2 placebo   high     -3 0.95 -7.31858 1.31858 0.1805095
      3     low   high     -2 0.95 -6.31858 2.31858 0.3166048

---

    Code
      as.data.frame(df_w)
    Output
        Group1 Group2     Psihat   CI       CI_low    CI_high           p p.crit
      1 Wine A Wine B 0.02142857 0.95 -0.021636832 0.06449397 0.195004531 0.0500
      2 Wine A Wine C 0.11428571 0.95  0.021475579 0.20709585 0.004915566 0.0169
      3 Wine B Wine C 0.08214286 0.95  0.008910564 0.15537515 0.008777396 0.0250

---

    Code
      as.data.frame(df)
    Output
         Group1 Group2         p     p.crit
      1 placebo    low 0.8118812 0.01666667
      2 placebo   high 0.8514851 0.02500000
      3     low   high 0.8613861 0.05000000

# model_parameters.AKP

    Code
      as.data.frame(model_parameters(mod))
    Output
        Estimate   CI    CI_low  CI_high
      1 2.481694 0.95 0.7911292 5.095739
                                                     Effectsize
      1 Algina-Keselman-Penfield robust standardized difference

# model_parameters.onesampb

    Code
      as.data.frame(model_parameters(mod))
    Output
           Estimate   CI     CI_low   CI_high   p n_Obs              Effectsize
      1 -0.08113998 0.95 -0.4146632 0.2417105 0.7    30 Robust location measure
                                 Method
      1 One-sample percentile bootstrap

