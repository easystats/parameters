# model_parameters output as expected

    Code
      as.data.frame(parameters(mod))
    Output
          Parameter Coefficient         SE   CI     CI_low   CI_high          t
      1 (Intercept)  1.65820588 0.58714925 0.95  0.4554852 2.8609266  2.8241642
      2          am -0.95618461 0.79273245 0.95 -2.5800234 0.6676542 -1.2061883
      3         cyl  0.30381127 0.08260183 0.95  0.1346091 0.4730135  3.6780209
      4      am:cyl  0.03280575 0.13020948 0.95 -0.2339163 0.2995278  0.2519459
        df_error            p
      1       28 0.0086365378
      2       28 0.2378382515
      3       28 0.0009892218
      4       28 0.8029230279

