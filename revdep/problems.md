# effectsize

<details>

* Version: 0.4.4-1
* GitHub: https://github.com/easystats/effectsize
* Source code: https://github.com/cran/effectsize
* Date/Publication: 2021-04-05 14:20:06 UTC
* Number of recursive dependencies: 145

Run `revdep_details(, "effectsize")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Last 13 lines of output:
        3. |   \-testthat:::quasi_capture(...)
        4. |     +-testthat:::.capture(...)
        5. |     | \-base::withCallingHandlers(...)
        6. |     \-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        7. +-effectsize::standardize_parameters(m, method = "pseudo", robust = TRUE)
        8. \-effectsize:::standardize_parameters.default(...)
        9.   \-effectsize:::.standardize_parameters_posthoc(...)
       10.     +-base::union(i_missing, which(to_complete))
       11.     | +-base::unique(c(as.vector(x), as.vector(y)))
       12.     | \-base::as.vector(y)
       13.     \-base::which(to_complete)
      
      [ FAIL 1 | WARN 0 | SKIP 16 | PASS 495 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'afex', 'BayesFactor', 'brms', 'car', 'lavaan', 'lm.beta',
      'mediation', 'MuMIn', 'pscl', 'tidymodels'
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: 'lavaan', 'lm.beta'
    ```

