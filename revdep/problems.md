# insight

<details>

* Version: 0.14.0
* GitHub: https://github.com/easystats/insight
* Source code: https://github.com/cran/insight
* Date/Publication: 2021-05-07 10:50:02 UTC
* Number of recursive dependencies: 334

Run `revdep_details(, "insight")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Last 13 lines of output:
      VGLM    linear loop  3 :  deviance = 5.129141
      VGLM    linear loop  4 :  deviance = 5.129141
      == Skipped tests ===============================================================
      * On CRAN (6)
      
      == Failed tests ================================================================
      -- Failure (test-standardize_names.R:45:5): (code run outside of `test_that()`) --
      names(standardize_names(z, style = "broom")) (`actual`) not equal to c(...) (`expected`).
      
      `actual[10:13]`:   "df.error" "p.value" "method" "alternative"
      `expected[10:12]`: "df.error" "p.value" "method"              
      
      [ FAIL 1 | WARN 1 | SKIP 6 | PASS 1983 ]
      Error: Test failures
      Execution halted
    ```

# statsExpressions

<details>

* Version: 1.0.1
* GitHub: https://github.com/IndrajeetPatil/statsExpressions
* Source code: https://github.com/cran/statsExpressions
* Date/Publication: 2021-04-13 15:10:02 UTC
* Number of recursive dependencies: 151

Run `revdep_details(, "statsExpressions")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Last 13 lines of output:
        `    ", " * "0.692" * "], ", italic("a")["Gunel-Dickey"] * " = " * `
        `    "1.000")`
      -- Failure (test-contingency_table.R:762:5): bayes factor (contingency tab) ----
      expr_text3$expression[[1]] (`actual`) not identical to ggplot2::expr(...) (`expected`).
      
      actual vs expected
        `paste("log"["e"] * "(BF"["01"] * ") = " * "-213.873" * ", ", `
      - `    widehat(italic("V"))["Cramer"]^"posterior" * " = " * "0.455" * `
      + `    widehat(italic("V"))["Cramer"]^"posterior" * " = " * "0.454" * `
        `        ", ", "CI"["95%"]^"HDI" * " [" * "0.417" * ", " * "0.495" * `
        `        "], ", italic("a")["Gunel-Dickey"] * " = " * "1.500")`
      
      [ FAIL 6 | WARN 4 | SKIP 12 | PASS 77 ]
      Error: Test failures
      Execution halted
    ```

