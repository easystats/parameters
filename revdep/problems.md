# performance

<details>

* Version: 0.10.5
* GitHub: https://github.com/easystats/performance
* Source code: https://github.com/cran/performance
* Date/Publication: 2023-09-12 08:50:02 UTC
* Number of recursive dependencies: 265

Run `revdepcheck::revdep_details(, "performance")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running 'testthat.R'
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Last 13 lines of output:
        'test-pkg-ivreg.R:8:3', 'test-r2_nakagawa.R:19:1',
        'test-test_likelihoodratio.R:55:1'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-cronbachs_alpha.R:14:3'): cronbachs_alpha, principal_components ──
      cronbachs_alpha(pca, verbose = FALSE) (`actual`) not equal to c(PC1 = 0.1101384) (`expected`).
      
        `actual`: 0.09
      `expected`: 0.11
      ── Failure ('test-cronbachs_alpha.R:15:3'): cronbachs_alpha, principal_components ──
      `cronbachs_alpha(pca)` did not throw the expected warning.
      
      [ FAIL 2 | WARN 2 | SKIP 24 | PASS 300 ]
      Error: Test failures
      Execution halted
    ```

