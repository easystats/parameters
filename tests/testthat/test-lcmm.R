skip_on_cran()
skip_on_os(c("mac", "linux", "solaris"))

skip_if_not_installed("lcmm")
skip_if_not_installed("datawizard")
skip_if_not_installed("curl")
skip_if_offline()
skip_if_not_installed("httr2")
skip_if_not_installed("withr")

withr::with_options(
  list(parameters_warning_exponentiate = TRUE),
  test_that("model_parameters lcmm", {
    out <- tryCatch(
      datawizard::data_read(
        "https://github.com/easystats/circus/raw/refs/heads/main/data/lcmm.rda"
      ),
      error = function(e) NULL
    )
    skip_if(is.null(out))

    expect_snapshot(print(model_parameters(out$mx_linear), zap_small = TRUE))
    expect_snapshot(print(model_parameters(out$mx_beta), zap_small = TRUE))
    expect_snapshot(print(model_parameters(out$mx_splines), zap_small = TRUE))

    expect_snapshot(print(model_parameters(out$m1_linear), zap_small = TRUE))
    expect_snapshot(print(model_parameters(out$m1_beta), zap_small = TRUE))
    expect_snapshot(print(model_parameters(out$m1_splines), zap_small = TRUE))

    expect_snapshot(print(model_parameters(out$m2_linear), zap_small = TRUE))
    expect_snapshot(print(model_parameters(out$m2_beta), zap_small = TRUE))
    expect_snapshot(print(model_parameters(out$m2_splines), zap_small = TRUE))
  })
)
