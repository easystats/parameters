skip_if(getRversion() < "4.0.0")
skip_if_not_installed("withr")

withr::with_options(
  list("parameters_warning_exponentiate" = TRUE),
  {
    test_that("print warning about complete separation", {
      d_sep <- data.frame(
        y = c(0, 0, 0, 0, 1, 1, 1, 1),
        x1 = c(1, 2, 3, 3, 5, 6, 10, 11),
        x2 = c(3, 2, -1, -1, 2, 4, 1, 0)
      )
      m_sep <- suppressWarnings(glm(y ~ x1 + x2, data = d_sep, family = binomial))
      out <- model_parameters(m_sep)
      expect_snapshot(print(out))
    })
  }
)

withr::with_options(
  list("parameters_warning_exponentiate" = TRUE),
  {
    test_that("print warning about complete separation", {
      data(mtcars)
      m_sep2 <- suppressWarnings(glm(am ~ gear, data = mtcars, family = binomial))
      out <- model_parameters(m_sep2)
      expect_snapshot(print(out))
    })
  }
)

withr::with_options(
  list("parameters_warning_exponentiate" = TRUE),
  {
    test_that("print warning about quasi complete separation", {
      data(mtcars)
      m_sep3 <- suppressWarnings(glm(vs ~ qsec, data = mtcars, family = binomial))
      out <- model_parameters(m_sep3)
      expect_snapshot(print(out))
    })
  }
)
