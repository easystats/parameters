skip_on_cran()

test_that("print() labels group-level estimates as random effects", {
  skip_if_not_installed("lme4")
  m <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  mp <- model_parameters(m, effects = "grouplevel")
  expect_identical(attributes(mp)$effects, "grouplevel")
  out <- utils::capture.output(print(mp))
  expect_true(any(startsWith(out, "# Random Effects")))
})

test_that("group-level estimates print as random effects without an Effects column", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("brms")
  skip_if_not_installed("httr2")

  m <- insight::download_model("brms_zi_3")
  skip_if(is.null(m))
  mp <- model_parameters(m, effects = "grouplevel")

  # the Effects column is dropped upstream because it is single-valued, so
  # the header decision has to come from the saved `effects` argument
  expect_false("Effects" %in% colnames(mp))
  expect_identical(attributes(mp)$effects, "grouplevel")

  out <- utils::capture.output(print(mp))
  headers <- out[startsWith(out, "#")]
  expect_length(headers, 2L)
  expect_true(all(startsWith(headers, "# Random Effects")))
})
