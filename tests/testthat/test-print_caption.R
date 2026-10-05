skip_on_cran()
skip_if_not_installed("lme4")
# insight 1.5.4.10 is needed to remove captions stored as attributes
skip_if_not_installed("insight", minimum_version = "1.5.4.10")

m <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
mp <- model_parameters(m)

test_that("print_md() with caption = '' removes component captions", {
  out <- print_md(mp)
  expect_true("Table: Fixed Effects" %in% out)
  expect_true("Table: Random Effects" %in% out)

  out <- print_md(mp, caption = "")
  expect_false(any(startsWith(out, "Table:")))
})

test_that("print() with caption = '' removes component captions", {
  out <- utils::capture.output(print(mp))
  expect_true(any(startsWith(out, "# Fixed Effects")))
  expect_true(any(startsWith(out, "# Random Effects")))

  out <- utils::capture.output(print(mp, caption = ""))
  expect_false(any(startsWith(out, "# Fixed Effects")))
  expect_false(any(startsWith(out, "# Random Effects")))
})
