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

test_that("caption = '' removes component captions of within-between models", {
  skip_if_not_installed("glmmTMB")
  data(qol_cancer, package = "parameters")
  d <- cbind(qol_cancer, datawizard::demean(qol_cancer, select = "phq4", by = "ID"))
  m2 <- suppressWarnings(glmmTMB::glmmTMB(
    QoL ~ time + phq4_within + phq4_between + (1 + phq4_within | ID),
    data = d
  ))
  mp2 <- model_parameters(m2, wb_component = TRUE)

  out <- utils::capture.output(print(mp2))
  expect_true(any(startsWith(out, "# within")))
  out <- utils::capture.output(print(mp2, caption = ""))
  expect_false(any(startsWith(out, "#")))

  out <- print_md(mp2)
  expect_true(any(startsWith(out, "Table:")))
  out <- print_md(mp2, caption = "")
  expect_false(any(startsWith(out, "Table:")))
})

test_that("user-supplied caption is printed for models without default caption", {
  mp3 <- model_parameters(lm(mpg ~ wt, data = mtcars))

  out <- print_md(mp3)
  expect_false(any(startsWith(out, "Table:")))
  out <- print_md(mp3, caption = "My caption")
  expect_true("Table: My caption" %in% out)

  out <- utils::capture.output(print(mp3))
  expect_false(any(grepl("My caption", out, fixed = TRUE)))
  out <- utils::capture.output(print(mp3, caption = "My caption"))
  expect_true(any(grepl("My caption", out, fixed = TRUE)))
})
