skip_on_cran()
skip_if_not_installed("lme4")

m <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
mp <- model_parameters(m)

test_that("caption = '' removes the caption of a single table", {
  mp_fixed <- model_parameters(m, effects = "fixed")

  out <- utils::capture.output(print(mp_fixed))
  expect_true(any(startsWith(out, "# Fixed Effects")))
  out <- print_md(mp_fixed)
  expect_true("Table: Fixed Effects" %in% out)

  out <- utils::capture.output(print(mp_fixed, caption = ""))
  expect_false(any(startsWith(out, "#")))
  out <- print_md(mp_fixed, caption = "")
  expect_false(any(startsWith(out, "Table:")))
})

test_that("print_md() with caption = '' removes component captions", {
  # insight 1.5.4.10 is needed to remove captions stored as attributes
  skip_if_not_installed("insight", minimum_version = "1.5.4.10")
  out <- print_md(mp)
  expect_true("Table: Fixed Effects" %in% out)
  expect_true("Table: Random Effects" %in% out)

  out <- print_md(mp, caption = "")
  expect_false(any(startsWith(out, "Table:")))
})

test_that("print() with caption = '' removes component captions", {
  skip_if_not_installed("insight", minimum_version = "1.5.4.10")
  out <- utils::capture.output(print(mp))
  expect_true(any(startsWith(out, "# Fixed Effects")))
  expect_true(any(startsWith(out, "# Random Effects")))

  out <- utils::capture.output(print(mp, caption = ""))
  expect_false(any(startsWith(out, "# Fixed Effects")))
  expect_false(any(startsWith(out, "# Random Effects")))
})

test_that("caption = '' removes component captions of within-between models", {
  skip_if_not_installed("insight", minimum_version = "1.5.4.10")
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
  skip_if_not_installed("insight", minimum_version = "1.5.4.10")
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

test_that("caption = '' removes captions of tables with an empty title attribute", {
  skip_if_not_installed("insight", minimum_version = "1.5.4.10")
  mp4 <- model_parameters(aov(cbind(mpg, hp) ~ factor(cyl), data = mtcars))
  expect_identical(attributes(mp4)$title, "")

  out <- utils::capture.output(print(mp4))
  expect_true(all(c("# mpg response", "# hp response") %in% out))
  out <- print_md(mp4)
  expect_true(all(c("Table: mpg response", "Table: hp response") %in% out))

  out <- utils::capture.output(print(mp4, caption = ""))
  expect_false(any(grepl("response", out, fixed = TRUE)))
  out <- print_md(mp4, caption = "")
  expect_false(any(grepl("response", out, fixed = TRUE)))
})

test_that("print_html() with caption = '' removes the title and keeps row groups", {
  skip_if_not_installed("gt")
  out <- print_html(mp)
  expect_identical(out[["_heading"]]$title, "Model Summary")
  expect_identical(trimws(out[["_row_groups"]]), c("Fixed Effects", "Random Effects"))

  out <- print_html(mp, caption = "")
  expect_null(out[["_heading"]]$title)
  expect_identical(trimws(out[["_row_groups"]]), c("Fixed Effects", "Random Effects"))
})

test_that("print_html() with tinytable and caption = '' removes the title", {
  skip_if_not_installed("tinytable")
  out <- print_html(mp, backend = "tt")
  expect_identical(out@caption, "Model Summary")

  out <- print_html(mp, backend = "tt", caption = "")
  expect_null(out@caption)
  html <- tinytable::save_tt(out, "html")
  expect_true(grepl("Fixed Effects", html, fixed = TRUE))
  expect_true(grepl("Random Effects", html, fixed = TRUE))
})
