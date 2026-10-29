skip_on_cran()
skip_if_not_installed("lme4")

m <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
mp <- model_parameters(m)

test_that("caption = '' removes the caption of a single table", {
  mp_fixed <- model_parameters(m, effects = "fixed")
  out <- utils::capture.output(print(mp_fixed, caption = ""))
  expect_true(any(startsWith(out, "Parameter")))
  expect_false(any(startsWith(out, "#")))
  out <- print_md(mp_fixed, caption = "")
  expect_true(any(startsWith(out, "|Parameter")))
  expect_false(any(startsWith(out, "Table:")))
})

test_that("print_md() with caption = '' removes component captions", {
  # insight 1.6.0 is needed to remove captions stored as attributes
  skip_if_not_installed("insight", minimum_version = "1.6.0")
  out <- print_md(mp, caption = "")
  expect_false(any(startsWith(out, "Table:")))
})

test_that("print() with caption = '' removes component captions", {
  skip_if_not_installed("insight", minimum_version = "1.6.0")
  out <- utils::capture.output(print(mp, caption = ""))
  expect_false(any(startsWith(out, "# Fixed Effects")))
  expect_false(any(startsWith(out, "# Random Effects")))
})

test_that("caption = '' removes component captions of within-between models", {
  skip_if_not_installed("insight", minimum_version = "1.6.0")
  skip_if_not_installed("glmmTMB")
  data(qol_cancer, package = "parameters")
  d <- cbind(qol_cancer, datawizard::demean(qol_cancer, select = "phq4", by = "ID"))
  m2 <- suppressWarnings(glmmTMB::glmmTMB(
    QoL ~ time + phq4_within + phq4_between + (1 + phq4_within | ID),
    data = d
  ))
  mp2 <- model_parameters(m2, wb_component = TRUE)
  out <- utils::capture.output(print(mp2, caption = ""))
  expect_false(any(startsWith(out, "#")))
  out <- print_md(mp2, caption = "")
  expect_false(any(startsWith(out, "Table:")))
})

test_that("user-supplied caption is printed for models without default caption", {
  skip_if_not_installed("insight", minimum_version = "1.6.0")
  mp3 <- model_parameters(lm(mpg ~ wt, data = mtcars))
  out <- print_md(mp3, caption = "My caption")
  expect_true("Table: My caption" %in% out)
  out <- utils::capture.output(print(mp3, caption = "My caption"))
  expect_true(any(grepl("My caption", out, fixed = TRUE)))
})

test_that("caption = '' removes captions of tables with an empty title attribute", {
  skip_if_not_installed("insight", minimum_version = "1.6.0")
  mp4 <- model_parameters(aov(cbind(mpg, hp) ~ factor(cyl), data = mtcars))
  expect_identical(attributes(mp4)$title, "")
  out <- utils::capture.output(print(mp4, caption = ""))
  expect_true(any(startsWith(out, "Parameter")))
  expect_false(any(grepl("^#.*response", out)))
  out <- print_md(mp4, caption = "")
  expect_true(any(startsWith(out, "|Parameter")))
  expect_false(any(grepl("^Table:.*response", out)))
})

test_that("print_html() with caption = '' removes the title and keeps row groups", {
  skip_if_not_installed("gt")
  out <- print_html(mp, caption = "")
  expect_null(out[["_heading"]]$title)
  expect_identical(trimws(out[["_row_groups"]]), c("Fixed Effects", "Random Effects"))
})

test_that("print_html() with tinytable and caption = '' removes the title", {
  skip_if_not_installed("tinytable")
  out <- print_html(mp, backend = "tt", caption = "")
  expect_null(out@caption)
  html <- tinytable::save_tt(out, "html")
  expect_true(grepl("Fixed Effects", html, fixed = TRUE))
  expect_true(grepl("Random Effects", html, fixed = TRUE))
})
