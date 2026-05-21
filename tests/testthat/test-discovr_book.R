skip_on_os("mac")
skip_on_cran()
skip_if_not_installed("discovr")

test_that("mp, test bf and pd, chapter 8.14", {
  skip_if_not_installed("rstanarm")
  set.seed(123)
  m <- rstanarm::stan_glm(sales ~ adverts + airplay + image, data = discovr::album_sales)
  out <- suppressWarnings(model_parameters(m, test = c("pd", "bf"), null = 0))

  expect_identical(
    capture.output(out),
    c(
      "Parameter   | Median |          95% CI |     pd |       BF |  Rhat | ESS (tail) |                     Prior",
      "-----------------------------------------------------------------------------------------------------------",
      "(Intercept) | -26.70 | [-59.82,  5.46] | 94.53% |    0.051 | 1.000 |       3252 | Normal (193.20 +- 201.75)",
      "adverts     |   0.08 | [  0.07,  0.10] |   100% | 4.65e+10 | 1.000 |       3038 |     Normal (0.00 +- 0.42)",
      "airplay     |   3.36 | [  2.83,  3.92] |   100% | 4.85e+11 | 1.000 |       2980 |    Normal (0.00 +- 16.44)",
      "image       |  11.11 | [  6.48, 15.82] |   100% |    88.36 | 1.000 |       3309 |   Normal (0.00 +- 144.59)"
    )
  )
})

test_that("mp, htests, chapter 9.9", {
  # fmt: skip
  cloak_tib <- data.frame(
    id = c("Kiara", "Anupama", "Collin", "Ian", "Vanessa", "Darrell", "Tyler", "Steven", "Katheryn", "Kirkpatrick", "Melissa", "Kinaana", "Shajee'a", "Sage", "Jorge", "Conan","Tamara","Man", "Joseph", "Israa", "Kathryn", "Luis", "Devante", "Jerry"),
    cloak = gl(2, 12, labels = c("No cloak", "Cloak")),
    mischief = c(3, 1, 5, 4, 6, 4, 6, 2, 0, 5, 4, 5, 4, 3, 6, 6, 8, 5, 5, 4, 2, 5, 7, 5)
  )
  cloak_mod <- t.test(mischief ~ cloak, data = cloak_tib)
  out <- model_parameters(cloak_mod)
  expect_identical(
    capture.output(print(out, table_width = Inf)),
    c(
      "Welch Two Sample t-test",
      "",
      "Parameter | Group | cloak = No cloak | cloak = Cloak | Difference |        95% CI | t(21.54) |     p",
      "----------------------------------------------------------------------------------------------------",
      "mischief  | cloak |             3.75 |             5 |      -1.25 | [-2.76, 0.26] |    -1.71 | 0.101",
      "",
      "Alternative hypothesis: true difference in means between group No cloak and group Cloak is not equal to 0"
    )
  )

  tab <- display(
    out,
    column_names = c(
      "Outcome",
      "Predictor",
      "No cloak",
      "Cloak",
      "Mean difference",
      "95% CI",
      "*t*",
      "*p*"
    )
  )
  expect_identical(
    as.character(tab),
    c(
      "Table: Welch Two Sample t-test",
      "",
      "|Outcome  | Predictor | No cloak | Cloak | Mean difference |        95% CI |   *t* |   *p* |",
      "|:--------|:---------:|:--------:|:-----:|:---------------:|:-------------:|:-----:|:-----:|",
      "|mischief |     cloak |     3.75 |     5 |           -1.25 | (-2.76, 0.26) | -1.71 | 0.101 |",
      "\nAlternative hypothesis: true difference in means between group No cloak and group Cloak is not equal to 0\n"
    )
  )

  # robust t-test based on trimmed means
  skip_if_not_installed("WSR2")
  cloak_yn <- WRS2::yuen(mischief ~ cloak, data = cloak_tib)
  out <- model_parameters(cloak_yn)
  expect_identical(
    capture.output(print(out, table_width = Inf)),
    c(
      "Yuen's test on trimmed means for independent samples",
      "",
      "t(12.26) |     p | Difference | Difference 95% CI | Estimate |                         Effectsize",
      "-------------------------------------------------------------------------------------------------",
      "1.48     | 0.165 |         -1 |     [-2.47, 0.47] |     0.40 | Explanatory measure of effect size"
    )
  )
})
