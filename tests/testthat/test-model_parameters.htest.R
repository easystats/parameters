skip_if_not_installed("effectsize")

## TODO: add more tests for different htest objects and effectsize types

test_that("model_parameters.htest", {
  params <- model_parameters(cor.test(mtcars$mpg, mtcars$cyl, method = "pearson"))
  expect_named(
    params,
    c(
      "Parameter1",
      "Parameter2",
      "r",
      "CI",
      "CI_low",
      "CI_high",
      "t",
      "df_error",
      "p",
      "Method",
      "Alternative"
    )
  )
  expect_equal(params$r, -0.852, tolerance = 0.05)

  expect_warning({
    params <- model_parameters(cor.test(mtcars$mpg, mtcars$cyl, method = "spearman"))
  })
  expect_equal(params$rho, -0.9108, tolerance = 0.05)

  expect_warning({
    params <- model_parameters(cor.test(mtcars$mpg, mtcars$cyl, method = "kendall"))
  })
  expect_equal(params$tau, -0.795, tolerance = 0.05)

  params <- model_parameters(t.test(iris$Sepal.Width, iris$Sepal.Length))
  expect_equal(params$Difference, -2.786, tolerance = 0.05)

  params <- model_parameters(t.test(mtcars$mpg ~ mtcars$vs))
  expect_equal(params$Difference, -7.940, tolerance = 0.05)

  params <- model_parameters(t.test(iris$Sepal.Width, mu = 1))
  expect_equal(params$Difference, 2.0573, tolerance = 0.05)
})

test_that("model_parameters.htest-2", {
  x <- c(A = 20, B = 15, C = 25)
  mp <- model_parameters(chisq.test(x))
  expect_named(mp, c("Chi2", "df", "p", "Method"))
})


test_that("model_parameters-chisq-test NULL", {
  mp <- model_parameters(stats::chisq.test(table(mtcars$am)))
  expect_equal(mp$Chi2, 1.125, tolerance = 1e-3)
  expect_named(mp, c("Chi2", "df", "p", "Method"))
})


test_that("model_parameters-chisq-test two way table", {
  mp2 <- suppressWarnings(model_parameters(stats::chisq.test(table(
    mtcars$am,
    mtcars$cyl
  ))))
  expect_equal(mp2$Chi2, 8.740733, tolerance = 1e-3)
  expect_named(mp2, c("Chi2", "df", "p", "Method"))
})

test_that("model_parameters-chisq-test works with `svychisq` objects", {
  skip_if_not_installed("survey")
  data(api, package = "survey")

  set.seed(123)
  dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
  m <- survey::svychisq(~ sch.wide + stype, dclus1)
  mp <- model_parameters(m)

  expect_equal(mp$F, 5.19337, tolerance = 1e-3)
  expect_named(mp, c("F", "df", "df_error", "p", "Method"))
})

test_that("model_parameters-chisq-test adjusted", {
  expect_message({
    mp <- model_parameters(
      stats::chisq.test(table(mtcars$am)),
      es_type = "phi",
      ci = 0.95
    )
  })
  expect_equal(mp$Chi2, 1.125, tolerance = 1e-3)
  expect_named(mp, c("Chi2", "df", "p", "Method"))
})

test_that("model_parameters-t-test standardized d", {
  params <- model_parameters(
    t.test(iris$Sepal.Width, iris$Sepal.Length),
    es_type = "cohens_d"
  )
  expect_equal(params$Cohens_d, -4.210417, tolerance = 0.05)
  expect_equal(params$d_CI_low, -4.655306, tolerance = 0.05)
  expect_named(
    params,
    c(
      "Parameter1",
      "Parameter2",
      "Mean_Parameter1",
      "Mean_Parameter2",
      "Difference",
      "CI",
      "CI_low",
      "CI_high",
      "Cohens_d",
      "d_CI_low",
      "d_CI_high",
      "t",
      "df_error",
      "p",
      "Method",
      "Alternative"
    )
  )
})

test_that("model_parameters-t-test standardized d-2", {
  mp <- model_parameters(
    t.test(mtcars$mpg ~ mtcars$vs),
    es_type = "cohens_d",
    verbose = FALSE
  )
  expect_equal(mp$Cohens_d, -1.696032, tolerance = 1e-3)
  expect_named(
    mp,
    c(
      "Parameter",
      "Group",
      "Mean_Group1",
      "Mean_Group2",
      "Difference",
      "CI",
      "CI_low",
      "CI_high",
      "Cohens_d",
      "d_CI_low",
      "d_CI_high",
      "t",
      "df_error",
      "p",
      "Method",
      "Alternative"
    )
  )
})

test_that("model_parameters-t-test reports the same unregarding of interface", {
  g1 <- 1:10
  g2 <- 7:20
  df <- data.frame(y = c(g1, g2), x = rep(c(0, 1), c(length(g1), length(g2))))
  compare_only <- c(
    "Difference",
    "CI",
    "CI_low",
    "CI_high",
    "t",
    "df_error",
    "p",
    "Method"
  )
  default_ttest <- model_parameters(t.test(x = g1, y = g2))[compare_only]
  formula_ttest <- model_parameters(t.test(y ~ x, df))[compare_only]
  expect_equal(default_ttest, formula_ttest, ignore_attr = TRUE)
})

test_that("model_parameters-Box.test works, and ignores partial matching", {
  set.seed(123)
  ts1 <- ts(rnorm(200, mean = 10, sd = 3))
  result1 <- Box.test(ts1, lag = 5, type = "Box-Pierce", fitdf = 2)
  result2 <- Box.test(ts1, lag = 5, type = "Ljung-Box", fitdf = 2)

  out1 <- model_parameters(result1)
  out2 <- model_parameters(result1, effects = "all")
  expect_equal(out1, out2, ignore_attr = TRUE)
  expect_named(out1, c("Parameter", "Chi2", "df_error", "p", "Method"))

  out1 <- model_parameters(result2)
  out2 <- model_parameters(result2, effects = "all")
  expect_equal(out1, out2, ignore_attr = TRUE)
})

test_that("model_parameters-htests removes $ from parameter and group names", {
  data(sleep)
  sleep2 <- reshape(sleep, direction = "wide", idvar = "ID", timevar = "group")
  out <- format(model_parameters(t.test(sleep2$extra.1, sleep2$extra.2, paired = TRUE)))
  expect_identical(out$Parameter, "extra.1")
  expect_identical(out$Group, "extra.2")
})

test_that("model_parameters-htests does not fail for fisher test > 2x3", {
  data(mtcars)
  m <- fisher.test(mtcars$cyl, mtcars$gear)
  out <- model_parameters(m)
  expect_named(out, c("Chi2", "df", "p", "Method", "Alternative"))
  expect_true(is.na(out$Chi2))
  expect_equal(out$p, 8e-05, tolerance = 1e-4)
})

test_that("model_parameters-htests no hard-coded formatting for proportions tests", {
  smokers <- c(83, 90, 129, 70)
  patients <- c(86, 93, 136, 82)
  out <- model_parameters(prop.test(smokers, patients))

  expect_identical(
    capture.output(out),
    c(
      "4-sample test for equality of proportions without continuity correction",
      "",
      "Proportion                        | Chi2(3) |     p",
      "---------------------------------------------------",
      "96.51% / 96.77% / 94.85% / 85.37% |   12.60 | 0.006",
      "",
      "Alternative hypothesis: two.sided"
    )
  )
  expect_equal(out$Estimate_1, 0.96512, tolerance = 1e-4)
  expect_named(
    out,
    c(
      "Estimate_1",
      "Estimate_2",
      "Estimate_3",
      "Estimate_4",
      "Chi2",
      "df",
      "p",
      "Method",
      "Alternative"
    )
  )

  # for penguins data
  skip_if(getRversion() < "4.5.0")
  data(penguins, package = "datasets")
  out <- model_parameters(prop.test(table(penguins$sex)))
  expect_identical(
    capture.output(out),
    c(
      "1-sample proportions test",
      "",
      "Proportion |       95% CI | Chi2(1) | Null_value |     p",
      "--------------------------------------------------------",
      "49.55%     | [0.44, 0.55] |    0.01 |       0.50 | 0.913",
      "",
      "Alternative hypothesis: true p is not equal to 0.5"
    )
  )
  expect_equal(out$Estimate, 0.4955, tolerance = 1e-3)
  expect_named(
    out,
    c(
      "Estimate",
      "CI",
      "CI_low",
      "CI_high",
      "Chi2",
      "df",
      "Null_value",
      "p",
      "Method",
      "Alternative"
    )
  )
})


test_that("model_parameters-BSDA", {
  skip_if_not_installed("BSDA")

  # t-test
  m <- suppressWarnings(BSDA::tsum.test(
    mean.x = 5.6,
    s.x = 2.1,
    n.x = 16,
    mu = 4.9,
    alternative = "greater"
  ))
  out <- model_parameters(m)
  expect_identical(
    capture.output(out),
    c(
      "One-sample t-Test",
      "",
      "Parameter    | Mean |   mu | Difference |    95% CI | t(15) |     p",
      "-------------------------------------------------------------------",
      "Summarized x | 5.60 | 4.90 |       0.70 | [4.68,  ] |  1.33 | 0.101",
      "",
      "Alternative hypothesis: true mean is greater than 4.9"
    )
  )

  m <- suppressWarnings(BSDA::tsum.test(
    mean.x = 5.6,
    s.x = 2.1,
    n.x = 16,
    mean.y = 7.3,
    s.y = 2.4,
    n.y = 18,
    mu = 4.9,
    alternative = "greater"
  ))
  out <- model_parameters(m)
  expect_identical(
    capture.output(print(out, table_width = Inf)),
    c(
      "Welch Modified Two-Sample t-Test",
      "",
      "Parameter1   | Parameter2 | Mean_Parameter1 | Mean_Parameter2 | Difference |     95% CI | t(32.00) |      p",
      "-----------------------------------------------------------------------------------------------------------",
      "Summarized x |          y |            5.60 |            7.30 |      -1.70 | [-3.01,  ] |    -8.55 | > .999",
      "",
      "Alternative hypothesis: true difference in means is greater than 4.9"
    )
  )

  # z-test
  x <- c(7.8, 6.6, 6.5, 7.4, 7.3, 7.0, 6.4, 7.1, 6.7, 7.6, 6.8)
  y <- c(4.5, 5.4, 6.1, 6.1, 5.4, 5.0, 4.1, 5.5)
  m <- BSDA::zsum.test(mean(x), sigma.x = 0.5, n.x = 11, mu = 2)
  out <- model_parameters(m)
  expect_identical(
    capture.output(out),
    c(
      "One-sample z-Test",
      "",
      "Parameter    | Mean | mu | Difference |       95% CI |     z |      p",
      "---------------------------------------------------------------------",
      "Summarized x | 7.02 |  2 |       5.02 | [6.72, 7.31] | 33.29 | < .001",
      "",
      "Alternative hypothesis: true mean is not equal to 2"
    )
  )

  m <- BSDA::zsum.test(
    mean(x),
    sigma.x = 0.5,
    n.x = 11,
    mean(y),
    sigma.y = 0.5,
    n.y = 8,
    mu = 2
  )
  out <- model_parameters(m)
  expect_identical(
    capture.output(print(out, table_width = Inf)),
    c(
      "Two-sample z-Test",
      "",
      "Parameter          | Mean_Parameter1 | Mean_Parameter2 | Difference |       95% CI |     z |     p",
      "--------------------------------------------------------------------------------------------------",
      "Summarized x and y |            7.02 |            5.26 |       1.76 | [1.30, 2.21] | -1.05 | 0.293",
      "",
      "Alternative hypothesis: true difference in means is not equal to 2"
    )
  )
})
