requiet("parameters")

options("parameters_interaction" = "*")

data(iris)
lm1 <- lm(Sepal.Length ~ Species, data = iris)
lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)


# Basic -------

out <- compare_parameters(lm1, lm2, lm3)
test_that("multiple model", {
  expect_snapshot(print(out))
})

# templates --------------

out <- compare_parameters(lm1, lm2, lm3, style = "se_p")
test_that("templates", {
  expect_snapshot(print(out))
})

out <- compare_parameters(lm1, lm2, lm3, style = "{estimate}{stars} ({se})")
test_that("templates, glue-1", {
  expect_snapshot(print(out))
})

out <- compare_parameters(lm1, lm2, lm3, style = "{estimate} ({ci_low}, {ci_high}), p={p}{stars}")
test_that("templates, glue-2", {
  expect_snapshot(print(out))
})

out <- compare_parameters(lm1, lm2, lm3, style = "{estimate} ({se})|{p}")
test_that("templates, glue-3, separate columnns", {  
  expect_snapshot(print(out))
})

# grouping parameters --------------

lm1 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
lm2 <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

# remove intercept
out <- compare_parameters(lm1, lm2, drop = "^\\(Intercept")

test_that("templates, glue-3, separate columnns", {
  expect_snapshot(
    print(out, groups = list(
      Species = c(
        "Species (versicolor)",
        "Species (virginica)"
      ),
      Interactions = c(
        "Species (versicolor) * Petal Length",
        "Species (virginica) * Petal Length"
      ),
      Controls = "Petal Length"
    ))
  )
  expect_snapshot(
    print(out, groups = list(
      Species = c(
        "Species (versicolor)",
        "Species (virginica)"
      ),
      Interactions = c(
        "Species (versicolor) × Petal Length", # note the unicode char!
        "Species (virginica) × Petal Length"
      ),
      Controls = "Petal Length"
    ), style = "{estimate}{stars}")
  )
  expect_snapshot(
    print(out, groups = list(
      Species = c(
        "Species (versicolor)",
        "Species (virginica)"
      ),
      Interactions = c(
        "Species (versicolor) × Petal Length", # note the unicode char!
        "Species (virginica) × Petal Length"
      ),
      Controls = "Petal Length"
    ), style = "{estimate}|{p}")
  )
})

options("parameters_interaction" = NULL)
