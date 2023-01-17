skip_if(getRversion() < "4.0.0")
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

out <- compare_parameters(lm1, lm2, lm3, select = "se_p")
test_that("templates", {
  expect_snapshot(print(out))
})

out <- compare_parameters(lm1, lm2, lm3, select = "{estimate}{stars} ({se})")
test_that("templates, glue-1", {
  expect_snapshot(print(out))
})

out <- compare_parameters(lm1, lm2, lm3, select = "{estimate} ({ci_low}, {ci_high}), p={p}{stars}")
test_that("templates, glue-2", {
  expect_snapshot(print(out))
})

out <- compare_parameters(lm1, lm2, lm3, select = "{estimate} ({se})|{p}")
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
        "Species (versicolor) * Petal Length", # note the unicode char!
        "Species (virginica) * Petal Length"
      ),
      Controls = "Petal Length"
    ), select = "{estimate}{stars}")
  )
  expect_snapshot(
    print(out, groups = list(
      Species = c(
        "Species (versicolor)",
        "Species (virginica)"
      ),
      Interactions = c(
        "Species (versicolor) * Petal Length", # note the unicode char!
        "Species (virginica) * Petal Length"
      ),
      Controls = "Petal Length"
    ), select = "{estimate}|{p}")
  )
})


.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest && requiet("glmmTMB")) {
  data("fish")

  m0 <- glm(count ~ child + camper, data = fish, family = poisson())

  m1 <- glmmTMB(
    count ~ child + camper + (1 | persons) + (1 | ID),
    data = fish,
    family = poisson()
  )

  m2 <- glmmTMB(
    count ~ child + camper + zg + (1 | ID),
    ziformula = ~ child + (1 | persons),
    data = fish,
    family = truncated_poisson()
  )

  cp <- compare_parameters(m0, m1, m2, effects = "all", component = "all")
  test_that("combination of different models", {
    expect_snapshot(print(cp))
  })
}

options("parameters_interaction" = NULL)
