requiet("glmmTMB")


skip_if(getRversion() < "4.0.0")
options("parameters_interaction" = "*")

# Splitting model components ----

data("Salamanders")
model <- glmmTMB(count ~ spp + mined + (1 | site),
  ziformula = ~ spp + mined,
  family = nbinom2(),
  data = Salamanders
)
out <- model_parameters(model, exponentiate = TRUE)

test_that("print model with multiple components", {
  expect_snapshot(print(out))
  expect_snapshot(print(out, split_component = FALSE))
})

# Adding model summaries -----

model <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
out <- model_parameters(model, summary = TRUE)

test_that("adding model summaries", {
  expect_snapshot(print(out))
})

# Group parameters ------

data(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$gear <- as.factor(mtcars$gear)
model <- lm(mpg ~ hp + gear * vs + cyl + drat, data = mtcars)

# don't select "Intercept" parameter
out <- model_parameters(model, drop = "^\\(Intercept")

test_that("grouped parameters", {
  expect_snapshot(
    print(out, groups = list(
      "Engine" = c("cyl6", "cyl8", "vs", "hp"),
      "Interactions" = c("gear4:vs", "gear5:vs"),
      "Controls" = c(2, 3, 7)
    ))
  )
  expect_snapshot(
    print(out,
      sep = "  ",
      groups = list(
        "Engine" = c("cyl6", "cyl8", "vs", "hp"),
        "Interactions" = c("gear4:vs", "gear5:vs"),
        "Controls" = c(2, 3, 7)
      )
    )
  )
})


# Table templates ------

test_that("select pattern", {
  expect_snapshot(print(out, select = "{coef} ({se})"))
  expect_snapshot(print(out, select = "{coef}{stars}|[{ci}]"))
  expect_snapshot(
    print(out, groups = list(
      "Engine" = c("cyl6", "cyl8", "vs", "hp"),
      "Interactions" = c("gear4:vs", "gear5:vs"),
      "Controls" = c(2, 3, 7)
    ), select = "{coef}{stars}|[{ci}]")
  )
  expect_snapshot(
    print(out,
      sep = "  ",
      groups = list(
        "Engine" = c("cyl6", "cyl8", "vs", "hp"),
        "Interactions" = c("gear4:vs", "gear5:vs"),
        "Controls" = c(2, 3, 7)
      ),
      select = "{coef}{stars}|[{ci}]"
    )
  )
})

options("parameters_interaction" = NULL)
