skip_on_cran()
skip_if_not_installed("withr")
skip_if(getRversion() < "4.0.0")

withr::with_options(
  list(parameters_interaction = "*", easystats_table_width = Inf),
  {
    # Splitting model components ----
    test_that("print model with multiple components", {
      skip_if_not_installed("glmmTMB")
      data("Salamanders", package = "glmmTMB")
      model <- glmmTMB::glmmTMB(count ~ spp + mined + (1 | site),
        ziformula = ~ spp + mined,
        family = glmmTMB::nbinom2(),
        data = Salamanders
      )
      out <- model_parameters(model, exponentiate = TRUE)
      expect_snapshot(print(out))
      expect_snapshot(print(out, split_component = FALSE))
    })

    # Adding model summaries -----
    test_that("adding model summaries", {
      # summary doesn't show the R2 if performance is not installed so the
      # snapshot breaks between R CMD check "classic" and "strict"
      skip_if_not_installed("performance")
      model <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
      out <- model_parameters(model, include_info = TRUE)
      expect_snapshot(print(out))
    })

    # Group parameters ------
    test_that("grouped parameters", {
      mtcars$cyl <- as.factor(mtcars$cyl)
      mtcars$gear <- as.factor(mtcars$gear)
      model <- lm(mpg ~ hp + gear * vs + cyl + drat, data = mtcars)

      # don't select "Intercept" parameter
      out <- model_parameters(model, drop = "^\\(Intercept")
      expect_snapshot(
        print(out, groups = list(
          Engine = c("cyl6", "cyl8", "vs", "hp"),
          Interactions = c("gear4:vs", "gear5:vs"),
          Controls = c(2, 3, 7)
        ))
      )
      expect_snapshot(
        print(out,
          sep = "  ",
          groups = list(
            Engine = c("cyl6", "cyl8", "vs", "hp"),
            Interactions = c("gear4:vs", "gear5:vs"),
            Controls = c(2, 3, 7)
          )
        )
      )
    })


    # Digits ------
    test_that("digits and ci_digits", {
      mtcars$cyl <- as.factor(mtcars$cyl)
      mtcars$gear <- as.factor(mtcars$gear)
      model <- lm(mpg ~ hp + gear + vs + cyl + drat, data = mtcars)
      expect_snapshot(model_parameters(model, digits = 4))
      expect_snapshot(model_parameters(model, digits = 4, ci_digits = 1))
      out <- model_parameters(model)
      expect_snapshot(print(out, digits = 4))
      expect_snapshot(print(out, digits = 4, ci_digits = 1))
    })


    # Table templates ------
    test_that("select pattern", {
      mtcars$cyl <- as.factor(mtcars$cyl)
      mtcars$gear <- as.factor(mtcars$gear)
      model <- lm(mpg ~ hp + gear * vs + cyl + drat, data = mtcars)

      # don't select "Intercept" parameter
      out <- model_parameters(model, drop = "^\\(Intercept")
      expect_snapshot(
        print(out, groups = list(
          Engine = c("cyl6", "cyl8", "vs", "hp"),
          Interactions = c("gear4:vs", "gear5:vs"),
          Controls = c(2, 3, 7)
        ))
      )
      expect_snapshot(print(out, select = "{coef} ({se})"))
      expect_snapshot(print(out, select = "{coef}{stars}|[{ci}]"))
      expect_snapshot(
        print(out, groups = list(
          Engine = c("cyl6", "cyl8", "vs", "hp"),
          Interactions = c("gear4:vs", "gear5:vs"),
          Controls = c(2, 3, 7)
        ), select = "{coef}{stars}|[{ci}]")
      )
      expect_snapshot(
        print(out,
          sep = "  ",
          groups = list(
            Engine = c("cyl6", "cyl8", "vs", "hp"),
            Interactions = c("gear4:vs", "gear5:vs"),
            Controls = c(2, 3, 7)
          ),
          select = "{coef}{stars}|[{ci}]"
        )
      )
    })
  }
)

withr::with_options(
  list(parameters_warning_exponentiate = TRUE),
  test_that("no more message about interpretation of log-resoponse", {
    data(mtcars)
    m <- lm(log(mpg) ~ gear, data = mtcars)
    out <- model_parameters(m, exponentiate = TRUE)
    expect_snapshot(print(out))
  })
)

withr::with_options(
  list(parameters_warning_exponentiate = TRUE),
  test_that("print model with multiple components", {
    skip_if_not_installed("mgcv")
    m <- mgcv::gam(vs ~ s(mpg), data = mtcars, family = "binomial")
    expect_snapshot(print(model_parameters(m)))
  })
)
