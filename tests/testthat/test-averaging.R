skip_on_cran()

skip_if_not_installed("MuMIn")
skip_if_not_installed("withr")
skip_if_not_installed("glmmTMB")
skip_if_not_installed("betareg")

withr::with_options(
  list(na.action = "na.fail"),
  test_that("MuMIn link functions", {
    library(MuMIn) # nolint
    set.seed(1234)
    dat <- data.frame(
      outcome = rbinom(n = 100, size = 1, prob = 0.35),
      var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.2)),
      var_cont = rnorm(n = 100, mean = 10, sd = 7),
      group = sample(letters[1:4], size = 100, replace = TRUE),
      stringsAsFactors = FALSE
    )
    dat$var_cont <- as.vector(scale(dat$var_cont))
    m1 <- glm(
      outcome ~ var_binom + var_cont,
      data = dat,
      family = binomial(link = "logit")
    )
    out <- MuMIn::model.avg(MuMIn::dredge(m1), fit = TRUE)
    mp <- model_parameters(out)
    expect_snapshot(print(mp))
  })
)

test_that("ggpredict, glmmTMB averaging", {
  library(MuMIn) # nolint
  data(FoodExpenditure, package = "betareg")
  m <- glmmTMB::glmmTMB(
    I(food / income) ~ income + (1 | persons),
    ziformula = ~1,
    data = FoodExpenditure,
    na.action = "na.fail",
    family = glmmTMB::beta_family()
  )
  set.seed(123)
  dr <- MuMIn::dredge(m)
  avg <- MuMIn::model.avg(object = dr, fit = TRUE)
  mp <- model_parameters(avg)
  expect_snapshot(print(mp))
})


withr::with_options(
  list(na.action = "na.fail"),
  test_that("ggpredict, poly averaging", {
    library(MuMIn)
    data(mtcars)
    mtcars$am <- factor(mtcars$am)

    set.seed(123)
    m <- lm(disp ~ mpg + I(mpg^2) + am + gear, mtcars)
    dr <- MuMIn::dredge(m, subset = dc(mpg, I(mpg^2)))
    dr <- subset(dr, !(has(mpg) & !has(I(mpg^2))))
    mod.avg.i <- MuMIn::model.avg(dr, fit = TRUE)
    mp <- model_parameters(mod.avg.i)
    expect_snapshot(print(mp))
  })
)

unloadNamespace("MuMIn")
