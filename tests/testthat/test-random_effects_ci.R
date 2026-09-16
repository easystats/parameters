skip_on_cran()

skip_on_os("mac")
skip_if_not_installed("lme4")

data(sleepstudy, package = "lme4")
data(cake, package = "lme4")
set.seed(123)
sleepstudy$Months <- sample.int(4, nrow(sleepstudy), TRUE)

m1 <- suppressMessages(lme4::lmer(
  angle ~ temperature + (temperature | recipe) + (temperature | replicate),
  data = cake
))
m2 <- suppressMessages(lme4::lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy))
m3 <- suppressMessages(lme4::lmer(
  angle ~ temperature + (temperature | recipe),
  data = cake
))
m4 <- suppressMessages(lme4::lmer(
  angle ~ temperature + (temperature | replicate),
  data = cake
))
m5 <- suppressMessages(lme4::lmer(
  Reaction ~ Days + (Days + Months | Subject),
  data = sleepstudy
))

## TODO also check messages for profiled CI

expect_message(
  {
    mp1 <- model_parameters(m1, ci_random = TRUE)
  },
  regex = "meaningful"
)
mp2 <- model_parameters(m2, ci_random = TRUE)
expect_message(
  {
    mp3 <- model_parameters(m3, ci_random = TRUE)
  },
  regex = "meaningful"
)
expect_message(
  {
    mp4 <- model_parameters(m4, ci_random = TRUE)
  },
  regex = "meaningful"
)
expect_message(
  {
    mp5 <- model_parameters(m5, ci_random = TRUE)
  },
  regex = "meaningful"
)


# model 1 ---------------------

test_that("random effects CIs, two slopes, categorical", {
  # fmt: skip
  expect_equal(
    mp1$CI_low,
    c(
      28.75405, 4.97865, -1.95011, -2.70111, -3.62199, -2.69102,
      4.28813, 0.21457, 0.5742, 0.28738, 0.02089, 1e-05, 0.55402, 0,
      0, 0.2781, 0.38107, 0.00945, -0.65343, -0.7222, -1, -0.99521,
      -0.97784, -1, -1, -1, -1, -1, -1, -0.99594, -1, -0.90229, -0.99736,
      -1, -0.98035, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, 4.07963
    ),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  expect_identical(
    mp1$Parameter,
    c(
      "(Intercept)",
      "temperature.L",
      "temperature.Q",
      "temperature.C",
      "temperature^4",
      "temperature^5",
      "SD (Intercept)",
      "SD (Intercept)",
      "SD (temperature.L)",
      "SD (temperature.Q)",
      "SD (temperature.C)",
      "SD (temperature^4)",
      "SD (temperature^5)",
      "SD (temperature.L)",
      "SD (temperature.Q)",
      "SD (temperature.C)",
      "SD (temperature^4)",
      "SD (temperature^5)",
      "Cor (Intercept~temperature.L)",
      "Cor (Intercept~temperature.Q)",
      "Cor (Intercept~temperature.C)",
      "Cor (Intercept~temperature^4)",
      "Cor (Intercept~temperature^5)",
      "Cor (Intercept~temperature.L)",
      "Cor (Intercept~temperature.Q)",
      "Cor (Intercept~temperature.C)",
      "Cor (Intercept~temperature^4)",
      "Cor (Intercept~temperature^5)",
      "Cor (temperature.L~temperature.Q)",
      "Cor (temperature.L~temperature.C)",
      "Cor (temperature.L~temperature^4)",
      "Cor (temperature.L~temperature^5)",
      "Cor (temperature.Q~temperature.C)",
      "Cor (temperature.Q~temperature^4)",
      "Cor (temperature.Q~temperature^5)",
      "Cor (temperature.C~temperature^4)",
      "Cor (temperature.C~temperature^5)",
      "Cor (temperature^4~temperature^5)",
      "Cor (temperature.L~temperature.Q)",
      "Cor (temperature.L~temperature.C)",
      "Cor (temperature.L~temperature^4)",
      "Cor (temperature.L~temperature^5)",
      "Cor (temperature.Q~temperature.C)",
      "Cor (temperature.Q~temperature^4)",
      "Cor (temperature.Q~temperature^5)",
      "Cor (temperature.C~temperature^4)",
      "Cor (temperature.C~temperature^5)",
      "Cor (temperature^4~temperature^5)",
      "SD (Observations)"
    )
  )

  expect_identical(
    mp1$Group,
    c(
      "",
      "",
      "",
      "",
      "",
      "",
      "replicate",
      "recipe",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "Residual"
    )
  )
})


# model 2 ---------------------

test_that("random effects CIs, simple slope", {
  expect_equal(
    mp2$CI_low,
    c(237.93546, 7.41637, 15.5817, 3.91828, -0.50907, 22.80044),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  expect_identical(
    mp2$Parameter,
    c(
      "(Intercept)",
      "Days",
      "SD (Intercept)",
      "SD (Days)",
      "Cor (Intercept~Days)",
      "SD (Observations)"
    )
  )

  expect_identical(mp2$Group, c("", "", "Subject", "Subject", "Subject", "Residual"))
})


# model 3 ---------------------

test_that("random effects CIs, categorical slope-1", {
  expect_equal(
    mp3$CI_low[14:28],
    c(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7.09933),
    tolerance = 1e-2,
    ignore_attr = TRUE
  )

  expect_equal(
    mp3$CI_low[1:12],
    c(
      30.91139,
      4.33247,
      -2.6798,
      -3.20703,
      -4.07681,
      -3.27237,
      0.06301,
      0,
      0,
      0.1192,
      0.32213,
      0
    ),
    tolerance = 1e-2,
    ignore_attr = TRUE
  )
  expect_identical(
    mp3$Parameter,
    c(
      "(Intercept)",
      "temperature.L",
      "temperature.Q",
      "temperature.C",
      "temperature^4",
      "temperature^5",
      "SD (Intercept)",
      "SD (temperature.L)",
      "SD (temperature.Q)",
      "SD (temperature.C)",
      "SD (temperature^4)",
      "SD (temperature^5)",
      "Cor (Intercept~temperature.L)",
      "Cor (Intercept~temperature.Q)",
      "Cor (Intercept~temperature.C)",
      "Cor (Intercept~temperature^4)",
      "Cor (Intercept~temperature^5)",
      "Cor (temperature.L~temperature.Q)",
      "Cor (temperature.L~temperature.C)",
      "Cor (temperature.L~temperature^4)",
      "Cor (temperature.L~temperature^5)",
      "Cor (temperature.Q~temperature.C)",
      "Cor (temperature.Q~temperature^4)",
      "Cor (temperature.Q~temperature^5)",
      "Cor (temperature.C~temperature^4)",
      "Cor (temperature.C~temperature^5)",
      "Cor (temperature^4~temperature^5)",
      "SD (Observations)"
    )
  )

  expect_identical(
    mp3$Group,
    c(
      "",
      "",
      "",
      "",
      "",
      "",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "Residual"
    )
  )
})


# model 4 ---------------------

test_that("random effects CIs, categorical slope-2", {
  # fmt: skip
  expect_equal(
    mp4$CI_low,
    c(
      28.88447, 4.96795, -1.93231, -1.98596, -2.68856, -2.55248,
      4.28006, 0.51326, 0.23954, 0.01238, 0, 0.5001, -0.68888, -0.74701,
      -1, -0.99746, -0.98211, -1, -0.99788, -1, -0.92289, -0.99878,
      -1, -0.98562, -1, -1, -1, 4.21137
    ),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  expect_identical(
    mp4$Parameter,
    c(
      "(Intercept)",
      "temperature.L",
      "temperature.Q",
      "temperature.C",
      "temperature^4",
      "temperature^5",
      "SD (Intercept)",
      "SD (temperature.L)",
      "SD (temperature.Q)",
      "SD (temperature.C)",
      "SD (temperature^4)",
      "SD (temperature^5)",
      "Cor (Intercept~temperature.L)",
      "Cor (Intercept~temperature.Q)",
      "Cor (Intercept~temperature.C)",
      "Cor (Intercept~temperature^4)",
      "Cor (Intercept~temperature^5)",
      "Cor (temperature.L~temperature.Q)",
      "Cor (temperature.L~temperature.C)",
      "Cor (temperature.L~temperature^4)",
      "Cor (temperature.L~temperature^5)",
      "Cor (temperature.Q~temperature.C)",
      "Cor (temperature.Q~temperature^4)",
      "Cor (temperature.Q~temperature^5)",
      "Cor (temperature.C~temperature^4)",
      "Cor (temperature.C~temperature^5)",
      "Cor (temperature^4~temperature^5)",
      "SD (Observations)"
    )
  )

  expect_identical(
    mp4$Group,
    c(
      "",
      "",
      "",
      "",
      "",
      "",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "replicate",
      "Residual"
    )
  )
})


# model 5 ---------------------

test_that("random effects CIs, double slope", {
  expect_equal(
    mp5$CI_low,
    c(237.99863, 7.4022, 12.63814, 3.91669, 0, -0.58599, -1, -1, 22.65226),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  expect_identical(
    mp5$Parameter,
    c(
      "(Intercept)",
      "Days",
      "SD (Intercept)",
      "SD (Days)",
      "SD (Months)",
      "Cor (Intercept~Days)",
      "Cor (Intercept~Months)",
      "Cor (Days~Months)",
      "SD (Observations)"
    )
  )

  expect_identical(
    mp5$Group,
    c(
      "",
      "",
      "Subject",
      "Subject",
      "Subject",
      "Subject",
      "Subject",
      "Subject",
      "Residual"
    )
  )
})


# no random intercept --------------------------
test_that("random effects CIs, simple slope", {
  data(sleepstudy, package = "lme4")
  set.seed(123)
  sleepstudy$Months <- sample.int(4, nrow(sleepstudy), TRUE)

  m2 <- lme4::lmer(Reaction ~ Days + (0 + Days | Subject), data = sleepstudy)
  m5 <- lme4::lmer(Reaction ~ Days + (0 + Days + Months | Subject), data = sleepstudy)

  mp2 <- model_parameters(m2)
  mp5 <- model_parameters(m5)
  expect_equal(
    mp2$CI_low,
    c(243.47155, 6.77765, 5.09041, 26.01525),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  expect_identical(
    mp2$Parameter,
    c("(Intercept)", "Days", "SD (Days)", "SD (Observations)")
  )

  expect_equal(
    mp5$CI_low,
    c(241.61021, 7.43503, 4.11446, 2.69857, -0.40595, 24.632),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  expect_identical(
    mp5$Parameter,
    c(
      "(Intercept)",
      "Days",
      "SD (Days)",
      "SD (Months)",
      "Cor (Days~Months)",
      "SD (Observations)"
    )
  )
})


# poly random slope --------------------------
test_that("random effects CIs, poly slope", {
  data(cake, package = "lme4")
  suppressMessages({
    m <- lme4::lmer(
      angle ~ poly(temp, 2) + (poly(temp, 2) | replicate) + (1 | recipe),
      data = cake
    )
  })
  mp <- model_parameters(m, ci_random = TRUE)

  # fmt: skip
  expect_equal(
    mp$CI_low,
    c(
      28.78854, 33.56292, -12.84337, 4.27419, 0.16217, 2.30959, 0.87852,
      -0.81742, -0.80941, -1, 4.32856
    ),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  expect_identical(
    mp$Parameter,
    c(
      "(Intercept)",
      "poly(temp, 2)1",
      "poly(temp, 2)2",
      "SD (Intercept)",
      "SD (Intercept)",
      "SD (poly(temp, 2)1)",
      "SD (poly(temp, 2)2)",
      "Cor (Intercept~poly(temp, 2)1)",
      "Cor (Intercept~poly(temp, 2)2)",
      "Cor (poly(temp, 2)1~poly(temp, 2)2)",
      "SD (Observations)"
    )
  )
})


# poly and categorical random slope --------------------------

test_that("random effects CIs, poly categorical slope", {
  ## NOTE check back every now and then and see if tests still work
  skip("works interactively")

  m <- lme4::lmer(
    angle ~ poly(temp, 2) + (poly(temp, 2) | replicate) + (temperature | recipe),
    data = cake
  )
  mp <- model_parameters(m, effects = "random", ci = TRUE)

  # fmt: skip
  expect_equal(
    mp$CI_low,
    c(
      4.2792, 0.21989, 2.70288, 1.18654, 0, 0, 0.2559, 0.53782, 0,
      -0.77131, -0.77805, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, 4.2204
    ),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  expect_identical(
    mp$Parameter,
    c(
      "SD (Intercept)",
      "SD (Intercept)",
      "SD (poly(temp, 2)1)",
      "SD (poly(temp, 2)2)",
      "SD (temperature.L)",
      "SD (temperature.Q)",
      "SD (temperature.C)",
      "SD (temperature^4)",
      "SD (temperature^5)",
      "Cor (Intercept~poly(temp, 2)1)",
      "Cor (Intercept~poly(temp, 2)2)",
      "Cor (Intercept~temperature.L)",
      "Cor (Intercept~temperature.Q)",
      "Cor (Intercept~temperature.C)",
      "Cor (Intercept~temperature^4)",
      "Cor (Intercept~temperature^5)",
      "Cor (poly(temp, 2)1~poly(temp, 2)2)",
      "Cor (temperature.L~temperature.Q)",
      "Cor (temperature.L~temperature.C)",
      "Cor (temperature.L~temperature^4)",
      "Cor (temperature.L~temperature^5)",
      "Cor (temperature.Q~temperature.C)",
      "Cor (temperature.Q~temperature^4)",
      "Cor (temperature.Q~temperature^5)",
      "Cor (temperature.C~temperature^4)",
      "Cor (temperature.C~temperature^5)",
      "Cor (temperature^4~temperature^5)",
      "SD (Observations)"
    )
  )

  expect_identical(
    mp$Group,
    c(
      "replicate",
      "recipe",
      "replicate",
      "replicate",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "replicate",
      "replicate",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "replicate",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "recipe",
      "Residual"
    )
  )
})
