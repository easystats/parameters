test_that("ordered factors", {
  data(PlantGrowth)
  m_ord <- lm(weight ~ as.ordered(group), PlantGrowth)
  pt <- parameters_type(m_ord)
  mp <- model_parameters(m_ord)
  expect_identical(pt$Type, c("intercept", "ordered", "ordered"))
  expect_identical(pt$Parameter, c("(Intercept)", "as.ordered(group).L", "as.ordered(group).Q"))
  expect_identical(mp$Parameter, c("(Intercept)", "as.ordered(group).L", "as.ordered(group).Q"))
  expect_identical(
    attributes(mp)$pretty_names,
    c(
      `(Intercept)` = "(Intercept)", `as.ordered(group).L` = "group [linear]",
      `as.ordered(group).Q` = "group [quadratic]"
    )
  )
})
