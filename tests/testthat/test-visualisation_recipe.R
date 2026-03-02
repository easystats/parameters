test_that("vis_recipe.cluster_analysis", {
  data(iris)
  result <- cluster_analysis(iris[, 1:4], n = 4)
  out <- visualisation_recipe(result)
  expect_named(out, c("l1", "l2", "l3"))
  expect_s3_class(out, "visualisation_recipe")
  expect_snapshot(print(out))
})
