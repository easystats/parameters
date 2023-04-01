.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest) {
  test_that("predict.parameters_efa works with verbose", {
    skip_if_not_installed("psych")
    d <- psych::bfi[, 1:25]
    d <- na.omit(d)
    efa <- psych::fa(d, nfactors = 5)
    out <- model_parameters(efa, sort = TRUE, threshold = "max")

    predictions <- predict(
      out,
      names = c("Neuroticism", "Conscientiousness", "Extraversion", "Agreeableness", "Opennness"),
      verbose = FALSE
    )
    expect_identical(dim(predictions), as.integer(c(2436, 5)))
  })
}
