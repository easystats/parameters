test_that("format_model_parameters", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  withr::with_options(
    list(parameters_interaction = "*"),
    {
      d <- structure(list(
        Drought = structure(c(
          1L, 1L, 1L, 1L, 1L, 1L,
          2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
          2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L,
          2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
          2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
          2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
          2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
        ), levels = c("no", "yes"), class = "factor"), Tree.ID = structure(c(
          1L,
          1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L, 6L, 7L, 7L, 8L, 8L, 9L,
          9L, 10L, 10L, 11L, 11L, 12L, 12L, 13L, 13L, 14L, 14L, 15L, 15L,
          16L, 16L, 17L, 17L, 18L, 18L, 19L, 19L, 20L, 20L, 21L, 21L, 22L,
          22L, 23L, 23L, 24L, 24L, 25L, 25L, 26L, 26L, 27L, 27L, 28L, 28L,
          29L, 29L, 30L, 30L, 31L, 31L, 32L, 32L, 33L, 33L, 34L, 34L, 35L,
          35L, 36L, 36L, 37L, 37L, 38L, 38L, 39L, 39L, 40L, 40L, 41L, 41L,
          42L, 42L, 43L, 43L, 44L, 44L, 45L, 45L, 46L, 46L, 47L, 47L, 48L,
          48L, 1L, 1L, 2L, 2L, 3L, 3L, 10L, 10L, 11L, 11L, 12L, 12L, 13L,
          13L, 14L, 14L, 15L, 15L, 16L, 16L, 17L, 17L, 18L, 18L, 19L, 19L,
          21L, 21L, 22L, 22L, 23L, 23L, 24L, 24L, 25L, 25L, 26L, 26L, 27L,
          27L, 28L, 28L, 29L, 29L, 30L, 30L, 31L, 31L, 32L, 32L, 33L, 33L,
          37L, 37L, 38L, 38L, 39L, 39L, 43L, 43L, 44L, 44L, 45L, 45L, 46L,
          46L, 47L, 47L, 48L, 48L
        ), levels = c(
          "102_6", "102_7", "102_8",
          "105_1", "105_2", "105_4", "111_7", "111_8", "111_9", "113_2",
          "113_4", "113_5", "114_7", "114_8", "114_9", "116_6", "116_7",
          "116_9", "122_3", "122_4", "122_5", "132_3", "132_4", "132_5",
          "242_2", "242_4", "242_5", "243_1", "243_2", "243_4", "245_1",
          "245_2", "245_5", "246_1", "246_2", "246_3", "251_10", "251_8",
          "251_9", "253_7", "253_8", "253_9", "254_6", "254_7", "254_8",
          "267_10", "267_6", "267_8"
        ), class = "factor"), Stratum = structure(c(
          1L,
          2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L,
          2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L,
          2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L,
          2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L,
          2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L,
          2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L,
          2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L,
          2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L,
          2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L,
          2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L,
          2L, 1L, 2L, 1L, 2L
        ), levels = c("lower", "upper"), class = "factor"),
        Year = structure(c(
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
          2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
          2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
          2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
          2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
          2L, 2L, 2L, 2L, 2L, 2L
        ), levels = c("1", "2"), class = "factor"),
        Treatment = c(
          "Control", "Control", "Control", "Control",
          "Control", "Control", "Drought", "Drought", "Drought", "Drought",
          "Drought", "Drought", "Control", "Control", "Control", "Control",
          "Control", "Control", "Drought", "Drought", "Drought", "Drought",
          "Drought", "Drought", "Control", "Control", "Control", "Control",
          "Control", "Control", "Control", "Control", "Control", "Control",
          "Control", "Control", "Drought", "Drought", "Drought", "Drought",
          "Drought", "Drought", "Drought", "Drought", "Drought", "Drought",
          "Drought", "Drought", "Drought", "Drought", "Drought", "Drought",
          "Drought", "Drought", "Drought", "Drought", "Drought", "Drought",
          "Drought", "Drought", "Drought", "Drought", "Drought", "Drought",
          "Drought", "Drought", "Drought", "Drought", "Drought", "Drought",
          "Drought", "Drought", "Control", "Control", "Control", "Control",
          "Control", "Control", "Control", "Control", "Control", "Control",
          "Control", "Control", "Control", "Control", "Control", "Control",
          "Control", "Control", "Control", "Control", "Control", "Control",
          "Control", "Control", "Control", "Control", "Control", "Control",
          "Control", "Control", "Drought", "Drought", "Drought", "Drought",
          "Drought", "Drought", "Control", "Control", "Control", "Control",
          "Control", "Control", "Control", "Control", "Control", "Control",
          "Control", "Control", "Drought", "Drought", "Drought", "Drought",
          "Drought", "Drought", "Drought", "Drought", "Drought", "Drought",
          "Drought", "Drought", "Drought", "Drought", "Drought", "Drought",
          "Drought", "Drought", "Drought", "Drought", "Drought", "Drought",
          "Drought", "Drought", "Drought", "Drought", "Drought", "Drought",
          "Control", "Control", "Control", "Control", "Control", "Control",
          "Control", "Control", "Control", "Control", "Control", "Control",
          "Control", "Control", "Control", "Control", "Control", "Control"
        ), branch_miner_No = c(
          41L, 25L, 47L, 49L, 50L, 49L, 49L,
          50L, 49L, 50L, 48L, 47L, 49L, 49L, 50L, 49L, 49L, 43L, 41L,
          45L, 49L, 37L, 50L, 49L, 50L, 50L, 50L, 49L, 45L, 44L, 49L,
          48L, 50L, 48L, 50L, 49L, 44L, 38L, 50L, 34L, 38L, 28L, 47L,
          39L, 50L, 49L, 47L, 50L, 42L, 19L, 47L, 46L, 50L, 50L, 49L,
          46L, 49L, 50L, 40L, 45L, 50L, 50L, 41L, 44L, 50L, 50L, 50L,
          46L, 50L, 48L, 50L, 50L, 48L, 38L, 49L, 42L, 39L, 31L, 49L,
          33L, 38L, 49L, 48L, 48L, 49L, 49L, 48L, 50L, 45L, 37L, 28L,
          25L, 45L, 45L, 39L, 35L, 38L, 43L, 46L, 34L, 49L, 33L, 40L,
          47L, 47L, 39L, 46L, 31L, 47L, 40L, 47L, 45L, 47L, 42L, 48L,
          47L, 39L, 25L, 37L, 46L, 38L, 42L, 44L, 48L, 47L, 46L, 48L,
          49L, 38L, 44L, 39L, 31L, 41L, 42L, 44L, 18L, 23L, 48L, 26L,
          26L, 28L, 32L, 47L, 46L, 49L, 33L, 47L, 38L, 35L, 17L, 39L,
          30L, 44L, 42L, 47L, 36L, 8L, 33L, 32L, 37L, 33L, 38L, 32L,
          45L, 47L, 41L
        ), branch_miner_Yes = c(
          9L, 25L, 3L, 1L, 0L,
          1L, 1L, 0L, 1L, 0L, 2L, 3L, 1L, 1L, 0L, 1L, 1L, 7L, 9L, 5L,
          1L, 13L, 0L, 1L, 0L, 0L, 0L, 1L, 5L, 6L, 1L, 2L, 0L, 2L,
          0L, 1L, 6L, 12L, 0L, 16L, 12L, 22L, 3L, 11L, 0L, 1L, 3L,
          0L, 8L, 31L, 3L, 4L, 0L, 0L, 1L, 4L, 1L, 0L, 10L, 5L, 0L,
          0L, 9L, 6L, 0L, 0L, 0L, 4L, 0L, 2L, 0L, 0L, 2L, 12L, 1L,
          8L, 11L, 19L, 1L, 17L, 12L, 1L, 2L, 2L, 1L, 1L, 2L, 0L, 5L,
          13L, 22L, 25L, 5L, 5L, 11L, 15L, 12L, 7L, 4L, 16L, 1L, 17L,
          10L, 3L, 3L, 11L, 4L, 19L, 3L, 10L, 3L, 5L, 3L, 8L, 2L, 3L,
          11L, 25L, 13L, 4L, 12L, 8L, 6L, 2L, 3L, 4L, 2L, 1L, 12L,
          6L, 11L, 19L, 9L, 8L, 6L, 32L, 27L, 2L, 24L, 24L, 22L, 18L,
          3L, 4L, 1L, 17L, 3L, 12L, 15L, 33L, 11L, 20L, 6L, 8L, 3L,
          14L, 42L, 17L, 18L, 13L, 17L, 12L, 18L, 5L, 3L, 9L
        )
      ), row.names = c(
        NA,
        -166L
      ), class = "data.frame")

      d$Year <- factor(d$Year)
      d$Drought <- as.factor(d$Drought)
      d$Stratum <- as.factor(d$Stratum)
      levels(d$Stratum) <- list(lower = "shade", upper = "sun")
      d$Tree.ID <- as.factor(d$Tree.ID)

      mod <- lme4::glmer(cbind(branch_miner_Yes, branch_miner_No) ~ Drought * Stratum + Drought * Year + Year * Stratum + (1 | Tree.ID),
                         data = d, family = binomial(), na.action = na.exclude
      )
      out <- model_parameters(mod, component = "conditional")
      expect_identical(
        attributes(out)$pretty_names,
        c(
          `(Intercept)` = "(Intercept)", Droughtyes = "Drought [yes]",
          Stratumupper = "Stratum [upper]", Year2 = "Year [2]",
          `Droughtyes:Stratumupper` = "Drought [yes] * Stratum [upper]",
          `Droughtyes:Year2` = "Drought [yes] * Year [2]",
          `Stratumupper:Year2` = "Stratum [upper] * Year [2]"
        )
      )
    }
  )

})