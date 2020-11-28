if (require("testthat") && require("parameters") && require("splines")) {
  data(iris)
  set.seed(123)
  iris$cat <- sample(LETTERS[1:4], nrow(iris), replace = TRUE)

  test_that("format_parameters-1", {
    model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
    fp <- format_parameters(model)
    expect_equal(fp, c(
      `(Intercept)` = "(Intercept)", Speciesversicolor = "Species [versicolor]",
      Speciesvirginica = "Species [virginica]", Sepal.Width = "Sepal.Width",
      `Speciesversicolor:Sepal.Width` = "Species [versicolor] * Sepal.Width",
      `Speciesvirginica:Sepal.Width` = "Species [virginica] * Sepal.Width"
    ))
  })

  test_that("format_parameters-2", {
    model <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)
    fp <- format_parameters(model)
    expect_equal(fp, c(
      `(Intercept)` = "(Intercept)", Sepal.Width = "Sepal.Width",
      Speciesversicolor = "Species [versicolor]", Speciesvirginica = "Species [virginica]",
      `Sepal.Width:Speciesversicolor` = "Sepal.Width * Species [versicolor]",
      `Sepal.Width:Speciesvirginica` = "Sepal.Width * Species [virginica]"
    ))
  })

  test_that("format_parameters-3", {
    model <- lm(Sepal.Length ~ Species * Sepal.Width * Petal.Length, data = iris)
    fp <- format_parameters(model)
    expect_equal(fp, c(
      `(Intercept)` = "(Intercept)", Speciesversicolor = "Species [versicolor]",
      Speciesvirginica = "Species [virginica]", Sepal.Width = "Sepal.Width",
      Petal.Length = "Petal.Length", `Speciesversicolor:Sepal.Width` = "Species [versicolor] * Sepal.Width",
      `Speciesvirginica:Sepal.Width` = "Species [virginica] * Sepal.Width",
      `Speciesversicolor:Petal.Length` = "Species [versicolor] * Petal.Length",
      `Speciesvirginica:Petal.Length` = "Species [virginica] * Petal.Length",
      `Sepal.Width:Petal.Length` = "Sepal.Width * Petal.Length", `Speciesversicolor:Sepal.Width:Petal.Length` = "(Species [versicolor] * Sepal.Width) * Petal.Length",
      `Speciesvirginica:Sepal.Width:Petal.Length` = "(Species [virginica] * Sepal.Width) * Petal.Length"
    ))
  })

  test_that("format_parameters-4", {
    model <- lm(Sepal.Length ~ Species * cat * Petal.Length, data = iris)
    fp <- format_parameters(model)
    expect_equal(fp, c(
      `(Intercept)` = "(Intercept)", Speciesversicolor = "Species [versicolor]",
      Speciesvirginica = "Species [virginica]", catB = "cat [B]", catC = "cat [C]",
      catD = "cat [D]", Petal.Length = "Petal.Length", `Speciesversicolor:catB` = "Species [versicolor] * cat [B]",
      `Speciesvirginica:catB` = "Species [virginica] * cat [B]", `Speciesversicolor:catC` = "Species [versicolor] * cat [C]",
      `Speciesvirginica:catC` = "Species [virginica] * cat [C]", `Speciesversicolor:catD` = "Species [versicolor] * cat [D]",
      `Speciesvirginica:catD` = "Species [virginica] * cat [D]", `Speciesversicolor:Petal.Length` = "Species [versicolor] * Petal.Length",
      `Speciesvirginica:Petal.Length` = "Species [virginica] * Petal.Length",
      `catB:Petal.Length` = "cat [B] * Petal.Length", `catC:Petal.Length` = "cat [C] * Petal.Length",
      `catD:Petal.Length` = "cat [D] * Petal.Length", `Speciesversicolor:catB:Petal.Length` = "(Species [versicolor] * cat [B]) * Petal.Length",
      `Speciesvirginica:catB:Petal.Length` = "(Species [virginica] * cat [B]) * Petal.Length",
      `Speciesversicolor:catC:Petal.Length` = "(Species [versicolor] * cat [C]) * Petal.Length",
      `Speciesvirginica:catC:Petal.Length` = "(Species [virginica] * cat [C]) * Petal.Length",
      `Speciesversicolor:catD:Petal.Length` = "(Species [versicolor] * cat [D]) * Petal.Length",
      `Speciesvirginica:catD:Petal.Length` = "(Species [virginica] * cat [D]) * Petal.Length"
    ))
  })

  test_that("format_parameters-5", {
    model <- lm(Sepal.Length ~ Species / Petal.Length, data = iris)
    fp <- format_parameters(model)
    expect_equal(fp, c(
      `(Intercept)` = "(Intercept)", Speciesversicolor = "Species [versicolor]",
      Speciesvirginica = "Species [virginica]", `Speciessetosa:Petal.Length` = "Species [setosa] : Petal.Length",
      `Speciesversicolor:Petal.Length` = "Species [versicolor] : Petal.Length",
      `Speciesvirginica:Petal.Length` = "Species [virginica] : Petal.Length"
    ))
  })

  test_that("format_parameters-6", {
    model <- lm(Sepal.Length ~ Petal.Length + (Species / Sepal.Width), data = iris)
    fp <- format_parameters(model)
    expect_equal(fp, c(
      `(Intercept)` = "(Intercept)", Petal.Length = "Petal.Length",
      Speciesversicolor = "Species [versicolor]", Speciesvirginica = "Species [virginica]",
      `Speciessetosa:Sepal.Width` = "Species [setosa] : Sepal.Width",
      `Speciesversicolor:Sepal.Width` = "Species [versicolor] : Sepal.Width",
      `Speciesvirginica:Sepal.Width` = "Species [virginica] : Sepal.Width"
    ))
  })

  test_that("format_parameters-7", {
    model <- lm(Sepal.Length ~ Species / Petal.Length * Sepal.Width, data = iris)
    fp <- format_parameters(model)
    expect_equal(fp, c(
      `(Intercept)` = "(Intercept)", Speciesversicolor = "Species [versicolor]",
      Speciesvirginica = "Species [virginica]", Sepal.Width = "Sepal.Width",
      `Speciessetosa:Petal.Length` = "Species [setosa] : Petal.Length",
      `Speciesversicolor:Petal.Length` = "Species [versicolor] : Petal.Length",
      `Speciesvirginica:Petal.Length` = "Species [virginica] : Petal.Length",
      `Speciesversicolor:Sepal.Width` = "Species [versicolor] * Sepal.Width",
      `Speciesvirginica:Sepal.Width` = "Species [virginica] * Sepal.Width",
      `Speciessetosa:Petal.Length:Sepal.Width` = "Species [setosa] : Petal.Length : Sepal.Width",
      `Speciesversicolor:Petal.Length:Sepal.Width` = "Species [versicolor] : Petal.Length : Sepal.Width",
      `Speciesvirginica:Petal.Length:Sepal.Width` = "Species [virginica] : Petal.Length : Sepal.Width"
    ))
  })

  test_that("format_parameters-8", {
    model <- lm(Sepal.Length ~ Species / (Petal.Length * Sepal.Width), data = iris)
    fp <- format_parameters(model)
    expect_equal(fp, c(
      `(Intercept)` = "(Intercept)", Speciesversicolor = "Species [versicolor]",
      Speciesvirginica = "Species [virginica]", `Speciessetosa:Petal.Length` = "Species [setosa] : Petal.Length",
      `Speciesversicolor:Petal.Length` = "Species [versicolor] : Petal.Length",
      `Speciesvirginica:Petal.Length` = "Species [virginica] : Petal.Length",
      `Speciessetosa:Sepal.Width` = "Species [setosa] : Sepal.Width",
      `Speciesversicolor:Sepal.Width` = "Species [versicolor] : Sepal.Width",
      `Speciesvirginica:Sepal.Width` = "Species [virginica] : Sepal.Width",
      `Speciessetosa:Petal.Length:Sepal.Width` = "Species [setosa] : Petal.Length : Sepal.Width",
      `Speciesversicolor:Petal.Length:Sepal.Width` = "Species [versicolor] : Petal.Length : Sepal.Width",
      `Speciesvirginica:Petal.Length:Sepal.Width` = "Species [virginica] : Petal.Length : Sepal.Width"
    ))
  })

  test_that("format_parameters-9", {
    model <- lm(Sepal.Length ~ Petal.Length + (Species / (Sepal.Width * Petal.Width)), data = iris)
    fp <- format_parameters(model)
    expect_equal(fp, c(
      `(Intercept)` = "(Intercept)", Petal.Length = "Petal.Length",
      Speciesversicolor = "Species [versicolor]", Speciesvirginica = "Species [virginica]",
      `Speciessetosa:Sepal.Width` = "Species [setosa] : Sepal.Width",
      `Speciesversicolor:Sepal.Width` = "Species [versicolor] : Sepal.Width",
      `Speciesvirginica:Sepal.Width` = "Species [virginica] : Sepal.Width",
      `Speciessetosa:Petal.Width` = "Species [setosa] : Petal.Width",
      `Speciesversicolor:Petal.Width` = "Species [versicolor] : Petal.Width",
      `Speciesvirginica:Petal.Width` = "Species [virginica] : Petal.Width",
      `Speciessetosa:Sepal.Width:Petal.Width` = "Species [setosa] : Sepal.Width : Petal.Width",
      `Speciesversicolor:Sepal.Width:Petal.Width` = "Species [versicolor] : Sepal.Width : Petal.Width",
      `Speciesvirginica:Sepal.Width:Petal.Width` = "Species [virginica] : Sepal.Width : Petal.Width"
    ))
  })

  test_that("format_parameters-10", {
    model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2), data = iris)
    fp <- format_parameters(model)
    expect_equal(fp, c(
      `(Intercept)` = "(Intercept)", Speciesversicolor = "Species [versicolor]",
      Speciesvirginica = "Species [virginica]", `poly(Sepal.Width, 2)1` = "Sepal.Width [1st degree]",
      `poly(Sepal.Width, 2)2` = "Sepal.Width [2nd degree]"
    ))
  })

  test_that("format_parameters-11", {
    model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2, raw = TRUE), data = iris)
    fp <- format_parameters(model)
    expect_equal(fp, c(
      `(Intercept)` = "(Intercept)", Speciesversicolor = "Species [versicolor]",
      Speciesvirginica = "Species [virginica]", `poly(Sepal.Width, 2, raw = TRUE)1` = "Sepal.Width [1st degree]",
      `poly(Sepal.Width, 2, raw = TRUE)2` = "Sepal.Width [2nd degree]"
    ))
  })

  test_that("format_parameters-12", {
    model <- lm(Sepal.Length ~ Petal.Length * bs(Petal.Width), data = iris)
    fp <- format_parameters(model)
    expect_equal(fp, c(
      `(Intercept)` = "(Intercept)", Petal.Length = "Petal.Length",
      `bs(Petal.Width)1` = "Petal.Width [1st degree]", `bs(Petal.Width)2` = "Petal.Width [2nd degree]",
      `bs(Petal.Width)3` = "Petal.Width [3rd degree]", `Petal.Length:bs(Petal.Width)1` = "Petal.Length * Petal.Width [1st degree]",
      `Petal.Length:bs(Petal.Width)2` = "Petal.Length * Petal.Width [2nd degree]",
      `Petal.Length:bs(Petal.Width)3` = "Petal.Length * Petal.Width [3rd degree]"
    ))
  })

  test_that("format_parameters-13", {
    model <- lm(Sepal.Length ~ Petal.Length * bs(Petal.Width, degree = 4), data = iris)
    fp <- format_parameters(model)
    expect_equal(fp, c(
      `(Intercept)` = "(Intercept)", Petal.Length = "Petal.Length",
      `bs(Petal.Width, degree = 4)1` = "Petal.Width [1st degree]",
      `bs(Petal.Width, degree = 4)2` = "Petal.Width [2nd degree]",
      `bs(Petal.Width, degree = 4)3` = "Petal.Width [3rd degree]",
      `bs(Petal.Width, degree = 4)4` = "Petal.Width [4th degree]",
      `Petal.Length:bs(Petal.Width, degree = 4)1` = "Petal.Length * Petal.Width [1st degree]",
      `Petal.Length:bs(Petal.Width, degree = 4)2` = "Petal.Length * Petal.Width [2nd degree]",
      `Petal.Length:bs(Petal.Width, degree = 4)3` = "Petal.Length * Petal.Width [3rd degree]",
      `Petal.Length:bs(Petal.Width, degree = 4)4` = "Petal.Length * Petal.Width [4th degree]"
    ))
  })

  test_that("format_parameters-14", {
    model <- lm(Sepal.Length ~ Petal.Length * ns(Petal.Width, df = 3), data = iris)
    fp <- format_parameters(model)
    expect_equal(fp, c(
      `(Intercept)` = "(Intercept)", Petal.Length = "Petal.Length",
      `ns(Petal.Width, df = 3)1` = "Petal.Width [1st degree]", `ns(Petal.Width, df = 3)2` = "Petal.Width [2nd degree]",
      `ns(Petal.Width, df = 3)3` = "Petal.Width [3rd degree]", `Petal.Length:ns(Petal.Width, df = 3)1` = "Petal.Length * Petal.Width [1st degree]",
      `Petal.Length:ns(Petal.Width, df = 3)2` = "Petal.Length * Petal.Width [2nd degree]",
      `Petal.Length:ns(Petal.Width, df = 3)3` = "Petal.Length * Petal.Width [3rd degree]"
    ))
  })

  test_that("format_parameters-15", {
    model <- lm(Sepal.Length ~ Petal.Length * I(Petal.Width^2), data = iris)
    fp <- format_parameters(model)
    expect_equal(fp, c(
      `(Intercept)` = "(Intercept)", Petal.Length = "Petal.Length",
      `I(Petal.Width^2)` = "Petal.Width^2", `Petal.Length:I(Petal.Width^2)` = "Petal.Length * Petal.Width^2"
    ))
  })

  test_that("format_parameters-16", {
    model <- lm(Sepal.Length ~ Petal.Length * as.factor(Species), data = iris)
    fp <- format_parameters(model)
    expect_equal(fp, c(
      `(Intercept)` = "(Intercept)", Petal.Length = "Petal.Length",
      `as.factor(Species)versicolor` = "Species [versicolor]", `as.factor(Species)virginica` = "Species [virginica]",
      `Petal.Length:as.factor(Species)versicolor` = "Petal.Length * Species [versicolor]",
      `Petal.Length:as.factor(Species)virginica` = "Petal.Length * Species [virginica]"
    ))
  })

  test_that("format_parameters-17", {
    if (require("pscl")) {
      data("bioChemists")
      model <- zeroinfl(art ~ fem + mar + kid5 + ment | kid5 + phd, data = bioChemists)
      fp <- format_parameters(model)
      expect_equal(fp, c(
        `count_(Intercept)` = "(Intercept)", count_femWomen = "fem [Women]",
        count_marMarried = "mar [Married]", count_kid5 = "kid5", count_ment = "ment",
        `zero_(Intercept)` = "(Intercept)", zero_kid5 = "kid5", zero_phd = "phd"
      ))
    }
  })

  test_that("format_parameters-18", {
    data(iris)
    levels(iris$Species) <- c("Species verti", "No Specieses", "Yes (Species)")
    model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
    fp <- format_parameters(model)
    expect_equal(fp, c(
      `(Intercept)` = "(Intercept)", `SpeciesNo Specieses` = "Species [No Specieses]",
      `SpeciesYes (Species)` = "Species [Yes (Species)]", Petal.Width = "Petal.Width",
      `SpeciesNo Specieses:Petal.Width` = "Species [No Specieses] * Petal.Width",
      `SpeciesYes (Species):Petal.Width` = "Species [Yes (Species)] * Petal.Width"
    ))
  })

  test_that("format_parameters-19", {
    data(mtcars)
    m1 <- lm(mpg ~ qsec:wt + wt:drat, data = mtcars)
    m2 <- lm(mpg ~ qsec:wt + wt / drat, data = mtcars)
    m3 <- lm(mpg ~ qsec:wt + wt:drat + wt, data = mtcars)
    m4 <- lm(mpg ~ qsec:wt + wt / drat + wt, data = mtcars)
    m5 <- lm(mpg ~ qsec * wt + wt:drat + wt, data = mtcars)
    m6 <- lm(mpg ~ wt + qsec + wt:qsec, data = mtcars)
    expect_equal(format_parameters(m1), c(`(Intercept)` = "(Intercept)", `qsec:wt` = "qsec : wt", `wt:drat` = "wt : drat"))
    expect_equal(format_parameters(m2), c(`(Intercept)` = "(Intercept)", wt = "wt", `qsec:wt` = "qsec : wt", `wt:drat` = "wt : drat"))
    expect_equal(format_parameters(m3), c(`(Intercept)` = "(Intercept)", wt = "wt", `qsec:wt` = "qsec : wt", `wt:drat` = "wt : drat"))
    expect_equal(format_parameters(m4), c(`(Intercept)` = "(Intercept)", wt = "wt", `qsec:wt` = "qsec : wt", `wt:drat` = "wt : drat"))
    expect_equal(format_parameters(m5), c(`(Intercept)` = "(Intercept)", qsec = "qsec", wt = "wt", `qsec:wt` = "qsec * wt", `wt:drat` = "wt : drat"))
    expect_equal(format_parameters(m6), c(`(Intercept)` = "(Intercept)", wt = "wt", qsec = "qsec", `wt:qsec` = "wt * qsec"))
  })
}
