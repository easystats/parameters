skip_on_cran()
skip_if_not_installed("blavaan")
skip_if_not_installed("lavaan")
skip_if_not_installed("insight", minimum_version = "1.6.0")
skip_if_not_installed("withr")

data(HolzingerSwineford1939, package = "lavaan")
model <- "visual =~ x1 + x2 + x3"
model_pred <- "visual =~ x1 + x2 + x3\nvisual ~ ageyr"

# fit the models once, with a small number of draws. bcfa() and bsem() call
# blavaan() unqualified, so blavaan must be attached
.fit_blavaan <- function(..., sem = FALSE) {
  withr::local_package("blavaan")
  if (sem) {
    fit <- blavaan::bsem(
      model_pred,
      data = HolzingerSwineford1939,
      n.chains = 1,
      burnin = 500,
      sample = 1000,
      seed = 123,
      bcontrol = list(cores = 1),
      ...
    )
  } else {
    fit <- blavaan::bcfa(
      model,
      data = HolzingerSwineford1939,
      n.chains = 1,
      burnin = 500,
      sample = 1000,
      seed = 123,
      bcontrol = list(cores = 1),
      ...
    )
  }
  fit
}
m_single <- suppressMessages(suppressWarnings(.fit_blavaan()))
m_groups <- suppressMessages(suppressWarnings(.fit_blavaan(
  group = "school",
  group.equal = "loadings"
)))
# a regression on an observed predictor, where std.nox differs from std.all
m_pred <- suppressMessages(suppressWarnings(.fit_blavaan(fixed.x = TRUE, sem = TRUE)))

# the blavaan type for each value of `standardize`
std_types <- c(
  latent = "std.lv",
  std.lv = "std.lv",
  no_exogenous = "std.nox",
  std.nox = "std.nox"
)

# operator of a parameter name such as "visual=~x2 (group 1)"
.sem_operator <- function(x) {
  regmatches(x, regexpr("=~|~~|~1|~", x))
}


test_that("model_parameters, blavaan, standardized", {
  for (m in list(m_single, m_groups, m_pred)) {
    std_names <- colnames(insight::get_parameters(m, standardize = TRUE))
    std_draws <- blavaan::standardizedPosterior(m)
    raw <- model_parameters(m)

    for (s in list(TRUE, "all", "std.all")) {
      out <- model_parameters(m, standardize = s)
      expect_s3_class(out, "parameters_sem")
      expect_setequal(out$Parameter, std_names)
      expect_false(any(c("ESS", "Rhat") %in% colnames(out)))

      # medians are the medians of the standardized draws
      expected <- apply(std_draws[, match(out$Parameter, std_names)], 2, stats::median)
      expect_equal(out$Median, unname(expected), tolerance = 1e-8)

      # components are the same as in the unstandardized output
      expect_false(anyNA(out$Component))
      shared <- out$Parameter %in% raw$Parameter
      expect_true(any(shared))
      expect_identical(
        out$Component[shared],
        raw$Component[match(out$Parameter[shared], raw$Parameter)]
      )
      raw_ops <- .sem_operator(raw$Parameter)
      expect_identical(
        out$Component[!shared],
        raw$Component[match(.sem_operator(out$Parameter[!shared]), raw_ops)]
      )
    }
  }
})


test_that("model_parameters, blavaan, standardized, compare with lavaan", {
  m_ml <- lavaan::cfa(model, data = HolzingerSwineford1939)
  ml <- lavaan::standardizedSolution(m_ml)
  ml <- ml[ml$op == "=~", ]
  for (s in list(TRUE, "all", "std.all")) {
    out <- model_parameters(m_single, standardize = s)
    loadings <- out$Median[match(paste0(ml$lhs, ml$op, ml$rhs), out$Parameter)]
    expect_length(loadings, 3)
    expect_lt(max(abs(loadings - ml$est.std)), 0.1)
  }
})


test_that("model_parameters, blavaan, unsupported standardize values", {
  raw <- model_parameters(m_single)

  expect_silent({
    out <- model_parameters(m_single, standardize = FALSE)
  })
  expect_identical(out$Parameter, raw$Parameter)
  expect_identical(out$Median, raw$Median)

  for (s in c("basic", "posthoc", "smart", "pseudo", "refit")) {
    w <- capture_warnings({
      out <- model_parameters(m_single, standardize = s)
    })
    expect_length(w, 1)
    for (accepted in c("\"std.all\"", "\"std.lv\"", "\"std.nox\"")) {
      expect_match(w, accepted, fixed = TRUE)
    }
    expect_identical(out$Parameter, raw$Parameter)
    expect_identical(out$Median, raw$Median)
  }
})


test_that("model_parameters, blavaan, std.lv and std.nox", {
  skip_if_not_installed("insight", minimum_version = "1.6.0")
  models <- list(single = m_single, groups = m_groups, pred = m_pred)
  for (nm in names(models)) {
    m <- models[[nm]]
    std_all <- model_parameters(m, standardize = "std.all")
    for (s in names(std_types)) {
      type <- std_types[[s]]
      # lavaan warns for each draw when std.nox equals std.all
      std_names <- colnames(suppressWarnings(insight::get_parameters(
        m,
        standardize = TRUE,
        type = type
      )))
      std_draws <- suppressWarnings(blavaan::standardizedPosterior(m, type = type))
      w <- capture_warnings({
        out <- model_parameters(m, standardize = s)
      })

      # one warning when std.nox equals std.all, because only the predictor
      # model has an exogenous covariate
      if (type == "std.nox" && nm != "pred") {
        expect_length(w, 1)
        expect_match(gsub("\\s+", " ", w), "no exogenous covariates", fixed = TRUE)
      } else {
        expect_length(w, 0)
      }

      # medians are the medians of the draws of the matching type
      expect_setequal(out$Parameter, std_names)
      expected <- apply(std_draws[, match(out$Parameter, std_names)], 2, stats::median)
      expect_equal(out$Median, unname(expected), tolerance = 1e-8)

      # same columns as for std.all, and every row has a component
      expect_identical(colnames(out), colnames(std_all))
      expect_false(any(c("ESS", "Rhat") %in% colnames(out)))
      expect_false(anyNA(out$Component))
    }
  }

  # on the predictor model, std.nox does not standardize the observed predictor
  nox <- model_parameters(m_pred, standardize = "std.nox")
  all <- model_parameters(m_pred, standardize = "std.all")
  expect_false(isTRUE(all.equal(
    nox$Median[nox$Parameter == "ageyr~~ageyr"],
    all$Median[all$Parameter == "ageyr~~ageyr"],
    tolerance = 1e-8
  )))
})


test_that("model_parameters, blavaan, std.nox with fixed.x = FALSE", {
  skip_if_not_installed("insight", minimum_version = "1.6.0")
  # the model has an exogenous covariate, but std.nox still equals std.all
  m_free <- suppressMessages(suppressWarnings(.fit_blavaan(fixed.x = FALSE, sem = TRUE)))
  w <- capture_warnings({
    out <- model_parameters(m_free, standardize = "std.nox")
  })
  expect_length(w, 1)
  expect_match(gsub("\\s+", " ", w), "fit with `fixed.x = FALSE`", fixed = TRUE)
  expect_no_match(gsub("\\s+", " ", w), "no exogenous covariates", fixed = TRUE)
  expect_no_condition(model_parameters(m_free, standardize = "std.nox", verbose = FALSE))
})


test_that("model_parameters, blavaan, std.lv and std.nox, compare with lavaan", {
  skip_if_not_installed("insight", minimum_version = "1.6.0")
  m_ml <- lavaan::cfa(model, data = HolzingerSwineford1939)
  for (s in names(std_types)) {
    ml <- suppressWarnings(lavaan::standardizedSolution(m_ml, type = std_types[[s]]))
    ml <- ml[ml$op == "=~", ]
    out <- model_parameters(m_single, standardize = s, verbose = FALSE)
    loadings <- out$Median[match(paste0(ml$lhs, ml$op, ml$rhs), out$Parameter)]
    expect_length(loadings, 3)
    expect_lt(max(abs(loadings - ml$est.std)), 0.05)
  }
})


test_that("model_parameters, blavaan, std.lv and std.nox, tests and verbose", {
  skip_if_not_installed("insight", minimum_version = "1.6.0")
  for (s in names(std_types)) {
    expect_error(
      model_parameters(m_single, standardize = s, test = "all"),
      regexp = "is not supported when standardizing",
      fixed = TRUE
    )
    w <- capture_warnings({
      out <- model_parameters(m_single, standardize = s, test = c("pd", "rope"))
    })
    expect_true(any(grepl("Scale-dependent inferential statistics", w, fixed = TRUE)))
    expect_false(any(c("ROPE_Percentage", "ROPE_low") %in% colnames(out)))
    expect_true("pd" %in% colnames(out))
    expect_no_condition(model_parameters(
      m_single,
      standardize = s,
      test = c("pd", "rope"),
      verbose = FALSE
    ))
  }
})


test_that("model_parameters, blavaan, standardized, type passed to insight", {
  skip_if_not_installed("insight", minimum_version = "1.6.0")
  # record the arguments of each standardized call to insight
  calls <- list()
  get_parameters_insight <- insight::get_parameters
  local_mocked_bindings(
    get_parameters = function(x, ...) {
      dots <- list(...)
      if (isTRUE(dots$standardize)) {
        calls[[length(calls) + 1L]] <<- dots
      }
      get_parameters_insight(x, ...)
    },
    .package = "insight"
  )

  for (s in list(TRUE, "all", "std.all")) {
    calls <- list()
    model_parameters(m_single, standardize = s)
    expect_length(calls, 1)
    expect_false("type" %in% names(calls[[1]]))
  }
  for (s in names(std_types)) {
    calls <- list()
    model_parameters(m_single, standardize = s, verbose = FALSE)
    expect_length(calls, 1)
    expect_identical(calls[[1]]$type, std_types[[s]])
  }
})


test_that("model_parameters, blavaan, standardized, tests and verbose", {
  expect_error(
    model_parameters(m_single, standardize = TRUE, test = "all"),
    regexp = "is not supported when standardizing",
    fixed = TRUE
  )
  expect_warning(
    {
      out <- model_parameters(m_single, standardize = TRUE, test = c("pd", "rope"))
    },
    regexp = "Scale-dependent inferential statistics",
    fixed = TRUE
  )
  expect_false(any(c("ROPE_Percentage", "ROPE_low") %in% colnames(out)))
  expect_true("pd" %in% colnames(out))
  expect_silent(model_parameters(m_single, standardize = "basic", verbose = FALSE))
})


test_that("model_parameters, blavaan, multiple groups with equal loadings", {
  # see https://github.com/easystats/parameters/issues/735
  out <- model_parameters(m_groups)
  expect_true(all(c("visual=~x2 (group 1)", "visual=~x2 (group 2)") %in% out$Parameter))
  expect_false(any(startsWith(out$Parameter, ".p")))
})


test_that("model_parameters, blavaan, standardized, old insight", {
  local_mocked_bindings(.insight_version = function() package_version("1.6.0"))
  expect_error(
    model_parameters(m_single, standardize = TRUE),
    regexp = "1.6.0",
    fixed = TRUE
  )
})


test_that("model_parameters, blavaan, std.lv and std.nox, old insight", {
  local_mocked_bindings(.insight_version = function() package_version("1.6.0"))
  for (s in names(std_types)) {
    expect_error(
      model_parameters(m_single, standardize = s),
      regexp = "1.6.0",
      fixed = TRUE
    )
  }
  expect_no_error(model_parameters(m_single, standardize = "std.all"))

  # below 1.6.0, the error for the new values still names the version they need
  local_mocked_bindings(.insight_version = function() package_version("1.6.0"))
  for (s in names(std_types)) {
    expect_error(
      model_parameters(m_single, standardize = s),
      regexp = "1.6.0",
      fixed = TRUE
    )
  }
})


test_that("model_parameters, blavaan, std.nox warning in other wording", {
  skip_if_not_installed("insight", minimum_version = "1.6.0")
  # a translated or reworded lavaan warning keeps the names of the types
  get_parameters_insight <- insight::get_parameters
  local_mocked_bindings(
    get_parameters = function(x, ...) {
      if (!isTRUE(list(...)$standardize)) {
        return(get_parameters_insight(x, ...))
      }
      for (i in 1:3) {
        warning("Die Werte von `std.nox' sind identisch mit `std.all'.", call. = FALSE)
      }
      suppressWarnings(get_parameters_insight(x, ...))
    },
    .package = "insight"
  )
  w <- capture_warnings(model_parameters(m_single, standardize = "std.nox"))
  expect_length(w, 1)
  expect_match(gsub("\\s+", " ", w), "no exogenous covariates", fixed = TRUE)
  expect_no_condition(model_parameters(
    m_single,
    standardize = "std.nox",
    verbose = FALSE
  ))
})
