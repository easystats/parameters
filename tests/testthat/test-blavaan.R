skip_on_cran()
skip_if_not_installed("blavaan")
skip_if_not_installed("lavaan")
skip_if_not_installed("insight", minimum_version = "1.6.0")
skip_if_not_installed("withr")

data(HolzingerSwineford1939, package = "lavaan")
model <- "visual =~ x1 + x2 + x3"
model_pred <- "visual =~ x1 + x2 + x3\nvisual ~ ageyr"

# bcfa() and bsem() call blavaan() unqualified, so blavaan must be attached
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
m_pred <- suppressMessages(suppressWarnings(.fit_blavaan(fixed.x = TRUE, sem = TRUE)))

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

    out <- model_parameters(m, standardize = TRUE)
    expect_s3_class(out, "parameters_sem")
    expect_false(any(c("ESS", "Rhat") %in% colnames(out)))

    expected <- apply(std_draws[, match(out$Parameter, std_names)], 2, stats::median)
    expect_equal(out$Median, unname(expected), tolerance = 1e-8)

    expect_false(anyNA(out$Component))
    shared <- out$Parameter %in% raw$Parameter
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
})


test_that("model_parameters, blavaan, unsupported standardize values", {
  raw <- model_parameters(m_single)

  expect_silent({
    out <- model_parameters(m_single, standardize = FALSE)
  })
  expect_identical(out$Median, raw$Median)

  w <- capture_warnings({
    out <- model_parameters(m_single, standardize = "basic")
  })
  expect_length(w, 1)
  expect_match(w, "\"std.nox\"", fixed = TRUE)
  expect_identical(out$Median, raw$Median)
})


test_that("model_parameters, blavaan, std.lv and std.nox", {
  models <- list(single = m_single, pred = m_pred)
  for (nm in names(models)) {
    m <- models[[nm]]
    std_all <- model_parameters(m, standardize = "std.all")
    for (type in c("std.lv", "std.nox")) {
      std_names <- colnames(suppressWarnings(insight::get_parameters(
        m,
        standardize = TRUE,
        type = type
      )))
      std_draws <- suppressWarnings(blavaan::standardizedPosterior(m, type = type))
      w <- capture_warnings({
        out <- model_parameters(m, standardize = type)
      })

      # only m_pred has an exogenous covariate
      if (type == "std.nox" && nm == "single") {
        expect_length(w, 1)
        expect_match(gsub("\\s+", " ", w), "no exogenous covariates", fixed = TRUE)
      } else {
        expect_length(w, 0)
      }

      expected <- apply(std_draws[, match(out$Parameter, std_names)], 2, stats::median)
      expect_equal(out$Median, unname(expected), tolerance = 1e-8)
      expect_identical(colnames(out), colnames(std_all))
    }
  }

  nox <- model_parameters(m_pred, standardize = "std.nox")
  all <- model_parameters(m_pred, standardize = "std.all")
  expect_false(isTRUE(all.equal(
    nox$Median[nox$Parameter == "visual~ageyr"],
    all$Median[all$Parameter == "visual~ageyr"],
    tolerance = 1e-8
  )))
})


test_that("model_parameters, blavaan, std.nox warning", {
  msg <- NULL
  get_parameters_insight <- insight::get_parameters
  local_mocked_bindings(
    get_parameters = function(x, ...) {
      if (!isTRUE(list(...)$standardize)) {
        return(get_parameters_insight(x, ...))
      }
      for (i in 1:3) {
        warning(msg, call. = FALSE)
      }
      suppressWarnings(get_parameters_insight(x, ...))
    },
    .package = "insight"
  )

  # a translated message keeps the names of the types
  msg <- "Die Werte von `std.nox' sind identisch mit `std.all'."
  w <- capture_warnings(model_parameters(m_single, standardize = "std.nox"))
  expect_length(w, 1)
  expect_match(gsub("\\s+", " ", w), "no exogenous covariates", fixed = TRUE)

  msg <- "`std.nox' values are identical to `std.all' values when fixed.x = FALSE."
  w <- capture_warnings(model_parameters(m_single, standardize = "std.nox"))
  expect_length(w, 1)
  expect_match(gsub("\\s+", " ", w), "fit with `fixed.x = FALSE`", fixed = TRUE)

  expect_no_condition(model_parameters(m_single, standardize = "std.nox", verbose = FALSE))
})


test_that("model_parameters, blavaan, standardized, type passed to insight", {
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
  local_mocked_bindings(.insight_version = function() package_version("1.5.4"))
  for (s in list(TRUE, "std.lv")) {
    expect_error(
      model_parameters(m_single, standardize = s),
      regexp = "1.6.0",
      fixed = TRUE
    )
  }
})


test_that("model_parameters, blavaan, standardized, constant draws are dropped", {
  # the standardized variance of the latent variable is fixed at 1
  out <- model_parameters(m_single, standardize = TRUE)
  expect_false("visual~~visual" %in% out$Parameter)
  expect_true(all(c("visual=~x2", "x1~~x1") %in% out$Parameter))
})


test_that("model_parameters, blavaan, standardized, missing and all-constant draws", {
  real <- insight::get_parameters(m_single, standardize = TRUE)
  draws <- NULL
  get_parameters_insight <- insight::get_parameters
  local_mocked_bindings(
    get_parameters = function(x, ...) {
      if (isTRUE(list(...)$standardize)) {
        return(draws)
      }
      get_parameters_insight(x, ...)
    },
    .package = "insight"
  )

  draws <- real
  draws[["visual=~x2"]][1] <- NaN
  out <- suppressWarnings(model_parameters(m_single, standardize = TRUE))
  expect_true("visual=~x2" %in% out$Parameter)
  expect_false("visual~~visual" %in% out$Parameter)

  draws <- real
  draws[] <- lapply(draws, function(x) rep(1, length(x)))
  expect_error(
    model_parameters(m_single, standardize = TRUE),
    regexp = "All standardized parameters of this model are fixed",
    fixed = TRUE
  )
})


test_that("model_parameters, blavaan, standardized, print footer", {
  withr::local_options(width = 300)
  .has_footer <- function(x, type = "") {
    any(grepl(
      paste0("Parameters are standardized", type),
      capture.output(print(x)),
      fixed = TRUE
    ))
  }
  out <- model_parameters(m_single, standardize = "latent")
  expect_true(.has_footer(out, " (type: std.lv)."))
  expect_false(.has_footer(model_parameters(m_single)))
  expect_false(.has_footer(model_parameters(m_single, standardize = TRUE, verbose = FALSE)))
})


test_that("model_parameters, blavaan, standardized, diagnostic", {
  w <- capture_warnings({
    out <- model_parameters(m_single, standardize = TRUE, diagnostic = c("ESS", "Rhat"))
  })
  w <- grep("diagnostic", w, fixed = TRUE, value = TRUE)
  expect_length(w, 1)
  expect_match(w, "`ESS`, `Rhat`", fixed = TRUE)
  expect_false(any(c("ESS", "Rhat") %in% colnames(out)))

  expect_no_warning(model_parameters(m_single, standardize = TRUE))
  expect_no_warning(model_parameters(m_single, standardize = TRUE, diagnostic = "none"))
  expect_no_warning(model_parameters(
    m_single,
    standardize = TRUE,
    diagnostic = "ESS",
    verbose = FALSE
  ))
})


test_that("model_parameters, blavaan, standardized, ci_method si", {
  expect_error(
    model_parameters(m_single, standardize = TRUE, ci_method = "SI"),
    regexp = "ci_method = \"si\"",
    fixed = TRUE
  )
})


test_that("model_parameters, blavaan, standardized, test names", {
  w <- capture_warnings({
    out <- model_parameters(
      m_single,
      standardize = TRUE,
      test = c("pd", "psig", "ROPE", "equivalence", "BF")
    )
  })
  expect_true("pd" %in% colnames(out))
  expect_false(any(
    c("ps", "ROPE_Percentage", "p_ROPE", "ROPE_Equivalence") %in% colnames(out)
  ))
  expect_true(any(grepl("Scale-dependent inferential statistics", w, fixed = TRUE)))

  expect_error(
    model_parameters(m_single, standardize = TRUE, test = "ALL"),
    regexp = "is not supported when standardizing",
    fixed = TRUE
  )

  for (tt in list(FALSE, "none", character(0))) {
    out <- model_parameters(m_single, standardize = TRUE, test = tt)
    expect_false(any(c("pd", "ps") %in% colnames(out)))
  }

  # other models keep p_significance
  expect_identical(.remove_scale_dependent_tests(c("pd", "ps")), c("pd", "ps"))
})
