skip_on_cran()
skip_if_not_installed("blavaan")
skip_if_not_installed("lavaan")
skip_if_not_installed("insight", minimum_version = "1.5.4.17")
skip_if_not_installed("withr")

data(HolzingerSwineford1939, package = "lavaan")
model <- "visual =~ x1 + x2 + x3"

# fit the models once, with a small number of draws. bcfa() calls blavaan()
# unqualified, so blavaan must be attached
.fit_blavaan <- function(...) {
  withr::local_package("blavaan")
  suppressMessages(suppressWarnings(blavaan::bcfa(
    model,
    data = HolzingerSwineford1939,
    n.chains = 1,
    burnin = 500,
    sample = 1000,
    seed = 123,
    bcontrol = list(cores = 1),
    ...
  )))
}
m_single <- .fit_blavaan()
m_groups <- .fit_blavaan(group = "school", group.equal = "loadings")

# operator of a parameter name such as "visual=~x2 (group 1)"
.sem_operator <- function(x) {
  regmatches(x, regexpr("=~|~~|~1|~", x))
}


test_that("model_parameters, blavaan, standardized", {
  for (m in list(m_single, m_groups)) {
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

  for (s in c("basic", "posthoc", "smart", "pseudo", "refit", "std.lv")) {
    expect_warning(
      {
        out <- model_parameters(m_single, standardize = s)
      },
      regexp = "std.all",
      fixed = TRUE
    )
    expect_identical(out$Parameter, raw$Parameter)
    expect_identical(out$Median, raw$Median)
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
  local_mocked_bindings(.insight_version = function() package_version("1.5.4.10"))
  expect_error(
    model_parameters(m_single, standardize = TRUE),
    regexp = "1.5.4.17",
    fixed = TRUE
  )
})
