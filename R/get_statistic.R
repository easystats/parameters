## TODO remove once insight is updated

#' @keywords internal
.get_statistic <- function(model, ...) {
  UseMethod(".get_statistic")
}


#' @keywords internal
.get_statistic.default <- function(model, statistic_column = 3, ...) {
  cs <- stats::coef(summary(model))
  cs_names <- tolower(dimnames(cs)[[2]])

  out <- .data_frame(
    Parameter = gsub("`", "", rownames(cs), fixed = TRUE),
    Statistic = as.vector(cs[, statistic_column])
  )

  if (any(c("t val.", "t", "t-value", "t.value", "t value", "tvalue") %in% cs_names)) {
    attr(out, "statistic") <- "t"
  } else if (any(c("z val.", "z", "z-value", "z.value", "z value", "zvalue", "wald") %in% cs_names)) {
    attr(out, "statistic") <- "z"
  } else {
    attr(out, "statistic") <- "statistic"
  }

  out
}



#' @keywords internal
.get_statistic.wbm <- function(model, ...) {
  s <- summary(model)
  stat <- c(
    s$within_table[, "t val."],
    s$between_table[, "t val."],
    s$ints_table[, "t val."]
  )
  params <- insight::get_parameters(model, effects = "fixed")

  out <- .data_frame(
    Parameter = params[[1]],
    Statistic = as.vector(stat),
    Component = params[[3]]
  )

  attr(out, "statistic") <- "t"
  out
}


#' @keywords internal
.get_statistic.mlm <- function(model, ...) {
  cs <- stats::coef(summary(model))

  out <- lapply(names(cs), function(i) {
    params <- cs[[i]]
    .data_frame(
      Parameter = rownames(params),
      Statistic = as.vector(params[, 3]),
      Response = gsub("^Response (.*)", "\\1", i)
    )
  })

  out <- do.call(rbind, out)
  attr(out, "statistic") <- "t"
  out
}



#' @keywords internal
.get_statistic.glmmTMB <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)

  cs <- .compact_list(stats::coef(summary(model)))
  x <- lapply(names(cs), function(i) {
    .data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = i, flatten = TRUE),
      Statistic = as.vector(cs[[i]][, 3]),
      Component = i
    )
  })

  stat <- do.call(rbind, x)
  stat$Component <- .rename_values(stat$Component, "cond", "conditional")
  stat$Component <- .rename_values(stat$Component, "zi", "zero_inflated")

  stat <- .filter_component(stat, component)
  attr(stat, "statistic") <- "z"

  stat
}


#' @keywords internal
.get_statistic.zeroinfl <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)

  cs <- .compact_list(stats::coef(summary(model)))
  x <- lapply(names(cs), function(i) {
    comp <- ifelse(i == "count", "conditional", "zi")
    .data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = comp, flatten = TRUE),
      Statistic = as.vector(cs[[i]][, 3]),
      Component = comp
    )
  })

  stat <- do.call(rbind, x)
  stat$Component <- .rename_values(stat$Component, "cond", "conditional")
  stat$Component <- .rename_values(stat$Component, "zi", "zero_inflated")

  stat <- .filter_component(stat, component)
  attr(stat, "statistic") <- "z"

  stat
}

#' @keywords internal
.get_statistic.hurdle <- .get_statistic.zeroinfl

#' @keywords internal
.get_statistic.zerocount <- .get_statistic.zeroinfl


#' @keywords internal
.get_statistic.MixMod <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  s <- summary(model)
  cs <- list(s$coef_table, s$coef_table_zi)
  names(cs) <- c("conditional", "zero_inflated")
  cs <- .compact_list(cs)
  x <- lapply(names(cs), function(i) {
    .data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = i, flatten = TRUE),
      Statistic = as.vector(cs[[i]][, 3]),
      Component = i
    )
  })

  stat <- do.call(rbind, x)
  .filter_component(stat, component)
  attr(stat, "statistic") <- "z"

  stat
}


#' @keywords internal
.get_statistic.gam <- function(model, statistic_column = 3, ...) {
  cs <- summary(model)$p.table
  cs.smooth <- summary(model)$s.table

  out <- .data_frame(
    Parameter = c(rownames(cs), rownames(cs.smooth)),
    Statistic = c(as.vector(cs[, statistic_column]), as.vector(cs.smooth[, statistic_column])),
    Component = c(rep("conditional", nrow(cs)), rep("smooth_terms", nrow(cs.smooth)))
  )

  if (insight::model_info(model)$is_binomial) {
    attr(out, "statistic") <- "z / Chisq"
  } else {
    attr(out, "statistic") <- "t / F"
  }

  out
}

#' @keywords internal
.get_statistic.gamm <- function(model, statistic_column = 3, ...) {
  model <- model$gam
  class(model) <- c("gam", "lm", "glm")
  .get_statistic.gam(model, statistic_column = statistic_column)
}


#' @keywords internal
.get_statistic.list <- function(model, statistic_column = 3, ...) {
  if ("gam" %in% names(model)) {
    model <- model$gam
    class(model) <- c("gam", "lm", "glm")
    .get_statistic.gam(model, statistic_column = statistic_column)
  }
}


#' @keywords internal
.get_statistic.gamlss <- function(model, statistic_column = 3, ...) {
  parms <- insight::get_parameters(model)
  utils::capture.output(cs <- summary(model))

  out <- .data_frame(
    Parameter = parms[[1]],
    Statistic = as.vector(cs[, statistic_column]),
    Component = parms$component
  )

  attr(out, "statistic") <- "t"
  out
}


#' @keywords internal
.get_statistic.lme <- function(model, ...) {
  .get_statistic.default(model, statistic_column = 4)
}

#' @keywords internal
.get_statistic.plm <- .get_statistic.default

.get_statistic.lm_robust <- .get_statistic.default

.get_statistic.ivreg <- .get_statistic.default

.get_statistic.geeglm <- .get_statistic.default

.get_statistic.censReg <- .get_statistic.default

.get_statistic.feis <- .get_statistic.default


.get_statistic.coxph <- function(model, ...) {
  .get_statistic.default(model, statistic_column = 4)
}

#' @keywords internal
.get_statistic.svyglm.nb <- function(model, ...) {
  parms <- insight::get_parameters(model)
  se <- standard_error(model)

  out <- .data_frame(
    Parameter = parms[[1]],
    Statistic = parms[[2]] / se$SE
  )

  attr(out, "statistic") <- "t"
  out
}

#' @keywords internal
.get_statistic.svyglm.zip <- .get_statistic.svyglm.nb

#' @keywords internal
.get_statistic.truncreg <- .get_statistic.default

#' @keywords internal
.get_statistic.tobit <- .get_statistic.default

#' @keywords internal
.get_statistic.negbin <- .get_statistic.default



#' @keywords internal
.get_statistic.vglm <- function(model, ...) {
  if (!requireNamespace("VGAM", quietly = TRUE)) {
    stop("Package 'VGAM' needed for this function to work. Please install it.")
  }

  cs <- VGAM::coef(VGAM::summary(model))

  out <- .data_frame(
    Parameter = gsub("`", "", rownames(cs), fixed = TRUE),
    Statistic = as.vector(cs[, 3])
  )

  attr(out, "statistic") <- "z"
  out
}

#' @keywords internal
.get_statistic.betareg <- function(model, ...) {
  parms <- insight::get_parameters(model)
  se <- standard_error(model)

  out <- .data_frame(
    Parameter = parms[[1]],
    Statistic = parms[[2]] / se$SE
  )

  attr(out, "statistic") <- "z"
  out
}


#' @keywords internal
.get_statistic.survreg <- function(model, ...) {
  parms <- insight::get_parameters(model)
  s <- summary(model)
  out <- .data_frame(
    Parameter = parms[[1]],
    Statistic = s$table[, 3]
  )

  attr(out, "statistic") <- "z"
  out
}

#' @keywords internal
.get_statistic.glimML <- function(model, ...) {
  if (!requireNamespace("aod", quietly = TRUE)) {
    stop("Package 'aod' required for this function to work. Please install it.")
  }

  parms <- insight::get_parameters(model)
  s <- methods::slot(aod::summary(model), "Coef")

  out <- .data_frame(
    Parameter = parms[[1]],
    Statistic = s[, 3]
  )

  attr(out, "statistic") <- "z"
  out
}


#' @keywords internal
.get_statistic.lrm <- function(model, ...) {
  parms <- insight::get_parameters(model)
  stat <- stats::coef(model) / sqrt(diag(stats::vcov(model)))

  out <- .data_frame(
    Parameter = parms[[1]],
    Statistic = as.vector(stat)
  )

  attr(out, "statistic") <- "z"
  out
}


#' @keywords internal
.get_statistic.ols <- .get_statistic.lrm

#' @keywords internal
.get_statistic.rms <- .get_statistic.lrm

#' @keywords internal
.get_statistic.psm <- .get_statistic.lrm



#' @importFrom stats qchisq
#' @importFrom utils capture.output
#' @keywords internal
.get_statistic.logistf <- function(model, ...) {
  parms <- insight::get_parameters(model)
  utils::capture.output(s <- summary(model))

  out <- .data_frame(
    Parameter = parms[[1]],
    Statistic = as.vector(stats::qchisq(1 - s$prob, df = 1))
  )

  attr(out, "statistic") <- "chisq"
  out
}


#' @keywords internal
.get_statistic.gee <- function(model, ...) {
  parms <- insight::get_parameters(model)
  cs <- stats::coef(summary(model))

  out <- .data_frame(
    Parameter = parms[[1]],
    Statistic = as.vector(cs[, "Naive z"])
  )

  attr(out, "statistic") <- "z"
  out
}


#' @keywords internal
.get_statistic.coxme <- function(model, ...) {
  beta <- model$coefficients
  out <- NULL

  if (length(beta) > 0) {
    out <- .data_frame(
      Parameter = names(beta),
      Statistic = as.vector(beta / sqrt(diag(stats::vcov(model))))
    )

    attr(out, "statistic") <- "z"
  }

  out
}

#' @keywords internal
.get_statistic.crch <- function(model, ...) {
  cs <- do.call(rbind, stats::coef(summary(model), model = "full"))
  params <- insight::get_parameters(model)

  out <- .data_frame(
    Parameter = params$parameter,
    Statistic = as.vector(cs[, 3])
  )

  attr(out, "statistic") <- "z"
  out
}


#' @keywords internal
.get_statistic.LORgee <- function(model, ...) {
  out <- .get_statistic.default(model)
  attr(out, "statistic") <- "z"
  out
}


#' @keywords internal
.get_statistic.bigglm <- function(model, ...) {
  parms <- insight::get_parameters(model)
  se <- standard_error(model)

  out <- .data_frame(
    Parameter = parms[[1]],
    Statistic = parms[[2]] / se$SE
  )

  attr(out, "statistic") <- "z"
  out
}


#' @keywords internal
.get_statistic.biglm <- function(model, ...) {
  parms <- insight::get_parameters(model)
  se <- standard_error(model)

  out <- .data_frame(
    Parameter = parms[[1]],
    Statistic = parms[[2]] / se$SE
  )

  attr(out, "statistic") <- "t"
  out
}


.get_statistic.rq <- function(model, ...) {

  stat <- tryCatch(
    {
      cs <- suppressWarnings(stats::coef(summary(model)))
      cs[, "t value"]
    },
    error = function(e) {
      cs <- suppressWarnings(stats::coef(summary(model, covariance = TRUE)))
      cs[, "t value"]
    }
  )

  params <- insight::get_parameters(model)

  out <- .data_frame(
    Parameter = params[[1]],
    Statistic = stat
  )

  attr(out, "statistic") <- "t"
  out
}


.get_statistic.crq <- .get_statistic.rq

.get_statistic.nlrq <- .get_statistic.rq
