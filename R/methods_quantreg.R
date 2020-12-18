# quantreg: .rq, .rqss, .crq, .nlrq

# model parameters ---------------------


#' @rdname model_parameters.cgam
#' @export
model_parameters.rqss <- model_parameters.cgam




# ci ---------------------


#' @export
ci.rq <- ci.default


#' @export
ci.rqss <- ci.default


#' @export
ci.crq <- ci.default


#' @export
ci.nlrq <- ci.default




# standard errors ---------------------


#' @export
standard_error.rq <- function(model, ...) {
  se <- .get_quantreg_se(model)
  if (is.null(se)) {
    vc <- insight::get_varcov(model)
    se <- as.vector(sqrt(diag(vc)))
  }

  params <- insight::get_parameters(model)
  params$SE <- se
  params[intersect(colnames(params), c("Parameter", "SE", "Component"))]
}


#' @export
standard_error.crq <- standard_error.rq


#' @export
standard_error.nlrq <- standard_error.rq


#' @export
standard_error.rqss <- function(model, component = c("all", "conditional", "smooth_terms"), ...) {
  component <- match.arg(component)

  cs <- summary(model)$coef
  se_column <- intersect(c("Std Error", "Std. Error"), colnames(cs))
  se <- cs[, se_column]

  params_cond <- insight::get_parameters(model, component = "conditional")
  params_smooth <- insight::get_parameters(model, component = "smooth_terms")

  out_cond <- .data_frame(
    Parameter = params_cond$Parameter,
    SE = se,
    Component = "conditional"
  )

  out_smooth <- .data_frame(
    Parameter = params_smooth$Parameter,
    SE = NA,
    Component = "smooth_terms"
  )

  switch(
    component,
    "all" = rbind(out_cond, out_smooth),
    "conditional" = out_cond,
    "smooth_terms" = out_smooth
  )
}


#' @importFrom stats coef setNames
#' @importFrom insight get_varcov
.get_quantreg_se <- function(model) {
  se <- tryCatch(
    {
      cs <- suppressWarnings(stats::coef(summary(model)))
      se_column <- intersect(c("Std Error", "Std. Error"), colnames(cs))
      if (length(se_column)) {
        cs[, se_column]
      } else {
        vc <- insight::get_varcov(model)
        as.vector(sqrt(diag(vc)))
      }
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(se)) {
    se <- tryCatch(
      {
        sc <- summary(model)
        if (all(unlist(lapply(sc, is.list)))) {
          list_sc <- lapply(sc, function(i) {
            .x <- as.data.frame(i)
            .x$Parameter <- rownames(.x)
            .x
          })
          out <- do.call(rbind, list_sc)
          se <- stats::setNames(out$coefficients.Std.Error, sprintf("tau (%g)", out$tau))
        } else {
          se <- stats::setNames(unname(sc$coefficients[, 4]), names(sc$coefficients[, 4]))
        }
      },
      error = function(e) {
        NULL
      }
    )
  }
  se
}




# p values ---------------------


#' @export
p_value.rq <- function(model, ...) {
  p <- .get_quantreg_p(model)

  params <- insight::get_parameters(model)
  params$p <- p
  params[intersect(colnames(params), c("Parameter", "p", "Component"))]
}


#' @export
p_value.crq <- p_value.rq


#' @export
p_value.nlrq <- p_value.rq


#' @export
p_value.rqss <- function(model, component = c("all", "conditional", "smooth_terms"), ...) {
  component <- match.arg(component)

  cs <- summary(model)$coef
  p_column <- intersect(c("Pr(>|t|)", "Pr(>|z|)"), colnames(cs))
  p_cond <- cs[, p_column]

  cs <- summary(model)$qsstab
  p_smooth <- cs[, "Pr(>F)"]

  params_cond <- insight::get_parameters(model, component = "conditional")
  params_smooth <- insight::get_parameters(model, component = "smooth_terms")

  out_cond <- .data_frame(
    Parameter = params_cond$Parameter,
    p = as.vector(p_cond),
    Component = "conditional"
  )

  out_smooth <- .data_frame(
    Parameter = params_smooth$Parameter,
    p = as.vector(p_smooth),
    Component = "smooth_terms"
  )

  switch(
    component,
    "all" = rbind(out_cond, out_smooth),
    "conditional" = out_cond,
    "smooth_terms" = out_smooth
  )
}


#' @importFrom stats coef setNames
#' @importFrom insight get_varcov
.get_quantreg_p <- function(model) {
  p <- tryCatch(
    {
      cs <- suppressWarnings(stats::coef(summary(model)))
      cs[, "Pr(>|t|)"]
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(p)) {
    p <- tryCatch(
      {
        .get_pval_from_summary(
          model,
          cs = suppressWarnings(stats::coef(summary(model, covariance = TRUE)))
        )
      },
      error = function(e) {
        NULL
      }
    )
  }

  if (is.null(p)) {
    p <- tryCatch(
      {
        sc <- summary(model)
        if (all(unlist(lapply(sc, is.list)))) {
          list_sc <- lapply(sc, function(i) {
            .x <- as.data.frame(i)
            .x$Parameter <- rownames(.x)
            .x
          })
          out <- do.call(rbind, list_sc)
          p <- stats::setNames(out[[grep("^coefficients\\.Pr", colnames(out))]], sprintf("tau (%g)", out$tau))
        } else {
          p <- stats::setNames(unname(sc$coefficients[, 6]), names(sc$coefficients[, 6]))
        }
      },
      error = function(e) {
        NULL
      }
    )
  }

  p
}
