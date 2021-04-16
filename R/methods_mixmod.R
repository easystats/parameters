
#' @export
model_parameters.MixMod <- model_parameters.glmmTMB


#' @rdname ci.merMod
#' @export
ci.MixMod <- function(x,
                      ci = .95,
                      component = c("all", "conditional", "zi", "zero_inflated"),
                      verbose = TRUE,
                      ...) {
  component <- match.arg(component)
  if (is.null(.check_component(x, component, verbose = verbose))) {
    return(NULL)
  }
  ci_wald(model = x, ci = ci, dof = Inf, component = component)
}


#' @rdname standard_error
#' @export
standard_error.MixMod <- function(model,
                                  effects = c("fixed", "random"),
                                  component = c("all", "conditional", "zi", "zero_inflated"),
                                  verbose = TRUE,
                                  ...) {
  component <- match.arg(component)
  effects <- match.arg(effects)

  if (effects == "random") {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package 'lme4' required to calculate standard errors for random effects. Please install it.")
    }
    rand.se <- lme4::ranef(model, post_vars = TRUE)
    vars.m <- attr(rand.se, "post_vars")
    all_names <- attributes(rand.se)$dimnames

    if (dim(vars.m[[1]])[1] == 1) {
      rand.se <- sqrt(unlist(vars.m))
    } else {
      rand.se <- do.call(
        rbind,
        lapply(vars.m, function(.x) t(as.data.frame(sqrt(diag(.x)))))
      )
      rownames(rand.se) <- all_names[[1]]
      colnames(rand.se) <- all_names[[2]]
      rand.se <- list(rand.se)
      names(rand.se) <- insight::find_random(model, flatten = TRUE)
    }
    rand.se
  } else {
    if (is.null(.check_component(model, component, verbose = verbose))) {
      return(NULL)
    }

    s <- summary(model)
    cs <- list(s$coef_table, s$coef_table_zi)
    names(cs) <- c("conditional", "zero_inflated")
    cs <- .compact_list(cs)
    x <- lapply(names(cs), function(i) {
      .data_frame(
        Parameter = insight::find_parameters(model, effects = "fixed", component = i, flatten = TRUE),
        SE = as.vector(cs[[i]][, 2]),
        Component = i
      )
    })

    se <- do.call(rbind, x)
    .filter_component(se, component)
  }
}


#' @rdname p_value.lmerMod
#' @export
p_value.MixMod <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), verbose = TRUE, ...) {
  component <- match.arg(component)
  if (is.null(.check_component(model, component, verbose = verbose))) {
    return(NULL)
  }

  s <- summary(model)
  cs <- list(s$coef_table, s$coef_table_zi)
  names(cs) <- c("conditional", "zero_inflated")
  cs <- .compact_list(cs)
  x <- lapply(names(cs), function(i) {
    .data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = i, flatten = TRUE),
      p = as.vector(cs[[i]][, 4]),
      Component = i
    )
  })

  p <- do.call(rbind, x)
  .filter_component(p, component)
}


#' @export
simulate_model.MixMod <- simulate_model.glmmTMB
