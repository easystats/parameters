
#' @export
model_parameters.MixMod <- model_parameters.glmmTMB


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

  .ci_generic(
    model = x,
    ci = ci,
    dof = Inf,
    component = component,
    ...
  )
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
    insight::check_if_installed("lme4")
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

    vc <- insight::get_varcov(model, effects = "fixed", component = "all", ...)
    se <- sqrt(diag(vc))

    x <- .data_frame(
      Parameter = names(se),
      SE = as.vector(se),
      Component = "conditional"
    )

    zi_parms <- grepl("^zi_", x$Parameter)
    if (any(zi_parms)) {
      x$Component[zi_parms] <- "zero_inflated"
      x$Parameter[zi_parms] <- gsub("^zi_(.*)", "\\1", x$Parameter[zi_parms])
    }

    .filter_component(x, component)
  }
}


#' @export
simulate_model.MixMod <- simulate_model.glmmTMB
