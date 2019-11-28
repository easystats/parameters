#' @rdname p_value_satterthwaite
#' @export
dof_satterthwaite <- function(model) {
  re_groups <- insight::get_random(model)

  predictors <- insight::find_predictors(model, effects = "fixed", component = "conditional", flatten = TRUE)
  predictors <- setdiff(predictors, names(re_groups))

  parameters <- insight::find_parameters(model)[["conditional"]]

  model_data <- insight::get_data(model)[predictors]
  has.intcp <- insight::has_intercept(model)

  term_assignment <- .find_term_assignment(model_data, predictors, parameters)

  ddf <- sapply(model_data, function(.x) {
    min(sapply(re_groups, .get_df_satter_approx, x = .x))
  })

  ltab <- table(ddf)
  ltab <- list(m = as.integer(names(ltab)), l = as.vector(ltab))

  ltab$ddf <- ltab$m - ltab$l
  if (has.intcp) ltab$ddf <- ltab$ddf - 1

  ii <- match(ddf, ltab$m)
  ddf[] <- ltab$ddf[ii]

  out <- numeric(length = length(parameters))
  out[which("(Intercept)" != parameters)] <- ddf[term_assignment]
  if (has.intcp) out[which("(Intercept)" == parameters)] <- min(ddf)

  unname(out)
}


#' @importFrom stats ave var
.get_df_satter_approx <- function(x, g){
  m <- nlevels(g)
  n <- length(x)
  x <- as.numeric(x)
  x.bar <- stats::ave(x, g)
  var.within <- stats::var(x - x.bar)
  var.between <- stats::var(x.bar)
  if (var.within >= var.between)
    return(n)
  else
    return(m)
}


#' @importFrom stats na.omit
.find_term_assignment <- function(model_data, predictors, parameters) {
  parms <- unlist(lapply(1:length(predictors), function(i) {
    p <- predictors[i]
    if (is.factor(model_data[[p]])) {
      ps <- paste0(p, levels(model_data[[p]]))
      names(ps)[1:length(ps)] <- i
      ps
    } else {
      names(p) <- i
      p
    }
  }))
  stats::na.omit(as.numeric(names(parms)[match(insight::clean_names(parameters), parms)]))
}
