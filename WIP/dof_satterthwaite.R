#' @rdname p_value_satterthwaite
#' @export
dof_satterthwaite <- function(model) {
  X <- stats::model.matrix(model)

  grps <- insight::get_random(model)

  tms <- insight::find_predictors(model, effects = "fixed", component = "conditional", flatten = TRUE)
  mf <- insight::get_data(model)

  names.xvars <- tms <- setdiff(tms, names(grps))

  has.intcp <- insight::has_intercept(model)
  xmf <- mf[names.xvars]

  xddf <- sapply(xmf, function(.x) {
    min(sapply(grps, .get_df_satter_approx, x = .x))
  })

  ltab <- table(xddf)
  ltab <- list(m = as.integer(names(ltab)), l = as.vector(ltab))
  ltab$ddf <- ltab$m - ltab$l

  if (has.intcp) ltab$ddf <- ltab$ddf - 1

  ii <- match(xddf, ltab$m)
  xddf[] <- ltab$ddf[ii]

  ddf <- structure(numeric(length = ncol(X)), names = colnames(X))
  assgn <- attr(X, "assign")

  ii <- which("(Intercept)" != names(ddf))
  ddf[ii] <- xddf[assgn]
  if (has.intcp) ddf["(Intercept)"] <- min(xddf)

  ddf
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
