#' #' Computation of p-values
#' #'
#' #' Computes p-values related to parameters.
#' #'
#' #' @param model Statistical model.
#' #' @param ... Arguments passed to or from other methods.
#' #'
#' #' @export
#' p_value <- function(model, ...) {
#'   UseMethod("p_value")
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' #' @title Get p-values from regression model objects
#' #' @name p_value
#' #'
#' #' @description This function returns the p-values for fitted model objects.
#' #'
#' #' @param fit A model object.
#' #' @param p.kr Logical, if \code{TRUE}, the computation of p-values is based on
#' #'         conditional F-tests with Kenward-Roger approximation for the df (see
#' #'         'Details').
#' #' @param ... Currently not used.
#' #'
#' #' @return A \code{data.frame} with the model coefficients' names (\code{term}),
#' #'         p-values (\code{p.value}) and standard errors (\code{std.error}).
#' #'
#' #' @details For linear mixed models (\code{lmerMod}-objects), the computation of
#' #'         p-values (if \code{p.kr = TRUE}) is based on conditional F-tests
#' #'         with Kenward-Roger approximation for the df, using the
#' #'         \CRANpkg{pbkrtest}-package. If \pkg{pbkrtest} is not available or
#' #'         \code{p.kr = FALSE}, or if \code{x} is a \code{glmerMod}-object,
#' #'         computation of p-values is based on normal-distribution assumption,
#' #'         treating the t-statistics as Wald z-statistics.
#' #'         \cr \cr
#' #'         If p-values already have been computed (e.g. for \code{merModLmerTest}-objects
#' #'         from the \CRANpkg{lmerTest}-package), these will be returned.
#' #'         \cr \cr
#' #'         The \code{print()}-method has a \code{summary}-argument, that - in
#' #'         case \code{p.kr = TRUE} - also prints information on the approximated
#' #'         degrees of freedom (see 'Examples'). A shortcut is the
#' #'         \code{summary()}-method, which simply calls \code{print(..., summary = TRUE)}.
#' #'
#' #' @examples
#' #' data(efc)
#' #' # linear model fit
#' #' fit <- lm(neg_c_7 ~ e42dep + c172code, data = efc)
#' #' p_value(fit)
#' #'
#' #' # Generalized Least Squares fit
#' #' library(nlme)
#' #' fit <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
#' #'            correlation = corAR1(form = ~ 1 | Mare))
#' #' p_value(fit)
#' #'
#' #' # lme4-fit
#' #' library(lme4)
#' #' sleepstudy$mygrp <- sample(1:45, size = 180, replace = TRUE)
#' #' fit <- lmer(Reaction ~ Days + (1 | mygrp) + (1 | Subject), sleepstudy)
#' #' pv <- p_value(fit, p.kr = TRUE)
#' #'
#' #' # normal output
#' #' pv
#' #'
#' #' # add information on df and t-statistic
#' #' print(pv, summary = TRUE)
#' #' # or
#' #' summary(pv)
#' #'
#' #' @importFrom stats coef pnorm
#' #' @importFrom broom tidy
#' #' @importFrom dplyr select
#' #' @importFrom stats coef pt pnorm
#' #' @export
#' p_value <- function(fit, ...) {
#'   UseMethod("p_value")
#' }
#'
#'
#' @export
# p_value.multinom <- function(fit, ...) {
#   fit %>%
#     broom::tidy() %>%
#     dplyr::select(.data$term, .data$p.value, .data$std.error)
# }
#
#'
#' #' @export
#' p_value.vglm <- function(fit, ...) {
#'   if (!requireNamespace("VGAM", quietly = TRUE))
#'     stop("Package `VGAM` required.", call. = FALSE)
#'
#'   cs <- VGAM::summary(fit)@coef3
#'   p <- cs[, 4]
#'   se <- cs[, 2]
#'
#'   make_it_so(p, se)
#' }
#'
#'
#' #' @importFrom stats coef vcov pnorm
#' #' @export
#' p_value.svyglm.nb <- function(fit, ...) {
#'   if (!isNamespaceLoaded("survey"))
#'     requireNamespace("survey", quietly = TRUE)
#'
#'   est <- stats::coef(fit)
#'   se <- sqrt(diag(stats::vcov(fit, stderr = "robust")))
#'   p <- 2 * stats::pnorm(abs(est / se), lower.tail = FALSE)
#'
#'   names(p) <- gsub("\\beta\\.", "", names(p), fixed = FALSE)
#'   make_it_so(p, se)
#' }
#'
#'
#' #' @export
#' p_value.svyglm <- function(fit, ...) {
#'   cs <- stats::coef(summary(fit))
#'   p <- cs[, 4]
#'   se <- cs[, 2]
#'
#'   make_it_so(p, se)
#' }
#'
#'
#' #' @export
#' p_value.gmnl <- function(fit, ...) {
#'   cs <- summary(fit)$CoefTable
#'   p <- cs[, 4]
#'   se <- cs[, 2]
#'
#'   pv <- make_it_so(p, se)
#'
#'   # rename intercepts
#'   intercepts <- grepl(":(intercept)", pv$term, fixed = TRUE)
#'   pv$term[intercepts] <- sprintf(
#'     "(Intercept: %s)",
#'     sub(":(intercept)", replacement = "", pv$term[intercepts], fixed = TRUE)
#'   )
#'
#'   pv
#' }
#'
#'
#' #' @export
#' p_value.polr <- function(fit, ...) {
#'   smry <- suppressMessages(as.data.frame(stats::coef(summary(fit))))
#'   tstat <- smry[[3]]
#'   se <- smry[[2]]
#'   p <- 2 * stats::pnorm(abs(tstat), lower.tail = FALSE)
#'   names(p) <- rownames(smry)
#'   make_it_so(p, se)
#' }
#'
#'
#' #' @export
#' p_value.gam <- function(fit, ...) {
#'   sm <- summary(fit)
#'   lc <- length(sm$p.coeff)
#'   p <- sm$p.pv[1:lc]
#'   se <- sm$se[1:lc]
#'   make_it_so(p, se)
#' }
#'
#'
#' #' @export
#' p_value.maxLik <- function(fit, ...) {
#'   p <- summary(fit)$estimate[, 4]
#'   se <- summary(fit)$estimate[, 2]
#'   make_it_so(p, se)
#' }
#'
#'
#' #' @export
#' p_value.pglm <- function(fit, ...) {
#'   p <- summary(fit)$estimate[, 4]
#'   se <- summary(fit)$estimate[, 2]
#'   make_it_so(p, se)
#' }
#'
#'
#' #' @export
#' p_value.default <- function(fit, ...) {
#'   if (grepl("^Zelig-", class(fit)[1])) {
#'     if (!requireNamespace("Zelig", quietly = T))
#'       stop("Package `Zelig` required. Please install", call. = F)
#'     p <- unlist(Zelig::get_pvalue(fit))
#'     se <- unlist(Zelig::get_se(fit))
#'   } else {
#'     p <- stats::coef(summary(fit))[, 4]
#'     se <- stats::coef(summary(fit))[, 2]
#'   }
#'   make_it_so(p, se)
#' }
#'
#'
#' #' @export
#' p_value.rlm <- function(fit, ...) {
#'   cs <- stats::coef(summary(fit))
#'   p <- 2 * stats::pnorm(abs(cs[, 3]), lower.tail = FALSE)
#'   se <- cs[, 2]
#'
#'   make_it_so(p, se)
#' }
#'
#' #' @export
#' p_value.svyolr <- function(fit, ...) {
#'   cs <- stats::coef(summary(fit))
#'   p <- 2 * stats::pnorm(abs(cs[, 3]), lower.tail = FALSE)
#'   se <- cs[, 2]
#'
#'   make_it_so(p, se)
#' }
#'
#' #' @rdname p_value
#' #' @export
#' p_value.lmerMod <- function(fit, p.kr = FALSE, ...) {
#'   # retrieve sigificance level of independent variables (p-values)
#'   cs <- stats::coef(summary(fit))
#'
#'   # remeber coef-names
#'   coef_names <- rownames(cs)
#'
#'   if (isTRUE(p.kr) && requireNamespace("pbkrtest", quietly = TRUE)) {
#'     # compute Kenward-Roger-DF for p-statistic. Code snippet adapted from
#'     # http://mindingthebrain.blogspot.de/2014/02/three-ways-to-get-parameter-specific-p.html
#'     message("Computing p-values via Kenward-Roger approximation. Use `p.kr = FALSE` if computation takes too long.")
#'     #first coefficients need to be data frame
#'     cs <- as.data.frame(cs)
#'     # get KR DF
#'     df.kr <- get_kr_df(fit)
#'     se.kr <- get_kr_se(fit)
#'     t.kr <- lme4::fixef(fit) / se.kr
#'     # compute p-values, assuming an approximate t-dist
#'     p <- 2 * stats::pt(abs(t.kr), df = df.kr, lower.tail = FALSE)
#'     # name vector
#'     names(p) <- coef_names
#'     attr(p, "df.kr") <- df.kr
#'     attr(p, "se.kr") <- se.kr
#'     attr(p, "t.kr") <- t.kr
#'   } else {
#'     message("Computing p-values via Wald-statistics approximation (treating t as Wald z).")
#'     p <- 2 * stats::pnorm(abs(cs[, 3]), lower.tail = FALSE)
#'   }
#'
#'   if (is.null(names(p))) {
#'     if (length(coef_names) == length(p)) names(p) <- coef_names
#'   }
#'
#'   sekr <- attr(p, "se.kr", exact = TRUE)
#'   if (!is.null(sekr))
#'     se <- sekr
#'   else
#'     se <- cs[, 2]
#'
#'   make_it_so(p, se)
#' }
#'
#'
#' #' @export
#' p_value.glmerMod <- function(fit, ...) {
#'   cs <- stats::coef(summary(fit))
#'   x <- check_pval_from_summary(fit, cs)
#'   if (is.null(x[[1]])) x <- wald_pval(fit, cs)
#'
#'   make_it_so(x$p, x$se)
#' }
#'
#'
#' #' @importFrom purrr compact map flatten_chr map_df
#' #' @importFrom dplyr bind_cols
#' #' @export
#' p_value.glmmTMB <- function(fit, ...) {
#'   cs <- purrr::compact(stats::coef(summary(fit)))
#'   x <- purrr::map(cs, ~ check_pval_from_summary(fit, .x))
#'   coef_names <- purrr::map(x, ~ names(.x[[1]])) %>% purrr::flatten_chr()
#'   model_names <- names(x)
#'   model_lengths <- length(x)
#'   x <- purrr::map_df(x, ~ dplyr::bind_cols(.x))
#'   p <- x[[1]]
#'   se <- x[[2]]
#'   names(p) <- coef_names
#'
#'   make_it_so(p, se, rep(model_names, each = nrow(x) / model_lengths))
#' }
#'
#'
#' #' @export
#' p_value.nlmerMod <- function(fit, ...) {
#'   cs <- stats::coef(summary(fit))
#'   x <- check_pval_from_summary(fit, cs)
#'   if (is.null(x[[1]])) x <- wald_pval(fit, cs)
#'
#'   make_it_so(x$p, x$se)
#' }
#'
#'
#' #' @export
#' p_value.lmerModLmerTest <- function(fit, ...) {
#'   cs <- stats::coef(summary(fit))
#'   x <- check_pval_from_summary(fit, cs)
#'   if (is.null(x[[1]])) x <- wald_pval(fit, cs)
#'
#'   make_it_so(x$p, x$se)
#' }
#'
#'
#' #' @export
#' p_value.gls <- function(fit, ...) {
#'   p <- summary(fit)$tTable[, 4]
#'   se <- summary(fit)$tTable[, 2]
#'   make_it_so(p, se)
#' }
#'
#'
#' #' @export
#' p_value.pggls <- function(fit, ...) {
#'   p <- summary(fit)$CoefTable[, 4]
#'   se <- summary(fit)$CoefTable[, 2]
#'   make_it_so(p, se)
#' }
#'
#'
#' #' @importFrom stats coef
#' check_pval_from_summary <- function(fit, cs) {
#'   p <- NULL
#'   se <- NULL
#'
#'   if (ncol(cs) >= 4) {
#'
#'     # do we have a p-value column based on t?
#'     pvcn <- which(colnames(cs) == "Pr(>|t|)")
#'
#'     # if not, do we have a p-value column based on z?
#'     if (length(pvcn) == 0)
#'       pvcn <- which(colnames(cs) == "Pr(>|z|)")
#'
#'     # if not, default to 4
#'     if (length(pvcn) == 0) pvcn <- 4
#'
#'     p <- cs[, pvcn]
#'
#'     if (is.null(names(p))) {
#'       coef_names <- rownames(cs)
#'       if (length(coef_names) == length(p)) names(p) <- coef_names
#'     }
#'
#'     se <- cs[, 2]
#'   }
#'
#'   list(p = p, se = se)
#' }
#'
#'
#' wald_pval <- function(fit, cs) {
#'   message("Computing p-values via Wald-statistics approximation (treating t as Wald z).")
#'
#'   p <- 2 * stats::pnorm(abs(cs[, 3]), lower.tail = FALSE)
#'   se <- cs[, 2]
#'
#'   coef_names <- rownames(cs)
#'
#'   if (is.null(names(p))) {
#'     if (length(coef_names) == length(p)) names(p) <- coef_names
#'   }
#'
#'   list(p = p, se = se)
#' }
#'
#'
#' make_it_so <- function(p, se, model = NULL) {
#'   res <- data_frame(
#'     term = names(p),
#'     p.value = as.vector(p),
#'     std.error = as.vector(se)
#'   )
#'
#'   if (!is.null(model)) res$model <- model
#'
#'   class(res) <- c("sj_pval", class(res))
#'
#'   attr(res, "df.kr") <- attr(p, "df.kr", exact = TRUE)
#'   attr(res, "se.kr") <- attr(p, "se.kr", exact = TRUE)
#'   attr(res, "t.kr") <- attr(p, "t.kr", exact = TRUE)
#'
#'   res
#' }
#'
#'
#' #' @importFrom lme4 fixef
#' get_kr_df <- function(x) {
#'   if (!requireNamespace("pbkrtest", quietly = TRUE))
#'     stop("Package `pbkrtest` required for Kenward-Rogers approximation.", call. = FALSE)
#'
#'   L <- diag(rep(1, length(lme4::fixef(x))))
#'   L <- as.data.frame(L)
#'   out <- suppressMessages(purrr::map_dbl(L, pbkrtest::get_Lb_ddf, object = x))
#'   names(out) <- names(lme4::fixef(x))
#'   out
#' }
#'
#'
#' #' @importFrom lme4 fixef
#' #' @importFrom purrr map_dbl
#' get_kr_se <- function(x) {
#'   if (!requireNamespace("pbkrtest", quietly = TRUE))
#'     stop("Package `pbkrtest` required for Kenward-Roger approximation.", call. = FALSE)
#'
#'   vcov_adj <- pbkrtest::vcovAdj(x)
#'
#'   fe <- lme4::fixef(x)
#'   le <- length(fe)
#'   Lmat <- diag(le)
#'
#'   se <- purrr::map_dbl(1:le, ~ sqrt(qform(Lmat[.x, ], as.matrix(vcov_adj))))
#'   names(se) <- names(fe)
#'
#'   se
#' }
#'
#'
#' qform <- function(x, A) sum(x * (A %*% x))
