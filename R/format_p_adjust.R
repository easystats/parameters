#' Format the name of the p-value adjustment methods
#'
#' Format the name of the p-value adjustment methods.
#'
#' @param method Name of the method.
#'
#' @examples
#' library(parameters)
#'
#' format_p_adjust("holm")
#' format_p_adjust("bonferroni")
#' @return A string with the full surname(s) of the author(s), including year of publication, for the adjustment-method.
#' @export
format_p_adjust <- function(method) {
  method <- tolower(method)

  switch(method,
    "holm" = "Holm (1979)",
    "hochberg" = "Hochberg (1988)",
    "hommel" = "Hommel (1988)",
    "bonferroni" = "Bonferroni",
    "fdr" = "Benjamini & Hochberg (1995)",
    "bh" = "Benjamini & Hochberg (1995)",
    "by" = "Benjamini & Yekutieli (2001)",
    "tukey" = "Tukey",
    "scheffe" = "Scheffe",
    "sidak" = "Sidak",
    method
  )
}





#' @importFrom stats ptukey p.adjust.methods p.adjust pf
.p_adjust <- function(params, p_adjust, model = NULL, verbose = TRUE) {

  # check if we have any adjustment at all, and a p-column
  if (!is.null(p_adjust) && "p" %in% colnames(params)) {

    # prepare arguments
    all_methods <- c(tolower(stats::p.adjust.methods), "tukey", "scheffe", "sidak")

    # for interaction terms, e.g. for "by" argument in emmeans
    # pairwise comparison, we have to adjust the rank resp. the
    # number of estimates in a comparison family
    rank_adjust <- tryCatch(
      {
        correction <- 1
        by_vars <- model@misc$by.vars
        if (!is.null(by_vars) && by_vars %in% colnames(params)) {
          correction <- .n_unique(params[[by_vars]])
        }
        correction
      },
      error = function(e) {
        1
      }
    )


    # only proceed if valid argument-value
    if (tolower(p_adjust) %in% all_methods) {

      # save old values, to check if p-adjustment worked
      old_p_vals <- params$p
      # find statistic column
      stat_column <- stats::na.omit(match(c("F", "t", "Statistic"), colnames(params)))

      if (tolower(p_adjust) %in% tolower(stats::p.adjust.methods)) {
        # base R adjustments
        params$p <- stats::p.adjust(params$p, method = p_adjust)
      } else if (tolower(p_adjust) == "tukey") {
        # tukey adjustment
        if ("df" %in% colnames(params) && length(stat_column) > 0) {
          params$p <- stats::ptukey(sqrt(2) * abs(params[[stat_column]]), nrow(params) / rank_adjust, params$df, lower.tail = FALSE)
        }
      } else if (tolower(p_adjust) == "scheffe" && !is.null(model)) {
        # scheffe adjustment
        if ("df" %in% colnames(params) && length(stat_column) > 0) {
          # 1st try
          scheffe_ranks <- try(qr(model@linfct)$rank, silent = TRUE)

          # 2nd try
          if (inherits(scheffe_ranks, "try-error") || is.null(scheffe_ranks)) {
            scheffe_ranks <- try(model$qr$rank, silent = TRUE)
          }

          if (inherits(scheffe_ranks, "try-error") || is.null(scheffe_ranks)) {
            scheffe_ranks <- nrow(params)
          }
          scheffe_ranks <- scheffe_ranks / rank_adjust
          params$p <- stats::pf(params[[stat_column]]^2 / scheffe_ranks,
            df1 = scheffe_ranks,
            df2 = params$df,
            lower.tail = FALSE
          )
        }
      } else if (tolower(p_adjust) == "sidak") {
        # sidak adjustment
        params$p <- 1 - (1 - params$p)^(nrow(params) / rank_adjust)
      }

      if (isTRUE(all.equal(old_p_vals, params$p)) && !identical(p_adjust, "none")) {
        if (verbose) {
          warning(paste0("Something went wrong. Could not apply ", p_adjust, "-adjustment."), call. = FALSE)
        }
      }
    } else if (verbose) {
      warning(paste0("'p_adjust' must be one of ", paste0(all_methods, collapse = ", ")), call. = FALSE)
    }
  }
  params
}
