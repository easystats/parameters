#' @examplesIf require("tinytable") && require("lme4") && require("glmmTMB")
#' \donttest{
#' data(iris)
#' data(Salamanders, package = "glmmTMB")
#' m1 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
#' m2 <- lme4::lmer(
#'   Sepal.Length ~ Petal.Length + Petal.Width + (1 | Species),
#'   data = iris
#' )
#' m3 <- glmmTMB::glmmTMB(
#'   count ~ spp + mined + (1 | site),
#'   ziformula = ~mined,
#'   family = poisson(),
#'   data = Salamanders
#' )
#' out <- compare_parameters(m1, m2, m3, effects = "all", component = "all")
#' print_table(out)
#' }
#' @rdname display.parameters_model
#' @export
print_table <- function(x, digits = 2, p_digits = 3, theme = "default", ...) {
  insight::check_if_installed(c("datawizard", "tinytable"))

  if (!inherits(x, "compare_parameters")) {
    insight::format_error("`print_table` can only be used with `compare_parameters` objects.")
  }

  # random parameters?
  random_variances <- any(unlist(lapply(attributes(x)$all_attributes, function(i) {
    i$ran_pars
  })))

  # remember attributes
  ci_lvl <- attributes(x)$all_attributes[[1]]$ci
  model_names <- attributes(x)$model_names

  # check if we have mixed models with random variance parameters. in such
  # cases, we don't need the group-column, but we rather merge it with the
  # parameter column
  if (isTRUE(random_variances)) {
    # if (any(c("brmsfit", "stanreg", "stanmvreg") %in% m_class)) {
    #   # rename random effect parameters names for stan models
    #   x <- .format_stan_parameters(x)
    # } else {
    #   x <- .format_ranef_parameters(x)
    # }
    x <- .format_ranef_parameters(x)
    x$Group <- NULL
  }

  # check if we have only have fixed effects, and if so, remove column
  if (!is.null(x$Effects) && all(x$Effects == "fixed")) {
    x$Effects <- NULL
  }
  # check if we have only have conditional component, and if so, remove column
  if (!is.null(x$Component) && all(x$Component == "conditional")) {
    x$Component <- NULL
  }

  # check if we have models with extra components (e.g., zero-inflated models)
  # if so, we need to create a group variable, so we can include subheaders in
  # the table, and we want to re-arrange rows
  if (!is.null(x$Component) || !is.null(x$Effects)) {
    # create group variable, so we can include subheaders in table
    x$groups <- paste0(x$Component, ".", x$Effects)
    x <- datawizard::data_arrange(x, c("Effects", "Component"))
    # remove further unused columns
    x$Component <- NULL
    x$Effects <- NULL
  }

  # we now iterate all model columns, remove non-used columns per model,
  # and create the formated CI columns etc.
  for (i in model_names) {
    x[paste0("SE.", i)] <- NULL
    x[paste0("df_error.", i)] <- NULL
    x[paste0("z.", i)] <- NULL
    x[paste0("t.", i)] <- NULL
    ci_pos <- which(colnames(x) == paste0("CI.", i))
    x[paste0("CI.", i)] <- NULL

    # format estimate columns
    estimate_col <- min(which(endsWith(colnames(x), paste0(".", i))))
    x[[estimate_col]] <- insight::format_value(
      x[[estimate_col]],
      digits = digits,
      zap_small = TRUE
    )

    # format CI columns
    x$CI <- insight::format_ci(
      x[[paste0("CI_low.", i)]],
      x[[paste0("CI_high.", i)]],
      digits = digits,
      ci = NULL,
      brackets = FALSE,
      zap_small = TRUE
    )
    colnames(x)[colnames(x) == "CI"] <- paste0(sprintf("%g", 100 * ci_lvl), "% CI.", i)
    x[paste0("CI_low.", i)] <- NULL
    x[paste0("CI_high.", i)] <- NULL

    # format p-values
    x[[paste0("p.", i)]] <- insight::format_p(
      x[[paste0("p.", i)]],
      digits = p_digits,
      name = NULL
    )

    # relocate CI columns to right position
    x <- x[c(1:(ci_pos - 1), ncol(x), ci_pos:(ncol(x) - 1))]
  }

  # used for subgroup headers, if available
  row_header_pos <- row_header_labels <- NULL

  if (!is.null(x$groups)) {
    # find start row of each subgroup
    row_header_pos <- which(!duplicated(x$groups))
    group_headers <- as.vector(x$groups[row_header_pos])
    for (i in seq_along(group_headers)) {
      gh <- .format_model_component_header(
        x = NULL,
        type = group_headers[i],
        split_column = "",
        is_zero_inflated = FALSE,
        is_ordinal_model = FALSE,
        is_multivariate = FALSE,
        ran_pars = random_variances,
        formatted_table = NULL
      )
      group_headers[i] <- gh$name
    }
    # create named list, required for tinytables
    row_header_labels <- as.list(stats::setNames(row_header_pos, group_headers))
    # since we have the group names in "row_header_labels" now , we can remove the column
    x$groups <- NULL
    # make sure that the row header positions are correct - each header
    # must be shifted by the number of rows above
    for (i in 2:length(row_header_pos)) {
      row_header_pos[i] <- row_header_pos[i] + (i - 1)
    }
  }

  # find out position of column groups
  col_groups <- lapply(model_names, function(i) {
    which(endsWith(colnames(x), paste0(".", i)))
  })
  names(col_groups) <- model_names

  # fix column names
  for (i in model_names) {
    colnames(x) <- gsub(paste0("\\.", i, "$"), "", colnames(x))
  }

  # base table
  out <- tinytable::tt(as.data.frame(x), caption = NULL, notes = NULL, ...)
  # add subheaders, if any
  if (!is.null(row_header_labels)) {
    out <- tinytable::group_tt(out, i = row_header_labels, j = col_groups)
    out <- tinytable::style_tt(out, i = row_header_pos, italic = TRUE)
  } else {
    out <- tinytable::group_tt(out, j = col_groups)
  }
  # style table
  out <- insight::apply_table_theme(out, x, theme = theme, sub_header_positions = row_header_pos)
  # make sure HTML is default output
  out@output <- "html"

  out
}
