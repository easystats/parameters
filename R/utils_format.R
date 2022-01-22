# output-format helper  -------------------------

# this function does the main composition of columns for the output

.format_output_style <- function(x, style, format, modelname) {
  linesep <- " "
  if (style %in% c("se", "ci")) {
    x$p_stars <- ""
  }

  if (style == "minimal") {
    ci_col <- colnames(x)[grepl(" CI$", colnames(x)) | colnames(x) == "CI"]
    param_col <- colnames(x)[1]
    x[[param_col]] <- trimws(paste0(x[[param_col]], linesep, x[[ci_col]]))
    x <- x[c(param_col, "p")]
    colnames(x) <- paste0(colnames(x), " (", modelname, ")")
  } else if (style %in% c("ci_p", "ci")) {
    ci_col <- colnames(x)[grepl(" CI$", colnames(x)) | colnames(x) == "CI"]
    param_col <- colnames(x)[1]
    x[[param_col]] <- trimws(paste0(x[[param_col]], x$p_stars, linesep, x[[ci_col]]))
    x <- x[param_col]
    colnames(x) <- modelname
  } else if (style %in% c("se_p", "se")) {
    param_col <- colnames(x)[1]
    x[[param_col]] <- trimws(paste0(x[[param_col]], x$p_stars, linesep, "(", x$SE, ")"))
    x <- x[param_col]
    colnames(x) <- modelname
  } else if (style %in% c("ci_p2")) {
    ci_col <- colnames(x)[grepl(" CI$", colnames(x)) | colnames(x) == "CI"]
    param_col <- colnames(x)[1]
    x[[param_col]] <- trimws(paste0(x[[param_col]], linesep, x[[ci_col]]))
    x <- x[c(param_col, "p")]
    colnames(x) <- paste0(colnames(x), " (", modelname, ")")
  } else if (style %in% c("se_p2")) {
    param_col <- colnames(x)[1]
    x[[param_col]] <- trimws(paste0(x[[param_col]], linesep, "(", x$SE, ")"))
    x <- x[c(param_col, "p")]
    colnames(x) <- paste0(colnames(x), " (", modelname, ")")
  }
  x[[1]][x[[1]] == "()"] <- ""
  x
}



.add_obs_row <- function(x, att, style) {
  observations <- unlist(lapply(att, function(i) {
    if (is.null(i$n_obs)) {
      NA
    } else {
      i$n_obs
    }
  }))
  weighted_observations <- unlist(lapply(att, function(i) {
    if (is.null(i$weighted_nobs)) {
      NA
    } else {
      i$weighted_nobs
    }
  }))

  # check if model had weights, and if due to missing values n of weighted
  # observations differs from "raw" observations
  if (!all(is.na(weighted_observations)) && !all(is.na(observations))) {
    if (!isTRUE(all.equal(as.vector(weighted_observations), as.vector(observations)))) {
      message(insight::format_message("Number of weighted observations differs from number of unweighted observations."))
    }
    observations <- weighted_observations
  }

  if (!all(is.na(observations))) {
    # add empty row, as separator
    empty_row <- do.call(data.frame, as.list(rep(NA, ncol(x))))
    colnames(empty_row) <- colnames(x)
    x <- rbind(x, empty_row)
    # add observations
    steps <- (ncol(x) - 1) / length(observations)
    empty_row[[1]] <- "Observations"
    insert_at <- seq(2, ncol(x), by = steps)
    for (i in 1:length(insert_at)) {
      empty_row[[insert_at[i]]] <- observations[i]
    }
    x <- rbind(x, empty_row)
  }
  x
}





# other helper ------------------------


.format_columns_single_component <- function(x,
                                             pretty_names,
                                             digits = 2,
                                             ci_digits = 2,
                                             p_digits = 3,
                                             ci_width = "auto",
                                             ci_brackets = TRUE,
                                             format = NULL,
                                             coef_name = NULL,
                                             zap_small = FALSE,
                                             ...) {
  # default brackets are parenthesis for HTML / MD
  if ((is.null(ci_brackets) || isTRUE(ci_brackets)) && (identical(format, "html") || identical(format, "markdown"))) {
    brackets <- c("(", ")")
  } else if (is.null(ci_brackets) || isTRUE(ci_brackets)) {
    brackets <- c("[", "]")
  } else {
    brackets <- ci_brackets
  }

  # fix coefficient column name for random effects
  if (!is.null(x$Effects) && all(x$Effects == "random") && any(colnames(x) %in% .all_coefficient_types())) {
    colnames(x)[colnames(x) %in% .all_coefficient_types()] <- "Coefficient"
  }

  # fix coefficient column name for mixed count and zi pars
  if (!is.null(x$Component) && sum(c("conditional", "zero_inflated", "dispersion") %in% x$Component) >= 2 && any(colnames(x) %in% .all_coefficient_types())) {
    colnames(x)[colnames(x) %in% .all_coefficient_types()] <- "Coefficient"
  }

  # random pars with level? combine into parameter column
  if (all(c("Parameter", "Level") %in% colnames(x))) {
    x$Parameter <- paste0(x$Parameter, " ", brackets[1], x$Level, brackets[2])
    x$Level <- NULL
  }

  insight::format_table(
    x,
    pretty_names = pretty_names,
    digits = digits,
    ci_width = ci_width,
    ci_brackets = ci_brackets,
    ci_digits = ci_digits,
    p_digits = p_digits,
    zap_small = zap_small,
    ...
  )
}




# helper to format the header / subheader of different model components --------------

.format_model_component_header <- function(x,
                                           type,
                                           split_column,
                                           is_zero_inflated,
                                           is_ordinal_model,
                                           is_multivariate = FALSE,
                                           ran_pars,
                                           formatted_table = NULL) {
  component_name <- switch(type,
    "mu" = ,
    "fixed" = ,
    "fixed." = ,
    "conditional" = ,
    "conditional." = "Fixed Effects",
    "random." = ,
    "random" = "Random Effects",
    "conditional.fixed" = ,
    "conditional.fixed." = ifelse(is_zero_inflated, "Fixed Effects (Count Model)", "Fixed Effects"),
    "conditional.random" = ifelse(ran_pars,
      "Random Effects Variances",
      ifelse(is_zero_inflated,
        "Random Effects (Count Model)", "Random Effects"
      )
    ),
    "zero_inflated" = "Zero-Inflated",
    "zero_inflated.fixed" = ,
    "zero_inflated.fixed." = "Fixed Effects (Zero-Inflated Model)",
    "zero_inflated.random" = "Random Effects (Zero-Inflated Model)",
    "survival" = ,
    "survival.fixed" = "Survival",
    "dispersion.fixed" = ,
    "dispersion.fixed." = ,
    "dispersion" = "Dispersion",
    "marginal" = "Marginal Effects",
    "emmeans" = "Estimated Marginal Means",
    "contrasts" = "Contrasts",
    "simplex.fixed" = ,
    "simplex" = "Monotonic Effects",
    "smooth_sd" = "Smooth Terms (SD)",
    "smooth_terms" = "Smooth Terms",
    "sigma.fixed" = ,
    "sigma.fixed." = ,
    "sigma" = "Sigma",
    "thresholds" = "Thresholds",
    "correlation" = "Correlation",
    "SD/Cor" = "SD / Correlation",
    "Loading" = "Loading",
    "location" = ,
    "location.fixed" = ,
    "location.fixed." = "Location Parameters",
    "scale" = ,
    "scale.fixed" = ,
    "scale.fixed." = "Scale Parameters",
    "extra" = ,
    "extra.fixed" = ,
    "extra.fixed." = "Extra Parameters",
    "nu" = "Nu",
    "tau" = "Tau",
    "meta" = "Meta-Parameters",
    "studies" = "Studies",
    "within" = "Within-Effects",
    "between" = "Between-Effects",
    "interactions" = "(Cross-Level) Interactions",
    "precision" = ,
    "precision." = "Precision",
    "infrequent_purchase" = "Infrequent Purchase",
    "auxiliary" = "Auxiliary",
    "residual" = "Residual",
    "intercept" = "Intercept",
    "regression" = "Regression",
    "latent" = "Latent",
    "time_dummies" = "Time Dummies",
    type
  )

  if (grepl("^conditional\\.(r|R)andom_variances", component_name)) {
    component_name <- trimws(gsub("^conditional\\.(r|R)andom_variances(\\.)*", "", component_name))
    if (nchar(component_name) == 0) {
      component_name <- "Random Effects Variances"
    } else {
      component_name <- paste0("Random Effects Variances: ", component_name)
    }
  }
  if (grepl("^conditional\\.(r|R)andom", component_name)) {
    component_name <- trimws(gsub("^conditional\\.(r|R)andom(\\.)*", "", component_name))
    if (nchar(component_name) == 0) {
      component_name <- ifelse(ran_pars, "Random Effects Variances", "Random Effects (Count Model)")
    } else {
      component_name <- paste0("Random Effects (Count Model): ", component_name)
    }
  }
  if (grepl("^zero_inflated\\.(r|R)andom", component_name)) {
    component_name <- trimws(gsub("^zero_inflated\\.(r|R)andom(\\.)*", "", component_name))
    if (nchar(component_name) == 0) {
      component_name <- "Random Effects (Zero-Inflated Model)"
    } else {
      component_name <- paste0("Random Effects (Zero-Inflated Model): ", component_name)
    }
  }
  if (grepl("^random\\.(.*)", component_name)) {
    component_name <- paste0("Random Effects: ", gsub("^random\\.", "", component_name))
  }

  # if we show ZI component only, make sure this appears in header
  if (!grepl("(Zero-Inflated Model)", component_name, fixed = TRUE) &&
    !is.null(formatted_table$Component) &&
    all(formatted_table$Component == "zero_inflated")) {
    component_name <- paste0(component_name, " (Zero-Inflated Model)")
  }

  # tweaking of sub headers

  if (isTRUE(attributes(x)$is_ggeffects)) {
    s1 <- gsub("(.*)\\.(.*) = (.*)", "\\1 (\\2 = \\3)", component_name)
    s2 <- ""
  } else if ("DirichletRegModel" %in% attributes(x)$model_class) {
    if (grepl("^conditional\\.", component_name) || split_column == "Response") {
      s1 <- "Response level:"
      s2 <- gsub("^conditional\\.(.*)", "\\1", component_name)
    } else {
      s1 <- component_name
      s2 <- ""
    }
  } else if (length(split_column) > 1 && "Response" %in% split_column && is_multivariate) {
    # This here only applies to brms multivariate response models
    component_name <- gsub("^conditional\\.(.*)", "Response level: \\1", component_name)
    component_name <- gsub("^sigma\\.(.*)", "Auxilliary parameters, response level: \\1", component_name)
    component_name <- gsub("(.*)fixed\\.(.*)", "\\1\\2", component_name)
    component_name <- gsub("(.*)random\\.(.*)", "Random effects, \\1\\2", component_name)
    s1 <- component_name
    s2 <- ""
  } else if (length(split_column) > 1 ||
    split_column %in% c("Subgroup", "Type", "Group") ||
    grepl(tolower(split_column), tolower(component_name), fixed = TRUE) ||
    component_name %in% c("Within-Effects", "Between-Effects", "(Cross-Level) Interactions")) {
    s1 <- component_name
    s2 <- ""
  } else if (split_column == "Response" && is_ordinal_model) {
    s1 <- "Response level:"
    s2 <- component_name
  } else {
    s1 <- component_name
    s2 <- ifelse(tolower(split_column) == "component", "", split_column)
  }

  list(name = component_name, subheader1 = s1, subheader2 = s2)
}





# helper grouping parameters -------------------


.parameter_groups <- function(x, groups) {
  # only apply to conditional component for now
  if ("Component" %in% colnames(x) && sum(x$Component == "conditional") == 0) {
    return(x)
  }
  if ("Component" %in% colnames(x)) {
    row_index <- which(x$Component == "conditional")
  } else {
    row_index <- 1:nrow(x)
  }

  x_other <- x[-row_index, ]
  x <- x[row_index, ]

  att <- attributes(x)
  indent_rows <- NULL
  indent_parameters <- NULL

  if (is.list(groups)) {

    # find parameter names and replace by rowindex
    group_rows <- lapply(groups, function(i) {
      if (is.character(i)) {
        i <- match(i, x$Parameter)
      }
      i
    })

    # sanity check - check if all parameter names in the
    # group list are spelled correctly
    misspelled <- sapply(group_rows, function(i) {
      any(is.na(i))
    })

    if (any(misspelled)) {
      # remove invalid groups
      group_rows[misspelled] <- NULL
      # tell user
      warning(insight::format_message(
        "Couldn't find one or more parameters specified in following groups:",
        paste0(names(misspelled[misspelled]), collapse = ", "),
        "Maybe you misspelled parameter names?"
      ), call. = FALSE)
    }


    # sort parameters according to grouping
    selected_rows <- unlist(group_rows)
    indent_parameters <- x$Parameter[selected_rows]
    x <- rbind(x[selected_rows, ], x[-selected_rows, ])

    # set back correct indices
    groups <- 1
    for (i in 2:length(group_rows)) {
      groups <- c(groups, groups[i - 1] + length(group_rows[[i - 1]]))
    }
    names(groups) <- names(group_rows)
  } else {

    # find parameter names and replace by rowindex
    group_names <- names(groups)
    groups <- match(groups, x$Parameter)
    names(groups) <- group_names

    # order groups
    groups <- groups[order(groups)]
  }


  empty_row <- x[1, ]
  for (i in 1:ncol(empty_row)) {
    empty_row[[i]] <- NA
  }

  for (i in length(groups):1) {
    x[seq(groups[i] + 1, nrow(x) + 1), ] <- x[seq(groups[i], nrow(x)), ]
    x[groups[i], ] <- empty_row
    x$Parameter[groups[i]] <- paste0("# ", names(groups[i]))
  }

  # find row indices of indented parameters
  if (!is.null(indent_parameters)) {
    indent_rows <- match(indent_parameters, x$Parameter)
  }

  # add other rows back
  if (nrow(x_other) > 0) {
    x <- rbind(x, x_other)
  }

  attributes(x) <- utils::modifyList(att, attributes(x))
  attr(x, "indent_rows") <- indent_rows
  attr(x, "indent_groups") <- "# "
  x
}



# .insert_row <- function(x, newrow, r) {
#   existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
#   existingDF[r,] <- newrow
#   existingDF
# }

.prepare_x_for_print <- function(x, select, coef_name, s_value) {
  # minor fix for nested Anovas
  if ("Group" %in% colnames(x) && sum(x$Parameter == "Residuals") > 1) {
    colnames(x)[which(colnames(x) == "Group")] <- "Subgroup"
  }

  if (!is.null(select)) {
    if (all(select == "minimal")) {
      select <- c("Parameter", "Coefficient", "Std_Coefficient", "CI", "CI_low", "CI_high", "p")
    } else if (all(select == "short")) {
      select <- c("Parameter", "Coefficient", "Std_Coefficient", "SE", "p")
    } else if (is.numeric(select)) {
      select <- colnames(x)[select]
    }
    select <- union(select, c("Parameter", "Component", "Effects", "Response", "Subgroup"))
    # for emmGrid objects, we save specific parameter names as attribute
    parameter_names <- attributes(x)$parameter_names
    if (!is.null(parameter_names)) {
      select <- c(parameter_names, select)
    }
    to_remove <- setdiff(colnames(x), select)
    x[to_remove] <- NULL
  }

  # remove columns that have only NA or Inf
  to_remove <- sapply(x, function(col) all(is.na(col) | is.infinite(col)))
  if (any(to_remove)) x[to_remove] <- NULL

  # For Bayesian models, we need to prettify parameter names here...
  mc <- attributes(x)$model_class
  cp <- attributes(x)$cleaned_parameters
  if (!is.null(mc) && !is.null(cp) && mc %in% c("stanreg", "stanmvreg", "brmsfit")) {
    if (length(cp) == length(x$Parameter)) {
      x$Parameter <- cp
    }
    pretty_names <- FALSE
  }

  # for bayesian meta, remove ROPE_CI
  if (isTRUE(attributes(x)$is_bayes_meta)) {
    x$CI <- NULL
    x$ROPE_CI <- NULL
    x$ROPE_low <- NULL
    x$ROPE_high <- NULL
  }

  if (!is.null(coef_name)) {
    colnames(x)[which(colnames(x) == "Coefficient")] <- coef_name
    colnames(x)[which(colnames(x) == "Std_Coefficient")] <- paste0("Std_", coef_name)
  }

  if (isTRUE(s_value) && "p" %in% colnames(x)) {
    colnames(x)[colnames(x) == "p"] <- "s"
    x[["s"]] <- log2(1 / x[["s"]])
  }

  x
}




.prepare_splitby_for_print <- function(x) {
  if (!is.null(attributes(x)$model_class) && any(attributes(x)$model_class == "mvord")) {
    x$Response <- NULL
  }
  split_by <- ""
  split_by <- c(split_by, ifelse("Component" %in% names(x) && .n_unique(x$Component) > 1, "Component", ""))
  split_by <- c(split_by, ifelse("Effects" %in% names(x) && .n_unique(x$Effects) > 1, "Effects", ""))
  split_by <- c(split_by, ifelse("Response" %in% names(x) && .n_unique(x$Response) > 1, "Response", ""))
  split_by <- c(split_by, ifelse("Group" %in% names(x) && .n_unique(x$Group) > 1, "Group", ""))
  split_by <- c(split_by, ifelse("Subgroup" %in% names(x) && .n_unique(x$Subgroup) > 1, "Subgroup", ""))

  split_by <- split_by[nchar(split_by) > 0]
  split_by
}




# this function is actually similar to "insight::print_parameters()", but more
# sophisticated, to ensure nicely outputs even for complicated or complex models,
# or edge cases...

#' @keywords internal
.format_columns_multiple_components <- function(x,
                                                pretty_names,
                                                split_column = "Component",
                                                digits = 2,
                                                ci_digits = 2,
                                                p_digits = 3,
                                                coef_column = NULL,
                                                format = NULL,
                                                ci_width = "auto",
                                                ci_brackets = TRUE,
                                                zap_small = FALSE,
                                                ...) {
  final_table <- list()

  ignore_group <- isTRUE(attributes(x)$ignore_group)
  ran_pars <- isTRUE(attributes(x)$ran_pars)
  is_ggeffects <- isTRUE(attributes(x)$is_ggeffects)


  # name of "Parameter" column - usually the first column, however, for
  # ggeffects objects, this column has the name of the focal term

  if (is_ggeffects) {
    parameter_column <- colnames(x)[1]
  } else {
    parameter_column <- "Parameter"
  }

  # default brackets are parenthesis for HTML / MD
  if ((is.null(ci_brackets) || isTRUE(ci_brackets)) && (identical(format, "html") || identical(format, "markdown"))) {
    ci_brackets <- c("(", ")")
  } else if (is.null(ci_brackets) || isTRUE(ci_brackets)) {
    ci_brackets <- c("[", "]")
  }


  # check ordinal / multivariate
  is_ordinal_model <- isTRUE(attributes(x)$ordinal_model)
  is_multivariate <- isTRUE(attributes(x)$multivariate_response)

  # zero-inflated stuff
  is_zero_inflated <- (!is.null(x$Component) & "zero_inflated" %in% x$Component)
  zi_coef_name <- attributes(x)$zi_coefficient_name

  # other special model-components, like emm_list
  coef_name2 <- attributes(x)$coefficient_name2

  # make sure we have correct order of levels from split-factor
  if (!is.null(attributes(x)$model_class) && all(attributes(x)$model_class == "mediate")) {
    x$Component <- factor(x$Component, levels = c("control", "treated", "average", "Total Effect"))
    x$Parameter <- trimws(gsub("(.*)\\((.*)\\)$", "\\1", x$Parameter))
  } else {
    x[split_column] <- lapply(x[split_column], function(i) {
      if (!is.factor(i)) i <- factor(i, levels = unique(i))
      i
    })
  }

  # fix column output
  if (inherits(attributes(x)$model, c("lavaan", "blavaan")) && "Label" %in% colnames(x)) {
    x$From <- ifelse(x$Label == "" | x$Label == x$To, x$From, paste0(x$From, " (", x$Label, ")"))
    x$Label <- NULL
  }

  if (inherits(attributes(x)$model, c("lavaan", "blavaan")) && !"Parameter" %in% colnames(x)) {
    parameter_column <- colnames(x)[1]
  }

  if (inherits(attributes(x)$model, c("lavaan", "blavaan")) && "Defined" %in% x$Component) {
    x$From[x$Component == "Defined"] <- ""
    x$Operator[x$Component == "Defined"] <- ""
    x$To <- ifelse(x$Component == "Defined", paste0("(", x$To, ")"), x$To)
  }

  # set up split-factor
  if (length(split_column) > 1) {
    split_by <- lapply(split_column, function(i) x[[i]])
  } else {
    split_by <- list(x[[split_column]])
  }
  names(split_by) <- split_column

  # make sure we have correct sorting here...
  tables <- split(x, f = split_by)

  # sanity check - only preserve tables with any data in data frames
  tables <- tables[sapply(tables, nrow) > 0]


  # fix table names for random effects, when we only have random
  # effects. in such cases, the wrong header (fixed effects) is chosen
  # to prevent this, we "fake" the name of the splitted components by
  # prefixing them with "random."

  if (!is.null(x$Effects) && all(x$Effects == "random") && !all(grepl("^random\\.", names(tables)))) {
    wrong_names <- !grepl("^random\\.", names(tables))
    names(tables)[wrong_names] <- paste0("random.", names(tables)[wrong_names])
  }


  for (type in names(tables)) {

    # do we have emmeans emlist? and contrasts?
    model_class <- attributes(tables[[type]])$model_class
    em_list_coef_name <- (!is.null(model_class) && "emm_list" %in% model_class &&
      "contrasts" %in% tables[[type]]$Component)

    # Don't print Component column
    for (i in split_column) {
      tables[[type]][[i]] <- NULL
    }

    # Smooth terms statistics
    if ("t / F" %in% names(tables[[type]])) {
      if (type == "smooth_terms") {
        names(tables[[type]])[names(tables[[type]]) == "t / F"] <- "F"
      }
      if (type == "conditional") {
        names(tables[[type]])[names(tables[[type]]) == "t / F"] <- "t"
      }
    } else if (type == "smooth_terms" && "t" %in% names(tables[[type]])) {
      names(tables[[type]])[names(tables[[type]]) == "t"] <- "F"
    }


    if ("z / Chi2" %in% names(tables[[type]])) {
      if (type == "smooth_terms") {
        names(tables[[type]])[names(tables[[type]]) == "z / Chi2"] <- "Chi2"
      }
      if (type == "conditional") {
        names(tables[[type]])[names(tables[[type]]) == "z / Chi2"] <- "z"
      }
    }

    # Don't print se and ci if all are missing
    if (all(is.na(tables[[type]]$SE))) tables[[type]]$SE <- NULL
    if (all(is.na(tables[[type]]$CI_low)) && all(is.na(tables[[type]]$CI_high))) {
      tables[[type]]$CI_low <- NULL
      tables[[type]]$CI_high <- NULL
    }
    # if (all(is.na(tables[[type]]$CI_low))) tables[[type]]$CI_low <- NULL
    # if (all(is.na(tables[[type]]$CI_high))) tables[[type]]$CI_high <- NULL

    # Don't print if empty col
    tables[[type]][sapply(colnames(tables[[type]]), function(x) {
      col <- tables[[type]][[x]]
      (all(col == "") | all(is.na(col))) && !grepl("_CI_(high|low)$", x)
    })] <- NULL

    attr(tables[[type]], "digits") <- digits
    attr(tables[[type]], "ci_digits") <- ci_digits
    attr(tables[[type]], "p_digits") <- p_digits

    # random pars with level? combine into parameter column
    if (all(c("Parameter", "Level") %in% colnames(tables[[type]]))) {
      tables[[type]]$Parameter <- paste0(tables[[type]]$Parameter, " ", ci_brackets[1], tables[[type]]$Level, ci_brackets[2])
      tables[[type]]$Level <- NULL
    }

    # rename columns for emmeans contrast part
    if (em_list_coef_name && !is.null(coef_column)) {
      colnames(tables[[type]])[which(colnames(tables[[type]]) == coef_column)] <- coef_name2
    }

    # rename columns for zero-inflation part
    if (grepl("^zero", type) && !is.null(zi_coef_name) && !is.null(coef_column)) {
      colnames(tables[[type]])[which(colnames(tables[[type]]) == coef_column)] <- zi_coef_name
      colnames(tables[[type]])[which(colnames(tables[[type]]) == paste0("Std_", coef_column))] <- paste0("Std_", zi_coef_name)
    }

    # rename columns for correlation part
    if (type == "correlation" && !is.null(coef_column)) {
      colnames(tables[[type]])[which(colnames(tables[[type]]) == coef_column)] <- "Estimate"
    }

    # rename columns for dispersion part
    if (grepl("^dispersion", type) && !is.null(coef_column)) {
      colnames(tables[[type]])[which(colnames(tables[[type]]) == coef_column)] <- "Coefficient"
    }

    # rename columns for random part
    if (grepl("random", type) && any(colnames(tables[[type]]) %in% .all_coefficient_types())) {
      colnames(tables[[type]])[colnames(tables[[type]]) %in% .all_coefficient_types()] <- "Coefficient"
    }

    if (grepl("random", type) && isTRUE(ran_pars)) {
      tables[[type]]$CI <- NULL
    }

    # for ggeffects objects, only choose selected lines, to have
    # a more compact output
    if (is_ggeffects && is.numeric(tables[[type]][[1]])) {
      n_rows <- nrow(tables[[type]])
      row_steps <- round(sqrt(n_rows))
      sample_rows <- round(c(1, stats::quantile(seq_len(n_rows), seq_len(row_steps - 2) / row_steps), n_rows))
      tables[[type]] <- tables[[type]][sample_rows, ]
      tables[[type]][[1]] <- insight::format_value(tables[[type]][[1]], digits = digits, protect_integers = TRUE)
    }

    formatted_table <- insight::format_table(tables[[type]], digits = digits, ci_digits = ci_digits, p_digits = p_digits, pretty_names = pretty_names, ci_width = ci_width, ci_brackets = ci_brackets, zap_small = zap_small, ...)
    component_header <- .format_model_component_header(x, type, split_column, is_zero_inflated, is_ordinal_model, is_multivariate, ran_pars, formatted_table)

    # exceptions for random effects
    if (.n_unique(formatted_table$Group) == 1) {
      component_header$subheader1 <- paste0(component_header$subheader1, " (", formatted_table$Group, ")")
      formatted_table$Group <- NULL
    }

    # remove non-necessary columns
    if (.n_unique(formatted_table$Component) == 1) {
      formatted_table$Component <- NULL
    }

    # no column with CI-level in output
    if (!is.null(formatted_table$CI) && .n_unique(formatted_table$CI) == 1) {
      formatted_table$CI <- NULL
    }

    table_caption <- NULL
    if (is.null(format) || format == "text") {
      # Print
      if (component_header$name != "rewb-contextual") {
        table_caption <- c(sprintf("# %s %s", component_header$subheader1, tolower(component_header$subheader2)), "blue")
      }
    } else if (format %in% c("markdown", "html")) {
      # Print
      if (component_header$name != "rewb-contextual") {
        table_caption <- sprintf("%s %s", component_header$subheader1, tolower(component_header$subheader2))
      }
      # replace brackets by parenthesis
      if (!is.null(parameter_column) && parameter_column %in% colnames(formatted_table)) {
        formatted_table[[parameter_column]] <- gsub("[", ci_brackets[1], formatted_table[[parameter_column]], fixed = TRUE)
        formatted_table[[parameter_column]] <- gsub("]", ci_brackets[2], formatted_table[[parameter_column]], fixed = TRUE)
      }
    }

    if (identical(format, "html")) {
      formatted_table$Component <- table_caption
    } else {
      attr(formatted_table, "table_caption") <- table_caption
    }

    # remove unique columns
    if (.n_unique(formatted_table$Effects) == 1) formatted_table$Effects <- NULL
    if (.n_unique(formatted_table$Group) == 1) formatted_table$Group <- NULL

    final_table <- c(final_table, list(formatted_table))
  }

  if (identical(format, "html")) {
    # fix non-equal length of columns
    final_table <- .fix_nonmatching_columns(final_table, is_lavaan = inherits(attributes(x)$model, c("lavaan", "blavaan")))
    do.call(rbind, final_table)
  } else {
    datawizard::compact_list(final_table)
  }
}




# helper to fix unequal number of columns for list of data frames,
# when used for HTML printing

.fix_nonmatching_columns <- function(final_table, is_lavaan = FALSE) {
  # fix for lavaan here
  if (is_lavaan) {
    for (i in 1:length(final_table)) {
      if (!is.null(final_table[[i]]$Link) && !is.null(final_table[[i]]$To)) {
        if (all(is.na(final_table[[i]]$Link))) {
          final_table[[i]]$Link <- final_table[[i]]$To
          final_table[[i]]$To <- NA
        }
      }
      colnames(final_table[[i]])[1] <- "Parameter"
      if (!is.null(final_table[[i]]$To) && all(is.na(final_table[[i]]$To))) {
        final_table[[i]]$To <- NULL
      }
    }
  }

  # then check for correct column length
  col_len <- sapply(final_table, function(i) length(colnames(i)))

  # remove non matching columns
  if (!all(col_len) == max(col_len)) {
    all_columns <- unique(unlist(lapply(final_table, colnames)))
    for (i in 1:length(final_table)) {
      missing_columns <- setdiff(all_columns, colnames(final_table[[i]]))
      if (length(missing_columns)) {
        a <- attributes(final_table[[i]])
        final_table[[i]][missing_columns] <- NA
        final_table[[i]] <- final_table[[i]][match(all_columns, colnames(final_table[[i]]))]
        attributes(final_table[[i]]) <- utils::modifyList(a, attributes(final_table[[i]]))
      }
    }
  }

  final_table
}
