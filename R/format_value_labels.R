# replace pretty names with value labels, when present ---------------

.format_value_labels <- function(params, ...) {
  NextMethod()
}


.format_value_labels.default <- function(params, model = NULL, ...) {
  pretty_labels <- NULL
  if (is.null(model)) {
    model <- .get_object(params)
  }

  # validation check
  if (.is_valid_model_input(model)) {
    # get data, but exclude response - we have no need for that label
    mf <- insight::get_data(model, source = "mf", verbose = FALSE)
    # sanity check - any labels (value labels)?
    has_labels <- vapply(
      mf,
      function(i) !is.null(attr(i, "labels", exact = TRUE)),
      logical(1)
    )
    # if we don't have value labels, we try to get data from environment
    # (which may preserve variable labels set via the labelled package)
    if (!any(has_labels)) {
      mf <- insight::get_data(model, source = "environment", verbose = FALSE)
      # for on-the-fly factor conversions in the formula (e.g., factor(cyl)),
      # insight::get_data() returns the original column (e.g., "cyl"), not the
      # expression column (e.g., "factor(cyl)"). Use insight::find_terms() to
      # detect such conversions and manually add converted columns, mirroring
      # the approach in .find_factor_levels(). Only add when the original
      # variable has a variable label, to preserve format_parameters formatting
      # for unlabelled on-the-fly factors (e.g., "as.factor(am)1" → "am [1]").
      if (!is.null(mf)) {
        model_terms <- .safe(insight::find_terms(model, verbose = FALSE))
        if (!is.null(model_terms[["conditional"]])) {
          factor_terms <- grep(
            "(as\\.factor|factor|as\\.character)",
            model_terms[["conditional"]],
            value = TRUE
          )
          cleaned_terms <- gsub(
            "(as\\.factor|factor|as\\.character)\\((.*)\\)",
            "\\2",
            factor_terms
          )
          for (k in seq_along(factor_terms)) {
            ft <- factor_terms[k] # e.g. "factor(cyl)"
            orig <- cleaned_terms[k] # e.g. "cyl"
            if (orig %in% colnames(mf) && !ft %in% colnames(mf)) {
              orig_label <- attr(mf[[orig]], "label", exact = TRUE)
              if (!is.null(orig_label)) {
                mf[[ft]] <- as.factor(mf[[orig]])
                attr(mf[[ft]], "label") <- orig_label
              }
            }
          }
        }
      }
    }
    resp <- insight::find_response(model, combine = FALSE)
    mf <- mf[, setdiff(colnames(mf), resp), drop = FALSE]

    # return variable labels, and for factors/characters, add labels for each level.
    # character variables must be handled like factors (creating one entry per unique
    # value), otherwise the lengths of lbs and preds will differ and setNames() fails,
    # causing ALL labels to be lost (fix for issue #1142).
    lbs <- lapply(colnames(mf), function(i) {
      vec <- mf[[i]]
      if (is.factor(vec) || is.character(vec)) {
        variable_label <- attr(vec, "label", exact = TRUE)
        value_labels <- names(attr(vec, "labels", exact = TRUE))
        if (is.null(variable_label)) {
          variable_label <- i
        }
        if (is.null(value_labels)) {
          if (is.character(vec)) {
            value_labels <- levels(as.factor(vec))
          } else {
            value_labels <- levels(vec)
          }
        }
        out <- paste0(variable_label, " [", value_labels, "]")
      } else {
        out <- attr(vec, "label", exact = TRUE)
      }
      if (is.null(out)) {
        i
      } else {
        out
      }
    })

    # coefficient names (not labels)
    preds <- lapply(colnames(mf), function(i) {
      if (is.character(mf[[i]])) {
        mf[[i]] <- as.factor(mf[[i]])
      }
      if (is.factor(mf[[i]])) {
        i <- paste0(i, levels(mf[[i]]))
      }
      i
    })

    # name elements
    names(lbs) <- names(preds) <- colnames(mf)
    pretty_labels <- .safe(stats::setNames(
      unlist(lbs, use.names = FALSE),
      unlist(preds, use.names = FALSE)
    ))

    # retrieve pretty names attribute
    pn <- attributes(params)$pretty_names
    # replace former pretty names with labels, if we have any labels
    # (else, default pretty names are returned)
    if (!is.null(pretty_labels)) {
      # for models from pscl, we have "count_" and "zero_" prefixes, which
      # we need to add to the "pretty_labels" names, so that we can match
      # them with the parameters
      if (inherits(model, c("zeroinfl", "hurdle"))) {
        pretty_labels <- c(
          stats::setNames(pretty_labels, paste0("count_", names(pretty_labels))),
          stats::setNames(pretty_labels, paste0("zero_", names(pretty_labels)))
        )
      }
      # check if we have any interactions, and if so, create combined labels
      interactions <- pn[grepl(":", names(pn), fixed = TRUE)]
      if (length(interactions)) {
        labs <- NULL
        for (i in names(interactions)) {
          # extract single coefficient names from interaction term
          out <- unlist(strsplit(i, ":", fixed = TRUE))
          # combine labels; fall back to pretty_names when a component has no
          # label entry (e.g. for on-the-fly factor conversions like factor(kid5))
          labs <- c(
            labs,
            paste(
              vapply(
                out,
                function(l) {
                  lbl <- pretty_labels[l]
                  if (is.na(lbl)) {
                    lbl <- pn[l]
                  }
                  lbl
                },
                character(1)
              ),
              collapse = " * "
            )
          )
        }
        # add interaction terms to labels string
        names(labs) <- names(interactions)
        pretty_labels <- c(pretty_labels, labs)
      }
      # make sure "invalid" labels are ignored
      common_labels <- intersect(names(pretty_labels), names(pn))
      pn[common_labels] <- pretty_labels[common_labels]
    }
    pretty_labels <- pn
  }

  # missing labels return original parameter name (e.g., variance components in mixed models)
  out <- stats::setNames(params$Parameter, params$Parameter)
  pretty_labels <- pretty_labels[names(pretty_labels) %in% params$Parameter]
  out[match(names(pretty_labels), params$Parameter)] <- pretty_labels

  out
}


.format_value_labels.comparisons <- function(params, model = NULL, ...) {
  pretty_labels <- NULL
  if (is.null(model)) {
    model <- .get_object(params)
  }

  # validation check
  # get data, but exclude response - we have no need for that label
  mf <- insight::get_data(model)

  resp <- insight::find_response(model, combine = FALSE)
  mf <- mf[, setdiff(colnames(mf), resp), drop = FALSE]

  # return variable labels, and for factors/characters, add labels for each level.
  # character variables must be handled like factors (creating one entry per unique
  # value), otherwise the lengths of lbs and preds will differ and setNames() fails,
  # causing ALL labels to be lost (fix for issue #1142).
  lbs <- lapply(colnames(mf), function(i) {
    vec <- mf[[i]]
    variable_label <- attr(vec, "label", exact = TRUE)
    if (is.null(variable_label)) {
      variable_label <- i
    }
    variable_label
  })

  # name elements
  names(lbs) <- colnames(mf)
  pretty_labels <- .safe(stats::setNames(
    unlist(lbs, use.names = FALSE),
    unlist(preds, use.names = FALSE)
  ))

  # retrieve pretty names attribute
  pn <- attributes(params)$pretty_names
  # replace former pretty names with labels, if we have any labels
  # (else, default pretty names are returned)
  if (!is.null(pretty_labels)) {
    # for models from pscl, we have "count_" and "zero_" prefixes, which
    # we need to add to the "pretty_labels" names, so that we can match
    # them with the parameters
    if (inherits(model, c("zeroinfl", "hurdle"))) {
      pretty_labels <- c(
        stats::setNames(pretty_labels, paste0("count_", names(pretty_labels))),
        stats::setNames(pretty_labels, paste0("zero_", names(pretty_labels)))
      )
    }
    # check if we have any interactions, and if so, create combined labels
    interactions <- pn[grepl(":", names(pn), fixed = TRUE)]
    if (length(interactions)) {
      labs <- NULL
      for (i in names(interactions)) {
        # extract single coefficient names from interaction term
        out <- unlist(strsplit(i, ":", fixed = TRUE))
        # combine labels; fall back to pretty_names when a component has no
        # label entry (e.g. for on-the-fly factor conversions like factor(kid5))
        labs <- c(
          labs,
          paste(
            vapply(
              out,
              function(l) {
                lbl <- pretty_labels[l]
                if (is.na(lbl)) {
                  lbl <- pn[l]
                }
                lbl
              },
              character(1)
            ),
            collapse = " * "
          )
        )
      }
      # add interaction terms to labels string
      names(labs) <- names(interactions)
      pretty_labels <- c(pretty_labels, labs)
    }
    # make sure "invalid" labels are ignored
    common_labels <- intersect(names(pretty_labels), names(pn))
    pn[common_labels] <- pretty_labels[common_labels]
  }
  pretty_labels <- pn

  # missing labels return original parameter name (e.g., variance components in mixed models)
  out <- stats::setNames(params$Parameter, params$Parameter)
  pretty_labels <- pretty_labels[names(pretty_labels) %in% params$Parameter]
  out[match(names(pretty_labels), params$Parameter)] <- pretty_labels

  out
}


# helper -------------------

.is_valid_model_input <- function(model) {
  !is.null(model) &&
    ((insight::is_regression_model(model) && !is.data.frame(model)) ||
      inherits(model, c("predictions", "comparisons", "slopes")))
}
