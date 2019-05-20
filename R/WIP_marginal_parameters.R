#' @title Marginal Parameters for Mixed Models
#'
#' Calculates marginal coefficients from fitted generalized linear mixed models.
#'
#' @param model A statistical model.
#' @param std_errors Logical indicating whether standard errors are to be computed.
#' @param link_fun A function transforming the mean of the repeated measurements outcome to the linear predictor scale. Typically, this derived from the family argument of mixed_model..
#' @param M Numeric scalar denoting the number of Monte Carlo samples.
#' @param K Numeric scalar denoting the number of samples from the sampling distribution of the maximum likelihood estimates.
#' @param seed Integer denoting the seed for the random number generation.
#' @param cores Integer giving the number of cores to use; applicable only when std_errors = TRUE.
#' @param sandwich Logical; if TRUE robust/sandwich standard errors are used in the calculations.
#' @param ... Arguments passed to or from other methods.
#'
#' @references Hedeker, D., du Toit, S. H., Demirtas, H., & Gibbons, R. D. (2018). A note on marginalization of regression parameters from mixed models of binary outcomes. Biometrics, 74(1), 354-361.
#'
#' @seealso \href{https://drizopoulos.github.io/GLMMadaptive/reference/marginal_coefs.html}{GLMMadaptive::marginal_coefs}.
#'
#'
#' @examples
#' \dontrun{
#' model <- GLMMadaptive::mixed_model(vs ~ mpg,
#'   random = ~ 1 | cyl, data = mtcars,
#'   family = binomial()
#' )
#' marginal_parameters(model)
#' }
#'
#' @importFrom stats model.offset offset plogis pnorm rnorm runif var vcov model.matrix
#' @importFrom utils as.relistable relist
#' @export
marginal_parameters <- function(model, ...) {
  UseMethod("marginal_parameters")
}


#' @rdname marginal_parameters
#' @export
marginal_parameters.MixMod <- function(model, std_errors = FALSE, link_fun = NULL,
                                       M = 3000, K = 100,
                                       seed = 1, cores = max(parallel::detectCores() - 1, 1),
                                       sandwich = FALSE, ...) {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package `MASS` required, but missing. Please install it first.")
  }

  if (!requireNamespace("parallel", quietly = TRUE)) {
    stop("Package `parallel` required, but missing. Please install it first.")
  }

  object <- model
  offset <- object$offset
  X <- stats::model.matrix(object$Terms$termsX, object$model_frames$mfX)
  id <- match(object$id[[1L]], unique(object$id[[1L]]))
  Z <- mapply(.constructor_Z, object$Terms$termsZ, object$model_frames$mfZ,
    MoreArgs = list(id = id), SIMPLIFY = FALSE
  )
  Z <- do.call("cbind", Z)
  betas <- .fixef_MixMod(object)
  D <- object$D
  if (!is.null(object$gammas)) {
    offset_zi <- stats::model.offset(object$model_frames$mfX_zi)
    X_zi <- model.matrix_MixMod(object$Terms$termsX_zi, object$model_frames$mfX_zi)
    if (!is.null(object$Terms$termsZ_zi)) {
      Z_zi <- mapply(.constructor_Z, object$Terms$termsZ_zi, object$model_frames$mfZ_zi,
        MoreArgs = list(id = id), SIMPLIFY = FALSE
      )
      Z_zi <- do.call("cbind", Z_zi)
    } else {
      Z_zi <- NULL
    }
    gammas <- .fixef_MixMod(object, "zero_part")
  } else {
    X_zi <- Z_zi <- gammas <- NULL
  }


  out <- list(betas = .marginal_parameters(object, X, betas, Z, X_zi, gammas, Z_zi, D, M, link_fun, seed, offset_zi))
  if (std_errors) {
    blocks <- split(seq_len(K), rep(seq_len(cores),
      each = ceiling(K / cores),
      length.out = K
    ))
    D <- object$D
    diag_D <- ncol(D) > 1 && all(abs(D[lower.tri(D)]) < sqrt(.Machine$double.eps))
    list_thetas <- list(betas = betas, D = if (diag_D) log(diag(D)) else .chol_transf(D))
    if (!is.null(object$phis)) {
      list_thetas <- c(list_thetas, list(phis = object$phis))
    }
    if (!is.null(gammas)) {
      list_thetas <- c(list_thetas, list(gammas = gammas))
    }
    tht <- unlist(as.relistable(list_thetas))
    V <- vcov(object, sandwich = sandwich)
    cluster_compute_marg_coefs <- function(block, tht, list_thetas, V, XX, Z, X_zi,
                                               Z_zi, M, .marginal_parameters, .chol_transf,
                                               object, link_fun, seed) {
      if (!exists(".Random.seed", envir = .GlobalEnv)) {
        runif(1)
      }
      RNGstate <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", RNGstate, envir = .GlobalEnv))
      n_block <- length(block)
      m_betas <- matrix(0.0, n_block, length(list_thetas[["betas"]]))
      for (b in seq_along(block)) {
        seed. <- seed + block[b]
        set.seed(seed.)
        new_tht <- relist(MASS::mvrnorm(1, tht, V), skeleton = list_thetas)
        new_betas <- new_tht$betas
        new_D <- if (diag_D) diag(exp(new_tht$D), length(new_tht$D)) else .chol_transf(new_tht$D)
        new_gammas <- new_tht$gammas
        m_betas[b, ] <- .marginal_parameters(object, XX, new_betas, Z, X_zi,
          new_gammas, Z_zi, new_D, M, link_fun,
          seed = seed., offset_zi
        )
      }
      m_betas
    }

    cl <- parallel::makeCluster(cores)
    res <- parallel::parLapply(cl, blocks, cluster_compute_marg_coefs,
      tht = tht,
      list_thetas = list_thetas, V = V, XX = X, Z = Z,
      X_zi = X_zi, Z_zi = Z_zi, M = M,
      object = object, .marginal_parameters = .marginal_parameters,
      .chol_transf = .chol_transf, link_fun = link_fun, seed = seed
    )
    parallel::stopCluster(cl)
    out$var_betas <- stats::var(do.call("rbind", res))
    dimnames(out$var_betas) <- list(names(out$betas), names(out$betas))
    ses <- sqrt(diag(out$var_betas))
    coef_table <- cbind(
      "Estimate" = out$betas, "Std.Err" = ses,
      "z-value" = out$betas / ses,
      "p-value" = 2 * stats::pnorm(abs(out$betas / ses), lower.tail = FALSE)
    )
    out$coef_table <- coef_table
  }
  class(out) <- "m_coefs"
  out
}











#' @keywords internal
.marginal_parameters <- function(object, X, betas, Z, X_zi, gammas, Z_zi, D, M,
                                 link_fun, seed, offset_zi) {
  if (!exists(".Random.seed", envir = .GlobalEnv)) {
    runif(1)
  }
  RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(assign(".Random.seed", RNGstate, envir = .GlobalEnv))
  mu_fun <- object$Funs$mu_fun
  if (is.null(link_fun)) {
    link_fun <- object$family$linkfun
  }
  if (is.null(link_fun)) {
    stop("you must specify the 'link_fun' argument.\n")
  }
  Xbetas <- c(X %*% betas)
  if (!is.null(offset)) {
    Xbetas <- Xbetas + offset
  }
  if (!is.null(gammas)) {
    eta_zi <- c(X_zi %*% gammas)
    if (!is.null(offset_zi)) {
      eta_zi <- eta_zi + offset_zi
    }
  }
  id <- match(object$id[[1]], unique(object$id[[1]]))
  nRE <- ncol(D)
  N <- nrow(X)
  n <- length(unique(id))
  eS <- eigen(D, symmetric = TRUE)
  ev <- eS$values
  V <- eS$vectors %*% diag(sqrt(pmax(ev, 0)), nRE)
  marg_inv_mu <- numeric(N)
  for (i in seq_len(n)) {
    set.seed(seed + i)
    id_i <- id == i
    b <- V %*% matrix(stats::rnorm(M * nRE), nRE, M)
    Zb <- Z[id_i, , drop = FALSE] %*% b[seq_len(ncol(Z)), , drop = FALSE]
    mu <- mu_fun(Xbetas[id_i] + Zb)
    if (!is.null(gammas)) {
      eta_zi_id_i <- eta_zi[id_i]
      if (!is.null(object$Terms$termsZ_zi)) {
        eta_zi_id_i <- eta_zi_id_i + Z_zi[id_i, , drop = FALSE] %*%
          b[-seq_len(ncol(Z)), , drop = FALSE]
      }
      mu <- plogis(eta_zi_id_i, lower.tail = FALSE) * mu
    }
    marg_inv_mu[id_i] <- link_fun(rowMeans(mu))
  }
  res <- c(solve(crossprod(X), crossprod(X, marg_inv_mu)))
  names(res) <- names(betas)
  res
}




#' @keywords internal
.constructor_Z <- function(termsZ_i, mfZ_i, id) {
  n <- length(unique(id))
  Zmats <- vector("list", n)
  for (i in seq_len(n)) {
    mf <- stats::model.frame(termsZ_i, mfZ_i[id == i, , drop = FALSE],
      drop.unused.levels = TRUE
    )
    mm <- stats::model.matrix(termsZ_i, mf)
    assign <- attr(mm, "assign")
    Zmats[[i]] <- mm[, c(t(sapply(unique(assign), function(x) which(assign == x)))),
      drop = FALSE
    ]
  }
  do.call("rbind", Zmats)
}



#' @keywords internal
.chol_transf <- function(x) {
  if (any(is.na(x) | !is.finite(x))) {
    stop("NA or infinite values in 'x'.\n")
  }
  if (is.matrix(x)) {
    k <- nrow(x)
    U <- chol(x)
    U[cbind(1:k, 1:k)] <- log(U[cbind(1:k, 1:k)])
    U[upper.tri(U, TRUE)]
  } else {
    nx <- length(x)
    k <- round((-1 + sqrt(1 + 8 * nx)) / 2)
    mat <- matrix(0, k, k)
    mat[upper.tri(mat, TRUE)] <- x
    mat[cbind(1:k, 1:k)] <- exp(mat[cbind(1:k, 1:k)])
    res <- crossprod(mat)
    attr(res, "L") <- t(mat)[lower.tri(mat, TRUE)]
    res
  }
}




#' @keywords internal
.fixef_MixMod <- function(object, sub_model = c("main", "zero_part"), ...) {
  sub_model <- match.arg(sub_model)
  if (sub_model == "main") {
    object$coefficients
  } else {
    if (!is.null(object$gammas)) {
      object$gammas
    } else {
      stop("the fitted model does not have an extra zero-part.")
    }
  }
}


#' @importFrom stats model.matrix
#' @keywords internal
model.matrix_MixMod <- function(object, type = c("fixed", "random", "zi_fixed", "zi_random"), ...) {
  type <- match.arg(type)
  switch(type,
    "fixed" = stats::model.matrix(object$Terms$termsX, object$model_frames$mfX),
    "random" = {
      id <- object$id[[1]]
      id <- match(id, unique(id))
      Z <- mapply(.constructor_Z, object$Terms$termsZ, object$model_frames$mfZ,
        MoreArgs = list(id = id), SIMPLIFY = FALSE
      )
      do.call("cbind", Z)
    },
    "zi_fixed" = stats::model.matrix(object$Terms$termsX_zi, object$model_frames$mfX_zi),
    "zi_random" = {
      id <- object$id[[1]]
      id <- match(id, unique(id))
      Z <- mapply(.constructor_Z, object$Terms$termsZ_zi, object$model_frames$mfZ_zi,
        MoreArgs = list(id = id), SIMPLIFY = FALSE
      )
      do.call("cbind", Z)
    }
  )
}


#' @keywords internal
model.frame_MixMod <- function(formula, type = c(
                                 "fixed", "random", "zi_fixed",
                                 "zi_random"
                               ), ...) {
  type <- match.arg(type)
  switch(type, "fixed" = formula$model_frames$mfX, "random" = formula$model_frames$mfZ,
    "zi_fixed" = formula$model_frames$mfX_zi,
    "zi_random" = formula$model_frames$mfZ_zi
  )
}


# model <- GLMMadaptive::mixed_model(Sepal.Length ~ Sepal.Width, random=~1|Species, data=iris, family="gaussian")
# model <- lme4::lmer(Sepal.Length ~ Sepal.Width + (1|Species), data=iris)
# model <- nlme::lme(Sepal.Length ~ Sepal.Width, random=~1|Species, data=iris)
# GLMMadaptive::marginal_coefs(model)
