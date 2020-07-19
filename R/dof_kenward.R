#' @rdname p_value_kenward
#' @importFrom insight find_parameters
#' @export
dof_kenward <- function(model) {
  parameters <- insight::find_parameters(model, effects = "fixed", flatten = TRUE)
  L <- as.data.frame(diag(rep(1, n_parameters(model, effects = "fixed"))))
  krvcov <- .vcov_kenward_ajusted(model)

  dof <- stats::setNames(sapply(L, .kenward_adjusted_ddf, model = model, adjusted_vcov = krvcov), parameters)

  attr(dof, "vcov") <- krvcov
  attr(dof, "se") <- abs(as.vector(sqrt(diag(as.matrix(krvcov)))))
  dof
}



# The following code was taken from the "pbkrtest" package and slightly modified
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
.kenward_adjusted_ddf <- function(model, linear_coef, adjusted_vcov) {
  .adjusted_ddf(adjusted_vcov, linear_coef, stats::vcov(model))
}


.adjusted_ddf <- function(adjusted_vcov, linear_coef, unadjusted_vcov = adjusted_vcov) {
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Package 'Matrix' required for this function to work. Please install it.")
  }
  if (!is.matrix(linear_coef)) {
    linear_coef <- matrix(linear_coef, ncol = 1)
  }
  vlb <- sum(linear_coef * (unadjusted_vcov %*% linear_coef))
  theta <- Matrix::Matrix(as.numeric(outer(linear_coef, linear_coef) / vlb), nrow = length(linear_coef))
  P <- attr(adjusted_vcov, "P")
  W <- attr(adjusted_vcov, "W")

  A1 <- A2 <- 0
  theta_unadjusted_vcov <- theta %*% unadjusted_vcov
  n.ggamma <- length(P)
  for (ii in 1:n.ggamma) {
    for (jj in c(ii:n.ggamma)) {
      e <- ifelse(ii == jj, 1, 2)
      ui <- as.matrix(theta_unadjusted_vcov %*% P[[ii]] %*% unadjusted_vcov)
      uj <- as.matrix(theta_unadjusted_vcov %*% P[[jj]] %*% unadjusted_vcov)
      A1 <- A1 + e * W[ii, jj] * (sum(diag(ui)) * sum(diag(uj)))
      A2 <- A2 + e * W[ii, jj] * sum(ui * t(uj))
    }
  }

  B <- (A1 + 6 * A2) / 2
  g <- (2 * A1 - 5 * A2) / (3 * A2)
  c1 <- g / (3 + 2 * (1 - g))
  c2 <- (1 - g) / (3 + 2 * (1 - g))
  c3 <- (3 - g) / (3 + 2 * (1 - g))
  EE <- 1 + A2
  VV <- 2 * (1 + B)
  EEstar <- 1 / (1 - A2)
  VVstar <- 2 * ((1 + c1 * B) / ((1 - c2 * B)^2 * (1 - c3 * B)))
  V0 <- 1 + c1 * B
  V1 <- 1 - c2 * B
  V2 <- 1 - c3 * B
  V0 <- ifelse(abs(V0) < 1e-10, 0, V0)
  rho <- (.divZero(1 - A2, V1))^2 * V0 / V2
  df2 <- 4 + 3 / (rho - 1)
  df2
}



.divZero <- function(x, y, tol = 1e-14) {
  ## ratio x/y is set to 1 if both |x| and |y| are below tol
  if (abs(x) < tol & abs(y) < tol) {
    1
  } else {
    x / y
  }
}



#' @importFrom stats update
.vcov_kenward_ajusted <- function(model) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work. Please install it.")
  }
  if (!(lme4::getME(model, "is_REML"))) {
    model <- stats::update(model, . ~ ., REML = TRUE)
  }
  .vcovAdj16_internal(stats::vcov(model), .get_SigmaG(model), lme4::getME(model, "X"))
}




#' @importFrom stats sigma
.get_SigmaG <- function(model) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work. Please install it.")
  }
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Package 'Matrix' required for this function to work. Please install it.")
  }

  GGamma <- lme4::VarCorr(model)
  SS <- .shgetME(model)

  ## Put covariance parameters for the random effects into a vector:
  ## Fixme: It is a bit ugly to throw everything into one long vector here; a list would be more elegant
  ggamma <- NULL
  for (ii in 1:(SS$n.RT)) {
    Lii <- GGamma[[ii]]
    ggamma <- c(ggamma, Lii[lower.tri(Lii, diag = TRUE)])
  }
  ggamma <- c(ggamma, stats::sigma(model)^2) ## Extend ggamma by the residuals variance
  n.ggamma <- length(ggamma)

  ## Find G_r:
  G <- NULL
  Zt <- lme4::getME(model, "Zt")
  for (ss in 1:SS$n.RT) {
    ZZ <- .shget_Zt_group(ss, Zt, SS$Gp)
    n.lev <- SS$n.lev.by.RT2[ss] ## ; cat(sprintf("n.lev=%i\n", n.lev))
    Ig <- Matrix::sparseMatrix(1:n.lev, 1:n.lev, x = 1)
    for (rr in 1:SS$n.parm.by.RT[ss]) {
      ## This is takes care of the case where there is random regression and several matrices have to be constructed.
      ## FIXME: I am not sure this is correct if there is a random quadratic term. The '2' below looks suspicious.
      ii.jj <- .index2UpperTriEntry(rr, SS$n.comp.by.RT[ss]) ##; cat("ii.jj:"); print(ii.jj)
      ii.jj <- unique(ii.jj)
      if (length(ii.jj) == 1) {
        EE <- Matrix::sparseMatrix(
          ii.jj,
          ii.jj,
          x = 1,
          dims = rep(SS$n.comp.by.RT[ss], 2)
       )
      } else {
        EE <- Matrix::sparseMatrix(ii.jj, ii.jj[2:1], dims = rep(SS$n.comp.by.RT[ss], 2))
      }
      EE <- Ig %x% EE  ## Kronecker product
      G  <- c(G, list(t(ZZ) %*% EE %*% ZZ))
    }
  }

  ## Extend by the indentity for the residual
  n.obs <- insight::n_obs(model)
  G <- c(G, list(Matrix::sparseMatrix(1:n.obs, 1:n.obs, x = 1)))

  Sigma <- ggamma[1] * G[[1]]
  for (ii in 2:n.ggamma) {
    Sigma <- Sigma + ggamma[ii] * G[[ii]]
  }

  list(Sigma = Sigma, G = G, n.ggamma = n.ggamma)
}




.index2UpperTriEntry <- function(k, N) {
  ## inverse of indexSymmat2vec
  ## result: index pair (i,j) with i>=j
  ## k: element in the vector of upper triangular elements
  ## example: N=3: k=1 -> (1,1), k=2 -> (1,2), k=3 -> (1,3), k=4 -> (2,2)
  aa    <- cumsum(N:1)
  aaLow <- c(0, aa[-length(aa)])
  i     <- which(aaLow < k & k <= aa)
  j     <- k - N * i + N - i * (3 - i) / 2 + i
  c(i, j)
}




.vcovAdj16_internal <- function(Phi, SigmaG, X) {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' required for this function to work. Please install it.")
  }
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Package 'Matrix' required for this function to work. Please install it.")
  }

  SigmaInv <- chol2inv(chol(Matrix::forceSymmetric(as.matrix(SigmaG$Sigma))))
  n.ggamma <- SigmaG$n.ggamma
  TT       <- as.matrix(SigmaInv %*% X)
  HH       <- OO <- vector("list", n.ggamma)

  for (ii in 1:n.ggamma) {
    HH[[ii]] <- as.matrix(SigmaG$G[[ii]] %*% SigmaInv)
    OO[[ii]] <- as.matrix(HH[[ii]] %*% X)
  }

  ## Finding PP, QQ
  PP <- QQ <- NULL
  for (rr in 1:n.ggamma) {
    OrTrans <- t(OO[[rr]])
    PP <- c(PP, list(Matrix::forceSymmetric(-1 * OrTrans %*% TT)))
    for (ss in rr:n.ggamma) {
      QQ <- c(QQ, list(OrTrans %*% SigmaInv %*% OO[[ss]]))
    }
  }

  PP <- as.matrix(PP)
  QQ <- as.matrix(QQ)

  Ktrace <- matrix(NA, nrow = n.ggamma, ncol = n.ggamma)
  for (rr in 1:n.ggamma) {
    HrTrans <- t(HH[[rr]])
    for (ss in rr:n.ggamma) {
      Ktrace[rr, ss] <- Ktrace[ss, rr] <- sum(HrTrans * HH[[ss]])
    }
  }

  ## Finding information matrix
  IE2 <- matrix(NA, nrow = n.ggamma, ncol = n.ggamma)
  for (ii in 1:n.ggamma) {
    Phi.P.ii <- Phi %*% PP[[ii]]
    for (jj in c(ii:n.ggamma)) {
      www <- .indexSymmat2vec(ii, jj, n.ggamma)
      IE2[ii, jj] <- IE2[jj, ii] <- Ktrace[ii, jj] -
        2 * sum(Phi * QQ[[www]]) + sum(Phi.P.ii * (PP[[jj]] %*% Phi))
    }
  }

  eigenIE2 <- eigen(IE2, only.values = TRUE)$values
  condi    <- min(abs(eigenIE2))

  WW <- if (condi > 1e-10)
    as.matrix(Matrix::forceSymmetric(2 * solve(IE2)))
  else
    as.matrix(Matrix::forceSymmetric(2 * MASS::ginv(IE2)))

  UU <- matrix(0, nrow = ncol(X), ncol = ncol(X))
  for (ii in 1:(n.ggamma - 1)) {
    for (jj in c((ii + 1):n.ggamma)) {
      www <- .indexSymmat2vec(ii, jj, n.ggamma)
      UU <- UU + WW[ii, jj] * (QQ[[www]] - PP[[ii]] %*% Phi %*% PP[[jj]])
    }
  }

  UU <- as.matrix(UU)
  UU <- UU + t(UU)
  for (ii in 1:n.ggamma) {
    www <- .indexSymmat2vec(ii, ii, n.ggamma)
    UU <- UU + WW[ii, ii] * (QQ[[www]] - PP[[ii]] %*% Phi %*% PP[[ii]])
  }

  GGAMMA <-  Phi %*% UU %*% Phi
  PhiA   <-  Phi + 2 * GGAMMA
  attr(PhiA, "P")     <- PP
  attr(PhiA, "W")     <- WW
  attr(PhiA, "condi") <- condi
  PhiA


  # n.ggamma <- SigmaG$n.ggamma
  #
  # M <- cbind(do.call(cbind, SigmaG$G), X)
  # SinvM <- chol2inv(chol(Matrix::forceSymmetric(SigmaG$Sigma))) %*% M
  #
  # v   <- c(rep(1:length(SigmaG$G), each = nrow(SinvM)), rep(length(SigmaG$G) + 1, ncol(X)))
  # idx <- lapply(unique.default(v), function(i) {
  #   which(v == i)
  # })
  # SinvG <- lapply(idx, function(z) {
  #   SinvM[, z]
  # })
  #
  # SinvX <- SinvG[[length(SinvG)]]
  # SinvG[length(SinvG)] <- NULL
  #
  # OO <- lapply(1:n.ggamma, function(i) {
  #   SigmaG$G[[i]] %*% SinvX
  # })
  #
  # PP <- vector("list", n.ggamma)
  # QQ <- vector("list", n.ggamma * (n.ggamma + 1) / 2)
  # index <- 1
  # for (r in 1:n.ggamma) {
  #   OOt.r <- t(OO[[r]])
  #   PP[[r]] <- -1 * (OOt.r %*%  SinvX)
  #   for (s in r:n.ggamma) {
  #     QQ[[index]] <- OOt.r %*% (SinvG[[s]] %*% SinvX)
  #     index <- index + 1
  #   }
  # }
  #
  # Ktrace <- matrix(NA, nrow = n.ggamma, ncol = n.ggamma)
  # for (r in 1:n.ggamma) {
  #   HHr <- SinvG[[r]]
  #   for (s in r:n.ggamma) {
  #     Ktrace[r, s] <- Ktrace[s, r] <- sum(HHr * SinvG[[s]])
  #   }
  # }
  #
  # ## Finding information matrix
  # IE2 <- matrix(0, nrow = n.ggamma, ncol = n.ggamma)
  # for (ii in 1:n.ggamma) {
  #   Phi.P.ii <- Phi %*% PP[[ii]]
  #   for (jj in c(ii:n.ggamma)) {
  #     www <- .indexSymmat2vec(ii, jj, n.ggamma)
  #     IE2[ii, jj] <- IE2[jj, ii] <- Ktrace[ii, jj] -
  #       2 * sum(Phi * QQ[[www]]) + sum(Phi.P.ii * (PP[[jj]] %*% Phi))
  #   }
  # }
  #
  # eigenIE2 <- eigen(IE2, only.values = TRUE)$values
  # condi    <- min(abs(eigenIE2))
  # WW <- if (condi > 1e-10)
  #   Matrix::forceSymmetric(2 * solve(IE2))
  # else
  #   Matrix::forceSymmetric(2 * MASS::ginv(IE2))
  #
  # UU <- matrix(0, nrow = ncol(X), ncol = ncol(X))
  # for (ii in 1:(n.ggamma - 1)) {
  #   for (jj in c((ii + 1):n.ggamma)) {
  #     www <- .indexSymmat2vec(ii, jj, n.ggamma)
  #     UU <- UU + WW[ii, jj] * (QQ[[www]] - PP[[ii]] %*% Phi %*% PP[[jj]])
  #   }
  # }
  #
  # UU <- UU + t(UU)
  # for (ii in 1:n.ggamma) {
  #   www <- .indexSymmat2vec(ii, ii, n.ggamma)
  #   UU  <- UU + WW[ii, ii] * (QQ[[www]] - PP[[ii]] %*% Phi %*% PP[[ii]])
  # }
  #
  # GGAMMA <-  Phi %*% UU %*% Phi
  # PhiA   <-  Phi + 2 * GGAMMA
  # attr(PhiA, "P")     <- PP
  # attr(PhiA, "W")     <- WW
  # attr(PhiA, "condi") <- condi
  # PhiA
}



.indexSymmat2vec <- function(i, j, N) {
  ## S[i,j] symetric N times N matrix
  ## r the vector of upper triangular element  in row major order:
  ## r= c(S[1,1],S[1,2]...,S[1,j], S[1,N], S[2,2],...S[N,N]
  ##Result: k: index of k-th element of r
  k <- if (i <= j) {
    (i - 1) * (N - i / 2) + j
  } else {
    (j - 1) * (N - j / 2) + i
  }
}




.shgetME <- function(model) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work. Please install it.")
  }

  Gp <- lme4::getME(model, "Gp")
  n.RT <- length(Gp) - 1  ## Number of random terms (i.e. of (|)'s)
  n.lev.by.RT <- sapply(lme4::getME(model, "flist"), function(x) length(levels(x)))
  n.comp.by.RT <- .get.RT.dim.by.RT(model)
  n.parm.by.RT <- (n.comp.by.RT + 1) * n.comp.by.RT / 2
  n.RE.by.RT <- diff(Gp)

  n.lev.by.RT2 <- n.RE.by.RT / n.comp.by.RT ## Same as n.lev.by.RT2 ???

  list(Gp           = Gp,           ## group.index
       n.RT         = n.RT,         ## n.groupFac
       n.lev.by.RT  = n.lev.by.RT,  ## nn.groupFacLevelsNew
       n.comp.by.RT = n.comp.by.RT, ## nn.GGamma
       n.parm.by.RT = n.parm.by.RT, ## mm.GGamma
       n.RE.by.RT   = n.RE.by.RT,   ## ... Not returned before
       n.lev.by.RT2 = n.lev.by.RT2, ## nn.groupFacLevels
       n_rtrms      = lme4::getME(model, "n_rtrms")
  )
}


## Alternative to .get_Zt_group
.shget_Zt_group <- function(ii.group, Zt, Gp, ...) {
  zIndex.sub <-  (Gp[ii.group] + 1):Gp[ii.group + 1]
  as.matrix(Zt[ zIndex.sub , ])
}



.get.RT.dim.by.RT <- function(model) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work. Please install it.")
  }
  ## output: dimension (no of columns) of covariance matrix for random term ii
  if (inherits(model, "mer")) {
    sapply(model@ST,function(X) nrow(X))
  } else {
    sapply(lme4::getME(model, "cnms"), length)
  }
}
