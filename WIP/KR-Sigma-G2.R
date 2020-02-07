## ##############################################################################
##
## LMM_Sigma_G: Returns VAR(Y) = Sigma and the G matrices
##
## Re-implemented in Banff, Canada, August 2013 by Søren Højsgaard
##
## ##############################################################################

get_SigmaG <- function(object, details=0) {
  UseMethod("get_SigmaG")
}

get_SigmaG.lmerMod  <- function(object, details=0) {
  .get_SigmaG( object, details )
}

get_SigmaG.mer  <- function(object, details=0) {
  LMM_Sigma_G( object, details )
}

.get_SigmaG  <- function(object, details=0) {

  DB     <- details > 0 ## For debugging only
  if (!.is.lmm(object))
    stop("'object' is not Gaussian linear mixed model")

  GGamma <- VarCorr(object)
  SS     <- .shgetME( object )

  ## Put covariance parameters for the random effects into a vector:
  ## Fixme: It is a bit ugly to throw everything into one long vector here; a list would be more elegant
  ggamma <- NULL
  for ( ii in 1:( SS$n.RT )) {
    Lii    <- GGamma[[ii]]
    ggamma <- c(ggamma, Lii[ lower.tri( Lii, diag=TRUE ) ] )
  }
  ggamma   <- c( ggamma, sigma( object )^2 ) ## Extend ggamma by the residuals variance
  n.ggamma <- length(ggamma)

  ## Find G_r:
  G  <- NULL
  Zt <- getME( object, "Zt" )
  for (ss in 1:SS$n.RT) {
    ZZ    <- .shget_Zt_group( ss, Zt, SS$Gp )
    n.lev <- SS$n.lev.by.RT2[ ss ] ## ; cat(sprintf("n.lev=%i\n", n.lev))
    Ig    <- sparseMatrix(1:n.lev, 1:n.lev, x=1)
    for (rr in 1:SS$n.parm.by.RT[ ss ]) {
      ## This is takes care of the case where there is random regression and several matrices have to be constructed.
      ## FIXME: I am not sure this is correct if there is a random quadratic term. The '2' below looks suspicious.
      ii.jj <- .index2UpperTriEntry( rr, SS$n.comp.by.RT[ ss ] ) ##; cat("ii.jj:"); print(ii.jj)
      ii.jj <- unique(ii.jj)
      if (length(ii.jj)==1){
        EE <- sparseMatrix(ii.jj, ii.jj, x=1, dims=rep(SS$n.comp.by.RT[ ss ], 2))
      } else {
        EE <- sparseMatrix(ii.jj, ii.jj[2:1], dims=rep(SS$n.comp.by.RT[ ss ], 2))
      }
      EE <- Ig %x% EE  ## Kronecker product
      G  <- c( G, list( t(ZZ) %*% EE %*% ZZ ) )
    }
  }

  ## Extend by the indentity for the residual
  n.obs <- nrow(getME(object,'X'))
  G    <- c( G, list(sparseMatrix(1:n.obs, 1:n.obs, x=1 )) )


  Sigma <- ggamma[1] * G[[1]]
  for (ii in 2:n.ggamma) {
    Sigma <- Sigma + ggamma[ii] * G[[ii]]
  }

  SigmaG <- list(Sigma=Sigma, G=G, n.ggamma=n.ggamma)
  SigmaG
}


.shgetME <- function( object ){
  Gp           <- getME( object, "Gp" )
  n.RT         <- length( Gp ) - 1  ## Number of random terms ( i.e. of (|)'s )
  n.lev.by.RT  <- sapply(getME(object, "flist"), function(x) length(levels(x)))
  n.comp.by.RT <- .get.RT.dim.by.RT( object )
  n.parm.by.RT <- (n.comp.by.RT + 1) * n.comp.by.RT / 2
  n.RE.by.RT   <- diff( Gp )

  n.lev.by.RT2 <- n.RE.by.RT / n.comp.by.RT ## Same as n.lev.by.RT2 ???

  list(Gp           = Gp,           ## group.index
       n.RT         = n.RT,         ## n.groupFac
       n.lev.by.RT  = n.lev.by.RT,  ## nn.groupFacLevelsNew
       n.comp.by.RT = n.comp.by.RT, ## nn.GGamma
       n.parm.by.RT = n.parm.by.RT, ## mm.GGamma
       n.RE.by.RT   = n.RE.by.RT,   ## ... Not returned before
       n.lev.by.RT2 = n.lev.by.RT2, ## nn.groupFacLevels
       n_rtrms      = getME( object, "n_rtrms")
       )
}

.getME.all <- function(obj) {
   nmME <- eval(formals(getME)$name)
   sapply(nmME, function(nm) try(getME(obj, nm)),
          simplify=FALSE)

}

## Alternative to .get_Zt_group
.shget_Zt_group <- function( ii.group, Zt, Gp, ... ){
  zIndex.sub <-  (Gp[ii.group]+1) : Gp[ii.group+1]
  ZZ <- Zt[ zIndex.sub , ]
  return(ZZ)
}


##
## Modular implementation
##

.get_GI_parms <- function( object ){
  GGamma <- VarCorr(object)
  parmList <- lapply(GGamma, function(Lii){  Lii[ lower.tri( Lii, diag=TRUE ) ] })
  parmList <- c( parmList, sigma( object )^2 )
  parmList
}

.get_GI_matrices <- function( object ){

  SS     <- .shgetME( object )
  Zt <- getME( object, "Zt" )

  G  <- NULL
  G  <- vector("list", SS$n.RT+1)

  for (ss in 1:SS$n.RT) {
    ZZ    <- .shget_Zt_group( ss, Zt, SS$Gp )
    n.lev <- SS$n.lev.by.RT2[ ss ] ## ; cat(sprintf("n.lev=%i\n", n.lev))
    Ig    <- sparseMatrix(1:n.lev, 1:n.lev, x=1)
    UU <- vector("list", SS$n.parm.by.RT)
    for (rr in 1:SS$n.parm.by.RT[ ss ]) {
      ii.jj <- .index2UpperTriEntry( rr, SS$n.comp.by.RT[ ss ] )
      ii.jj <- unique(ii.jj)
      if (length(ii.jj)==1){
        EE <- sparseMatrix(ii.jj, ii.jj, x=1, dims=rep(SS$n.comp.by.RT[ ss ], 2))
      } else {
        EE <- sparseMatrix(ii.jj, ii.jj[2:1], dims=rep(SS$n.comp.by.RT[ ss ], 2))
      }
      EE <- Ig %x% EE  ## Kronecker product
      UU[[ rr ]] <- t(ZZ) %*% EE %*% ZZ
    }
    G[[ ss ]] <- UU
  }
  n.obs <- nrow(getME(object,'X'))
  G[[ length( G ) ]] <- sparseMatrix(1:n.obs, 1:n.obs, x=1 )
  G
}
