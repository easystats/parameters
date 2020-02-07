## ##############################################################################
##
## LMM_Sigma_G: Returns VAR(Y) = Sigma and the G matrices
##
## ##############################################################################

LMM_Sigma_G  <- function(object, details=0) { 

    DB     <- details > 0 ## For debugging only
    
    if (!.is.lmm(object))
        stop("'object' is not Gaussian linear mixed model")
    
    GGamma <- VarCorr(object)  
    ## Indexing of the covariance matrix;
    ## this is somewhat technical and tedious
    Nindex <- .get_indices(object)
    
    ## number of random effects in each groupFac; note: residual error excluded!
    n.groupFac <- Nindex$n.groupFac
    
    ## the number of random effects for each grouping factor
    nn.groupFacLevels <- Nindex$nn.groupFacLevels
    
    ## size of the symmetric variance Gamma_i for reach groupFac
    nn.GGamma <- Nindex$nn.GGamma
    
    ## number of variance parameters of each GGamma_i  
    mm.GGamma   <-  Nindex$mm.GGamma
    
    ## not sure what this is...
    group.index <- Nindex$group.index
    
    ## writing the covariance parameters for the random effects into a vector: 
    ggamma <- NULL
    for ( ii in 1:(n.groupFac) ) {
        Lii <- GGamma[[ii]]
        nu  <- ncol(Lii)
        ## Lii[lower.tri(Lii,diag=TRUE)= Lii[1,1],Lii[1,2],Lii[1,3]..Lii[1,nu],
        ##                               Lii[2,2], Lii[2,3] ...
        ggamma<-c(ggamma,Lii[lower.tri(Lii,diag=TRUE)])
    }
    
    ## extend ggamma by the residuals variance such that everything random is included
    ggamma   <- c( ggamma, sigma( object )^2 )
    n.ggamma <- length(ggamma)
    
    ## Find G_r:
    Zt <- getME( object, "Zt" )
    
    t0 <- proc.time()
    G  <- NULL
    ##cat(sprintf("n.groupFac=%i\n", n.groupFac))
    for (ss in 1:n.groupFac) {
        ZZ <- .get_Zt_group(ss, Zt, object)
        ##cat("ZZ\n"); print(ZZ)
        
        n.levels <- nn.groupFacLevels[ss]
        ##cat(sprintf("n.levels=%i\n", n.levels))
        
        Ig <- sparseMatrix(1:n.levels, 1:n.levels, x=1)
        ##print(Ig)
        for (rr in 1:mm.GGamma[ss]) {
            ii.jj <- .indexVec2Symmat(rr,nn.GGamma[ss])
            ##cat("ii.jj:"); print(ii.jj)
            ii.jj <- unique(ii.jj)
            
            if (length(ii.jj)==1){
                EE <- sparseMatrix(ii.jj, ii.jj, x=1, dims=rep(nn.GGamma[ss],2))
            } else {
                EE <- sparseMatrix(ii.jj, ii.jj[2:1], dims=rep(nn.GGamma[ss],2))
            }
            ##cat("EE:\n");print(EE)
            
            EE <- Ig %x% EE  ## Kronecker product
            G  <- c( G, list( t(ZZ) %*% EE %*% ZZ ) )
        }
    }
    
    ## Extend by the indentity for the residual
    nobs <- nrow(getME(object,'X'))
    G    <- c( G, list(sparseMatrix(1:nobs, 1:nobs, x=1 )) ) 
    
    
    if(DB){cat(sprintf("Finding G  %10.5f\n", (proc.time()-t0)[1] )); t0 <- proc.time()}
    
    Sigma <- ggamma[1] * G[[1]]
    for (ii in 2:n.ggamma) {
        Sigma <- Sigma + ggamma[ii] * G[[ii]]
    }
    
    if(DB){cat(sprintf("Finding Sigma:    %10.5f\n", (proc.time()-t0)[1] ));
        t0 <- proc.time()}
    
    SigmaG <- list(Sigma=Sigma, G=G, n.ggamma=n.ggamma)
    SigmaG
}  

.get_indices <-function(object) {

  ## ff = number of random effects terms (..|F1) + (..|F1) are group factors!
  ## without the residual variance output: list of several indices

  ## we need  the number of random-term factors 
  Gp <- getME(object,"Gp")

  ff <- length(Gp)-1 
  gg <- sapply(getME(object,"flist"), function(x)length(levels(x)))

  qq <- .get.RT.dim.by.RT( object ) ##;  cat("qq:\n"); print(qq)
  
  ## number of variance parameters of each GGamma_i
  ss <- qq * (qq+1) / 2

  ## numb of random effects per level of random-term-factor
  nn.groupFac <- diff(Gp)  
  ##cat("nn.groupFac:\n"); print(nn.groupFac)
  
  ## number  of levels for each  random-term-factor; residual error here excluded!
  nn.groupFacLevels <- nn.groupFac / qq

  ## this is  the number of random term factors, should possible get a more approriate name
  list(n.groupFac           = ff, 
       nn.groupFacLevelsNew = gg,                # length of different grouping factors
       nn.groupFacLevels    = nn.groupFacLevels, # vector of the numb. levels for each random-term-factor
       nn.GGamma            = qq,
       mm.GGamma            = ss,
       group.index          = Gp)
}

.get_Zt_group <- function(ii.group, Zt, object) {

  ## ii.group : the index number of a grouping factor
  ## Zt       : the transpose of the random factors design matrix Z
  ## object   : A mer or lmerMod model
  ##output :  submatrix of Zt belongig to grouping factor ii.group

  Nindex            <- .get_indices(object)
  nn.groupFacLevels <- Nindex$nn.groupFacLevels
  nn.GGamma         <- Nindex$nn.GGamma
  group.index       <- Nindex$group.index
  .cc               <- class(object)

##   cat(".get_Zt_group\n");
##   print(group.index)
##   print(ii.group)
  
  zIndex.sub <-
    if (.cc %in% "mer") {
      Nindex$group.index[ii.group]+
        1+c(0:(nn.GGamma[ii.group]-1))*nn.groupFacLevels[ii.group] +
          rep(0:(nn.groupFacLevels[ii.group]-1),each=nn.GGamma[ii.group]) 
    } else {
      if (.cc %in% "lmerMod" ) {
        c((group.index[ii.group]+1) : group.index[ii.group+1])
      }
    }
  ZZ <- Zt[ zIndex.sub , ]
  return(ZZ)
}













