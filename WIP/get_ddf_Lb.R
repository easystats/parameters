#' @title Adjusted denomintor degress freedom for linear estimate for linear
#'     mixed model.
#' 
#' @description Get adjusted denomintor degress freedom for testing Lb=0 in a
#'     linear mixed model where L is a restriction matrix.
#'
#' @name get_ddf_Lb
#' 
#' @aliases get_Lb_ddf get_Lb_ddf.lmerMod Lb_ddf
#' 
#' @param object A linear mixed model object.
#' @param L A vector with the same length as \code{fixef(object)} or a matrix
#'     with the same number of columns as the length of \code{fixef(object)}
#' @param V0,Vadj Unadjusted and adjusted covariance matrix for the fixed
#'     effects parameters. Undjusted covariance matrix is obtained with
#'     \code{vcov()} and adjusted with \code{vcovAdj()}.
#' @return Adjusted degrees of freedom (adjusment made by a Kenward-Roger
#'     approximation).
#' 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{KRmodcomp}}, \code{\link{vcovAdj}},
#'     \code{\link{model2restrictionMatrix}},
#'     \code{\link{restrictionMatrix2model}}
#' @references Ulrich Halekoh, Søren Højsgaard (2014)., A Kenward-Roger
#'     Approximation and Parametric Bootstrap Methods for Tests in Linear Mixed
#'     Models - The R Package pbkrtest., Journal of Statistical Software,
#'     58(10), 1-30., \url{http://www.jstatsoft.org/v59/i09/}
#'
#' @keywords inference models
#' @examples
#' 
#' (fmLarge <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
#' ## removing Days
#' (fmSmall <- lmer(Reaction ~ 1 + (Days|Subject), sleepstudy))
#' anova(fmLarge,fmSmall)
#' 
#' KRmodcomp(fmLarge, fmSmall)  ## 17 denominator df's
#' get_Lb_ddf(fmLarge, c(0,1)) ## 17 denominator df's
#' 
#' # Notice: The restriction matrix L corresponding to the test above
#' # can be found with
#' L <- model2restrictionMatrix(fmLarge, fmSmall)
#' L
#' 

#' @rdname get_ddf_Lb
get_Lb_ddf <- function(object, L){
    UseMethod("get_Lb_ddf")
}

#' @rdname get_ddf_Lb
get_Lb_ddf.lmerMod <- function(object, L){
    Lb_ddf(L, vcov(object), vcovAdj(object))
}

#' @rdname get_ddf_Lb
Lb_ddf <- function(L, V0, Vadj) {
    if (!is.matrix(L))
        L = matrix(L, nrow = 1)
    Theta <- t(L) %*% solve(L %*% V0 %*% t(L), L)
    P <- attr(Vadj, "P")
    W <- attr(Vadj, "W")
    A1 <- A2 <- 0
    ThetaV0 <- Theta %*% V0
    n.ggamma <- length(P)
    for (ii in 1:n.ggamma) {
        for (jj in c(ii:n.ggamma)) {
            e <- ifelse(ii == jj, 1, 2)
            ui <- ThetaV0 %*% P[[ii]] %*% V0
            uj <- ThetaV0 %*% P[[jj]] %*% V0
            A1 <- A1 + e * W[ii, jj] * (.spur(ui) * .spur(uj))
            A2 <- A2 + e * W[ii, jj] * sum(ui * t(uj))
        }
    }
    q <- nrow(L)        # instead of finding rank
    B <- (1/(2 * q)) * (A1 + 6 * A2)
    g <- ((q + 1) * A1 - (q + 4) * A2)/((q + 2) * A2)
    c1 <- g/(3 * q + 2 * (1 - g))
    c2 <- (q - g)/(3 * q + 2 * (1 - g))
    c3 <- (q + 2 - g)/(3 * q + 2 * (1 - g))
    EE <- 1 + (A2/q)
    VV <- (2/q) * (1 + B)
    EEstar <- 1/(1 - A2/q)
    VVstar <- (2/q) * ((1 + c1 * B)/((1 - c2 * B)^2 * (1 - c3 * B)))
    V0 <- 1 + c1 * B
    V1 <- 1 - c2 * B
    V2 <- 1 - c3 * B
    V0 <- ifelse(abs(V0) < 1e-10, 0, V0)
    rho <- 1/q * (.divZero(1 - A2/q, V1))^2 * V0/V2
    df2 <- 4 + (q + 2)/(q * rho - 1)
    df2
}

#' @rdname get_ddf_Lb
#' @param Lcoef Linear contrast matrix
get_ddf_Lb <- function(object, Lcoef){
    UseMethod("get_ddf_Lb")
}

#' @rdname get_ddf_Lb
get_ddf_Lb.lmerMod <- function(object, Lcoef){
    ddf_Lb(vcovAdj(object), Lcoef, vcov(object))
}


#' @rdname get_ddf_Lb
#' @param VVa Adjusted covariance matrix
#' @param VV0 Unadjusted covariance matrix
ddf_Lb <- function(VVa, Lcoef, VV0=VVa){

    .spur = function(U){
        sum(diag(U))
    }
    .divZero = function(x,y,tol=1e-14){
        ## ratio x/y is set to 1 if both |x| and |y| are below tol
        x.y  =  if( abs(x)<tol & abs(y)<tol) {1} else x/y
        x.y
    }

    if (!is.matrix(Lcoef))
        Lcoef = matrix(Lcoef, ncol = 1)

    vlb = sum(Lcoef * (VV0 %*% Lcoef))
    Theta = Matrix(as.numeric(outer(Lcoef, Lcoef) / vlb), nrow=length(Lcoef))
    P = attr(VVa, "P")
    W = attr(VVa, "W")

    A1 = A2 = 0
    ThetaVV0 = Theta%*%VV0
    n.ggamma = length(P)
    for (ii in 1:n.ggamma) {
        for (jj in c(ii:n.ggamma)) {
            e = ifelse(ii==jj, 1, 2)
            ui = ThetaVV0 %*% P[[ii]] %*% VV0
            uj = ThetaVV0 %*% P[[jj]] %*% VV0
            A1 =  A1 +  e* W[ii,jj] * (.spur(ui) * .spur(uj))
            A2 =  A2 +  e* W[ii,jj] *  sum(ui * t(uj))
        }}

    ## substituted q = 1 in pbkrtest code and simplified
    B  =  (A1 + 6 * A2) / 2
    g  =  (2 * A1 - 5 * A2)  / (3 * A2)
    c1 =  g/(3 + 2 * (1 - g))
    c2 =  (1 - g) / (3 + 2 * (1 - g))
    c3 =  (3 - g) / (3 + 2 * (1 - g))
    EE =  1 + A2
    VV =  2 * (1 + B)
    EEstar  =  1/(1 - A2)
    VVstar  =  2 * ((1 + c1 * B)/((1 - c2 * B)^2  *  (1 - c3 * B)))
    V0 = 1 + c1 * B
    V1 = 1 - c2 * B
    V2 = 1 - c3 * B
    V0 = ifelse(abs(V0) < 1e-10, 0, V0)
    rho  = (.divZero(1 - A2, V1))^2 * V0/V2
    df2  =  4 + 3 / (rho - 1)
    ## cat(sprintf("Lcoef: %s\n", toString(Lcoef)))
    ## cat(sprintf("df2: %f\n", df2))
    df2
}

























## .get_ddf: Adapted from Russ Lenths 'lsmeans' package.
## Returns denom d.f. for testing lcoefs'beta = 0 where lcoefs is a vector
# VVA is result of call to VVA = vcovAdj(object, 0) in pbkrtest package
# VV is vcov(object) ## May not now be needed
# lcoefs is contrast of interest
# varlb is my already-computed value of lcoef' VV lcoef = est variance of lcoef'betahat

## .get_ddf <- function(VVa, VV0, Lcoef, varlb) {

##     ## print(VVa); print(VV0)
##     ##   ss<<-list(VVa=VVa, VV0=VV0)

##   .spur = function(U){
##       ##print(U)
##     sum(diag(U))
##   }
##   .divZero = function(x,y,tol=1e-14){
##     ## ratio x/y is set to 1 if both |x| and |y| are below tol
##     x.y  =  if( abs(x)<tol & abs(y)<tol) {1} else x/y
##     x.y
##   }

##   vlb = sum(Lcoef * (VV0 %*% Lcoef))
##   Theta = Matrix(as.numeric(outer(Lcoef, Lcoef) / vlb), nrow=length(Lcoef))
##   P = attr(VVa, "P")
##   W = attr(VVa, "W")

##   A1 = A2 = 0
##   ThetaVV0 = Theta%*%VV0
##   n.ggamma = length(P)
##   for (ii in 1:n.ggamma) {
##     for (jj in c(ii:n.ggamma)) {
##       e = ifelse(ii==jj, 1, 2)
##       ui = ThetaVV0 %*% P[[ii]] %*% VV0
##       uj = ThetaVV0 %*% P[[jj]] %*% VV0
##       A1 =  A1 +  e* W[ii,jj] * (.spur(ui) * .spur(uj))
##       A2 =  A2 +  e* W[ii,jj] *  sum(ui * t(uj))
##     }}

##   ## substituted q = 1 in pbkrtest code and simplified
##   B  =  (A1 + 6 * A2) / 2
##   g  =  (2 * A1 - 5 * A2)  / (3 * A2)
##   c1 =  g/(3 + 2 * (1 - g))
##   c2 =  (1 - g) / (3 + 2 * (1 - g))
##   c3 =  (3 - g) / (3 + 2 * (1 - g))
##   EE =  1 + A2
##   VV =  2 * (1 + B)
##   EEstar  =  1/(1 - A2)
##   VVstar  =  2 * ((1 + c1 * B)/((1 - c2 * B)^2  *  (1 - c3 * B)))
##   V0 = 1 + c1 * B
##   V1 = 1 - c2 * B
##   V2 = 1 - c3 * B
##   V0 = ifelse(abs(V0) < 1e-10, 0, V0)
##   rho  = (.divZero(1 - A2, V1))^2 * V0/V2
##   df2  =  4 + 3 / (rho - 1)
##   ## cat(sprintf("Lcoef: %s\n", toString(Lcoef)))
##   ## cat(sprintf("df2: %f\n", df2))

##   df2
## }
