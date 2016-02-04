maxN <- function(x, N){
  len <- length(x)
  if(N>len){
    warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  sort(x,decreasing=T, na.last=T)[N]
}

statYear <- function(K, Kvar,
                     poll, stat, 
                     abs.tol=NULL, rel.tol=NULL, eps=NULL,
                     threshold=NULL, nrank=NULL,
                     necess=0.75) {
  
  ## default values for invalidation
  if(poll=="PM10" & stat=="mean") {
    if(is.null(eps))     eps=10
    if(is.null(abs.tol)) abs.tol=20
    if(is.null(rel.tol)) rel.tol=0.5
  }
  if(poll=="PM10" & stat=="nexc") {
    if(is.null(eps))       eps=10
    if(is.null(abs.tol))   abs.tol=20
    if(is.null(rel.tol))   rel.tol=0.5
    if(is.null(threshold)) threshold=50
  }
  if(poll=="PM10" & stat=="rank") {
    if(is.null(eps))       eps=10
    if(is.null(abs.tol))   abs.tol=20
    if(is.null(rel.tol))   rel.tol=0.5
    if(is.null(nrank))     nrank=36
  }
  if(poll=="PM2.5" & stat=="mean") {
    if(is.null(eps))     eps=10
    if(is.null(abs.tol)) abs.tol=20
    if(is.null(rel.tol)) rel.tol=0.5
  }
  if(poll=="NO2" & stat=="mean") {
    if(is.null(eps))     eps=10
    if(is.null(abs.tol)) abs.tol=30
    if(is.null(rel.tol)) rel.tol=0.5
  }
  if(poll=="O3" & stat=="nexc") {
    if(is.null(eps))       eps=10
    if(is.null(abs.tol))   abs.tol=30
    if(is.null(rel.tol))   rel.tol=0.5
    if(is.null(threshold)) threshold=120
  }
  if(poll=="O3" & stat=="rank") {
    if(is.null(eps))       eps=10
    if(is.null(abs.tol))   abs.tol=30
    if(is.null(rel.tol))   rel.tol=0.5
    if(is.null(nrank))     nrank=26
  }
  
  ## invalidation
  Kvalid <- inval(K, Kvar,
                  abs.tol, rel.tol, eps)
  Nvalid <- colSums(!is.na(Kvalid))
  Ndays <- nrow(Kvalid)
  if(stat=="nexc") {
    #out <- round(colSums(Kvalid>threshold, na.rm=T)*Ndays/Nvalid)
    out <- round(colSums(Kvalid>threshold, na.rm=T))
  }
  if(stat=="mean") {
    out <- colMeans(Kvalid, na.rm=T)
  }
  if(stat=="rank") {
    out <- apply(X=Kvalid, MARGIN=2, FUN=maxN, N=nrank)
  }
  out[Nvalid<(Ndays*necess)] <- NA
  
  return(out)
}

