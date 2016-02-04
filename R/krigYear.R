krigYear <- function (poll, year, stat,
                      ctm.daily,
                      obs.daily,
                      emis.winter,emis.summer,
                      elevation,
                      start=paste(year,"-01-01",sep=""),
                      end=paste(year,"-12-31",sep=""),
                      lambda=lambda,
                      verbose=FALSE) {
  ## load package
  require("pesco")
  library(sp)
  library(fields)
  library(akima)
  
  ## sequence of days
  allDays <- format(seq.POSIXt(from=as.POSIXct(start),
                               to=as.POSIXct(end),
                               by="1 days"), format="%Y-%m-%d")
  first=TRUE; goodDays=NULL
  kk <- NULL; kvar <- NULL; coord <- NULL
  for(myDay in allDays) {
    ## prepare data for the day
    if(verbose) cat("krigYear: preparing daily data...\n")
    dataDay <- try(prepare.day(day = myDay,
                               obs.daily = obs.daily,
                               ctm.daily = ctm.daily,
                               pollutant=poll,
                               emis.winter = emis.winter,
                               emis.summer = emis.summer,
                               elev = elevation,
                               verbose = TRUE))
    
    ## kriging
    check <- class(dataDay)!="try-error"
    if(check) check <- check & 
      length(dataDay$obs.day[,poll])>3 &
      !is.na(dataDay$ctm.day$points$z[1])
    if(check) {
      K <- try(kriging (x.pnt = dataDay$ctm.day$points$x, 
                        y.pnt = dataDay$ctm.day$points$y, 
                        obs = dataDay$obs.day[,poll], 
                        model = dataDay$ctm.day,
                        proxy.1 = dataDay$emis.day, 
                        proxy.2 = dataDay$elev.day, 
                        lambda = lambda)) 
    }
          
    if(check) check <- check & class(K)!="try-error"
    if(check) {
      if(first) {
        coord <- list(x=K$x, y=K$y)
        first=FALSE
      } 
      goodDays <- c(goodDays, myDay)
      kk   <- rbind(kk, K$K)    
      kvar <- rbind(kvar, K$K.var)    
    } 
    if(verbose) cat(paste("krigYear: processed ",myDay,"\n",sep=""))
  }
  
  ## fill missing days
  KK  <- matrix(NA, ncol=ncol(kk), nrow=length(allDays))
  Kvar<- matrix(NA, ncol=ncol(kk), nrow=length(allDays))
  idx <- match(goodDays, allDays)
  for(i in 1:length(goodDays)) {
    KK[idx[i],]   <- kk[i,]
    Kvar[idx[i],] <- kvar[i,]    
  }
  
  rownames(KK)   <- allDays
  rownames(Kvar) <- allDays
  out <- list(KK=KK, Kvar=Kvar, coord=coord)
  return(out)
}


inval <- function(K, Kvar, abs.tol, rel.tol, eps) {
  uabs <- sqrt(Kvar)
  urel <- uabs/(K+eps) # aggiunto eps al denominatore per evitare invalidazioni eccessive con valori molto bassi
  valid <- (uabs<eps) | (uabs<abs.tol & urel<rel.tol)
  out <- K
  out[!valid] <- NA
  return(out)
}


