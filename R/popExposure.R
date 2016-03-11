file.pop <- "~/util/geodata/pop_1km.txt.2010"
file.alt <- "~/util/geodata/altit_1km.dat.2010"
alt <- read.csv(file.alt, header=F, sep=" ")
pop <- read.csv(file.pop)
source("R/xyz2grid.R")
xyz2grid(x=pop$x, y=pop$y, z=pop$pop)->Pop
xyz2grid(x=alt[,1]*1000, y=alt[,2]*1000, z=alt[,3])->Alt
Pop@data[[1]][Alt@data[[1]] < -100] <- NA

freqExposure <- function(Indic, Pop, na.correct=TRUE) {
  library(Hmisc)
  pp <- (0:1000)/1000
  expos.pop <- wtd.quantile(x=Indic@data[[1]], 
                            weights=Pop@data[[1]], 
                            probs=pp, 
                            na.rm=T)
  expos.ter <- quantile(x=Indic@data[[1]], 
                        probs=pp, 
                        na.rm=T)
  pop <- Pop@data[[1]]
  indic <- Indic@data[[1]]
  na.indic <- which(is.na(indic))
  na.pop <- sum(pop[na.indic], na.rm=TRUE)/sum(pop, na.rm=TRUE)
  na.ter <- length(pop[which(is.na(indic) & !is.na(pop))])/length(pop[which(!is.na(pop))])
  qu.pop <- paste(as.numeric(gsub(names(expos.pop),
                                  pattern="%",
                                  replacement=""))*(1-na.pop),"%")
  qu.ter <- paste(as.numeric(gsub(names(expos.ter),
                                  pattern="%",
                                  replacement=""))*(1-na.ter),"%")
  if(na.correct){
    names(expos.pop) <- qu.pop
    names(expos.ter) <- qu.ter
  }
  out <- list(expos.pop=expos.pop, expos.ter=expos.ter, 
              naPerc.pop=na.pop*100, naPerc.ter=na.ter*100,
              na.pop=na.pop, na.ter=na.ter)
  return(out)
}

thresholdExposure <- function(Indic, Pop, threshold, resol=1) {
  areaCell <- resol^2
  pop   <- Pop@data[[1]]
  indic <- Indic@data[[1]]
  above <- which(indic>threshold  & !is.na(pop))
  below <- which(indic<=threshold & !is.na(pop))
  isna  <- which(is.na(indic)     & !is.na(pop))
  pop.above <- sum(pop[above])
  pop.below <- sum(pop[below])
  pop.isna  <- sum(pop[isna])
  ter.above <- length(pop[above])*areaCell
  ter.below <- length(pop[below])*areaCell
  ter.isna  <- length(pop[isna])*areaCell
  pop.tot <- sum(pop, na.rm=T)
  ter.tot <- length(which(!is.na(pop)))
  percPop.above <- pop.above/pop.tot*100
  percPop.below <- pop.below/pop.tot*100
  percPop.isna  <- pop.isna/pop.tot*100
  percTer.above <- ter.above/ter.tot*100
  percTer.below <- ter.below/ter.tot*100
  percTer.isna  <- ter.isna/ter.tot*100
  out <- list(pop.above=pop.above,
              pop.below=pop.below,
              pop.isna=pop.isna,
              ter.above=ter.above,
              ter.below=ter.below,
              ter.isna=ter.isna,
              percPop.above=percPop.above,
              percPop.below=percPop.below,
              percPop.isna=percPop.isna,
              percTer.above=percTer.above,
              percTer.below=percTer.below,
              percTer.isna=percTer.isna)
}

plotFreqExposure <- function(expo, Year, Stat, Poll, Unit, Threshold=NULL, expo.Thr=NULL) {
  oldpar <- par(no.readonly=TRUE)
  par(mar=c(5,5,3,0.5))
  plot(rev(as.numeric(gsub(names(expo$expos.pop),
                           pattern="%",replacement=""))),
       expo$expos.pop, type="l", 
       xlab="%", xlim=c(0,100),
       ylim=c(min(c(expo$expos.pop, Threshold*0.8)),max(c(expo$expos.pop, Threshold*1.2))),
       ylab=Unit, las=1, lwd=2,
       xaxt="n")
  lines(rev(as.numeric(gsub(names(expo$expos.ter),
                            pattern="%",replacement=""))), 
        expo$expos.ter, lty=2, lwd=2)
  axis(1, at=seq(0,100,length.out=5),
       labels=seq(0,100,length.out=5))
  legend("topright", bty="n",
         legend=c("popolazione esposta","territorio esposto"), lwd=2, lty=c(1,2))
  mtext(text=Poll, side=3, line=2, adj=0, font=2)
  mtext(text=paste("anno",Year), side=3, line=1, adj=0, font=1)
  mtext(text=Stat, side=3, line=0, adj=0, font=1)
  if(!is.null(Threshold) & !is.null(expo.Thr)) {
    abline(h=Threshold, lty=3)
    abline(v=expo.Thr, lty=3)
    text(x=expo.Thr, y=Threshold, adj=c(0,0),
         labels=paste(signif(expo.Thr,2),"%",sep=""))    
  }
  par(oldpar)
}

fname2description <- function(fname, unit=bquote(mu*g/m^3)) {
  fname <- basename(fname)
  sss <- strsplit(fname,split="_")[[1]]
  poll <- sss[1]
  stat <- sss[2]
  year <- as.numeric(gsub(sss[3],pattern="\\.asc",replacement=""))
  if(substr(stat,1,4)=="mean") {
    Stat <- "media annua"
    Unit <- unit
  }
  if(substr(stat,1,4)=="nexc") {
    Stat <- bquote("superamenti giornalieri dei" ~ .(substr(stat,5,nchar(stat))) ~ .(unit))
    Unit <- "giorni"
  }
  if(substr(stat,1,4)=="rank") {
    Stat <- paste(substr(stat,5,nchar(stat)),"esimo valore giornaliero massimo",sep="")
    Unit <- unit
  }
  Poll <- poll
  if(poll=="NO2") Poll <- "biossido di azoto"
  if(poll=="O3") Poll <- "ozono"
  Threshold <- NULL
  if(poll=="O3" & substr(stat,1,4)=="nexc") Threshold <- 25
  if(poll=="O3" & substr(stat,1,4)=="rank") Threshold <- 120
  if(poll=="PM10" & substr(stat,1,4)=="mean") Threshold <- 40
  if(poll=="PM10" & substr(stat,1,4)=="rank") Threshold <- 50
  if(poll=="PM10" & substr(stat,1,4)=="nexc") Threshold <- 35
  if(poll=="PM2.5" & substr(stat,1,4)=="mean") Threshold <- 25
  if(poll=="NO2" & substr(stat,1,4)=="mean") Threshold <- 40
  description <- list(Poll=Poll, Stat=Stat, Year=year, Unit=Unit, Threshold=Threshold)
  return(description)
}

## tabella dell'esposizione con soglie multiple
tabExposure <- function(Indic, thresholds, Pop, 
                        Year=fname2description(names(Indic@data)[1])$Year){
  pop   <- Pop@data[[1]]
  indic <- Indic@data[[1]]
  idx <- cut(indic,c(-Inf,thresholds,Inf))
  tab <- tapply(X=pop, INDEX=idx, FUN=sum, na.rm=T)
  tab[is.na(tab)] <- 0
  ND <- sum(pop[is.na(indic)], na.rm=T)
  tab <- c(ND,tab)
  names(tab)[1] <- "ND"
  tab <- round(tab/sum(tab)*100,1)
  tab <- as.data.frame(tab)
  colnames(tab) <- Year
  return(tab)
}

barplotExposure <- function(Files, thresholds, Pop) {
  for (File in Files) {
    Indic <- readAsciiGrid(File)
    Tab <- tabExposure(Indic, thresholds, Pop)
    if(File==Files[1]) {TAB <- Tab} else {TAB <- cbind(TAB,Tab)}
  }
  ny <- ncol(TAB)
  filepdf <- paste("barplotExp_",
                   paste(strsplit(names(Indic@data)[1], 
                                  split = "_")[[1]][1:2],
                         collapse="_"),"_",
                   paste(range(as.numeric(colnames(TAB))),
                         collapse="-"),
                   ".pdf",sep="")
  pdf(filepdf,width=8,height=5)
  par(mar=c(5,5,3,0.5))
  cols <- c("grey","white","darkgreen","green","orange","red")
  barplot(as.matrix(TAB),col=cols,xlab="anno",ylab="% di popolazione",
          xlim=c(0,ny*1.4))
  ll <- c("n.d.",paste("<",thresholds[1]),
          paste(thresholds,"-",thresholds[-1])[-4],
          paste(">",thresholds[4]))
  Descr <- fname2description(names(Indic@data)[1])
  if(class(Descr$Unit)=="character") {
    leg.title <- Descr$Unit
  } else {
    leg.title <- as.expression(Descr$Unit)
  }
  legend("topright",
         rev(ll),
         fill=rev(cols),bty="n",
         title=leg.title)
  mtext(paste(Descr$Poll,"di fondo"), side=3, line=2, font = 2, adj=0)
  mtext(bquote("popolazione esposta a" ~ .(Descr$Stat) ), side=3, line=1, font = 2, adj=0)
  dev.off()
  
}

## esposizione con soglia singola
getExposure <- function(Indic, threshold, Pop, necess=0.75){
  pop   <- Pop@data[[1]]
  indic <- Indic@data[[1]]
  iOver <- which(indic>threshold)
  iNa   <- which(is.na(indic))
  pOver <- sum(pop[iOver], na.rm=T)
  pNa   <- sum(pop[iNa  ], na.rm=T)
  pTot  <- sum(pop, na.rm=T)
  pOver <- ifelse(pNa/pTot>(1-necess),
                  NA,
                  pOver/pTot)
  return(pOver)
}


getExposure.years <- function(Files, threshold, Pop, ...) {
  Exp <- Year <- NULL
  for (File in Files) {
    Indic <- readAsciiGrid(File)
    Exp <- c(Exp,getExposure(Indic, threshold=threshold, Pop=Pop, ...))
    Year <- c(Year,fname2description(names(Indic@data)[1])$Year)
  }
  names(Exp) <- Year
  return(Exp)
}
