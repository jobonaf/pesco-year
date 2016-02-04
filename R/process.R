stat.n.save <- function(Kout, poll, stat, year, threshold=NULL, nrank=NULL) {
  out <- statYear(K=Kout$KK, Kvar=Kout$Kvar, poll=poll, stat=stat, threshold=threshold, nrank=nrank)
  out <- xyz2grid(x=Kout$coord$x, y=Kout$coord$y, z=out)
  write.asciigrid(x=out, fname=paste("/home/giovanni/R/projects/pesco-annuale/out/",
                                     poll,"_",stat,threshold,nrank,"_",year,
                                     ".asc",sep=""))
}

processYear <- function (poll, year, verbose=F) {
  source("R/config.R")
  configProcess(poll)
  
  ## prepara dati CTM
  if(verbose) cat(paste("processYear: ",poll,", preparing CTM data...","\n",sep=""))
  setwd(paste("/home/giovanni/R/projects/pesco-annuale/data/",
              year,"/",poll,"/",sep=""))
  ctm.daily <- prepCtmYear(poll, year, stat=daily.stat, verbose=verbose)
  save(ctm.daily, file=paste("nin",year,"_",poll,daily.stat,".rda",sep=""))
  
  ## prepara dati osservati
  if(verbose) cat(paste("processYear: ",poll,", preparing observed data...","\n",sep=""))
  setwd(paste("/home/giovanni/R/projects/pesco-annuale/data/",
              year,"/",poll,"/",sep=""))
  obs.daily <- prepObsYear(poll, year, stat=obs.daily.stat, verbose=verbose)
  save(obs.daily, file=paste("obs",year,"_",poll,daily.stat,".rda",sep=""))
  
  ## prepara proxy
  if(verbose) cat(paste("processYear: ",poll,", preparing proxies...","\n",sep=""))
  require("pesco")
  data(emissions)
  data(elevation)
  if(poll=="PM10" | poll=="PM2.5") {
    emis.winter <- emissions$PM10.winter
    emis.summer <- emissions$PM10.summer
  }
  if(poll=="O3" | poll=="NO2") {
    emis.winter <- emissions$NOx.winter
    emis.summer <- emissions$NOx.summer
  }
  
  ## lancia PESCO
  if(verbose) cat(paste("processYear: ",poll,", running PESCO...","\n",sep=""))
  setwd("/home/giovanni/R/projects/pesco-annuale/out/")
  timeKrig <- system.time(Kout <- krigYear(poll, year,
                                           ctm.daily,
                                           obs.daily,
                                           emis.winter=emis.winter,
                                           emis.summer=emis.summer,
                                           elevation=elevation,
                                           stat=daily.stat,
                                           lambda=lambda,
                                           verbose=verbose))
  
  ## salva l'output giornaliero
  save(Kout, file=paste("/home/giovanni/R/projects/pesco-annuale/out/",
                        "pesco",year,"_",poll,daily.stat,".rda",sep=""))

}

synthYear <- function (poll, year, verbose=F) {
  configProcess(poll)
  load(file=paste("/home/giovanni/R/projects/pesco-annuale/out/",
                  "pesco",year,"_",poll,daily.stat,".rda",sep=""))

  ## fa le statistiche annuali
  if(verbose) cat(paste("synthYear: ",poll,", post-processing PESCO...","\n",sep=""))
  if(calc.mean)          stat.n.save(Kout, poll, stat="mean", year)
  if(!is.null(threshold))stat.n.save(Kout, poll, stat="nexc", year, threshold=threshold)
  if(!is.null(nrank))    stat.n.save(Kout, poll, stat="rank", year, nrank=nrank)
  if(verbose) cat(paste("synthYear: ",poll,", done.\n",sep=""))
}
