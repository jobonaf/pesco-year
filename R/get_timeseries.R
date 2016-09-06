get_ts <- function(File,Lat,Lon,fileout=NULL) {
  source("/home/giovanni/R/projects/pesco-annuale/R/config.R")
  source("/home/giovanni/R/projects/pesco-annuale/R/krigYear.R")
  library(pesco)
  load(File)
  cat(paste("processing",File),sep = "\n")
  xPoll <- strsplit(basename(File), split = "_")[[1]][2]
  xPoll <- gsub(x = xPoll, pattern = ".rda", replacement = "")
  Poll <- gsub(x = xPoll, pattern = "mean", replacement = "")
  Poll <- gsub(x = Poll, pattern = "max8h", replacement = "")
  Stat <- rawToChar(grepRaw(Poll,xPoll,value = T,invert = T))
  configInval(Poll)
  Kvalid <- inval(Kout$KK, Kout$Kvar,
                  abs.tol, rel.tol, eps)
  Time <- dimnames(Kout$KK)[[1]]
  coo <- ll2utm(rlat = Lat, rlon = Lon, iz = 32)
  dist <- sqrt((coo$x-Kout$coord$x)^2 + (coo$y-Kout$coord$y)^2)
  idx <- which.min(dist)
  cat(paste0("Selected cell ",idx,". Distance: ",round(dist[idx])," meters"),sep="\n")
  
  if(is.null(fileout)) {
    fileout <- paste("pescoDaily_",Poll,Stat,"_",
                     paste(format(as.POSIXct(Time[c(1,length(Time))]),
                                  format="%Y%m%d"),
                           collapse="-"),"_",
                     sprintf(Kout$coord$x[idx],fmt="%iX"),"_",
                     sprintf(Kout$coord$y[idx],fmt="%iY"),".csv",sep="")
  }
  out <- data.frame(Time,round(Kvalid[ ,idx]))
  colnames(out)[2] <- paste(Poll,Stat,sep="_")
  if(!is.na(fileout)) write.csv(out,file=fileout,row.names = F,quote = F)
  return(out)
}
