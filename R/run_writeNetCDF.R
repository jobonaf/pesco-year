source("R/write.R")
source("R/krigYear.R")
source("R/config.R")

## scelta
#library(tcltk)
yy <- select.list(2009:2014)
ff <- dir(path = "out/", pattern = paste("pesco",yy,"_",sep=""),
          full.names = T)
FF <- select.list(ff, multiple = T,
                  title = "select file to process")

## elaborazione
for (File in FF) {
  rm(Kout)
  load(File)
  cat(paste("processing",File),sep = "\n")
  Poll <- strsplit(basename(File), split = "_")[[1]][2]
  Poll <- gsub(x = Poll, pattern = ".rda", replacement = "")
  Poll <- gsub(x = Poll, pattern = "mean", replacement = "")
  Poll <- gsub(x = Poll, pattern = "max8h", replacement = "")
  #print(Poll)
  configInval(Poll)
  Kvalid <- inval(Kout$KK, Kout$Kvar,
                  abs.tol, rel.tol, eps)
  Time <- dimnames(Kout$KK)[[1]]
  Coords <- Kout$coord
  pesco2ncdf(Kvalid, Time, Coords, Poll)
}


