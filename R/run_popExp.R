source("R/popExposure.R")
source("R/spatialAggr.R")
library(maptools)
Files <- dir(path="out/", pattern=".asc", full.names=TRUE)

for(File in Files) {
  dd <- readAsciiGrid(File)
  freqExposure(Indic=dd, Pop=Pop)->expo
  desc <- fname2description(File)
  if(!is.null(desc$Threshold)) {
    TE <- thresholdExposure(Indic=dd, Pop=Pop, 
                            threshold=desc$Threshold)
    #print(paste(File,paste(paste(names(TE),lapply(TE,FUN=signif,digits=2),sep="="),collapse=", ")))
    tE <- TE$percPop.above
  } else {
    tE <- NULL
  }
  fileout <- paste("freqExposure_",gsub(basename(File),pattern=".asc",replacement=".pdf"),sep="")
  pdf(fileout,width=6,height=5)
  plotFreqExposure(expo, Year=desc$Year, Stat=desc$Stat, Poll=desc$Poll, Unit=desc$Unit, Threshold=desc$Threshold, expo.Thr=tE)
  dev.off()

  mapComuni(File)  
}


