prepCtmYear <- function (poll, year, stat, verbose=FALSE) {
  require(pesco)
  setwd(paste("/home/giovanni/R/projects/pesco-annuale/data/",
              year,"/",poll,"/",sep=""))
  pp <- gsub(tolower(poll), pattern="\\.", replacement="")
  if(verbose) cat("prepCtmYear: reading CTM NetCDF...\n")
  ctm <- read.ncdf.arpaer(paste("nin",year,"_",pp,".nc",sep=""),pollutant=pp)
  check <- apply(ctm$data, MARGIN=3, FUN=mean, na.rm=T)
  ctm$data[,,check==0] <- NA  ## NEI NOSTRI NETCDF I MANCANTI SONO STRANAMENTE NULLI
  if(verbose) cat("prepCtmYear: calculating CTM daily data...\n")
  ctm.daily <- dailyCtm(ctm,statistic=stat)
  return(ctm.daily)
}

prepObsYear <- function (poll, year, stat=NULL, verbose=FALSE) {
  require(pesco)
  if(verbose) cat("prepObsYear: reading observed data...")
  obs <- qaria2long(datafiles=dir(pattern="asc"),
                    anafile=paste("anagr_stzqa.",poll,
                                  ".",year,".dat",sep=""))
  if(!is.null(stat)) {
    if(verbose) cat("prepObsYear: calculating observed daily data...\n")
    obs <- dailyObs(data=obs, statistic=stat, pollutant=poll)
  }
  obs <- obs[!is.na(obs[,poll]),] ## remove missing observations
  return(obs)
}
