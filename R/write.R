pesco2ncdf <- function(Kvalid, Time, Coords, Poll,
                       cellcentre.offset=c(510500,4843500),
                       cellsize=c(1000,1000),
                       cells.dim=c(297,161)) {
  ## define destination grid
  library(sp)
  grid <- GridTopology(cellcentre.offset, cellsize, cells.dim)
  Coo <- coordinates(grid)
  XX <- sort(unique(Coo[,1]))
  YY <- sort(unique(Coo[,2]))
  
  ## put data in the destination grid
  library(data.table)
  kk <- data.frame(x=rep(Kout$coord$x, each=nrow(Kvalid)),  
                   y=rep(Kout$coord$y, each=nrow(Kvalid)), 
                   value=as.vector(Kvalid),
                   time=Time)
  Kdt <- as.data.table(kk)
  setkey(x=Kdt, x, y, time)
  KK <- Kdt[CJ(XX, YY, Time)]
  
  ## arrange data into a 3D array
  nx <- length(XX)
  ny <- length(YY)
  nt <- length(Time)
  K3d <- array(KK$value, dim=c(nt,ny,nx))
  K3d <- aperm(K3d, perm=c(3,2,1))
  
  ## write NetCDF
  library(ncdf)
  xx <- dim.def.ncdf(name="west_east", units="meters", vals=XX)
  yy <- dim.def.ncdf(name="south_north", units="meters", vals=YY)
  zz <- dim.def.ncdf(name="bottom_top", units="meters", vals=1)
  dd <- as.Date(Time)-as.Date("1900-01-01")
  tt <- dim.def.ncdf(name="Time", 
                     units="days since 1900-01-01", 
                     vals=as.integer(dd))
  var <- var.def.ncdf(name=Poll, units="micrograms per cubic meter", dim=list(xx,yy,zz,tt), missval=-999., prec="single")
  TT <- format(as.Date(Time), format="%Y-%m-%d_%H:%M:%S")
  dimnchar <- dim.def.ncdf(name = "DateStrLen", units = "", vals = 1:max(nchar(TT)) )
  vartime <- var.def.ncdf(name="Times", units="", dim=list(dimnchar,tt), missval=NA, prec="char")
  fileout <- paste("pescoDaily_",Poll,"_",
                   paste(format(as.POSIXct(Time[c(1,length(Time))]),
                                format="%Y%m%d"),
                         collapse="-"),".nc",sep="")
  nc <- create.ncdf(fileout, vars=list(var,vartime))
  put.var.ncdf(nc, varid=Poll, vals=K3d)
  put.var.ncdf(nc, varid="Times", vals=TT)
  close.ncdf(nc)
}
