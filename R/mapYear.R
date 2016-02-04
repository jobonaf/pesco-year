SpatialGridDataFrame.UTMkm2m <- function(X) {
  grid.in <- getGridTopology(X)
  grid.out <- GridTopology(cellcentre.offset=grid.in@cellcentre.offset*1000,cellsize=grid.in@cellsize*1000,cells.dim=grid.in@cells.dim)
  Y <- SpatialGridDataFrame(grid=grid.out,data=X@data)
  return(Y)
}

mapIndic <- function (poll, stat, year, km2m=FALSE) {
  ## parameters
  if(poll=="PM10")  polname <- "PM10 di fondo"
  if(poll=="PM2.5") polname <- "PM2.5 di fondo"
  if(poll=="NO2")   polname <- expression(NO[2]~" di fondo")
  if(poll=="O3")    polname <- "ozono di fondo"
  if(stat=="mean")  statname <- expression("media annua"~(mu*g/m^3))
  threshold <- NULL
  nrank <- NULL
  if(poll=="PM10" & stat=="mean") {      
    units <- expression(mu*g/m^3)
    levs <- c(10,20,30,40)
  }
  if(poll=="PM10" & stat=="nexc") {
    threshold <- 50
    units <- "giorni"
    statname <- expression("numero di giorni in cui la media giornaliera supera i 50"~mu*g/m^3)
    levs <- c(10,20,35,50)
  }
  if(poll=="PM10" & stat=="rank") {
    nrank <- 36
    units <- expression(mu*g/m^3)
    statname <- expression("media giornaliera: 36esimo valore massimo dell'anno"~(mu*g/m^3))
    levs <- c(15,30,50,75)
  }
  if(poll=="PM2.5" & stat=="mean") {
    units <- expression(mu*g/m^3)
    levs <- c(10,15,20,25)
  }
  if(poll=="NO2" & stat=="mean") {
    units <- expression(mu*g/m^3)
    levs <- c(10,20,30,40)
  }
  if(poll=="O3" & stat=="nexc") {
    threshold <- 120
    units <- "giorni"
    statname <- expression("numero di giorni in cui il massimo giornaliero della media mobile su 8 ore supera i 120"~mu*g/m^3)
    levs <- c(10,25,50,75)
  }
  if(poll=="O3" & stat=="rank") {
    nrank <- 26
    units <- expression(mu*g/m^3)
    statname <- expression("massimo giornaliero della media mobile su 8h: 26esimo valore massimo dell'anno"~(mu*g/m^3))
    levs <- c(60,90,120,150)
  }
  
  ## read data
  library(sp)
  library(maptools) # used here to read shapefiles
  fname <- paste("/home/giovanni/R/projects/pesco-annuale/out/",
                 poll,"_",stat,
                 threshold[stat=="nexc"],
                 nrank[stat=="rank"],
                 "_",year,
                 ".asc",sep="")
  indic <- readAsciiGrid(fname)
  if(km2m) {
    Indic <- SpatialGridDataFrame.UTMkm2m(indic)
  } else {
    Indic <- indic
  }
  
  ## read shapefile
  ocean    <- readShapePoly("~/util/geodata/ocean_europe_UTM32")
  regioni  <- readShapePoly("~/util/geodata/reg2011_g")
  province <- readShapePoly("~/util/geodata/prov2011_g")
  proj4string(obj=regioni)  <- CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0")
  proj4string(obj=province) <- CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0")
  proj4string(obj=ocean)    <- CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0")
  regER   <- regioni  [regioni$COD_REG==8,]
  provER  <- province [province$COD_REG==8,]
  provNER <- province [province$COD_REG!=8,]
  
  ## bounding box
  BB <- regER@bbox
  dx <- BB[1,2]-BB[1,1]
  dy <- BB[2,2]-BB[2,1]
  xmin <- BB[1,1]-0.1*dx
  xmax <- BB[1,2]+0.1*dx
  ymin <- BB[2,1]-0.1*dy
  ymax <- BB[2,2]+0.1*dy
  
  ## plot
  outfile <- gsub(basename(fname),
                  pattern="asc",
                  replacement="pdf")
  pdf(file=outfile,width=8,height=5)
  par(mar=c(0.5,0.5,4,0.5))
  library(fields)
  nl <- length(levs)+1
  cols <- colorRampPalette(c("steelblue","olivedrab","yellow","orange","orangered3"))(nl)
  
  image(Indic,breaks=c(0,levs,1000),col=cols,
        setParUsrBB=T,xlim=c(xmin,xmax),ylim=c(ymin,ymax),
        tick=0)
  contour(Indic,levels=levs,add=T,lwd=3, col=cols, labels="")
  plot(provER,border="grey30",add=T,lwd=1)
  plot(provNER,border="white",col="white",add=T,lwd=1)
  #plot(regioni,border="grey",add=T,lwd=2)
  plot(ocean,border="lightskyblue",col="lightskyblue1",add=T,lwd=2)
  #plot(ocean,border="transparent",col="white",add=T,lwd=2)
  plot(regER,border="grey",add=T,lwd=2)
  source("~/R/projects/pesco-annuale/R/draw.marks.R")
  draw.marks(file="~/util/geodata/marks_emrutm2.dat",
             xyz=c(2,1,3),xy.fact=1000,cex=0.8)
  rect(xmin,ymin,xmax,ymax)
  
  legend("bottomleft",inset=0.03,fill=rev(c("white",cols)),
         y.intersp=0.8,cex=0.9,bg="white",
         title=units,
         legend=rev(c("n.d.",
                      paste("<",levs[1]),
                      paste(levs[-length(levs)],
                            levs[-1],sep="-"),
                      paste(">",levs[length(levs)])))
        )
  
  mtext(side=3,line=3,text=polname,font=2,adj=0,padj=1,cex=1)
  mtext(side=3,line=2,text=statname,font=3,adj=0,padj=1,cex=0.8)
  mtext(side=3,line=1,text=paste("anno:", year),font=1,adj=0,padj=1,cex=0.8)
  dev.off()

}


mapYear <- function (poll, year, verbose) {
  ## parameters
  if(poll=="PM10")  stats <- c("mean","nexc","rank")
  if(poll=="PM2.5") stats <- c("mean")
  if(poll=="NO2")   stats <- c("mean")
  if(poll=="O3")    stats <- c("nexc","rank")
 
  for(stat in stats) {
    mapIndic(poll=poll, stat=stat, year=year)
    if(verbose) cat(paste("mapYear: ",poll,", mapping ",stat,"...\n",sep=""))
  }
}