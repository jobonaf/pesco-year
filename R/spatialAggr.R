mapComuni <- function(fileIndic) {
  library(maptools) # used here to read shapefiles
  comuni <- readShapePoly("/home/giovanni//util/geodata/com2011_g")
  proj4string(obj=comuni) <- CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0")
  comER  <- comuni[comuni$COD_REG==8,]
  comER@data <- droplevels(comER@data)
  comER@data$NOME_TED <- NULL
  Encoding(x=levels(comER@data$NOME_COM))<-"latin1"
  
  Indic <- readAsciiGrid(fileIndic)
  proj4string(obj=Indic) <- CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0")
  
  Median <- aggregate(x=Indic, by=comER, FUN=quantile, probs=0.5, na.rm=T)
  P90    <- aggregate(x=Indic, by=comER, FUN=quantile, probs=0.9, na.rm=T)
  Mean   <- aggregate(x=Indic, by=comER, FUN=mean, na.rm=T)
  
  pr <- function(code) {
    paste("PC"[code==33],
          "PR"[code==34],
          "RE"[code==35],
          "MO"[code==36],
          "BO"[code==37],
          "FE"[code==38],
          "RA"[code==39],
          "FC"[code==40],
          "RN"[code==99],
          sep="")
  }
  prov <- function(code) sapply(X=code, FUN=pr)
  library(fields)
  
  #spplot(P90,col.regions=tim.colors(100))
  
  source("R/popExposure.R")
  file.pop <- "~/util/geodata/pop_1km.txt.2010"
  file.alt <- "~/util/geodata/altit_1km.dat.2010"
  alt <- read.csv(file.alt, header=F, sep=" ")
  pop <- read.csv(file.pop)
  source("R/xyz2grid.R")
  xyz2grid(x=pop$x, y=pop$y, z=pop$pop)->Pop
  xyz2grid(x=alt[,1]*1000, y=alt[,2]*1000, z=alt[,3])->Alt
  Pop@data[[1]][Alt@data[[1]] < -100] <- NA
  proj4string(obj=Pop) <- CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0")
  
  Descr <- fname2description(fileIndic)
  Thr <- Descr$Threshold
  
  Ter <- Pop
  CellArea <- Indic@grid@cellsize[1]*Indic@grid@cellsize[2]*0.001*0.001
  Ter@data[[1]] <- (Pop@data[[1]]+1)/(Pop@data[[1]]+1)*CellArea
  
  Pop@data[[2]] <- Pop@data[[1]]*(Indic@data[[1]]>Thr)
  Pop@data[[3]] <- Pop@data[[1]]*(is.na(Indic@data[[1]]))
  POP   <- aggregate(x=Pop, by=comER, FUN=sum, na.rm=T)
  POP@data[[4]] <- POP@data[[2]]*100/POP@data[[1]]
  names(POP@data) <- c("totale","esposta","non.class","perc.esposta")
  
  Ter@data[[2]] <- Ter@data[[1]]*(Indic@data[[1]]>Thr)
  Ter@data[[3]] <- Ter@data[[1]]*(is.na(Indic@data[[1]]))
  TER   <- aggregate(x=Ter, by=comER, FUN=sum, na.rm=T)
  TER@data[[4]] <- TER@data[[2]]*100/TER@data[[1]]
  names(TER@data) <- c("totale","esposto","non.class","perc.esposto")
  
  ### media pesata con popolazione
  library(Hmisc)
  cc <- over(Pop,comER)$NOME_COM
  WMean <- Mean ; WMean@data[[1]] <- NA
  for (i in 1:length(comER@data$NOME_COM)) {
    idx <- which(as.character(comER@data$NOME_COM[i])==as.character(cc))
    WMean@data[[1]][i] <- wtd.mean(Indic@data[[1]][idx], 
                                   weights=Pop@data[[1]][idx],
                                   normwt=TRUE, na.rm=T)
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
  
  filepdf <- paste("mapPopEsposta_",gsub(basename(fileIndic),
                                         pattern=".asc",
                                         replacement=".pdf"),sep="")
  pdf(file=filepdf,width=8,height=5)
  
  
  par(mar=c(0.5,0.5,4,0.5))
  var <- POP@data$esposta
  #bb <- pretty(c(0,var),n=100)
  bb <- unique(signif(round(c(0,2^pretty(log2(POP@data$totale),n=100)),-2),2))
  nn <- length(bb)
  cc <- tim.colors(nn-1)
  ll <- sprintf(bb,fmt="%i"); ll[which(((1:nn)%%10)!=1)] <- ""
  plot(comER, col=cc[cut(var,breaks=bb)],
       border="grey", xlim=c(xmin,xmax), ylim=c(ymin,ymax),setParUsrBB=T)
  mtext(paste(Descr$Poll,"di fondo"), side=3, line=3, font = 2, adj=0)
  mtext(paste("anno",Descr$Year), side=3, line=2, font = 1, adj=0)
  mtext(Descr$Stat, side=3, line=1, font = 1, adj=0)
  mtext(bquote("popolazione esposta (oltre" ~
                 .(Descr$Threshold) ~ .(Descr$Unit)*")"), side=3, line=0, font = 2, adj=0)
  
  plot(provER,border="grey30",add=T,lwd=1)
  plot(provNER,border="white",col="white",add=T,lwd=1)
  plot(ocean,border="lightskyblue",col="lightskyblue1",add=T,lwd=2,
       xlim=c(xmin,xmax), ylim=c(ymin,ymax))
  plot(regER,border="grey30",add=T,lwd=2)
  rect(xmin,ymin,xmax,ymax)
  
  ## legend
  library(plotrix)
  dx <- xmax-xmin
  dy <- ymax-ymin
  color.legend(xl=xmin+0.05*dx,yb=ymin+0.05*dy,
               xr=xmin+0.1*dx,yt=ymin+0.4*dy,
               legend=ll,
               rect.col=c("white",cc),
               gradient="y",align="rb")
  dev.off()
  
  Tab <- data.frame(Comune=comER$NOME_COM,
                    Provincia=prov(comER$COD_PRO),
                    MediaPesataSulComune=round(WMean@data[[1]]),
                    Perc90SulComune=round(P90@data[[1]]),
                    MediaSulComune=round(Mean@data[[1]]),
                    MedianaSulComune=round(Median@data[[1]]),
                    PopTot=POP@data$totale,
                    PopEsposta=POP@data$esposta,
                    PopNonClass=POP@data$non.class,
                    PercPopEsposta=round(POP@data$esposta*100/POP@data$totale),
                    TerTot=TER@data$totale,
                    TerEsposto=TER@data$esposto,
                    TerNonClass=TER@data$non.class,
                    PercTerEsposto=round(TER@data$esposto*100/TER@data$totale))
  filecsv <- paste("Comuni_",gsub(basename(fileIndic),
                                  pattern=".asc",
                                  replacement=".csv"),sep="")
  write.table(x=Tab,row.names=FALSE,file=filecsv,quote=TRUE,sep=",")
}
