## genera il plot 'stile Agenzia Europea' della popolazione esposta

source("R/popExposure.R")
vars <- c("NO2_mean","O3_nexc","PM10_nexc","PM2.5_mean")
Thr <- c(40,25,35,25)
nv <- length(vars)
library(maptools)
file.pop <- "~/util/geodata/pop_1km.txt.2010"
file.alt <- "~/util/geodata/altit_1km.dat.2010"
alt <- read.csv(file.alt, header=F, sep=" ")
pop <- read.csv(file.pop)
source("R/xyz2grid.R")
xyz2grid(x=pop$x, y=pop$y, z=pop$pop)->Pop
xyz2grid(x=alt[,1]*1000, y=alt[,2]*1000, z=alt[,3])->Alt
Pop@data[[1]][Alt@data[[1]] < -100] <- NA
proj4string(obj=Pop) <- CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towg
                            s84=0,0,0")
EXP <- Descr <- NULL
for (i in 1:nv) {
  system(paste("ls out/",vars[i],"*asc",sep=""),
         intern = T)->Files
  EXP <- cbind(EXP,getExposure.years(Files, threshold = Thr[i], Pop))
  descr <- fname2description(Files[1])
  Descr <- c(Descr,
             as.expression(bquote(.(descr$Stat) ~ "di" ~ .(descr$Poll) ~ "oltre" ~ .(descr$Threshold) ~ .(descr$Unit))))
}
colnames(EXP) <- vars
EXP <- signif(EXP*100,2)
cols <- RColorBrewer::brewer.pal(ncol(EXP),"Set1")
oldpar <- par()
filepdf <- "timeseriesExposure.pdf"
pdf(filepdf,width=8,height=5)
par(mar=c(3,5,7,1))
for (i in 1:ncol(EXP)) {
  if(i==1) {
    plot(EXP[,i], col=cols[i], pch=19,
         ylim=c(0,100), xaxt="n", yaxt="n", frame=FALSE,
         xlab="", ylab="popolazione esposta",
         xlim=c(1,nrow(EXP)))
  } else {
    points(EXP[,i], pch=19, col=cols[i])
  }
  lines(EXP[,i], lty=1, lwd=3, col=cols[i])
}
axis(1, at=ax<-1:nrow(EXP), labels = rownames(EXP), tick = F)
axis(2, at=ay<-(0:5)*20, labels = paste0(ay,"%"), tick = F, lty=0, las=2)
abline(h = ay, v = ax, col="lightgrey", lty="dotted")
legend(x = 1, y = 150, xpd = T, bty = "n", ncol = 1, legend = Descr,
       col=cols, pch=19, lwd=3)
par(oldpar)
dev.off()

write.csv(file="timeseriesExposure.csv",EXP)
