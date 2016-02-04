source("R/popExposure.R")
vars <- c("NO2_mean","O3_nexc","PM10_mean","PM10_nexc","PM2.5_mean")
Thr <- list(c(10,20,30,40),
            c(10,25,50,75),
            c(10,20,30,40),
            c(10,20,35,50),
            c(10,15,20,25))
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

for (i in 1:nv) {
  system(paste("ls out/",vars[i],"*asc",sep=""),
         intern = T)->Files
  barplotExposure(Files, thresholds = Thr[[i]], Pop)
}
