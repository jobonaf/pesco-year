selectStaz <- function(poll, year, fileAna, only.rrqa=TRUE){
  library(pesco, lib.loc="/home/giovanni/R/x86_64-pc-linux-gnu-library/3.2")
  library(arpautils)
  read.sql(fileAna)->ANA
  Ana <- ANA
  if(poll=="PM10")  pl<-"PM"
  if(poll=="PM2.5") pl<-"P2"
  if(poll=="O3")    pl<-"O3"
  if(poll=="NO2")   pl<-"N2"
  aa<-NULL
  con <- dbqa.connect()

  ## selezione stazioni
  for(pr in c("PC","PR","RE","MO","BO","FE","FC","RA","RN")) {
    ii <- dbqa.list.active.staz(con, 
                                Day=as.POSIXct(paste(year,"-02-01",sep="")), 
                                prov=pr)
    ff <- dbqa.list.active.staz(con, 
                                Day=as.POSIXct(paste(year,"-11-30",sep="")), 
                                prov=pr)
    aa <- c(aa, intersect(ii,ff)) # tengo solo le stazioni attive almeno 10 mesi
    if(only.rrqa) aa <- aa[which(dbqa.isrrqa(con,aa))]
  }
  dbDisconnect(con)
  cond <- Ana[,pl]==1 & Ana$CLA%/%10==3 & as.character(Ana$CODE)%in%aa
  #cond <- Ana[,pl]==1 & Ana$CLA%/%10==3 
  Ana <- Ana[cond,]
  cc <- Ana$CODE
  
  ## scrittura file
  check <- substr(scan(fileAna,what="",sep="\n",blank.lines.skip = F),1,3)
  nsk <- which(check=="---")[1]
  hea <- scan(fileAna,nlines=nsk,what="",sep="\n")
  dat <- scan(fileAna,skip=nsk,what="",sep="\n")
  idx <- which(ANA$CODE %in% cc)
  out <- c(hea,dat[idx])
  write(out, file=paste("anagr_stzqa.",poll,".",year,".dat",sep=""))
  
  return(Ana)
}