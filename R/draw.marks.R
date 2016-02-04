draw.marks <- function(file,xyz=c(2,1,3),cex=0.5,pch=1,adj=c(0,0),srt=0,
                       col="black",col.txt=col,xy.fact=1,cex.txt=cex) {
  Tab <- read.table(file,head=F)
  x <- Tab[,xyz[1]]*xy.fact
  y <- Tab[,xyz[2]]*xy.fact
  z <- Tab[,xyz[3]]
  z <- gsub(z, pattern="_", replacement=" ")
  if(pch>0)points(x,y,cex=cex,pch=pch, col=col)
  if(cex.txt>0) text(x,y,z,cex=cex.txt,adj=adj,srt=srt, col=col.txt)
}

