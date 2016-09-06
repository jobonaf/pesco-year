Anag <- list()
Anag[[1]] <- pesco::read.sql(filein<-select.list(dir(pattern="dat"), title="stazioni PM10?"))[,1:8]
Anag[[2]] <- pesco::read.sql(filein<-select.list(dir(pattern="dat"), title="stazioni PM2.5?"))[,1:8]
Anag[[3]] <- pesco::read.sql(filein<-select.list(dir(pattern="dat"), title="stazioni NO2?"))[,1:8]
Anag[[4]] <- pesco::read.sql(filein<-select.list(dir(pattern="dat"), title="stazioni ozono?"))[,1:8]

ANAG <- NULL
for (i in 1:4) {
  Anag[[i]] <- cbind(Anag[[i]],matrix(0,nrow=nrow(Anag[[i]]),ncol=4))
  Anag[[i]][,8+i] <- 1
  ANAG <- rbind(ANAG,Anag[[i]])
}

PM10 <- tapply(X=ANAG[,9], INDEX=ANAG$CODE, FUN=max)
PM25 <- tapply(X=ANAG[,10], INDEX=ANAG$CODE, FUN=max)
NO2  <- tapply(X=ANAG[,11], INDEX=ANAG$CODE, FUN=max)
O3   <- tapply(X=ANAG[,12], INDEX=ANAG$CODE, FUN=max)
ANAG <- ANAG[match(unique(ANAG$CODE),ANAG$CODE),2:8]

Nome <- NULL; Comu <- NULL
for (i in 1:nrow(ANAG)) {
 Nome <- c(Nome,pesco::Capital(pesco::small(pesco::trim(as.character(ANAG[i,1])))))
 Comu <- c(Comu,pesco::Capital(pesco::small(pesco::trim(as.character(ANAG[i,2])))))
}
Area <- c("n.d.","urbana","suburb.","rurale")[as.numeric(as.character(ANAG[,7]))%%10+1]
Tipo <- c("n.d.","traffico","industr.","fondo")[as.numeric(as.character(ANAG[,7]))%/%10+1]
Table <- cbind(Nome,Comu,
               as.character(ANAG[,3]),as.character(ANAG[,4]),
               as.character(ANAG[,5]),as.character(ANAG[,6]),
               Area,Tipo,
               c("","x")[NO2+1],c("","x")[O3+1],
               c("","x")[PM10+1],c("","x")[PM25+1])
idx <- order(Table[,1])
Table <- Table[idx,]

write(file="stazioni.tex","\\begin{landscape}")
write(file="stazioni.tex","\\begin{table}",append=TRUE)
write(file="stazioni.tex","\\caption{Stazioni utilizzate dal modello PESCO}\\label{tab:stazioni}",append=TRUE)
write(file="stazioni.tex","\\begin{small}",append=TRUE)
write(file="stazioni.tex","\\begin{tabular}{|lll|llr|rr|cccc|}",append=TRUE)
write(file="stazioni.tex"," \\hline",append=TRUE)
write(file="stazioni.tex","nome&Comune& prov.&latitudine &longitudine & quota &area & tipo  &$NO_2$  &$O_3$  &$PM_{10}$  &$PM_{2.5}$ \\\\",append=TRUE)
write(file="stazioni.tex"," &   &   & ($^{\\circ}~N$) & ($^{\\circ}~E$) & (m msl) & & & & & & \\\\",append=TRUE)
write(file="stazioni.tex","\\hline",append=TRUE)
write(file="stazioni.tex","\\hline",append=TRUE)

for (i in 1:nrow(ANAG)){
  line <- paste(paste(Table[i,],collapse="&"),"\\\\")
  write(file="stazioni.tex",line,append=TRUE)
}

write(file="stazioni.tex","\\hline",append=TRUE)
write(file="stazioni.tex","\\end{tabular}",append=TRUE)
write(file="stazioni.tex","\\end{small}",append=TRUE)
write(file="stazioni.tex","\\end{table}",append=TRUE)
write(file="stazioni.tex","\\end{landscape}",append=TRUE)
