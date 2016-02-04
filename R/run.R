source("~/R/projects/pesco-annuale/R/krigYear.R")
source("~/R/projects/pesco-annuale/R/prepYear.R")
source("~/R/projects/pesco-annuale/R/statYear.R")
source("~/R/projects/pesco-annuale/R/xyz2grid.R")
source("~/R/projects/pesco-annuale/R/process.R")
source("~/R/projects/pesco-annuale/R/mapYear.R")

polls=c("PM10", "PM2.5", "O3", "NO2")
years=2015
verb=TRUE
proc=TRUE # lancia PESCO su tutti i giorni dell'anno
synt=TRUE # fa le statistiche annuali finali
maps=TRUE # fa le mappe

for(year in years) {
  for(poll in polls) {
    if(proc)processYear(poll=poll, year=year, verbose=verb)  
    if(synt)synthYear  (poll=poll, year=year, verbose=verb)  
    if(maps)mapYear    (poll=poll, year=year, verbose=verb)  
  }
}
