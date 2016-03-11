empty_package <- function(name="qualita-dell-aria-valutazione-delle-concentrazioni-di-fondo") {
  library(ckanr)
  ckanr_setup(url = "http://ckan.arpa.emr.it", key = "72739867-29a5-4c3a-8a7d-96d5b5493f5f")
  pack <- package_search(q=paste0("name:",name))
  pack_id <- pack$results[[1]]$resources[[1]]$package_id
  nres <- length(pack$results[[1]]$resources)
  for(i in nres:1) {
    res_id <- pack$results[[1]]$resources[[i]]$id
    cat(paste("deleting",pack$results[[1]]$resources[[i]]$name),sep="\n")
    resource_delete(res_id)
  }
  
}

pesco2ckan <- function(poll,indic,year,type,filein=NULL) {
  library(ckanr)
  ckanr_setup(url = "http://ckan.arpa.emr.it", key = "72739867-29a5-4c3a-8a7d-96d5b5493f5f")
  pack <- package_search(q="name:qualita-dell-aria-valutazione-delle-concentrazioni-di-fondo")
  pack_id <- pack$results[[1]]$id
  nres <- length(pack$results[[1]]$resources)
  
  Indic <- switch(indic,
                  "mean"="media annua",
                  "nexc50"="superamenti giornalieri",
                  "nexc120"="superamenti del massimo giornaliero della media mobile su 8h",
                  "rank36"="36esimo valore giornaliero più alto",
                  "rank26"="26esimo valore più alto del max giornaliero della media mobile su 8h")
  Type <- switch(type,
                 "tab"="tabelle comunali",
                 "grd"="dati su griglia")
  Name <- paste(poll,year,indic,"-",Type)
  Descr <- paste0(poll,": ",Indic," - ",Type," ",year)
  if(is.null(filein)) {
    path <- "~/R/projects/pesco-annuale/out/"
    prefix <- switch (type,
                      "tab" = "Comuni_",
                      "grd" = "")
    suffix <- switch (type,
                      "tab" = ".csv",
                      "grd" = ".asc")
    fmt <- switch (type,
                   "tab" = "CSV",
                   "grd" = "esri-ascii-raster")
    filein <- paste0(path,prefix,poll,"_",indic,"_",year,suffix)
  }
  cat(paste0(filein,": '",Name,"'"),sep = "\n")
  ds_create_dataset(package_id = pack_id,
                    name = Name,
                    path = filein)
  pack <- package_search(q="name:qualita-dell-aria-valutazione-delle-concentrazioni-di-fondo")
  nres <- length(pack$results[[1]]$resources)
  res_id <- pack$results[[1]]$resources[[nres]]$id
  resource_patch(list(format = fmt, description=Descr),
                 id = res_id)
}
