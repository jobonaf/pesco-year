for (type in c("grd","tab")) {
  for (year in 2009:2015) {
    for (poll in c("PM10","PM2.5","NO2")) {
      pesco2ckan(poll = poll,
                 "mean",
                 year = year,
                 type = type)
    }
    pesco2ckan(poll = "PM10",
               "nexc50",
               year = year,
               type = type)
    pesco2ckan(poll = "O3",
               "nexc120",
               year = year,
               type = type)
  }
}
