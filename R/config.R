
configInval <- function(poll) {
  ## default values for invalidation
  if(poll=="PM10") {
    eps     <<- 10
    abs.tol <<- 20
    rel.tol <<- 0.5
  } else if(poll=="PM2.5") {
    eps     <<- 10
    abs.tol <<- 20
    rel.tol <<- 0.5
  } else if(poll=="NO2") {
    eps     <<- 10
    abs.tol <<- 30
    rel.tol <<- 0.5
  } else if(poll=="O3") {
    eps     <<- 10
    abs.tol <<- 30
    rel.tol <<- 0.5
  } else {
    stop(paste("Pollutant",poll,"not managed"))
  }
}


configProcess <- function (poll) {
  ## parametri
  if(poll=="PM10") {
    daily.stat <<- "mean"
    obs.daily.stat <<- NULL
    lambda     <<- 0
    threshold  <<- 50
    nrank      <<- 36
    calc.mean  <<- TRUE
  }
  if(poll=="PM2.5") {
    daily.stat <<- "mean"
    obs.daily.stat <<- NULL
    lambda     <<- 0
    threshold  <<- NULL
    nrank      <<- NULL
    calc.mean  <<- TRUE
  }
  if(poll=="O3") {
    daily.stat <<- "max8h"
    obs.daily.stat <<- daily.stat
    lambda     <<- 0
    threshold  <<- 120
    nrank      <<- 26
    calc.mean  <<- FALSE
  }
  if(poll=="NO2") {
    daily.stat <<- "mean"
    obs.daily.stat <<- daily.stat
    lambda     <<- 0
    threshold  <<- NULL
    nrank      <<- NULL
    calc.mean  <<- TRUE
  }
}
