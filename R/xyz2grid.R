xyz2grid <- function(x, y, z,
                     cellcentre.offset=c(510500,4843500),
                     cellsize=c(1000,1000),
                     cells.dim=c(297,161)) {
  library(sp)
  grid <- GridTopology(cellcentre.offset, cellsize, cells.dim)
  idx <- getGridIndex(cc=cbind(x,y), grid=grid)
  nc <- nrow(coordinates(grid))
  Z <- rep(NA,nc)
  Z[idx] <- z
  Dat <- SpatialGridDataFrame(grid=grid, data=data.frame(val=Z))
  return(Dat)
}