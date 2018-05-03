inputAersurface <- function(lc, file) {
  # "export raster to input aersurface"
  #
  # Args:
  #   lc <- raster landcover
  #   file <- name file exported
  # Return:
  #   export file
  #
  # load libraries
  pkgTest <- function(x) {
    if (!require(x,character.only = TRUE)) {
      install.packages(x,dep=TRUE)
        if(!require(x,character.only = TRUE))
          stop("Package not found")
    }
  }
  pkgTest("raster")
  pkgTest("rgdal")
  #
  # check resolution 
  res.lg <- res(lc) == c(30, 30)
  if (!(res.lg[1] & res.lg[2])) {
    cat("The resolution is not 30 x 30\n")
    return()
  }
  #
  # check data type
  if (!dataType(lc) == "INT1U") {
    cat("The data type is not INT1U\n")
    return()
  }
  #
  # check projection
  if (showEPSG(as.character(crs(lc))) == "4326") {
    lc.geo <- lc
  } else {
    cat("Change projection to lonlat and datum to WGS84\n")
    lc.geo <- projectRaster(lc, crs = CRS("+init=epsg:4326"), method = "ngb")
  }
  #
  # create new CRS
  cat("Create personal projection\n")
  lon_0 = (xmin(lc.geo) + xmax(lc.geo)) / 2
  lat_0 = (ymin(lc.geo) + ymax(lc.geo)) / 2
  lat_1 = lat_0 + 5
  lat_2 = lat_0 - 5
  srUser = paste0("+proj=aea +lat_1=", as.character(lat_1),
                  " +lat_2=", as.character(lat_2),
                  " +lat_0=", as.character(lat_0),
                  " +lon_0=", as.character(lon_0),
                  " +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
                  " ellps=WGS84 +towgs84=0,0,0")
  #
  cat("Create input landcover for aersurface\n")
  lc.user <- projectRaster(lc.geo, crs = srUser,
                           res = c(30,30), method = "ngb")
  #
  # create color table NLCD92
  lc.user[is.na(lc.user[])] <- 0
  lc.user <- AddColortable(lc.user)
  #
  #Wirte rater
  writeRaster(lc.user, file, format = "GTiff", overwrite = T,
              dataType="INT1U", options = c("COMPRESS=NONE"))
  #
  # Import raster
  lc.user <- raster(file)
  cat("Input landcover created\n")
  #
  detach("package:raster", unload = TRUE)
  detach("package:rgdal", unload = TRUE)
  #
  return(lc.user)
}
cat("\"inputAersurface\" has load")
