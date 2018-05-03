splineOUT <- function(file.name, UTM = 18, rsl = 100, remove.shp = FALSE){
  pkgTest <- function(x) {
    if (!require(x,character.only = TRUE)) {
      install.packages(x,dep = TRUE)
        if(!require(x,character.only = TRUE))
          stop("Package not found")
    }
  }
  pkgTest("raster")
  pkgTest("rgdal")
  pkgTest("RSAGA")
  nc <- unlist(strsplit(file.name, "/"))
  nc <- unlist(strsplit(nc, "\\."))
  nc <- nc[NROW(nc)-1]
  nc.num <- nchar(nc)
  name.tif <- paste0(nc, ".TIF")
  if (remove.shp) {
    name.shp <- "points_saga.shp"
  } else {
    name.shp <- paste0(nc, ".shp")    
  }
  df <- read.fwf(paste0(file.name),
                 widths = rep(14, 3), skip = 8)
  names(df) <- c("x", "y", "conc")
  coordinates(df) <- ~x+y
  crs(df) <- CRS(paste0("+init=epsg:327", UTM))
  shapefile(df, name.shp)
  #
  # Spline with SAGA 2.2.0
  work_env <- rsaga.env(workspace = getwd(),
  path = "C:/SAGA-GIS/saga-2.2.0_x64")
  #
  cat("\n")
  cat(">>> Interpolating with spline\n")
  cat("\n")
  #
  rsaga.geoprocessor("grid_spline", module = 2,
                   env = work_env,
                   param = list(SHAPES = name.shp,
                                FIELD = "conc",
                                TARGET_USER_SIZE = rsl,
                                TARGET_OUT_GRID = "spline_saga"))
  #rsaga.get.modules("io_gdal", env = work_env)
  #rsaga.get.usage("io_gdal", 2, env = work_env)
  #
  cat("\n")
  cat(">>> Creating GEOTIFF file\n")
  cat("\n")
  #
  rsaga.geoprocessor("io_gdal", module = 2,
                     env = work_env,
                     param = list(GRIDS = "spline_saga.sgrd",
                                  FILE = name.tif))
  rst <- raster(name.tif)
  lista <- list.files()
  #
  rem <- lista[which(substr(lista, 1, 11) == "spline_saga")]
  file.remove(rem)
  #
  if (remove.shp) {
    rem <- lista[which(substr(lista, 1, 11) == "points_saga")]
    file.remove(rem)
  }
  return(rst)
}
cat("\"splineOUT\" has load\n")
