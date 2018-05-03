aersurface <- function(inputs = NULL, landcover, center, radio = 1, sectors = 12,
                       asg = "A",
                       airport = FALSE, arid = FALSE, snow = FALSE){
  # "create inpur aersurface"
  #
  # Args:
  #   input  : parameters list
  #   rst    : raster file or path of raster file
  #   center : vector with two values, x and y in UTM
  #   rDIO   : radio evaluation in km
  #   sctors : sectors (min = 1, max = 12)
  #   asg    : annual (A), month (M), or seasonal (S)
  # Return:
  #   file input aersurface
  #
  # check inputs raster
  #
  # check inputs
  if (is.null(inputs)) {
    inputs <- list(landcover = landcover, center = center, radio = radio,
                sectors = sectors, asg = asg, airport = airport,
                arid = arid, snow = snow)
  }
  if (!class(inputs$landcover)[1] == "RasterLayer") {
    inputs$landcover <- raster(landcover)
  }
  #
  # Check sectors numbers
  if (inputs$sectors > 12){
    cat("The sectors doesn't <= 12\n")
    return()
  }
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
  pkgTest("rgeos")
  pkgTest("dplyr")
  #
  # Check class file

  #
  # import values landcover NLCD92
  val.lc <- values(rst)
  cnt.lc <- length(val.lc)
  #
  # create sections
  p <- data.frame(x = inputs$center[1], y = inputs$center[2])
  xy <- SpatialPoints(p, proj4string = crs(rst))
  rm <- inputs$radio * 1000 * 1.01
  crcl <- buffer(xy, width = rm)
  if (inputs$sectors > 1) {
    scts <- seq(0, 360 - 360 / inputs$sectors, length.out = inputs$sectors)
    yn <- inputs$center[2] + rm*cos(scts*pi/180)
    xn <- inputs$center[1] + rm*sin(scts*pi/180)
    idl <- paste0("s", 1:inputs$sectors)
    lns <- list()
    for (i in 1:inputs$sectors) {
      lns[[i]] <- cbind(c(inputs$center[1], xn[i]), c(inputs$center[2], yn[i])) # error
      lns[[i]] <- Lines(list(Line(lns[[i]])), ID = idl[i])
    }
    lpi <- SpatialLines(lns, proj4string = crs(rst)) #
    blpi <- buffer(lpi, width = 0.000001)
    difp <- gDifference(crcl, blpi)
    disp <- disaggregate(difp)
    scts.names <- seq(360/(2*inputs$sectors), 360 - 360/(2*inputs$sectors),
                  length.out = inputs$sectors)
    yn.names <- inputs$center[2] + (rm/2)*cos(scts.names*pi/180)
    xn.names <- inputs$center[1] + (rm/2)*sin(scts.names*pi/180)
    #names.idl <- paste0("s", 1:inputs$sectors)
    pts.names <- data.frame(x = xn.names, y = yn.names)
    pts.names <- SpatialPoints(pts.names, proj4string = crs(rst))
    pts.names$sector <- idl
    disp$sector <- over(disp, pts.names[, "sector"])[, 1]
    #names(disp) <- idl
  } else {
    disp <- crcl
    disp$sector <- "s1"
  }
  #
  values.ID <- list()
  # extact zonal
  for (i in 1:inputs$sectors){
    rst.zonal <- mask(rst, disp[i, ])
    val <- values(rst.zonal)
    values.ID[[i]] <- val[!is.na(val)]
  }
  names(values.ID) <- disp$sector
  #
  # Month/Season assignments
  # select values
  load("datasurf.RData")
  #s.airport <- paste0("prt.", airport)
  #s.arid <- paste0("ard.", arid)
  #if (arid == "yes") {
  #  if (snow == "yes") {
  #    cat("The zone is arid, then doesn't have continuous snow\n")
  #  }
  #  snow = "no"
  #}
  #if (snow == "yes") {
  #  albedo$E3 <- NULL
  #} else if (snow == "no") {
  #  albedo$E4 <- NULL
  #}
  #albedo  %>% 
  #  filter(SF %in% c("def", s.airport, s.arid)) -> s.albedo
  #if (asg == "A") {
  #  albedo %>% 
  #    mutate(A = rowMeans(.[3:6])) %>% 
  #    select(ID, A) -> Anual
  #  anual <- c()
  #  for (i in 1:cnt.lc) {
  #    anual = Anual[which(Anual[, 1] == val.lc[i]), 2]
  #  }
  #  rst$anual <- anual
  #  nums <- extract(rst, disp)
  #  result <- sapply(nums, mean)
  #}
  return(values.ID)
  # seasonal
  # anual mean
  # winter, sunset, spring, autumn
  #
}
cat("\"surface\" has load\n")