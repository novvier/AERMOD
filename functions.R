InputAersurface <- function(lc, file) {
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
read.PLT <- function(file, nstart, ...){
  # f = file o list files (character)
  # st = start line (numeric)
  # en = end line (numeric)
  # type = for "flow" or "dist"  
  # name = name of site model in lowercase
  d = list()
  j = length(f)
  a = st - 1
  z = en - st + 1
  namelc = tolower(name)
  name0 <- tolower(f)
  name1 <- gsub(".out","",name0)
  name2 <- gsub(paste0("out/",namelc),"",name1)
  if(type=="flow"){
    for (i in 1:j){
      d[[i]] = read.fortran(f[i],
        c("F8","1X","F8","F12","F8","7X","A3"), skip = a, n=z)
      names(d[[i]]) = c("FlowSector","Max1hr","Dist","Altitude","Period")
      d[[i]]$param <- name2[i]
    }
  }
  if(type=="dist"){
    #dr <- list()
    for (i in 1:j){
      d[[i]] = read.fortran(f[i],
        c("F14","F8","F13","5X","F14","F8","F13"), skip = a, n=z)
      names(d[[i]]) = rep(c("Dist","Max1hr","Altitude"),2)
      d[[i]] = rbind(d[[i]][,1:3], d[[i]][,4:6])
      d[[i]] = d[[i]][which(!is.na(d[[i]][,1])),]
      d[[i]]$param <- name2[i]
    }
  }
  names(d) <- name2
  i = 1
  while(i < j){
    if(i==1) df <- d[[i]]
    df <- rbind(df,d[[i+1]])
    i=i+1
  }
  return(df)
}
run.aermod <- function(x, name, stage, demo = FALSE){
  x <- toupper(x)
  if (stage == 0 | stage == 1) {
    stage = 0
  } else {
    stage = 1:stage
  }
  if(demo){
    out = "out_demo"
    modelo ="AERMOD_DEMO"
  } else {
    out = "out"
    modelo = "AERMOD"
  }
  file.run <- file(paste0("AERMOD_", name, ".BAT"))
  #---
  Head <- c("echo MODEL > time.txt",
            "REM ***********************************",
            paste0("REM EJECUTAR MODELO AERMOD - ", name),
            "REM ***********************************",
            "REM",
            "REM CREAR DIRECTORIO DE SALIDAS",
            "MKDIR ..\\out",
            "REM COPIAR RECEPTORES",
            paste0("COPY ..\\..\\2_aermap\\", out,
                   "\\", name, "_RECEPTOR.ROU RECEPTOR.ROU"),
            "REM COPIAR ARCHIVOS AERMET",
            paste0("COPY ..\\..\\3_aermet\\out",
                   "\\", name, "_SURFACE.SFC SURFACE.SFC"),
            paste0("COPY ..\\..\\3_aermet\\out",
                   "\\", name, "_PROFILE.PFL SURFACE.PFL"),
            "REM")
  Body <- list()
  body.text <- function(x, name, stage, demo = FALSE){
    if(demo){
      out = "out_demo"
    } else {
      out = "out"
    }
    fol <- paste0(stage, "_", x) 
    inp <- paste0(name, fol)
    Body <- c("REM ***********************************",
              paste0("REM ESCENARIO ", stage, " - ", x),
              "REM ***********************************",
              "REM",
              paste0("echo ", stage, x, " >> time.txt"),
              "echo %time% >> time.txt",
              "REM",
              paste0("MKDIR ..\\", out, "\\STG", fol),
              paste0("MKDIR ..\\", out, "\\STG", fol, "\\OUT"),
              paste0("MKDIR ..\\", out, "\\STG", fol, "\\PLT"),
              "REM",
              "REM COPIAR INPUT AERMOD",
              paste0("COPY ", inp, ".INP AERMOD.INP"),
              "REM EJECUTAR AERMOD",
              modelo,
              "REM MOVER OUTPUTs A DIRECTORIO DE SALIDAD",
              "REN *.out *.",
              paste0("REN *. *_", inp, ".OUT"),
              paste0("MOVE *.out ..\\", out, "\\STG", fol, "\\OUT\\"),
              "REN *.PLT *.",
              paste0("REN *. *_", inp, ".PLT"),
              paste0("MOVE *.PLT ..\\", out, "\\STG", fol, "\\PLT\\"),
              "REM",
              "echo %time% >> time.txt",
              "REM")
    return(Body)
  }
  for (j in 1:NROW(stage)){
    for (i in 1:NROW(x)){
      Body[[(NROW(x)*j+i)]] <- body.text(x[i], name, stage[j], demo)
    }
  }
  Body <- unlist(Body)
  writeLines(c(Head, Body), file.run)
  close(file.run)
}
#run.aermod("PM10", "CEMPIU", 1)
#run.aermod(c("PM10", "CO"), "CEMPIU", 3, demo = TRUE)
#