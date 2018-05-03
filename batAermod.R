batAermod <- function(name, x, stage, type = 1, name2 = "",
                      folder = "model/aermod/inp/", run = FALSE){
  # Create a bat file for execute multiples inputs aermod
  #
  # Args:
  #   x     : a verctor of pollutants. Example: c("PM10", "SO2")
  #   name  : name of projecto
  #   stage ; stages of model. Example: 2 (construction and operation) 
  #   type  : 0 = run dem; 1 = run defoutl; 2 = second run
  #   run   : TRUE run model; FALSE not run model
  # demo
  # Return:
  #   file *.bat whith lines run of AERMOD 
  #
  # Examples:
  #   BatAermod("PM10", "CEMPIU", 1)
  #   BatAermod(c("PM10", "CO"), "CEMPIU", 3, demo = TRUE)
  #
  x <- toupper(x)
  if (type == 0) {
    out = "out_demo"
    modelo ="AERMOD_DEMO"
    demo = TRUE
    kind = "_DEMO"
    map = "_DEMO"
  } else if (type == 1) {
    out = "out"
    modelo = "AERMOD"
    demo = FALSE
    kind = ""
    map = ""
  } else if (type == 2) {
    out = "out"
    modelo = "AERMOD_2"
    demo = FALSE
    kind = "_2"
    map = ""
  }
  file.run <- file(paste0(folder, "AERMOD_", name, kind, name2, ".BAT"))
  #---
  Head <- c("REM STAR",
            paste0("echo MODEL > TIME", kind, name2, ".txt"),
            "REM ***********************************",
            paste0("REM EJECUTAR MODELO AERMOD - ", name),
            "REM ***********************************",
            "REM",
            "REM CREAR DIRECTORIO DE SALIDAS",
            paste0("MKDIR ..\\out", tolower(kind)),
            "REM COPIAR RECEPTORES",
            paste0("COPY ..\\..\\aermap\\out",
                   "\\RECEPTOR_", name, map, ".ROU RECEPTOR.ROU"),
            "REM COPIAR ARCHIVOS AERMET",
            paste0("COPY ..\\..\\aermet\\out",
                   "\\SURFACE_", name, ".SFC SURFACE.SFC"),
            paste0("COPY ..\\..\\aermet\\out",
                   "\\PROFILE_", name, ".PFL PROFILE.PFL"),
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
              paste0("echo ", stage, x, " >> TIME", kind, name2, ".txt"),
              paste0("echo %time% >> TIME", kind, name2, ".txt"),
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
              paste0("echo %time% >> TIME", kind, name2, ".txt"),
              "REM")
    return(Body)
  }
  Tail <- c("REM ***********************************",
            "REM BORRAR INPUTS DEFAULT",
            "REM ***********************************",
            "REM",
            "DEL SURFACE.SFC",
            "DEL PROFILE.PFL",
            "DEL RECEPTOR.ROU",
            "DEL AERMOD.INP",
            "REM END")
  for (j in 1:NROW(stage)){
    for (i in 1:NROW(x)){
      Body[[(NROW(x)*j+i)]] <- body.text(x[i], name, stage[j], demo)
    }
  }
  Body <- unlist(Body)
  writeLines(c(Head, Body, Tail), file.run)
  close(file.run)
  if(run){
    folder.current <- getwd()
    setwd(paste0(folder.current, "/", folder))
    system("cmd.exe", input = paste0("AERMOD_", name, ".BAT"))
    setwd(folder.current)
  }
}
cat("\"batAermod\" has load\n")
#batAermod("CEMPIU",
#          c("pm10", "pm25", "co", "no2", "so2"),
#          0,
#          "model/aermod/inp/")