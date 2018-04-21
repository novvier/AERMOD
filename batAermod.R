BatAermod <- function(x, name, stage, demo = FALSE){
  # Create a bat file for execute multiples inputs aermod
  #
  # Args:
  #   x     :  a verctor of pollutants. Example: c("PM10", "SO2")
  #   name  :  name of projecto
  #   stage ;  stages of model. Example: 2 (construction and operation) 
  #   demo  :  TRUE if the run model is only demo receptors
  #
  # Return:
  #   file *.bat whith lines run of AERMOD 
  #
  # Examples:
  #   BatAermod("PM10", "CEMPIU", 1)
  #   BatAermod(c("PM10", "CO"), "CEMPIU", 3, demo = TRUE)
  #
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