mkEmission <- function(emi, stage, file.name,
                       Relhgt = 2.13, Syinit = 7.91, Szinit = 1.98){
  pkgTest <- function(x) {
    if (!require(x,character.only = TRUE)) {
      install.packages(x,dep = TRUE)
        if(!require(x,character.only = TRUE))
          stop("Package not found")
    }
  }
  pkgTest("tidyverse")
  emi <- emi[, c("idem", stage)]
  names(emi) <- c("idem", "Vlemis")
  if (sum(emi$Vlemis, na.rm = TRUE) == 0) {
    cat(paste0("No presenta emisiones ", 
               gsub("X", "", stage),"\n"))
    return()
  } else {
    cat(paste0("Lista de fuentes para ", 
               gsub("X", "", stage),"\n"))
  }
  loc <- read.table("model/aermap/out/SOURCE_UEAMTE.SOU",
                         skip = 11)
  names(loc) <- c("SO","LOCATION","Srcid", "type", "x", "y", "z")
  loc$idem <- substr(loc$Srcid, 1, 6)
  df <- merge(loc, emi, by = "idem", all.x = TRUE)
  df %>% 
    #mutate(ids = str_pad(ids, 2, pad = "0")) %>%
    #mutate(Srcid = paste0(idem, ids),
    mutate(SRCPARAM = "SRCPARAM",
           Relhgt = Relhgt,
           Syinit = Syinit,
           Szinit = Szinit) %>% 
    filter(Vlemis != 0) -> df
  df %>% 
    group_by(idem) %>% 
    count() -> resumen
  print(resumen)
  df %>% 
    #filter(type == "VOLUME") %>% 
    mutate(texting = sprintf("%2s%9s%10s%17.10f%10.2f%10.2f%10.2f",
                           SO, SRCPARAM, Srcid,
                           Vlemis, Relhgt, Syinit, Szinit)) %>% 
    select(texting) -> emi.out
  df %>% 
    mutate(texting = sprintf("%2s%9s%10s%10s%14.2f%14.2f%14.2f",
                           SO, LOCATION, Srcid, type, x, y, z)) %>% 
    select(texting) -> loc.out
  sep = "** ---------------------------------------------------------------------"
  lista <- list(loc.out, sep, emi.out)
  lista <- do.call(rbind, lista)
  write.table(lista, file.name, col.names = FALSE,
            row.names = FALSE, quote = FALSE)
}
cat("\"mkEmission\" has load\n")