readPLT <- function(file, nstart, ...){
  # Read PLT file (output AERMOD)
  #
  # Args:
  #   f    : file o list files (character)
  #   st   : start line (numeric)
  #   en   : end line (numeric)
  #   type :  for "flow" or "dist"  
  #   name : name of site model in lowercase
  #
  # Return:
  # Table add results of AERMOD
  # 
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