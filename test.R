##### create raster
ext <- extent((380000-1500),
              (380000+1500),
              (8800000-1500),
              (8800000+1500))
rst <- raster(ncols = 100, nrows= 100, ext = ext, crs = crs("+init=epsg:32718"))
#values(rst) <- sample(1:20, 10000, replace = TRUE)
values(rst) <- sample(c(81,82), 10000, replace = TRUE)
inputs = list(landcover = rst,
              center = c(380000, 8800000),
              radio = 1, # km
              sectors = 12,
              asg = "A",
              airport = FALSE,
              arid = FALSE,
              snow = FALSE)
lst <- aersurface(inputs)
lst <- list(s1 = rep(81, 24), s2 = rep(82, 41))
df <- data.frame(ID = c(81, 82), E1 = c(0.5, 0.4))
new.lst <- list()
for (i in 1:length(lst)){
  new.lst[[i]] <- c(rep(0, length(lst[[i]])))
  for(j in 1:length(lst[[i]])){
    new.lst[[i]][j] <- df$E1[which(df$ID == lst[[i]][j])]
  }
  new.lst[[i]] <- mean(new.lst[[i]])
}
x <- unlist(new.lst)
library(readxl)
albedo <- as.data.frame(read_xls("datasurf.xls", sheet = "albedo"))
bowen <- as.data.frame(read_xls("datasurf.xls", sheet = "bowen"))
roughness <- as.data.frame(read_xls("datasurf.xls", sheet = "roughness"))
save(albedo, bowen, roughness, file = "datasurf.RData")

rst <- splineOUT("ALL01HR.OUT", remove.shp = TRUE)

"ALL01HR.OUT"
