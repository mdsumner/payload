library(raster)
library(gris)
library(rgl)
library(rgdal)
buildandplot <- function(x, z = x, xyz = FALSE, an = TRUE, ...) {
  y <- bgl(x, z)
  args <- list(...)
  bad <- is.na(y$vb[3, ])
  y$vb[3,bad] <- 0
  if(!isLonLat(x)) y$vb[1:2, ] <- t(project(t(y$vb[1:2, ]), projection(x), inv = TRUE))
  if (xyz) y$vb[1:3, ] <- t(llh2xyz(t(y$vb[1:3, ]), ...))
  if (!xyz & !is.null(args$exag)) {
    y$vb[3,] <- y$vb[3,] * args$exag
  }
  
  if (an) y <- addNormals(y)
  
  y$vb[3,bad] <- NA_real_
  
  shade3d(y, col = "grey")
  invisible(y)
}





ssh <- brick("ssh_decjan_2015.grd")



mesh <- buildandplot(ssh[[1]], xyz = FALSE, exag = 2)
rssh <- buildandplot(ssh[[1]], xyz = FALSE, exag = 10, an = FALSE)
## mesh$vb is used to resample from ssh[[i]]
zs <- extract(ssh, t(mesh$vb[1:2, ])) * 10
rgl.close()
pts <- expand.grid(x = c(xmin(ssh), xmax(ssh)), 
                   y = c(ymin(ssh), ymax(ssh)), 
                   z = range(zs, na.rm = TRUE))

rgl.points(pts)

while(TRUE) {
for (i in seq(nlayers(ssh))) {
  rssh$vb[3,] <- zs[,i] #extract(ssh[[i]], t(mesh$vb[1:2, ]))
  
  if (i > 1) rgl.pop()
  #Sys.sleep(0.2)
  shade3d(rssh, col = "grey")
  
  #scan("", 1)
}
rgl.pop()
}
