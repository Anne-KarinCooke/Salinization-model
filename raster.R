## Rain 
## making a subdaily distribution

Rain_mm <- Rain*10 #mm
Rain_mm


deltat=12


for (i in length(Rain_mm)){
  Rain_mm[j]<-
}






### playing around with raster package

require(raster)

# Create 100x100 random raster with a Z range of 500-1500
r <- raster(ncols=100, nrows=100, xmn=0)
r[] <- runif(ncell(r), min=500, max=1500)  

# Gaussian Kernel Function
GaussianKernel <- function(sigma=s, n=d) {
  m <- matrix(nc=n, nr=n)
  col <- rep(1:n, n)
  row <- rep(1:n, each=n)
  x <- col - ceiling(n/2)
  y <- row - ceiling(n/2)
  m[cbind(row, col)] <- 1/(2*pi*sigma^2) * exp(-(x^2+y^2)/(2*sigma^2))
  m / sum(m)
}

# Create autocorrelated raster using 9x9 Gaussian Kernel with a sigma of 1
r.sim <- focal(r, w=GaussianKernel(sigma=1, n=9))

# Plot results
par(mfcol=c(1,2))
plot(r, main="Random Raster")
plot(r.sim, main="Autocorrelated Random Raster sigma=1, n=9")


##### Random fields

library(fields)
# load Maunga Whau volcano (Mt Eden) elevation dataset (matrix format)
data(volcano)

# reduce size
volcano2 <- volcano[10:55, 14:51]
filled.contour(volcano2, color.palette=terrain.colors, asp=1)
cols <- length(volcano2[1,])
rows <- length(volcano2[,1])

# create dataframe (xyz format)
X <- rep(1:cols, each=rows)
Y <- rep(1:rows, cols)
Z <- as.vector(volcano2)
volcano.df <- data.frame(X,Y,Z,cellid=1:cols*rows)
attach(volcano.df)
quilt.plot(Y,X,Z,nrow=rows,ncol=cols,add=F)

# create a spatial autocorrelation signature
# coordinate list
coords <- data.frame(X,Y)
# distance matrix
dist <- as.matrix(dist(coords))

# create a correlation structure (exponential)
str <- -0.1 # strength of autocorrelation, inv. proportional to str
omega1 <- exp(str*dist)

# calculate correlation weights, and invert weights matrix
weights <- chol(solve(omega1))
weights_inv <- solve(weights)

# create an autocorrelated random field
set.seed(1011)
error <- weights_inv %*% rnorm(dim(dist)[1])
quilt.plot(Y,X,error,nrow=rows,ncol=cols,add=F)

# create a variable as a linear function of the elevation
a <- 10
b <- 0.5
Z2 <- a + b*Z
quilt.plot(Y,X,Z2,nrow=rows,ncol=cols,add=F)

# add the autocorrelated error to the new variable
Z3 <- Z2 + error
quilt.plot(Y,X,Z3,nrow=rows,ncol=cols,add=F)

# export data (xyz format)
write.table(data.frame(X,Y,Z,Z2,Z3), file="data.txt", row.names=F, sep="\t")
