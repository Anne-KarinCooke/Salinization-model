### Principal component analysis

#cleaning the data
##occations where plants die
for(j in 1:nrow(Store)){
  if(is.na(Store$Pzero[j])){
    Store_failure <- rbind(Store_failure, Store[j,])
    Store<-Store[-j,]
  }
}

nrow(Store)
# -> going to anaylse this later. However there are heaps of fails
  

head(Store)
ncol(Store)


# Scatterplots
require(car)
library(car)
scatterplotMatrix(Store[,15:25])
scatterplotMatrix(Store[,c(15,17,19,21,23,24,25)])


# principal component analysis
pca <- prcomp(Store[,-26], scale. = T) #taking out the Pzero

summary(pca)

pca$rotation
pca_rot <-pca$rotation[1:14,15:25]
pca_rot
head(pca_rot)

# Eigenvalues
eig <- (pca$sdev)^2
# Variances in percentage
variance <- eig*100/sum(eig)
# Cumulative variances
cumvar <- cumsum(variance)
eig.pca <- data.frame(eig = eig, variance = variance,
                                    cumvariance = cumvar)
head(eig.pca)



barplot(eig.pca[, 2], names.arg=1:nrow(eig.pca), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.pca), 
      eig.pca[, 2], 
      type="b", pch=19, col = "red")



library("factoextra")
eig.val <- get_eigenvalue.pca)
head(eig.val)