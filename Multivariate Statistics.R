### Principal component analysis

#cleaning the data
##occations where plants die as an indicator (goes NA, if model crashes)
for(j in 1:nrow(Store)){
  if(is.na(Store$Pzero[j])){
    Store_failure <- rbind(Store_failure, Store[j,])
    Store<-Store[-j,]
  }
}

# no crashes in Store! :D Store.txt in the repo is a Monte Carlo simulation with Heavy clay, 800 days and 5000 runs
# 
# nrow(Store)
# head(Store)
# ncol(Store)


#### LINEAR REGRESSION


mydata <- read.table("Store.txt")
mydata
attach(mydata)

fit <- lm(meanM ~ Z + Zr + d + ConcConst + CM.gw + c + alpha + lambda, data = mydata)
summary(fit)

MZ <- lm(meanM ~ Z, data=mydata)
summary(MZ)





 

# show results
# 
# ######################################################################################
# ##
# 
# mydata <- Store
# 
# 
# # mydata <-mydata[c(1,2,3,4,7,8,9,10,11,12,13,14,15,17,19,21,23,23,25)]
#       
# ### CLUSTER ANALYSIS      
# 
# mydata <- scale(mydata) # standardize variables
# 
# 
# 
# # Determine number of clusters
# # wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
# # for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
# #                                      centers=i)$withinss)
# # plot(1:15, wss, type="b", xlab="Number of Clusters",
# #      ylab="Within groups sum of squares")
# # 
# # 
# 
# #hierarchical clustering
# clusters <- hclust(dist(mydata))
# plot(clusters)
# 
# clusterCut <- cutree(clusters, 6)
# 
# 
# 
# # K-Means Cluster Analysis
# fit <- kmeans(mydata, 14) # 14 cluster solution
# # get cluster means 
# aggregate(mydata,by=list(fit$cluster),FUN=mean)
# # append cluster assignment
# mydata1 <- data.frame(mydata, fit$cluster)
# 
# # K-Means Clustering with 14 clusters
# fit <- kmeans(mydata1, 14)
# 
# # Cluster Plot against 1st 2 principal components
# 
# # vary parameters for most readable graph
# library(cluster) 
# clusplot(mydata1, fit$cluster, color=TRUE, shade=TRUE, 
#          labels=2, lines=0)
# 
# # Centroid Plot against 1st 2 discriminant functions
# library(fpc)
# plotcluster(mydata1, fit$cluster)
# 
# 
# 
# # Ward Hierarchical Clustering
# d <- dist(mydata, method = "euclidean") # distance matrix
# fit <- hclust(d, method="ward") 
# plot(fit) # display dendogram
# groups <- cutree(fit, k=10) # cut tree into 5 clusters
# # draw dendogram with red borders around the 5 clusters 
# rect.hclust(fit, k=10, border="red")
# # dendogram click to view
# # The pvclust( ) function in the pvclust package provides p-values for hierarchical clustering based on multiscale bootstrap resampling. Clusters that are highly supported by the data will have large p values. Interpretation details are provided Suzuki. Be aware that pvclust clusters columns, not rows. Transpose your data before using.
# # Ward Hierarchical Clustering with Bootstrapped p values
# library(pvclust)
# fit <- pvclust(mydata, method.hclust="ward",
#                method.dist="euclidean")
# plot(fit) # dendogram with p values
# # add rectangles around groups highly supported by the data
# pvrect(fit, alpha=.95)
# 
# 
# 
# 
# # Model Based Clustering
# library(mclust)
# fit <- Mclust(mydata)
# plot(fit) # plot results 
# summary(fit) # display the best model
# 
# 
# 
# 
# 
# # principal component analysis
# pca <- prcomp(Store[,-26], scale. = T) #taking out the Pzero
# 
# summary(pca)
# 
# pca$rotation
# pca_rot <-pca$rotation[1:14,15:25]
# pca_rot
# head(pca_rot)
# 
# # Eigenvalues
# eig <- (pca$sdev)^2
# # Variances in percentage
# variance <- eig*100/sum(eig)
# # Cumulative variances
# cumvar <- cumsum(variance)
# eig.pca <- data.frame(eig = eig, variance = variance,
#                                     cumvariance = cumvar)
# head(eig.pca)
# 
# 
# 
# barplot(eig.pca[, 2], names.arg=1:nrow(eig.pca), 
#         main = "Variances",
#         xlab = "Principal Components",
#         ylab = "Percentage of variances",
#         col ="steelblue")
# # Add connected line segments to the plot
# lines(x = 1:nrow(eig.pca), 
#       eig.pca[, 2], 
#       type="b", pch=19, col = "red")
# 
# 
# 
# library("factoextra")
# eig.val <- get_eigenvalue.pca)
# head(eig.val)