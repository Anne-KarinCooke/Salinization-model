### Principal component analysis



# Store_pca <- Store[9:19]
# head(Store_pca)


pca <- prcomp(Store[-20], scale. = T)

summary(pca)
pca

pca
pca <-pca$rotation[9:19,9:19]

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