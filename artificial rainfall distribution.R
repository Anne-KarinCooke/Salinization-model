r<-c(0,0,0,10,20,40,20,10,0,0,0)
x<-c(0:12)

par(mfrow = c(1,2))

barplot(r,x, col="skyblue",xlab=c("time increments (2h)"),ylab=c("percent of daily rainfall"))

r2<-c(0,0,0,0,0,0,0,0,25,75,0,0)
x2<-seq(0,12)
barplot(r2,x2, col="skyblue", xlab=c("time increments (2h)"), ylab=c("percent of daily rainfall"))

