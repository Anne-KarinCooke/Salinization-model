
# source the Moisture function script
source("MoistureFunction.R")
source("Rainfall.R")

set.seed(1000)

alpha <- seq(0.6,1.5,by=0.1) ## 0.5
lambda <- seq(0.1,1,by=0.1)

Store <- list()
sub_store <- list()
time <- 1000
delta <- 0

for (i in 1:length(alpha)) {
  for (j in 1:length(lambda)) {
    # generate the rainfall
    Rain <- Precip(time,alpha[i],lambda[j],delta)
    Rainlist <- data.frame(Precip(time,alpha[i],lambda[j],delta))
    
    sub_store[[j]] <-data.frame(alpha_o=rep(alpha[i],time),
                                lambda_o=rep(lambda[j],time),
                                balances(Rain,plotit=T, soilpar=list(K_s=K_s,n=n,Zr=Zr,s_fc=s_fc)))
    
  }
  Store[[i]] <- sub_store
}

#str(Store)

require(ggplot2)

### Plotting M and P for different lambdas

lambda_sum <- do.call(rbind,Store[][[2]])
lambda_sum$time <- rep(1:time,length(lambda))

pl <- ggplot(lambda_sum,aes(x=time,y=P, colour="P (plant biomass density [gm-2])")) + geom_line()
pl  <- pl + geom_line(aes(x=time,y=M, colour="Moisture"))  
pl <- pl +  facet_wrap(~lambda_o, ncol=2)   #, colour=lambda_o (put this in aes-brackets)
pl  
pl  + ggtitle("Plant biomass P and soilmoisture M for different lambdas") +  geom_line(aes(x=time, y=SmM, colour= "S (soil salt mg/L")) + theme(plot.title = element_text(lineheight=.8, face="bold"))
#+ scale_colour_hue() #name="blances", breaks=c("M", "P"), labels=c("Soil moisture", "Plant biomass"))
#pl + geom_bar(aes(x=time, y=Rain, colour="blue"))


### Plotting M and P for different alphas

lambda_sum_all <- do.call(rbind,do.call(rbind,Store))
lambda_sum_all$time <- rep(rep(1:time,length(lambda)),length(alpha))


pa <- ggplot(lambda_sum_all,aes(x=time,y=SmM,col=lambda_o)) + geom_line(linetype=1) 
pa <- pa + scale_color_gradient(low="blue", high="red") + facet_wrap(~alpha_o, ncol=2)   #, colour=lambda_o (put this in aes-brackets)
pa  
# pa  + ggtitle("Soil moisture M and salinity for different alphas") +  
#   geom_line(aes(x=time, y=CM, colour= "red")) + 
#   theme(plot.title = element_text(lineheight=.8, face="bold"))

hist(lambda_sum_all$In,breaks=100)

pb <- ggplot(lambda_sum_all,aes(x=time,y=P,col=lambda_o)) + geom_line(linetype=1) 
pb <- pb + scale_color_gradient(low="blue", high="red") + facet_wrap(~alpha_o, ncol=2)   #, colour=lambda_o (put this in aes-brackets)
pb  


### Storing the generated rainfall in "Rainlist"

for (i in 1:length(alpha)) {
  for (j in 1:length(lambda)) {
Rainlist[[i]][[j]] <- list(Rain <- Precip(time,alpha[i],lambda[j],delta))
    }
  }
#str(Rainlist)
#Rainlist



