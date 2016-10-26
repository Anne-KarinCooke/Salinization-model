# Climate loop for hillslop cascade

source("Hillslope cascade.R")
#  alpha <- seq(0.6,1.5,by=0.1) 
#  lambda <- seq(0.1,1,by=0.1)

alpha <- c(0.6,1.5) 
lambda <- c(0.1,1)
g <-c(1,2,3)
 
 Store <- list()
 sub_store <- list()
 time <- 1000
 delta <- 0
 set.seed(1000)
 

 
#   for (g in 1:length(grids)) {

for (i in 1:length(alpha)) {
     
     for (j in 1:length(lambda)) {
       # generate the rainfall
       Rain <- Precip(time,alpha[i],lambda[j],delta)
       Rainlist <- data.frame(Precip(time,alpha[i],lambda[j],delta))
       
       sub_store[[j]] <-data.frame(alpha_o=rep(alpha[i],time),
                                    lambda_o=rep(lambda[j],time),
                                   
                                    
                                    balances2D(Rain,plotit=T,  soilpar=soilpar,
                                             vegpar=vegpar, par=par)) 
       
       
       
       }
       Store[[i]] <- sub_store
     }
#    gstore[[g]] <-Store 
#     }
   


 
# Plotting M and P for different lambdas
# 
# ```{r}
# require(ggplot2)
# lambda_sum <- do.call(rbind,Store[][[2]])
# lambda_sum$time <- rep(1:time,length(lambda))
# 
# pl <- ggplot(lambda_sum,aes(x=time,y=P, colour="P (plant biomass density [gm-2])")) + geom_line()
# pl  <- pl + geom_line(aes(x=time,y=M, colour="Moisture"))  
# pl <- pl +  facet_wrap(~lambda_o, ncol=2)   #, colour=lambda_o (put this in aes-brackets) 
# pl  + ggtitle("Plant biomass P and soilmoisture M for varying lambdas") +  geom_line(aes(x=time, y=SmM, colour= "S (soil salt mg/L")) + theme(plot.title = element_text(lineheight=.8, face="bold"))
# ```
# 
# Plotting Soil water salt concentration for different alphas and lambdas
# 
# ```{r}
# lambda_sum_all <- do.call(rbind,do.call(rbind,Store))
# lambda_sum_all$time <- rep(rep(1:time,length(lambda)),length(alpha))
# 
# 
# pa <- ggplot(lambda_sum_all,aes(x=time,y=SmM,col=lambda_o)) + geom_line(linetype=1) 
# pa <- pa + scale_color_gradient(low="blue", high="red") + facet_wrap(~alpha_o, ncol=2) + ggtitle("Soil water salt mass [g] for varying alphas and lambdas") + theme(plot.title = element_text(lineheight=.8, face="bold"))
# pa  
# ```

# Comparing results

# Biomass without salt
# summary(lambda_sum_woSalt$P)
# # Biomass with salt
# summary(lambda_sum$P)
# #Soil moisture without salt
# # summary(lambda_sum_woSalt$M)
# #Soil moisture with salt
# summary(lambda_sum$M)
# summary(lambda_sum$Svir)
# # summary(1+lambda_sum$Svir)
# # summary(-lambda_sum$Svir*n*Zr)
# 
# 



