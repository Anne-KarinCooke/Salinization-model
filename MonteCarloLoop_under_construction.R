# script to run Monte Carlo sensitivity analysis 
# "Morris" type sensitivity

# read in the parameter ranges
MC_par <- read.csv("MC_Parameters.txt", header=F)

# source the model functions
source("modelfunctions.R")
source("Soil input for MonteCarloop.R")

# Z=1000 #mm
vegpar <- list(Zr = Zr)

alpha_i=1 #maximum infiltration rate per day (K_s and therefore soil type dependency in balances function code)

k=12 # Saco et al, 2013
W0=0.2 # Saco et al, 2013
gmax=0.05 # Saco et al, 2013
k1=5 # Saco et al, 2013
# c=10  # Saco et al, 2013
f= 1  # f is the soil salt leaching efficiency (whether some salt is retained)
# ConcConst = 0.1 # ConcConst is the concentration of the salt in the infiltrating water in g/l
# CM.gw = 0.1 # salt concentration in groundwater
# d=0.24 # fraction of plant mortality

par <- list(alpha_i=alpha_i,k=k, W0=W0, gmax=gmax, k1=k1, c=c, f=f, ConcConst=ConcConst, CM.gw= CM.gw, d=d)
# ..............................................

# Develop a storage data frame
runs = 1000

par_MC <- as.data.frame(matrix(0,nrow=runs,ncol=nrow(MC_par), byrow=F))
colnames(par_MC) <- MC_par[,1]


set.seed(runs)
MC_soilpar <- read.csv("MC_soilnames.txt", header=F)  
MC_soilpar <- data.frame(MC_soilpar)                   


soilpar <-as.data.frame(matrix(0,nrow=length(soilpar),ncol=length(MC_soilpar[,1]), byrow=F))
# soilpar <- Soil(stype=toString(MC_soilpar[i,1]))

# Adding variable soil data, so the runs occur between different soils, too
colnames(soilpar)<- c("b","n","K_s", "psi_s_bar", "h1bar", "hb")
for (i in 1:nrow(soilpar)){
  x[i]<-toString(MC_soilpar[i,1])
}
row.names(soilpar) <-x

for (i in 1:nrow(soilpar)) {

 soilpar[i,]  <- Soil(stype=toString(MC_soilpar[i,1]))

}


library(foreach)

foreach(l = 1:nrow(soilpar)) %do% soilpar[l,]


library(mefa)
soilpar_runs <- rep(soilpar,runs) 
soilpar_runs_sample<- soilpar_runs[sample(nrow(soilpar_runs),runs),]


# Lognormal distribution parameter logmean
mu <-function(m,v){
  mu <- log(m/(sqrt(1+(v/(m*m)))))
  return(mu)
}
# Lognormal distribution parameter logsd
sigma <-function(m,v){
  sigma <- sqrt(log(1+(v/(m*m))))
  return(sigma)
}

for (i in 1:nrow(MC_par)) {
  par_MC[,i] <- runif(runs,MC_par[i,2],MC_par[i,3])
  par_MC[,2] <- rlnorm(runs,mu(400,5000), sigma(400,5000))
  par_MC[,3] <- rlnorm(runs,mu(0.1,0.1), sigma(0.1,0.1))
}

Store <- data.frame(soilpar_runs_sample, par_MC,meanM = numeric(length=runs),sdM = numeric(length=runs),
                    meanSmM = numeric(length=runs),sdSmM = numeric(length=runs),
                    meanP = numeric(length=runs), sdP = numeric(length=runs),
                    meanCM = numeric(length=runs), sdCM = numeric(length=runs),
                    minCM = numeric(length=runs), maxCM = numeric(length=runs),
                    cum_flux = numeric(length=runs),Pzero=numeric(length=runs))

Store_failure <- data.frame()


time <- 400
delta <- 0
 system.time(
for (j in 1:runs) {
  
  
  alpha <- Store$alpha[j]
  lambda <- Store$lambda[j]
  Rain <- Precip(time,alpha,lambda,delta)
  vegpar$Zr <- Store$Zr[j]
  par$d <- Store$d[j]
  par$ConcConst <- Store$ConcConst[j]
  par$CM.gw <- Store$CM.gw[j]
  par$c <- Store$c[j]
  Z=Store$Z[j]
  
   soilpar<-soilpar_runs_sample[j,]


  result <- balances(Rain,plotit=T, par=par,soilpar, vegpar)
  
  # mean and standard deviation of SOIL MOSTURE
  Store$meanM[j] <- mean(result$M[200:time]) 
  Store$sdM[j] <- sd(result$M[200:time]) 
  
  # mean and standard deviation of SOIL SALT MASS
  Store$meanSmM[j] <- mean(result$SmM[200:time]) 
  Store$sdSmM[j] <- sd(result$SmM[200:time]) 
  
  # mean and standard deviation of PLANT BIOMASS
  Store$meanP[j] <- mean(result$P[200:time]) 
  Store$sdP[j] <- sd(result$P[200:time]) 
  #events where P (plant biomass) hits 0, plants die
  Store$Pzero[j] <- ifelse(any(result$P[200:time]==0),1,0)
  
  # mean, standard deviation, minima and maxima of SOIL SALT CONCENTRATION
  Store$meanCM[j] <- mean(result$CM[200:time]) 
  Store$sdCM[j] <- sd(result$CM[200:time])
  Store$minCM[j] <- min(result$CM[200:time])
  Store$maxCM[j] <- max(result$CM[200:time]) 
  
  # sum of cumulative water flux
  Store$cum_flux[j] <- sum(result$flux[200:time]) 
  
}

)


