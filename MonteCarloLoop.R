# script to run Monte Carlo sensitivity analysis 
# "Morris" type sensitivity

# read in the parameter ranges
MC_par <- read.csv("MC_Parameters.txt", header=F)

# source the model functions
source("modelfunctions.R")

# ------------------
# Definitions
# ------------------
# ...................
# Soil and vegetation
# ...................
# # Sandy Clay Loam
# n<-0.367 # porosity
# # more soil variables for evaporation & losses
# # Hydraulic conductivity
# K_s<-52.08*10 # mm/day
# # campbell's b
# b<-6.4069 # neurotheta sandy clay loam
# # van Genuchten parameters
# #     avg <- 0.0521
# #     nvg <- 1.237
# # s_fc<-0.2677/n # Field capacity _ not used 
# # This is the bubbling pressure
# psi_s_bar<--1.2E-3 #
# h1bar =  -psi_s_bar 
# hb = psi_s_bar*-10^5 # mm

Medium Heavy Clay
n<-0.4473 # porosity
# more soil variables for evaporation & losses
# Hydraulic conductivity
K_s<-2.82*10 # mm/day
# Campbell's b
b<-16.1501 # neurotheta Medium heavy clay
# van Genuchten parameters
# avg <- 0.0613
# nvg <- 1.086
# s_fc<-0.3936/n # Field capacity
# bubbling pressure
psi_s_bar<--1.4E-3 # I

# # Coarse sand
# n<-0.368 # porosity
# # more soil variables for evaporation & losses
# # Hydraulic conductivity
# K_s<-182.68*10 # mm/day
# # Campbell's b
# b<- 4.1152
# # van Genuchten parameters
# # avg <- 0.0712   # Soil hydraulic parameters for van Genuchtan function
# # nvg <- 1.392    # soil hydraulic papameters for van Genuchtan function
# # s_fc<-0.1895/n # Field capacity
# 
# psi_s_bar<--0.61E-3 # 
# h1bar =  -psi_s_bar 
# hb = psi_s_bar*-10^5 # mm

# 
# ### Loamy sand
# n<-0.37 # porosity
# # more soil variables for evaporation & losses
# # Hydraulic conductivity
# K_s<-175.3 # cm/day
# # Campbell's b
# b<-4.5206
# # Van Genuchten parameters
# # avg <- 0.0641
# # nvg <- 1.344
# # s_fc<-0.2098/n # Field capacity
# 
# psi_s_bar<--0.66E-3 # I

h1bar =  -psi_s_bar 
hb = psi_s_bar*-10^5 # mm

# groundwater depth is in the MC values
#Z =3000 # [mm] actual groundwater depth 

# parameters describing the soil
soilpar <- list(b = b, n = n, K_s = K_s, 
                psi_s_bar = psi_s_bar, h1bar = h1bar, hb = hb)
# Vegetation 1 (Grass)
# paspalum secateum F-I and R-I, 2004
# This is just an initial definition
Zr = 400 # soil depth (mm)   Check Also Table 2...Fernandez-Illescas and Rodriguez-Iturbe...2001
# parameters describing the root zone   
vegpar <- list(Zr = Zr)
#................................................


# ..........Other parameters ....................
# Initial definition of parameters describing plant dynamics and salt features
alpha_i=1 #maximum infiltration rate per day (K_s and therefore soil type dependency in balances function code)

k=12 # Saco et al, 2013
W0=0.2 # Saco et al, 2013
gmax=0.05 # Saco et al, 2013
k1=5 # Saco et al, 2013
c=10  # Saco et al, 2013
f= 1  # f is the soil salt leaching efficiency (whether some salt is retained)
ConcConst = 0.1 # ConcConst is the concentration of the salt in the infiltrating water in g/l
CM.gw = 0.1 # salt concentration in groundwater
d=0.24 # fraction of plant mortality

par <- list(alpha_i=alpha_i,k=k, W0=W0, gmax=gmax, k1=k1, c=c, f=f, ConcConst=ConcConst, CM.gw= CM.gw, d=d)
# ..............................................

# Develop a storage data frame
runs = 5000

par_MC <- as.data.frame(matrix(0,nrow=runs,ncol=nrow(MC_par), byrow=F))
colnames(par_MC) <- MC_par[,1]
set.seed(runs)

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
  par_MC[,3] <- rlnorm(runs,mu(0.24,0.1), sigma(0.24,0.01))
}


Store <- data.frame(par_MC,meanM = numeric(length=runs),sdM = numeric(length=runs),
                    meanSmM = numeric(length=runs),sdSmM = numeric(length=runs),
                    meanP = numeric(length=runs), sdP = numeric(length=runs),
                    meanCM = numeric(length=runs), sdCM = numeric(length=runs),
                    minCM = numeric(length=runs), maxCM = numeric(length=runs),
                    cum_flux = numeric(length=runs),Pzero=numeric(length=runs))

 Store_failure <- data.frame()


time <- 800

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

# SandyClayLoam_failure 
# Sand_failure<- Store_failure
