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
# Sandy Clay Loam
n<-0.367 # porosity
# more soil variables for evaporation & losses
# Hydraulic conductivity
K_s<-52.08*10 # mm/day
# campbell's b
b<-6.4069 # neurotheta sandy clay loam
# van Genuchten parameters
#     avg <- 0.0521
#     nvg <- 1.237
s_fc<-0.2677/n # Field capacity
# This is the bubbling pressure
psi_s_bar<--1.2E-3 #
h1bar =  -psi_s_bar 
hb = psi_s_bar*-10^5 # mm

# groundwater depth is in the MC values
#Z =3000 # [mm] actual groundwater depth 

# parameters describing the soil
soilpar <- list(b = b, n = n, s_fc = s_fc, K_s = K_s, 
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
f= 0.8  # f is the soil salt leaching efficiency (whether some salt is retained)
ConcConst = 0.1 # ConcConst is the concentration of the salt in the infiltrating water in g/l
CM.gw = 0.1 # salt concentration in groundwater
d=0.24 # fraction of plant mortality

par <- list(alpha_i=alpha_i,k=k, W0=W0, gmax=gmax, k1=k1, c=c, f=f, ConcConst=ConcConst, CM.gw= CM.gw, d=d)
# ..............................................

# Develop a storage data frame
runs = 10

par_MC <- as.data.frame(matrix(0,nrow=runs,ncol=nrow(MC_par), byrow=F))
colnames(par_MC) <- MC_par[,1]
set.seed(runs)
for (i in 1:nrow(MC_par)) {
  par_MC[,i] <- runif(runs,MC_par[i,2],MC_par[i,3])
}

Store <- data.frame(par_MC,meanM = numeric(length=runs),sdM = numeric(length=runs),
                    meanSmM = numeric(length=runs),sdSmM = numeric(length=runs),
                    meanP = numeric(length=runs), sdP = numeric(length=runs),
                    meanCM = numeric(length=runs), sdCM = numeric(length=runs),
                    minCM = numeric(length=runs), maxCM = numeric(length=runs),
                    cum_flux = numeric(length=runs),Pzero=numeric(length=runs))
time <- 600
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
 Store$meanM[j] <- mean(result$M[200:600]) 
  Store$sdM[j] <- sd(result$M[200:600]) 
 
 
 # mean and standard deviation of SOIL SALT MASS
 Store$meanSmM[j] <- mean(result$SmM[200:600]) 
 Store$sdSmM[j] <- sd(result$SmM[200:600]) 
 
 
 # mean and standard deviation of PLANT BIOMASS
 Store$meanP[j] <- mean(result$P[200:600]) 
 Store$sdP[j] <- sd(result$P[200:600]) 
 #events where P (plant biomass) hits 0, plants die
 Store$Pzero[j] <- ifelse(any(result$P[200:600]==0),1,0)
 
 
 # mean, standard deviation, minima and maxima of SOIL SALT CONCENTRATION
 Store$meanCM[j] <- mean(result$CM[200:600]) 
 Store$sdCM[j] <- sd(result$CM[200:600])
 Store$minCM[j] <- min(result$CM[200:600])
 Store$maxCM[j] <- max(result$CM[200:600])
 
 
 # sum of cumulative water flux
 Store$cum_flux[j] <- sum(result$flux[200:600]) 

}
)

