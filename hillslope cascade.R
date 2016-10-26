# R= 0.0821 # gas constant [ L atm mol-1 K-1 ]
# Temp= 298 # temperature in Kelvin,  25 degrees Celsius
# Mm= 58.44 # molar mass of NaCl in g/mol

#SOIL :
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


## OVERLAND FLOW parameter
cn= 1 #conversion factor cn
Mn= 10 #Manning's n, Mn
Sl = 0.01 # Slope Sl



#................................................
# Vegetation 1 (Grass)
# paspalum secateum F-I and R-I, 2004

Zr = 400 # soil depth (mm)   Check Also Table 2...Fernandez-Illescas and Rodriguez-Iturbe...2001


# parameters describing the root zone   
vegpar <- list(Zr = Zr)


# parameters describing the soil
soilpar <- list(b = b, n = n, s_fc = s_fc, K_s = K_s, 
                psi_s_bar = psi_s_bar, h1bar = h1bar, hb = hb, cn=cn, Mn=Mn, Sl=Sl)
# parameters describing plant dynamics and salt features

alpha_i=1 #maximum infiltration rate per day, This needs to be a fraction of h (p117 Saco and Moreno-Las Heras) 

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


## Source functions
setwd("C:/Users/acoo7451/Desktop/hillslope model") 
source("Rainfall.R")
source("Infiltration.R")
source("Flux.R")
source("Vegetation functions.R")


OF<- function(h, soilpar){
  
  q=(soilpar$cn/soilpar$Mn)*h^(5/3)*soilpar$Sl^(1/2)
  
  return(q)
}


###### GRID CELLS
grids <- 3

### DIVERGENCE, diffusion
# Dm is the soil moisture diffusivity parameter
Dm = 0.27 # from Saco et al, 2013, in m^2/d
#distance between the "buckets" in mm
dist= 3000
# mm GROUNDWATER DEPTH from the surface
Z<-c(1,2,3)
Z[1]<- 1000 # mm 
Z[2]<- Z[1]-dist*sin(soilpar$Sl)
Z[3]<- Z[2]-dist*sin(soilpar$Sl) # downhill the distance to the groundwater becomes smaller, logically
Z

balances2D <- function(Rain, par,plotit=T,
                       soilpar,
                       vegpar){
  
  # f is the soil salt leaching efficiency (whether some salt is retained)
  # ConcConst is the concentration of the salt in the infiltrating water
  # Storage vectors
  
  # Storage vectors for the daily steps are initialized.
  
  
  M <- matrix(0,nrow= length(Rain), ncol =grids)  # soil moisture [mm]
  h <- matrix(0,nrow= length(Rain), ncol =grids) # infiltration depth [mm]
  P <- matrix(0,nrow= length(Rain), ncol =grids) #biomass density []
  CM<- matrix(0,nrow= length(Rain), ncol =grids) # Salt concentration in soil water in g/L or g/mm
  SmI<- matrix(0,nrow= length(Rain), ncol =grids) # Salt mass in infiltrating water [g]
  SmM <- matrix(0,nrow= length(Rain), ncol =grids) # Salt mass in soil water [g]
  In <- matrix(0,nrow= length(Rain), ncol =grids) # infiltration [mm]
  Svir <- matrix(0,nrow= length(Rain), ncol =grids) # virtual saturation
  flux<- matrix(0,nrow= length(Rain), ncol =grids)

  
  q<- matrix(0,nrow= length(Rain), ncol =grids) # Overland flow, runoff
  Diff<- matrix(0,nrow= length(Rain), ncol =grids) # divergence of soil moisture from one grid cells to the next
 # Overfl<- matrix(0,nrow= length(Rain), ncol =grids)
  # We decided to split the numerical calculations for the daily into 12 substeps.
  
  deltat <- 12 # split in 12 increments
  
  
  
  # Storage vectors for the substeps are initialized.
  
  M_sub <- matrix(0,nrow= deltat, ncol =grids) # soil moisture
  h_sub <-matrix(0,nrow= deltat, ncol =grids)  # flow depth
  I_sub <- matrix(0,nrow= deltat, ncol =grids)  #Infiltration
  WU_sub <-matrix(0,nrow= deltat, ncol =grids)  # Water uptake in mm
  P_sub <- matrix(0,nrow= deltat, ncol =grids)  # Biomass density
  Gr_sub <- matrix(0,nrow= deltat, ncol =grids)  # Growth of biomass
  Mo_sub<- matrix(0,nrow= deltat, ncol =grids)  # Mortality of biomass
  SmI_sub <- matrix(0,nrow= deltat, ncol =grids)  #salt mass in infiltration water
  SmM_sub<- matrix(0,nrow= deltat, ncol =grids)  # salt mass in soil
  CM_sub<- matrix(0,nrow= deltat, ncol =grids) # salt concentration in soil 
  Svir_sub<- matrix(0,nrow= deltat, ncol =grids)  # virtual saturation
 flux_sub<- matrix(0,nrow= deltat, ncol =grids)  # leaching

  
  q_sub<-matrix(nrow= deltat, ncol =grids)  # overland flow  
  Diff_sub<- matrix(nrow=deltat, ncol =grids) # divergence of soil moisture from one grid cells to the next
  
  h.old<-rep(0,grids)
  P.old<-rep(0,grids)
  M.old<-rep(0,grids)
  SmI.old<-rep(0,grids)
  CM.old<-rep(0,grids)
  Svir.old<-rep(0,grids)

  
  L_salt<-matrix(0,nrow= deltat, ncol =grids)
  U_salt<-matrix(0,nrow= deltat, ncol =grids)

  
  
  #initital 
  # M[1,1] <- 5
  # h[1,1] <- 10 
  # P[1,1] <- 10
  # CM[1,1]<- 0
  # Initial values to start the simulation.
  
  #   
  #   M[1,1,1] <- 5
  #   h[1,1,1] <- 10 
  #   P[1,1,1] <- 10
  #   CM[1,1,1]<- 0
  # Svir[1] <- s_fc
  
  
  timeincr= 1/deltat
 
 for (g in 1:grids) { #############################  loop for hillslope gridcells
  
  for (t in 2:length(Rain)){
    
    for (tt in 1:(deltat-1)) {
      
      
        
        h.old[g] <- ifelse(tt==1,h[t-1,g],h_sub[tt,g])
        P.old[g] <- ifelse(tt==1,P[t-1,g],P_sub[tt,g])
        M.old[g] <- ifelse(tt==1,M[t-1,g],M_sub[tt,g])
        SmI.old[g] <-ifelse(tt==1,SmI[t-1,g],SmI_sub[tt,g])
        CM.old[g] <-ifelse(tt==1,CM[t-1,g],CM_sub[tt,g])
        Svir.old[g] <-ifelse(tt==1,Svir[t-1,g],Svir_sub[tt,g])
      
        
        # Balance for water depth on soil, h
        
        ifelse(g>1,h_sub[tt+1,g] <- h.old[g] + ifelse(tt==1,Rain[t]+q_sub[tt,g-1],q_sub[tt, g-1]) - Infil(h.old[g], P.old[g],par)*timeincr,
               h_sub[tt+1,g] <- h.old[g] + ifelse(tt==1,Rain[t],0) - Infil(h.old[g], P.old[g],par)*timeincr) ## g=1 is on top of the hill and does not receive any runoff from another cell
        
        # Overland flow, runoff
        
        q_sub[tt,g]<-ifelse(g>1,OF(h=h_sub[tt,g],par),0)  ## g=1 is on top of the hill and does not receive any runoff from another cell
        
        
        # Infiltration
        ifelse(h_sub[tt+1,g]<soilpar$K_s, soilpar$alpha_i <- 1, soilpar$alpha_i<- (1-(h_sub[tt+1,g]-soilpar$K_s)/h_sub[tt+1,g]))
        
        I_sub[tt,g] <- Infil(h.old[g], P.old[g],par)*timeincr
        
        #  1. Update soil moisture with infiltration
        
        M_sub[tt + 1,g] <- M.old[g] + I_sub[tt,g] #+ Diff[tt,g-1]     # plus soil moisture diffusing from grid cell higher up  
        
        # Now do all plant uptake and growth
        # water uptake by plants: include infiltration in available water
        
        
        WU_sub[tt,g] <- WU(M_sub[tt + 1,g],P.old[g],par)*timeincr 
        
        # growth rate
        Gr_sub[tt,g] <- Gr(M=Svir.old[g], P.old[g],par)*timeincr 
        # Mortality
        Mo_sub[tt,g]<- Mo(P.old[g], M=M.old[g], Svir=Svir.old[g], par)*timeincr
        
        
        # calculate plant biomass balance
        P_sub[tt + 1,g] <- P.old[g] + Gr_sub[tt,g]- Mo_sub[tt,g] 
        
        
        # re-calculate water balance
        # 2. before leaching
        M_sub[tt + 1,g] <- M.old[g] + I_sub[tt,g] - WU_sub[tt,g] #- L_sub[tt] 
        

        
        # 3. calculate leaching and capillary rise amount
        flux_sub[tt+1,g]<-do.call(L_n,list(M=M_sub[tt+1,g],Z=Z[g],soilpar=soilpar,vegpar=vegpar))
        
    
        # Divergence, soil moisture diffusion
        Diff[tt,g]<- M_sub[tt+1,g]*(sqrt(Dm)*timeincr)*((Z[g+1]-Z[g])/dist)  
        
              
        # 4. final adjust soil moisture for leaching/rise AND DIVERGENCE
        M_sub[tt + 1,g] <- M_sub[tt + 1,g] + flux_sub[tt+1,g]*timeincr - Diff[tt,g] 
        
        
        # calculate saltbalance
  
        
        # Salt leaching
        L_salt[tt+1,g] <- ifelse(flux_sub[tt+1,g]<0, par$f*CM_sub[tt+1,g]*flux_sub[tt+1,g]*timeincr,0) # leaching of salt
        
        # salt uplfow
        U_salt[tt+1,g] <- ifelse(flux_sub[tt+1,g]>0, par$CM.gw*flux_sub[tt+1,g]*timeincr,0) # rise of salt
        
        # salt mass coming in with infiltration
        SmI_sub[tt+1,g]<- SmI.old[g] + I_sub[tt,g]*par$ConcConst 
        
        #salt mass in soil
        SmM_sub[tt+1,g] <- SmI_sub[tt+1,g] + U_salt[tt+1,g] - L_salt[tt+1,g]
        
        # salt concentration in soil
        CM_sub[tt+1,g]<- (SmM_sub[tt+1,g]/M_sub[tt+1,g])*(1/58.44)         
        
        # Virtual saturation (Shah et al., 2012), here in [mm] to be in the same unit as M
        Svir_sub[tt + 1,g]<-soilpar$n*vegpar$Zr*((soilpar$h1bar*10^-1)^(1/soilpar$b))*
          ((soilpar$h1bar*10^-1)*(M_sub[tt + 1,g]/
                              (soilpar$n*vegpar$Zr))^(-soilpar$b)
           +(3.6*CM_sub[tt + 1,g]))^(-1/soilpar$b)
        
        
        
     
      } 
      
      # Aggregating the substep results to daily values.
      
      P[t,g] = P_sub[deltat,g]
      M[t,g] = M_sub[deltat,g]
      h[t,g] = h_sub[deltat,g]
      CM[t,g] = CM_sub[deltat,g]
#      SmM[t,g] = SmI[t,g] = SmM_sub[deltat,g]  ###
      SmI[t,g] = SmI_sub[deltat,g]
      SmM[t,g] = SmM_sub[deltat,g]
      In[t,g]= sum(I_sub)
      Svir[t,g] = Svir_sub[deltat,g]
      flux[t,g]= sum(flux_sub[deltat,g])
      q[t,g] = q_sub[deltat,g]

      
      
    }
  }
  
  # Plotting
  
  if (plotit==T) {  
    plot(M[g], type="l",ylim=c(0,100),xlim=c(0,time),xlab=("time [d]"), main=paste("lambda=", lambda[j],"alpha=", alpha[i], "grid=", grids[g]))
    points(Rain*10, type="h", col="skyblue")
    
    abline(h=0, col="Gray50",lwd=2,lty=2)
    
    lines(SmM[g],type="l", col="red")
    lines(CM[g],type="l", col="purple")
    lines(P[g],type="l", col="green")
    
    
    #  legend("topright",cex=1, pt.cex=0.4, c("Moisture [mm]","Rainfall [mm]*10","overland flow depth[mm] ","salt mass in soil water [g]", "salt concentration in soil water [g/l]", "Plant biomass density [g/m^2]"),
    #           col=c("black","skyblue","blue","red","purple","green"),lty=1)
    #  
    
  }
  
  Out <- data.frame(P=P,M=M,h=h, CM=CM, SmM=SmM, In=In, flux=flux, Svir=Svir)
  return(Out)
}


  