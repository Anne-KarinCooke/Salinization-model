R= 0.0821 # gas constant [ L atm mol-1 K-1 ]
Temp= 298 # temperature in Kelvin,  25 degrees Celsius
Mm= 58.44 # molar mass of NaCl in g/mol

#SOIL : Coarse Sand
b= 4.1152# Campbell's b
n = 0.368 # porosity
s_fc = 0.1895/n # Field capacity
K_s = 182.68*10 # cm/day to mm/day # Hydraulic conductivity
psi_s_bar = -0.61E-3 
h1bar =  -psi_s_bar 
hb = psi_s_bar*-10^4
 Ep=10
soilpar <- list(b = b, n = n, s_fc = s_fc, K_s = K_s, 
                psi_s_bar = psi_s_bar, h1bar = h1bar, hb = hb)
# vEG: GRASS
Zr = 40 # soil depth (cm)   Also Table 2...Fernandez-Illescas and Rodriguez-Iturbe...2001
c=0.4
vegpar <- list(Zr = Zr, c=c, Ep=Ep)

## OVERLAND FLOW parameter
cn= 1 #conversion factor cn
Mn= 10 #Manning's n, Mn
Sl = 0.01 # Slope Sl
gmax=0.05
k1=5

## Source functions
setwd("/home/anne-karin/Schreibtisch/Salinization updated")
source("Rainfall.R")
source("Leaching&Upflow.R")
source("G function (Eagelson 1978).R")
source("Infiltration function.R")
source("Vegetation functions.R")
source("Overland flow.R")



###### GRID CELLS
grids <- 3


#distance between the "buckets"
dist= 300
# cm GROUNDWATER DEPTH from the surface
Z<-c(1,2,3)
Z[1]<- 100 # cm 
Z[2]<- Z[1]-dist*sin(Sl)
Z[3]<- Z[2]-dist*sin(Sl) # downhill the distance to the groundwater becomes smaller, logically
Z

balances2D <- function(Rain, par=par_in,plotit=F,
                     soilpar,
                     vegpar,
                     f= 0.4, ConcConst = 0.1,  CM.gw = 5){
  
  # f is the soil salt leaching efficiency (whether some salt is retained)
  # ConcConst is the concentration of the salt in the infiltrating water
  # Storage vectors
  
  # Storage vectors for the daily steps are initialized.
  
  
  M <- matrix(nrow= length(Rain), ncol =grids)  # soil moisture [mm]
  h <- matrix(nrow= length(Rain), ncol =grids) # infiltration depth [mm]
  P <- matrix(nrow= length(Rain), ncol =grids) #biomass density []
  CM<- matrix(nrow= length(Rain), ncol =grids) # Salt concentration in soil water in g/L or g/mm
  SmI<- matrix(nrow= length(Rain), ncol =grids) # Salt mass in infiltrating water [g]
  SmM <- matrix(nrow= length(Rain), ncol =grids) # Salt mass in soil water [g]
  In <- matrix(nrow= length(Rain), ncol =grids) # infiltration [mm]
  # Leach <- rep(0,length(Rain)) # leaching [mm]
  Svir <- matrix(nrow= length(Rain), ncol =grids) # virtual saturation
  
  L<- matrix(nrow= length(Rain), ncol =grids)
  U<- matrix(nrow= length(Rain), ncol =grids)
  q<- matrix(nrow= length(Rain), ncol =grids) # Overland flow, runoff
  
  
  
  # We decided to split the numerical calculations for the daily into 12 substeps.
  
  deltat <- 12 # split in 12 increments
  

  
  # Storage vectors for the substeps are initialized.
  
  M_sub <- matrix(nrow= deltat, ncol =grids) # soil moisture
  h_sub <-matrix(nrow= deltat, ncol =grids)  # flow depth
  I_sub <- matrix(nrow= deltat, ncol =grids)  #Infiltration
  WU_sub <-matrix(nrow= deltat, ncol =grids)  # Water uptake in mm
  P_sub <- matrix(nrow= deltat, ncol =grids)  # Biomass density
  Gr_sub <- matrix(nrow= deltat, ncol =grids)  # Growth of biomass
  Mo_sub<- matrix(nrow= deltat, ncol =grids)  # Mortality of biomass
  SmI_sub <- matrix(nrow= deltat, ncol =grids)  #salt mass in infiltration water
  SmM_sub<- matrix(nrow= deltat, ncol =grids)  # salt mass in soil
  CM_sub<- matrix(nrow= deltat, ncol =grids) # salt concentration in soil 
  Svir_sub<- matrix(nrow= deltat, ncol =grids)  # virtual saturation
  L_sub<- matrix(nrow= deltat, ncol =grids)  # leaching
  U_sub<-matrix(nrow= deltat, ncol =grids)  # capillary upflow
  q_sub<-matrix(nrow= deltat, ncol =grids)  # overland flow  

  h.old<-rep(0,grids)
  P.old<-rep(0,grids)
  M.old<-rep(0,grids)
  SmI.old<-rep(0,grids)
  CM.old<-rep(0,grids)
  Svir.old<-rep(0,grids)

  L_salt<-rep(0,grids)
  U_salt<-rep(0,grids)

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
  
  for (t in 2:length(Rain)){
    
    for (tt in 1:(deltat-1)) {
      
      for (g in 1:grids) { #############################  loop for hillslope gridcells
      
      h.old[g] <- ifelse(tt==1,h[t-1,g],h_sub[tt,g])
      P.old[g] <- ifelse(tt==1,P[t-1,g],P_sub[tt,g])
      M.old[g] <- ifelse(tt==1,M[t-1,g],M_sub[tt,g])
      SmI.old[g] <-ifelse(tt==1,SmI[t-1,g],SmI_sub[tt,g])
      CM.old[g] <-ifelse(tt==1,CM[t-1,g],CM_sub[tt,g])
      Svir.old[g] <-ifelse(tt==1,Svir[t-1,g],Svir_sub[tt,g])
    
      # Balance for water depth on soil, h
      
      ifelse(g>1,h_sub[tt+1,g] <- h.old[g] + ifelse(tt==1,Rain[t]+q_sub[tt,g-1],q_sub[tt, g-1]) - Infil(h.old[g], P.old[g])*timeincr,
             h_sub[tt+1,g] <- h.old[g] + ifelse(tt==1,Rain[t],0) - Infil(h.old[g], P.old[g])*timeincr) ## g=1 is on top of the hill and does not receive any runoff from another cell
      

       # Overland flow, runoff
      
      ifelse(g>1, q_sub[tt,g]<- OF(cn=cn, Mn=Mn, h=h_sub[tt,g], Sl=Sl), q_sub[tt,g]<-0)  ## g=1 is on top of the hill and does not receive any runoff from another cell
      
      
      # Infiltration
      
      I_sub[tt,g] <- Infil(h.old[g], P.old[g])*timeincr
      
      #  1. Update soil moisture with infiltration
      
      M_sub[tt + 1,g] <- M.old[g] + I_sub[tt,g]      
      
      # Now do all plant uptake and growth
      # water uptake by plants: include infiltration in available water
      
      
      WU_sub[tt,g] <- WU(M_sub[tt + 1,g],P.old[g],par)*timeincr 
      
      # growth rate
      Gr_sub[tt,g] <- Gr(M_sub[tt + 1,g], P.old[g],par)*timeincr 
      # Mortality
      Mo_sub[tt,g]<- Mo(P.old[g], M=M.old[g], Svir=Svir.old[g], d=par_in$d)*timeincr
      # calculate plant biomass balance
      P_sub[tt + 1,g] <- P.old[g] + Gr_sub[tt,g]- Mo_sub[tt,g] 
      
      
      # re-calculate water balance
      # 2. before leaching
      M_sub[tt + 1,g] <- M.old[g] + I_sub[tt,g] - WU_sub[tt,g] #- L_sub[tt] 
      
      # Calculate salt concentration in the soil
      CM_sub[tt+1,g]<- SmM_sub[tt+1,g]/M_sub[tt + 1,g] 
      #browser()
      # 3. calculate leaching amount
      #       L_sub[tt] <- L(M_sub[tt+1], soilpar$K_s, soilpar$s_fc,soilpar$Zr,soilpar$n)*timeincr

      L_sub[tt,g]<-do.call(L_n,list(M=M_sub[tt + 1,g],Z=Z[g],P=P_sub[tt+1,g],soilpar=soilpar,vegpar=vegpar))$L
  
      U_sub[tt,g]<-do.call(L_n, list(M=M_sub[tt + 1,g],Z=Z[g],P=P_sub[tt+1,g],soilpar=soilpar,vegpar=vegpar))$U #soilpar=soilpar,vegpar=vegpar#       U_sub[tt,g]<-do.call(L_n,list(M=M_sub[tt + 1,g],Z=Z[g],P=P_sub[tt+1,g],soilpar=soilpar,vegpar=vegpar))$U       

      # 4. final adjust soil moisture for leaching
      M_sub[tt + 1,g] <- M_sub[tt + 1,g] - L_sub[tt,g]*timeincr + U_sub[tt,g]*timeincr  ## - divergence 
      
      # calculate saltbalance
      L_salt[g] <- CM.old[g]*L_sub[tt,g]*timeincr
      U_salt[g] <- CM.gw[g]*U_sub[tt,g]*timeincr
      
      SmI_sub[tt+1,g]<- SmI.old[g] + I_sub[tt,g]*ConcConst - L_salt[g] 
      SmM_sub[tt+1,g]<- SmI_sub[tt + 1,g] + U_salt[g]
      

      Svir_sub[tt + 1,g]<-soilpar$n*vegpar$Zr*((soilpar$h1bar)^(1/soilpar$b))*
        ((soilpar$h1bar)*(M_sub[tt + 1,g]/
                            (soilpar$n*vegpar$Zr))^(-soilpar$b)
         +(3.6*CM_sub[tt + 1,g]))^(-1/soilpar$b)
      
    } 
    
    # Aggregating the substep results to daily values.
    
    P[t,g] = P_sub[deltat,g]
    M[t,g] = M_sub[deltat,g]
    h[t,g] = h_sub[deltat,g]
    CM[t,g] = CM_sub[deltat,g]
    SmM[t,g] = SmI[t,g] = SmM_sub[deltat,g]
    In[t,g]= I_sub[deltat,g]
    L[t,g] = L_sub[deltat,g]
    U[t,g] = U_sub[deltat,g]
    Svir[t,g] = Svir_sub[deltat,g]
    q[t,g] = q_sub[deltat,g]
    
    
    }
  }
  
  # Plotting
  
  if (plotit==T) {  
    plot(M, type="l",ylim=c(0,100),xlim=c(0,time),xlab=("time [d]"), main=paste("lambda=", lambda[j],"alpha=", alpha[i]))
    points(Rain*10, type="h", col="skyblue")
    
    lines(Svir*10,type="l", col="blue")
    abline(h=0, col="Gray50",lwd=2,lty=2)
    
    lines(SmM,type="l", col="red")
    lines(CM,type="l", col="purple")
    lines(P,type="l", col="green")
    
    
    #  legend("topright",cex=1, pt.cex=0.4, c("Moisture [mm]","Rainfall [mm]*10","overland flow depth[mm] ","salt mass in soil water [g]", "salt concentration in soil water [g/l]", "Plant biomass density [g/m^2]"),
    #           col=c("black","skyblue","blue","red","purple","green"),lty=1)
    #  
    
  }
  
  Out <- data.frame(P=P,M=M,h=h, CM=CM, SmM=SmM, In=In, L=L, U=U, Svir=Svir)
  return(Out)
}

  
  
  
