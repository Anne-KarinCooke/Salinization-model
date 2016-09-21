balances <- function(Rain, par=par_in,plotit=F,
                      soilpar,
                      vegpar,
                      f= 0.4, ConcConst = 0.1,  CM.gw = 5){
   
   # f is the soil salt leaching efficiency (whether some salt is retained)
   # ConcConst is the concentration of the salt in the infiltrating water
   # Storage vectors


# Storage vectors for the daily steps are initialized.


   M <- rep(0,length(Rain)) # soil moisture [mm]
   h <- rep(0,length(Rain)) # infiltration depth [mm]
   P <- rep(0,length(Rain)) #biomass density []
   CM<- rep(0,length(Rain)) # Salt concentration in soil water in g/L or g/mm
   SmI<- rep(0,length(Rain)) # Salt mass in infiltrating water [g]
   SmM <- rep(0,length(Rain)) # Salt mass in soil water [g]
   In <- rep(0,length(Rain)) # infiltration [mm]
# Leach <- rep(0,length(Rain)) # leaching [mm]
   Svir <- rep(0,length(Rain)) # virtual saturation
  
   L<- rep(0,length(Rain))
   U<- rep(0,length(Rain))


# Initial values to start the simulation.

   
   M[1] <- 3
   h[1] <- 10 
   P[1] <- 3
   CM[1]<- 0
   Svir[1] <- M[1]


# We decided to split the numerical calculations for the daily into 12 substeps.

   deltat <- 12 # split in 12 increments

  
# Storage vectors for the substeps are initialized.

   M_sub <- rep(0,deltat)
   h_sub <- rep(0,deltat)
   I_sub <- rep(0,deltat)
   #Q_sub <- rep(0,deltat)
   WU_sub <-rep(0,deltat) # Water uptake in mm
   P_sub <- rep(0,deltat) 
   Gr_sub <- rep(0,deltat) # Growth of biomass
   Mo_sub<- rep(0,deltat) # Mortality of biomass
   SmI_sub <- rep(0,deltat) 
   SmM_sub<- rep(0,deltat) 
   CM_sub<- rep(0,deltat) 
   # Posm_bar<- rep(0,deltat) # osmotic pressure of soil water in bar
   # fosm<- rep(0,deltat) # "osmotic factor", reduces available soil water to plant water uptake
   L_sub <- rep(0,deltat) # leaching
  Svir_sub <- rep(0,deltat) # virtual saturation

   L_sub<-rep(0,deltat) # calculates leakage loss without evaporation loss
   U_sub<-rep(0,deltat)

  
   timeincr= 1/deltat
   
  for (t in 2:length(Rain)){
    
    for (tt in 1:(deltat-1)) {
      
      h.old <- ifelse(tt==1,h[t-1],h_sub[tt])
      P.old <- ifelse(tt==1,P[t-1],P_sub[tt])
      M.old <- ifelse(tt==1,M[t-1],M_sub[tt])
      SmI.old <-ifelse(tt==1,SmI[t-1],SmI_sub[tt])
      CM.old <-ifelse(tt==1,CM[t-1],CM_sub[tt])
     Svir.old <-ifelse(tt==1,Svir[t-1],Svir_sub[tt])

# The impact of the the osmotic pressure is calculated using Van't Hoff's law. For that, molarity is calculated dividing the 
# concentration by the molar mass of NaCl (58.44 g/mol). The osmotic pressure is the product of molarity, universal gas constant and temperature in Kelvin.
# The wilting point is found at an pressure of -15 bar. 
# Between 0 and 15 bar, an "osmotic factor", scaled from 1 to 0, is added to the Water uptake function. At fosm = 0, not water uptake by plants is possoble.
#       Posm_bar[tt]= ((CM.old/Mm)*R*Temp)*1.01325
#       ifelse(Posm_bar[tt]<=15,fosm[tt] <- 1-(Posm_bar[tt]/15), fosm[tt] <- 0)    #fosm, osmotic factor

# Balance for water depth on soil

      h_sub[tt+1] <- h.old + ifelse(tt==1,Rain[t],0) - Infil(h.old, P.old)*timeincr

# Infiltration

      I_sub[tt] <- Infil(h.old, P.old)*timeincr

#  1. Update soil moisture with infiltration

      M_sub[tt + 1] <- M.old + I_sub[tt]      
      
# Now do all plant uptake and growth
# water uptake by plants: include infiltration in available water
      WU_sub[tt] <- WU(M=Svir_sub[tt + 1],P.old,par)*timeincr 
      
      # growth rate
      Gr_sub[tt] <- Gr(M_sub[tt + 1], P.old,par)*timeincr 
      # Mortality
      Mo_sub[tt]<- Mo(P.old, M=M.old, Svir=Svir.old, d=par_in$d)*timeincr
      # calculate plant biomass balance
      P_sub[tt + 1] <- P.old + Gr_sub[tt]- Mo_sub[tt] 
      
      
# re-calculate water balance
# 2. before leaching
      M_sub[tt + 1] <- M.old + I_sub[tt] - WU_sub[tt] #- L_sub[tt] 

# Calculate salt concentration in the soil
      CM_sub[tt+1]<- SmM_sub[tt+1]/M_sub[tt + 1] 
#browser()
      # 3. calculate leaching amount
#       L_sub[tt] <- L(M_sub[tt+1], soilpar$K_s, soilpar$s_fc,soilpar$Zr,soilpar$n)*timeincr
#    L_sub[tt] <- L_n(M=M_sub[tt],Z=Z,soilpar=soilpar,vegpar=vegpar)$L
   L_sub[tt]<-do.call(L_n,list(M = M_sub[tt + 1], P = P_sub[tt + 1], Z = Z,
                               soilpar = soilpar,vegpar = vegpar))$L 
   
   U_sub[tt]<-do.call(L_n,list(M = M_sub[tt + 1],P = P_sub[tt + 1], Z = Z,
                               soilpar = soilpar, vegpar = vegpar))$U
   # U_sub[tt]<-do.call(L_n,list(M=M_sub[tt + 1],Z=Z,soilpar=soilpar,vegpar=vegpar))$U
      # 4. final adjust soil moisture for leaching
       M_sub[tt + 1] <- M_sub[tt + 1] - L_sub[tt]*timeincr + U_sub[tt]*timeincr

# calculate saltbalance
    L_salt <- CM.old*L_sub[tt]*timeincr
    U_salt <- CM.gw*U_sub[tt]*timeincr
       
      SmI_sub[tt+1]<- SmI.old + I_sub[tt]*ConcConst - L_salt
      SmM_sub[tt+1]<- SmI_sub[tt + 1] 
       
#       SmGW_sub[tt+1] <- SmGW.old + L_salt - U_salt
      

#browser()
    # I have now scaled Svir to M  
    Svir_sub[tt + 1]<-soilpar$n*vegpar$Zr*((soilpar$h1bar)^(1/soilpar$b))*
                     ((soilpar$h1bar)*(M_sub[tt + 1]/
                       (soilpar$n*vegpar$Zr))^(-soilpar$b)
                       +(3.6*CM_sub[tt + 1]))^(-1/soilpar$b)

    } 
  
# Aggregating the substep results to daily values.

    P[t] = P_sub[deltat]
    M[t] = M_sub[deltat]
    h[t] = h_sub[deltat]
    CM[t] = CM_sub[deltat]
    SmM[t] = SmI[t] = SmM_sub[deltat]
    In[t]= I_sub[deltat]
    L[t] = L_sub[deltat]
    U[t] = U_sub[deltat]
    Svir[t] = Svir_sub[deltat]

}


# Plotting
 
if (plotit==T) {  
  plot(M, type="l",ylim=c(0,100),xlim=c(0,time),xlab=("time [d]"), main="with salt") # main=paste("lambda=", lambda[j],"alpha=", alpha[i]))
  points(Rain*10, type="h", col="skyblue")
   
    lines(Svir,type="l", col="blue")
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
