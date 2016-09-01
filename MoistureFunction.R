
###############################################################
#Constants
R= 0.0821 # gas constant [ L atm mol-1 K-1 ]
Temp= 298 # temperature in Kelvin,  25 degrees Celsius
Mm= 58.44 # molar mass of NaCl in g/mol


# ### SOIL INPUT - medium heavy Clay
# n<-0.4473
# s_fc<-0.3936/n # Field capacity
# # Hydraulic conductivity
# K_s<-2.82*10 # mm/day

# Coarse Sand
n<-0.368 # porosity
s_fc<-0.1895/n # Field capacity
# Hydraulic conductivity
K_s<-182.68*10 # cm/day


# root depth
Zr=40 #cm



#### Salt leaching parameters
# f is the  leaching efficiency of the soil, has to be calibrated from field data 

f= 0.4 # between 0 and 1, here it is just an assumption to start with


# INFILTRATION FUNCTION ##########################################################################

Infil <- function(h,P, alpha_i=0.25, k=12, W0=0.2){
  
  I=alpha_i*h*(P+k*W0)/(P+k)
  
  return (I)
}


##### WATER UPTAKE FUNCTION#####################################################################


par_in <- list(gmax=0.05, k1=5)
WU <- function(M,P,fosm, par=par_in){ 
  
  WU=par$gmax*((M*fosm)/((M*fosm)+par$k1))*P
  
  return(WU)
}


#### PLANT BIOMASS FUNCTION ####################################################################

par_in$c=10

G <- function(M,P,fosm,par=par_in) {
  
  G = par$c*WU(M,P,fosm,par)
  
  return(G)
}


#### Mortality function ################################################################

Mo <- function(P,d=0.24) {
  
  Mo = P*d
  
  return(Mo)
}

####Leaching function ########################################################################

L <- function(M,K_s,s_fc,Zr_in=Zr,n_1=n) {
  browser()
  L=M/(Zr_in*n_1)*exp(-(s_fc-M/(Zr_in*n_1))*K_s)
  
  return(L)
}

# test L
 M <- seq(0,Zr*n,length=100)
 plot(M,L(M,K_s,s_fc=0.9))
 lines(M,L(M,K_s,s_fc=1))
 lines(M,L(M,K_s,s_fc=0.75),col="red")

###########################################################################################

 balances <- function(Rain, par=par_in,plotit=F,
                      soilpar=list(n=n,Zr=Zr, s_fc=s_fc, K_s=K_s),
                      f= 0.4, ConcConst = 2) {
   
   # f is the soil salt leaching efficiency (whether some salt is retained)
   # ConcConst is the concentration of the salt in the infiltrating water
   # Storage vectors
   M <- rep(0,length(Rain))
   h <- rep(0,length(Rain))
   #Q <- rep(0,length(Rain))
   P <- rep(0,length(Rain))
   CM<- rep(0,length(Rain))
   SmI<- rep(0,length(Rain))
   SmM <- rep(0,length(Rain))
   In <- rep(0,length(Rain))
   Leach <- rep(0,length(Rain)) # leaching
   
   # Initial values # changed a little bit, they seemed to high in earlier code examples...
   M[1] <- 5
   h[1] <- 10 
   P[1] <- 10
   CM[1]<- 0
   
   deltat <- 12 # split in 12 increments
   
   
   M_sub <- rep(0,deltat)
   h_sub <- rep(0,deltat)
   I_sub <- rep(0,deltat)
   #Q_sub <- rep(0,deltat)
   WU_sub <-rep(0,deltat)
   P_sub <- rep(0,deltat)
   G_sub <- rep(0,deltat)
   Mo_sub<- rep(0,deltat)
   SmI_sub <- rep(0,deltat) # Sm = salt mass in g in infiltration water
   SmM_sub<- rep(0,deltat) # Sm = salt mass in g in soil water
   CM_sub<- rep(0,deltat) # Salt concentration in soil water in g/L or g/mm
   Posm_bar<- rep(0,deltat) # osmotic pressure of soil water in bar
   fosm<- rep(0,deltat) # "osmotic factor", reduces available soil water to plant water uptake
   L_sub <- rep(0,deltat) # leaching
   
  
   timeincr= 1/deltat
   
   
  
  for (t in 2:length(Rain)){
    
    for (tt in 1:(deltat-1)) {
      browser()
      
      h.old <- ifelse(tt==1,h[t-1],h_sub[tt])
      P.old <- ifelse(tt==1,P[t-1],P_sub[tt])
      M.old <- ifelse(tt==1,M[t-1],M_sub[tt])
      SmI.old <-ifelse(tt==1,SmI[t-1],SmI_sub[tt])
      CM.old <-ifelse(tt==1,CM[t-1],CM_sub[tt])

      # calculating the osmotic pressure
      #       Molarity = (CM_sub/Mm) # Molarity of NaCl, (g/L*mol/g) = mol/L or mol/mm
      #       Posm_atm = Molarity*R*K # osmotic pressure in atm, Van't Hoff's law
      #       Posm_bar = Posm_atm*1.01325# osmotic pressure in bar  
      Posm_bar[tt]= ((CM.old/Mm)*R*Temp)*1.01325
      # Wilting point at 15 bar 
      #osmotic factor, fosm
      ifelse(Posm_bar[tt]<=15,fosm[tt] <- 1-(Posm_bar[tt]/15), fosm[tt] <- 0)   
      
      # balance for water depth on soil
      h_sub[tt+1] <- h.old + ifelse(tt==1,Rain[t],0) - Infil(h.old, P.old)*timeincr
      # Infiltration
      I_sub[tt] <- Infil(h.old, P.old)*timeincr
      # runoff need rethink this     
      #Q_sub <- ifelse(tt==1,Rain[t]-I_sub[tt],0)

      # 1. Update soil moisture with infiltration
      M_sub[tt + 1] <- M.old + I_sub[tt]      
      
      # Now do all plant uptake and growth
      # water uptake by plants: include infiltration in available water
      WU_sub[tt] <- WU(M_sub[tt+1],P.old,fosm[tt],par)*timeincr
      
      # growth rate
      G_sub[tt] <- G(M_sub[tt+1],P.old,fosm[tt],par)*timeincr
      # Mortality
      Mo_sub[tt]<- Mo(P.old)*timeincr
      # calculate plant biomass balance
      P_sub[tt + 1] <- P.old + G_sub[tt]- Mo_sub[tt] 
      
      # re-calculate water balance
      # 2. before leaching
      M_sub[tt + 1] <- M.old + I_sub[tt] - WU_sub[tt] #- L_sub[tt] 
      # 3. calculate leaching amount
      L_sub[tt] <- L(M_sub[tt+1], soilpar$K_s, soilpar$s_fc,soilpar$Zr,soilpar$n)*timeincr
      # 4. final adjust soil moisture for leaching
       M_sub[tt + 1] <- M_sub[tt+1] - L_sub[tt]
      
      # calculate saltbalance
      L_salt <- CM.old*L_sub[tt]
      SmI_sub[tt+1]<- SmI.old + I_sub[tt]*ConcConst - L_salt
      # this line is actually superfluous
      SmM_sub[tt+1] <- SmI_sub[tt+1] 
      

    ### Calculate salt concentration in the soil
      CM_sub[tt+1]<- SmM_sub[tt+1]/M_sub[tt+1]

# from:
# Ritzema, H. P. Drainage principles and applications. 
# No. Ed. 2. International Institute for Land Reclamation and Improvement (ILRI), 1994.
      

    } 
    
    #Q[t] <- Q_sub
    P[t] = P_sub[deltat]
    M[t] = M_sub[deltat]
    h[t] = h_sub[deltat]
    CM[t] = CM_sub[deltat]
    # note the added SmI[t]
    SmM[t] = SmI[t] = SmM_sub[deltat]
    In[t]= I_sub[tt]
    Leach[t] = L_sub[tt]

}


if (plotit==T) {  
  plot(M, type="l",ylim=c(0,30),xlim=c(0,time),main=paste("lambda=", lambda[j],"alpha=", alpha[i]))
  points(Rain*10, type="h", col="skyblue")
  #   lines(Rain*10, type="h", col="deepskyblue", xlim=c(0,time), ylim=c(0,150), xlab="time", ylab="State variable" )
#      lines(Leach,type="l", col="orange")
#      lines(h,type="l", col="green")
     abline(h=0, col="Gray50",lwd=2,lty=2)
  
#   lines(Q,type="l", col="darkblue")
    lines(P,type="l", col="green")
  lines(SmM,type="l", col="red")
  lines(CM,type="l", col="purple")
  
  
  # legend("topleft",c("Moisture","Rainfall","Infiltration","overland depth","salt concentration in soil water"),
         # col=c("black","skyblue","darkblue","green","red"),lty=1)#,"skyblue"),lty=1)
}
Out <- data.frame(P=P,M=M,h=h, CM=CM, SmM=SmM, In=In, Leach=Leach)
return(Out)
}







