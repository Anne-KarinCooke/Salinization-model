##### WATER UPTAKE FUNCTION#####################################################################


par_in <- list(gmax=0.05, k1=5)
WU <- function(M,P,par=par_in){ 
# using Svir in here means scaling Svir back to M, easier to do at Svir in balances  
#  WU=par$gmax*((M*(1+Svir))/(((M*(1+Svir))+par$k1)))*P 
  WU=par$gmax*(M/((M+par$k1)))*P 
  
  return(WU)
}


#### PLANT BIOMASS FUNCTION ####################################################################

par_in$c=10

Gr <- function(M,P,par=par_in) { 
  
  Gr = par$c*WU(M,P,par)
  
  return(Gr)
}


#### Mortality function ################################################################

#Mo <- function(P,d=0.24) {
#  Mo = P*d
 # return(Mo)
#}
Mo <- function(P,M,Svir,d=0.1) {
  # needs to be M/Svir because both are "large" numbers
  # you want a number ~1 for multiplication, or <0.1 for addition
  Mo = P*(d*(M/Svir))
  
  return(Mo)
}
