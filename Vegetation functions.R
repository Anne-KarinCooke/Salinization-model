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