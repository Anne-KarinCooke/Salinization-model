# Plant water uptake
WU <- function(M,P,par){ 
  # using Svir in here means scaling Svir back to M, easier to do at Svir in balances  
  #  WU=par$gmax*((M*(1+Svir))/(((M*(1+Svir))+par$k1)))*P 
  WU=par$gmax*(M/((M+par$k1)))*P 
  
  return(WU)
}

#Plant Growth

Gr <- function(M,P,par) { 
  
  Gr = par$c*WU(M,P,par)
  
  return(Gr)
}


## Plant mortality function WITH SALT INFLUENCE, WITH VIRTUAL SATURATION

Mo <- function(P,M,Svir,par) {
  # needs to be M/Svir because both are "large" numbers
  # you want a number ~1 for multiplication, or <0.1 for addition
  Mo = P*(par$d*(M/Svir))
  
  return(Mo)
  
}