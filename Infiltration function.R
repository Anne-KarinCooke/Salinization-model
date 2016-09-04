# INFILTRATION FUNCTION ##########################################################################

Infil <- function(h,P, alpha_i=0.25, k=12, W0=0.2){
  
  I=alpha_i*h*(P+k*W0)/(P+k)
  
  return (I)
}