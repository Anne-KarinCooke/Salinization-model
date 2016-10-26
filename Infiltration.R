Infil <- function(h,P, par){
  
  I=par$alpha_i*h*(P+par$k*par$W0)/(P+par$k)
  
  return (I)
}