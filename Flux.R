# vertical flux 


L_n <- function(M,Z,soilpar,vegpar) {
  Zr <- vegpar$Zr
  hb <- - soilpar$psi_s_bar*10^5 
  soilpar$s_fc <- (Z/hb)^(-1/soilpar$b)
  
  s=M/(soilpar$n*vegpar$Zr)
  
  psi = hb*s^-soilpar$b
  

  m=2 + 3/soilpar$b # in Salvucci's paper it is called n, but I called it m here to not confuse it with porosity

  
  qf <-((Z/hb)^(-m)-(psi/hb)^(-m))/(1+(psi/hb)^(-m)+(m-1)*(Z/hb)^(-m))

  # Mass flux

  flux <- soilpar$K_s*qf

  
  return(flux)
}