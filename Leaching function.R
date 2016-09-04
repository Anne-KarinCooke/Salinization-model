# ####Leaching function ########################################################################
# 
L <- function(M,K_s,s_fc,Zr_in=Zr,n_1=n) {
  
  L=M/(Zr_in*n_1)*exp(-((s_fc-M/(Zr_in*n_1))*(K_s/Zr_in*n_1)))
  
  return(L)
}

# L <- function(M,K_s,beta=12, s_fc,Zr_in=Zr,n_1=n) {
#   
#   L=(K_s/(exp(beta*(1-s_fc)))-1)
#     
#   M/(Zr_in*n_1)*
#   
#   return(L)
# }

# M/(Zr_in*n_1)*(1/K_s*exp(-((s_fc-M/(Zr_in*n_1)))))


# L <- function(M,K_s,s_fc,Zr_in=Zr,n_1=n) {
#   
#   L=M/(Zr_in*n_1)/(1+K_s*exp(-((s_fc*M/(Zr_in*n_1)))))
#   
#   return(L)
# }

# L <- function(M,K_s,s_fc,Zr_in=Zr,n_1=n) {
#   
#   L=M/(Zr_in*n_1)*exp(-(K_s*(s_fc*M/(Zr_in*n_1))))
#   
#   return(L)
# }
# 
# # test L
#  M <- seq(0,Zr*n,length=100)
#  plot(M,L(M,K_s,s_fc=0.9))
#  lines(M,L(M,K_s,s_fc=1))
#  lines(M,L(M,K_s,s_fc=0.75),col="red")

