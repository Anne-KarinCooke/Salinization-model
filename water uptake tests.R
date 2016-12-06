
k=12 # Saco et al, 2013
W0=0.2 # Saco et al, 2013

gmax=0.04# Saco et al, 2013
k1=100 # Saco et al, 2013


c=10  # Saco et al, 2013
f= 1  # f is the soil salt leaching efficiency (whether some salt is retained)
ConcConst = 0.1 # ConcConst is the concentration of the salt in the infiltrating water in g/l
CM.gw = 0.1 # salt concentration in groundwater
d=0.24 # fraction of plant mortality

par <- list(alpha_i=alpha_i,k=k, W0=W0, gmax=gmax, k1=k1, c=c, f=f, ConcConst=ConcConst, CM.gw= CM.gw, d=d)

WU <- function(M,P,par){ 
  # using Svir in here means scaling Svir back to M, easier to do at Svir in balances  
  #  WU=par$gmax*((M*(1+Svir))/(((M*(1+Svir))+par$k1)))*P 
  WU=par$gmax*(M/((M+par$k1)))*P 
  
  return(WU)
}

M <- seq(0,100,1)
# P <-seq(0,100,1)
P=10
Wup<- WU(M,P,par=par)
plot(M,Wup)
Wup2<- WU(M,P=20,par=par)
lines(Wup2, col="red")
Wup3<- WU(M,P=5,par=par)
lines(Wup3, col='green')

Wup4<- WU(M,P=1,par=par)
lines(Wup4)

