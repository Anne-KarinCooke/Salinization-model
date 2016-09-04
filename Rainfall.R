# Rainfall function
Precip <- function(time,alpha,lambda,delta) {
  # generate a vector of times between rainfall events > delta
  f_P<-floor(rexp(time,lambda*exp(-delta/alpha))) # vector of times between rainfall occurrences (equation 4 & 8)
  # generate a binary vector from this (impulse function)
  binary.vec <- unlist(lapply(1:time,function(i) c(rep(0,f_P[i]),1)))
  R <- rexp(length(binary.vec),1/alpha)*binary.vec 
  return(R[1:time])
}
