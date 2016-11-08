
# ------------------------------------------
# SOIL FUNCTION
# Soil data as a function
# To simplify soil input
# ########################################
Soil <- function(stype) {
  # wilting point
  psi_sh<--10
  soil <- stype
  if (soil == "L Med Clay") {
    # Medium Light Clay
    n<-0.418 # porosity
    # more soil variables for evaporation & losses
    K_s<-3.51 # cm/day
    b<-13.48 # neurotheta LMC
    nvg <- 1.089
    avg <- 0.0591
    s_fc<-0.364/n # Field capacity
    # This is the bubbling pressure
    psi_s_bar<--1.5E-3 # IS IN HERE SO DON'T NEED TO DEFINE IN THE VEG FILES
    
  }
  
  if (soil == "S Clay Loam") {
    # Sandy Clay Loam
    n<-0.367 # porosity
    # more soil variables for evaporation & losses
    # Hydraulic conductivity
    K_s<-52.08 # cm/day
    # campbell's b
    b<-6.4069 # neurotheta sandy clay loam
    # van Genuchten parameters
    avg <- 0.0521
    nvg <- 1.237
    s_fc<-0.2677/n # Field capacity
    # This is the bubbling pressure
    psi_s_bar<--1.2E-3 # IS IN HERE SO DON'T NEED TO DEFINE IN THE VEG FILES
  }
  
  if (soil == "Loamy Sand") {
    # Loamy sand
    n<-0.37 # porosity
    # more soil variables for evaporation & losses
    # Hydraulic conductivity
    K_s<-175.3 # cm/day
    # Campbell's b
    b<-4.5206
    # Van Genuchten parameters
    avg <- 0.0641
    nvg <- 1.344
    s_fc<-0.2098/n # Field capacity
    
    psi_s_bar<--0.66E-3 # IS IN HERE SO DON'T NEED TO DEFINE IN THE VEG FILES
    # This is the bubbling pressure
  }
  
  if (soil == "H Clay") {
    # Medium Heavy Clay
    n<-0.4473 # porosity
    # more soil variables for evaporation & losses
    # Hydraulic conductivity
    K_s<-2.82 # cm/day
    # Campbell's b
    b<-16.1501 # neurotheta Medium heavy clay
    # van Genuchten parameters
    avg <- 0.0613
    nvg <- 1.086
    s_fc<-0.3936/n # Field capacity
    # bubbling pressure
    psi_s_bar<--1.4E-3 # IS IN HERE SO DON'T NEED TO DEFINE IN THE VEG FILES

  }
  
  if (soil == "M Clay") {
    # Medium Clay
    n<-0.4391 # porosity
    # more soil variables for evaporation & losses
    # hydraulic conductivity
    K_s<-6.04 # cm/day
    # Campbell's b from neurotheta
    b<- 13.5127
    # Van Genuchten parameters
    avg <- 0.0507
    nvg <- 1.088
    s_fc<-0.3818/n # Field capacity
    # This is the bubbling pressure
    psi_s_bar<--1.75E-3 # IS IN HERE SO DON'T NEED TO DEFINE IN THE VEG FILES
  }
  if (soil == "C Sand") {
    # Coarse Sand
    n<-0.368 # porosity
    # more soil variables for evaporation & losses
    # Hydraulic conductivity
    K_s<-182.68 # cm/day
    # Campbell's b
    b<- 4.1152
    # van Genuchten parameters
    avg <- 0.0712   # Soil hydraulic parameters for van Genuchtan function
    nvg <- 1.392    # soil hydraulic papameters for van Genuchtan function
    s_fc<-0.1895/n # Field capacity
    
    psi_s_bar<--0.61E-3 # IS IN HERE SO DON'T NEED TO DEFINE IN THE VEG FILES
    # This is the bubbling pressure
  }
#   if (soil == "HC") {   ## Soil types used for Sodicity paper
#     # Coarse Sand
#     n<-0.42 # porosity
#     # more soil variables for evaporation & losses
#     # Hydraulic conductivity
#     K_s<-5 # cm/day
#     # Campbell's b using neurotheta
#     b<- 13.5
#     # van Genuchten parameters
#     nvg <- 1.089   # Soil hydraulic parameters for van Genuchtan function
#     avg <- 0.0591  # Soil hydraulic parameters for van Genuchtan function
#     s_fc<-0.364/n # Field capacity
#     # This is the bubbling pressure
#     psi_s_bar<--1.5E-3 # IS IN HERE SO DON'T NEED TO DEFINE IN THE VEG FILES
#   }
#   if (soil == "LC") {   ## Soil types used for Sodicity paper (Shah et al. 2013)
#     # Coarse Sand
#     n<-0.42 # porosity
#     # more soil variables for evaporation & losses
#     # Hydraulic conductivity
#     K_s<-3.5 # cm/day
#     # Campbell's b using neurotheta
#     b<- 16
#     # van Genuchten parameters
#     nvg <- 1.089   # Soil hydraulic parameters for van Genuchtan function
#     avg <- 0.0591  # Soil hydraulic parameters for van Genuchtan function
#     s_fc<-0.364/n # Field capacity
#     # This is the bubbling pressure
#     psi_s_bar<--1.5E-3 # IS IN HERE SO DON'T NEED TO DEFINE IN THE VEG FILES
#   }
#   if (soil == "SCL") {    ## Soil types used for Sodicity paper (Shah et al. 2013)
#     # Coarse Sand
#     n<-0.42 # porosity
#     # more soil variables for evaporation & losses
#     # Hydraulic conductivity
#     K_s<-50 # cm/day
#     # Campbell's b using neurotheta
#     b<- 13.5
#     # van Genuchten parameters
#     nvg <- 1.089   # Soil hydraulic parameters for van Genuchtan function
#     avg <- 0.0591  # Soil hydraulic parameters for van Genuchtan function
#     s_fc<-0.364/n # Field capacity
#     # This is the bubbling pressure
#     psi_s_bar<--1.5E-3 # IS IN HERE SO DON'T NEED TO DEFINE IN THE VEG FILES
#   }
#   
  # Other derived parameters

  h1bar =  -psi_s_bar 
  hb = psi_s_bar*-10^5 # mm
  K_s=10* K_s # mm/day

  # Create an output list

  soilpar <- list(b = b, n = n, K_s = K_s, 
                  psi_s_bar = psi_s_bar, h1bar = h1bar, hb = hb)
soilpar <-as.matrix(soilpar)
  
  return(soilpar)
}



