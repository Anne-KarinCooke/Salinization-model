
###############################################################
#Constants
R= 0.0821 # gas constant [ L atm mol-1 K-1 ]
Temp= 298 # temperature in Kelvin,  25 degrees Celsius
Mm= 58.44 # molar mass of NaCl in g/mol


# ### SOIL INPUT - medium heavy Clay
# n<-0.4473
# s_fc<-0.3936/n # Field capacity
# # Hydraulic conductivity
# K_s<-2.82*10 # mm/day
# # Campbell's b
# b<-16.1501 # neurotheta Medium heavy clay
# beta <- 2*b+4

# Coarse Sand
n<-0.368 # porosity
s_fc<-0.1895/n # Field capacity
# Hydraulic conductivity
K_s<-182.68*10 # cm/day
# Campbell's b
b<- 4.1152
beta <- 2*b+4
# beta

# root depth
Zr=40 #cm



#### Salt leaching parameters
# f is the  leaching efficiency of the soil, has to be calibrated from field data 

f= 0.4 # between 0 and 1, here it is just an assumption to start with
