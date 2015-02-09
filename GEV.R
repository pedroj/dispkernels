# Analysis of generalized extreme value (GEV) distributions.
library(ismev)
require(adehabitatHS)               # For simulations of Levy walks
source("./functions/mykernel.R")    # Sourcing the kernel function.

# Different types of dispersal Kernels. ------------------------------------
# dd<- abs(rnorm(1000, mean = 32.7, sd = 415)) # Normal distrib.
# dd<- rweibull(1000, shape= 1, scale = 3.5) # Weibull distrib.
# dd<- rgamma(1000, scale= 100, shape= 10) # Gamma distrib.
                                    # req. adehabitatHS
dd<- as.data.frame(simm.levy(1:1000, mu = 1.5, burst = "mu = 1.5")) 
dd<- na.omit(dd$dist)

# With function mykernel.
mykernel(dd, bw= 20, h= 2000) # Extract the distances vector
gevdd<-gev.fit(dd)
gev.diag(gevdd)

# Prunus mahaleb dispersal distances data. ---------------------------------
assdist <-read.table("distances.txt",header=TRUE,sep="\t",dec=",",na.strings="NA")
str(assdist)
dd<- assdist$dist
# With function mykernel.
mykernel(dd, bw= 100, h= 50) # Extract the distances vector
gevdd<-gev.fit(dd)
gev.diag(gevdd)
