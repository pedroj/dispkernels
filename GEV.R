# Analysis of generalized extreme value (GEV) distributions.
library(ismev)
require(adehabitatHS)               # For simulations of Levy walks

# Sourcing the kernel function code from GitHub.
require(downloader)
link = "https://raw.githubusercontent.com/pedroj/dispkernels/master/functions/mykernel.R"
file = "mykernel.R"
if(!file.exists(file)) download(link, file, mode = "wb")
source(file)

# Different types of dispersal Kernels. ------------------------------------
# dd<- abs(rnorm(1000, mean = 32.7, sd = 415)) # Normal distrib.
# dd<- rweibull(1000, shape= 1, scale = 3.5)   # Weibull distrib.
# dd<- rgamma(1000, scale= 100, shape= 10)     # Gamma distrib.
                                               # req. adehabitatHS
dd<- as.data.frame(simm.levy(1:1000, mu = 1.5, burst = "mu = 1.5")) 
dd<- na.omit(dd$dist)

# With function mykernel.
mykernel(dd, bw= 20, h= 2000) # Extract the distances vector
gevdd<-gev.fit(dd)
gev.diag(gevdd)

# Prunus mahaleb dispersal distances data. ---------------------------------
# Get the data from my GitHub repository.
require(downloader)
link = "https://raw.githubusercontent.com/pedroj/dispkernels/master/distances.txt"
file = "distances.txt"
if(!file.exists(file)) download(link, file, mode = "wb")
assdist <- read.table(file, sep = "\t", dec = ".", 
    header = TRUE, na.strings="NA")
str(assdist)
dd<- assdist$dist
# With function mykernel.
mykernel(dd, bw= 100, h= 50) # Extract the distances vector
gevdd<-gev.fit(dd)
gev.diag(gevdd)

# Truncate events after 1200 m to get the within-stand dispersal events
ddtrunc=dd[dd<1200]
mykernel(ddtrunc, bw= 100, h= 10) # Extract the distances vector
gevdd<-gev.fit(ddtrunc)
gev.diag(gevdd)
