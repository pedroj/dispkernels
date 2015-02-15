# Analysis of generalized extreme value (GEV) distributions.
require(ismev)
require(extreme)
require(adehabitatHS)               # For simulations of Levy walks

# Sourcing the kernel function code from GitHub.
require(downloader)
link = "https://raw.githubusercontent.com/pedroj/dispkernels/master/functions/mykernel.R"
file = "./functions/mykernel.R"
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
mykernel(dd, bw= 20, h= 200)     # Extract the distances vector
ddtrunc=dd[dd<3000]              # Truncate below a distance
mykernel(ddtrunc, bw= 10, h= 20) # Extract the distances vector

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

# Notice the output:
# ∗ $conv gives a value of zero (in row[1] of the output), which 
#   indicates success- full convergence, i.e. no errors in fitting;
# In the case of the full dispersal kernel I get convergence errors!!
# ∗ $nllh shows the negative (maximised) log–likelihood;
# ∗ $mle shows the maximum likelihood estimates for μ, σ and ξ respectively;
# ∗ $se gives the associated standard errors for these parameters.

gev.diag(gevdd)

# Truncate events after 1200 m to get the within-stand dispersal events
ddtrunc=dd[dd<1200]
mykernel(ddtrunc, bw= 10, h= 10) # Extract the distances vector
gevdd<-gev.fit(ddtrunc)
gev.diag(gevdd)
gev.ret(ddtrunc,100)
# If we want to construct a confidence interval for q100, we are better off using the method of profile–likelihood as described in Section 1.2.8. We can use the function gev.prof(fit,period,lower-bound, upper-bound). This com- mand is slightly unstable, and relies on an appropriate choice of the bounds for the profile–likelihood. For the Boston annual maxima, the following works well for the 100–year level:
gev.prof(gevdd,100,60,150)

# Note that this enables us to read off the 95% confidence interval (the default) for q100. Suppose we wanted a 99% interval we would use:
gev.prof(gevdd,100,60,150,conf=0.99)

