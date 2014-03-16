#----------------------------------------------------------------------
# Simulations with gamma distribution
# The Gamma distribution with parameters shape = a and scale = s has 
# density:
# f(x)= 1/(s^a Gamma(a)) x^(a-1) e^-(x/s); 
# for x ≥ 0, a > 0 and s > 0. 
# Here Gamma(a) is the function implemented by R's gamma().
#----------------------------------------------------------------------
# Load packages
library(MASS)

# First, we generate 1000 random numbers from a gamma distribution 
# with scale = 0.5 and shape = 2.5
rg <- rgamma(1000, scale= 2, shape= 0.1)

par(bg="white", las=1, cex=1.1)
plot(density(rg, bw= 0.5, cut=0), las=1, lwd=2,
    xlim=c(0,5),col="steelblue")

# Dispersal Kernel
knormal<-abs(rnorm(1000, mean = 20, sd = 415))
kweibull <- rweibull(1000, scale=1, shape=3.5)
kgamma <- rgamma(1000, scale= 2, shape= 0.1)
kgamma <- rgamma(1000, scale= 2, shape= 1.5)


truehist(kgamma, xlim=c(0, max(kgamma)),
    # ylim=c(0,0.012),
    prob= T, #h= 10, 
    xlab= "Distance (m)",
    ylab= "Probability",
    col= rgb(0, 0, 1, 0.2),
    lty= 0)
rug(kgamma, side= 1, col= "red")


mykernel(kgamma, bw=10, h=0.5)

# NOTES ---------------------------------------------------------------
-log(dgamma(1:4, shape = 1))
p <- (1:9)/10
pgamma(qgamma(p, shape = 2), shape = 2)
1 - 1/exp(qgamma(p, shape = 1))

# even for shape = 0.001 about half the mass is on numbers
# that cannot be represented accurately (and most of those as zero)
pgamma(.Machine$double.xmin, 0.001)
pgamma(5e-324, 0.001)  # on most machines 5e-324 is the smallest
# representable non-zero number
table(rgamma(1e4, 0.001) == 0)/1e4
#----------------------------------------------------------------------



