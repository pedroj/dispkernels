# The Weibull distribution.
# Load packages
library(MASS)
library(car)

# First, we generate 1000 random numbers from a Weibull distribution 
# with scale = 1 and shape = 1.5
rw <- rweibull(1000, scale= 0.5, shape= 2.5)

# We can calculate a kernel density estimation to inspect the 
# distribution. Because the Weibull distribution has support 
# [0,+Infinity), we are truncating the density at 0.

par(bg="white", las=1, cex=1.1)
plot(density(rw, bw=0.5, cut=0), las=1, lwd=2,
    xlim=c(0,5),col="steelblue")

# Dispersal Kernel
knormal<-abs(rnorm(1000, mean = 20, sd = 415))
kweibull <- rweibull(1000, scale=1, shape=3.5)

# We can omit the truehist and step directly to the Kernel.
mykernel<- function (dd, xlim= c(0,max(dd)), prob= T, 
    bw= 20, h= 10) {
    require(MASS)
    truehist(dd, xlim=c(0,max(dd)),
    # ylim=c(0,0.012),
    prob= T,h= 10, 
    xlab= "Distance (m)",
    ylab= "Probability",
    col=rgb(0, 0, 1, 0.2),
    lty=0)
rug(dd, side=1, col="red")
    # Kernel
    d<-density(dd,bw=20,from=0,to=max(dd)) # add density estimate
    # These are resampled (randomized) values to visualize a confidence envelope.
    for (k in 1: 100) {
        di<- sample(dd, length(dd), replace = T, prob = NULL)
        ddi<-density(di, bw=20, from=0, to=max(dd)) # add density estimate
        lines(ddi, xlim=c(0, max(dd)), col="grey", lwd=0.3)
    }
    lines(d,xlim=c(0,max(dd)),col="blue") # Averaged resampled kernel
}
mykernel(knormal)
mykernel(rw)



