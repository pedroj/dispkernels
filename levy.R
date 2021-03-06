##########################################################################
### LÉVY WALK CODE 
##########################################################################
require(adehabitatHS)
set.seed(411)
w <- simm.levy(1:500, mu = 1.5, burst = "mu = 1.5")
u <- simm.levy(1:500, mu = 2, burst = "mu = 2")
v <- simm.levy(1:500, mu = 2.5, burst = "mu = 2.5")
x <- simm.levy(1:500, mu = 3, burst = "mu = 3")
par(mfrow=c(2,2))
lapply(list(w,u,v,x), plot, perani=FALSE)
lapply(list(w,u,v,x), plot, addpoints = FALSE)

# A single source
w <- simm.levy(1:500, mu = 2.2)
lapply(list(w), plot, addpoints = FALSE)

##########################################################################
#Description
# 
# This function simulates a Levy walk
# 
# Usage
# 
# simm.levy(date = 1:500, mu = 2, l0 = 1, x0 = c(0, 0),
#           id = "A1", burst = id, typeII = TRUE)
# Arguments
# 
# date	 a vector indicating the date (in seconds) at which relocations should be simulated. This vector can be of class POSIXct. *Note that the time lag between two relocations should be constant* (regular trajectories required)
# mu	 The exponent of the Levy distribution
# l0	 The minimum length of a step
# x0	 a vector of length 2 containing the coordinates of the startpoint of the trajectory
# id	 a character string indicating the identity of the simulated animal (see help(ltraj))
# burst	 a character string indicating the identity of the simulated burst (see help(ltraj))
# typeII	 logical. Whether the simulated trajectory should be of type II (TRUE, time recorded) or not (FALSE, time not recorded). See help(ltraj).
# Details
# 
# This function simulates a Levy flight with exponent mu. This is done by sampling a random relative angle from a uniform distribution (-pi, pi) for each step, and a step length generated by dt * (l0 * (runif(1)^(1/(1 - mu))))
# 
# Value
# an object of class ltraj
##########################################################################
# Brownian motion
set.seed(253)
u <- simm.mba(1:1000, sigma = diag(c(4,4)), 
              burst = "Brownian motion")
v <- simm.mba(1:1000, sigma = matrix(c(2,-0.8,-0.8,2), ncol = 2),
              burst = "cov(x,y) > 0")
w <- simm.mba(1:1000, mu = c(0.1,0), burst = "drift > 0")
x <- simm.mba(1:1000, mu = c(0.1,0),
              sigma = matrix(c(2, -0.8, -0.8, 2), ncol=2),
              burst = "Drift and cov(x,y) > 0")
z <- c(u, v, w, x)
plot(z, addpoints = FALSE, perani = FALSE)
##########################################################################
# Function for Levy walk from source fixed point
#-------------------------------------------------------------------------
function (date = 1:500, mu = 2, l0 = 1, x0 = c(0, 0), id = "A1", 
    burst = id, typeII = TRUE) 
{
#    if (typeII) 
#        class(date) <- c("POSIX", "POSIXct")
    date = 1:500
    mu = 2
    l0 = 1
    x0 = c(0, 0)
    id = "A1"
    burst = id
    typeII = TRUE
    n <- length(date)
    dt <- c(diff(unclass(date)))
    if (all(dt - dt[1] > 1e-07)) 
        stop("the time lag between relocations should be constant")
    ang <- runif(n - 2, -pi, pi)
    v = dt * (l0 * (runif(n - 1)^(1/(1 - mu))))
    ang = cumsum(c(runif(1, 0, 2 * pi), ang))
    si = c(x0[2], x0[2] + cumsum(v * sin(ang)))
    co = c(x0[1], x0[1] + cumsum(v * cos(ang)))
    res <- as.ltraj(data.frame(co, si), date, id, burst, typeII = typeII)
    return(res)
}
##########################################################################
# Fat-tailed kernel function. Clark (1998) Amer. Nat.
# f(x)= c/[2 alpha Tau(1/c)] exp(-|x/alpha |^c)
# c and h are the shape and distance parameters.
# The negative exponential (c = 1) and the Gaussian (c = 2) are 
# special cases of this formula, and c < 1 is considered a fat-tailed
# kernel. 
# E.g., c= 0.45, alpha= 21 km.

alpha= 100  # Distance parameter.
c= 0.35     # Shape parameter.
for (x in 0:100)
    curve(1/(c/(2*alpha * gamma(1/c) * exp(-abs(x/alpha)^c))), 
          from=0, to=100, n=100)

# curve(expr, from = NULL, to = NULL, n = 101, add = FALSE,
# type = "l", xname = "x", xlab = xname, ylab = NULL,
# log = NULL, xlim = NULL, ...)
