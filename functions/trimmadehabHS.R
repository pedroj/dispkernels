#----------------------------------------------------------------------
# FUNCTION to trimm the dist values of simulated adehabitatHS paths
# Here I trimm the NA's from the simulated Levy values 
trimmadehabHS<- function (w) { # w is the object generated 
                               # by adehabitatHS, as in e.g., 
                               # w <- simm.levy(1:500, mu = 2, 
                               #                burst = "mu = 2")
    ww<- as.data.frame(w[!is.na(w)])
    # str(ww)
    summary(ww$dist)
    dd<- ww$dist[!is.na(ww$dist)] # dd holds the vector of distances
    # summary(dd) # dd holds just the distance vector
}
#----------------------------------------------------------------------
