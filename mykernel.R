# FUNCTION mykernel ------------------------------------------------------
# We can omit the truehist and step directly to the Kernel.
# Input in function call is the data vector of distances and an upper
# limit for estimating the kernel.
mykernel<- function (dd, up= max(dd), prob= T, 
    bw= 20, h= 10) {
    # up is the X axis maximum limit.
    # bw is the "window" width for the density kernel.
    # h is the binning width.
    require(MASS)
    truehist(dd, xlim=c(0, up),
        # ylim=c(0,0.012),
        prob= T, h= h, 
        xlab= "Distance (m)",
        ylab= "Probability",
        col= rgb(0, 0, 1, 0.2),
        lty= 0)
    rug(dd, side= 1, col= "red")
    # Kernel
    d<- density(dd, bw= bw, from= 0, to= up) # add density estimate
    # These are resampled (randomized) values to visualize a confidence envelope.
    for (k in 1: 100) {
        di<- sample(dd, length(dd), replace = T, prob = NULL)
        ddi<- density(di, bw= bw, from= 0, to= up) # add density estimate
        lines(ddi, xlim= c(0, up), col= "grey", lwd= 0.3)
    }
    lines(d, xlim= c(0, up), col="blue") # Averaged resampled kernel
}
