#----------------------------------------------------------------------
# Plot randomized kernel with specific mean and SD values.
# Plot two kernels in the same plot.
#
ddTF<- rweibull(1000, shape= 1, scale = 3.5) # Weibull distrib.
ddGC<- rweibull(1000, shape= 0.7, scale = 3.0) # Weibull distrib.
mydd<- data.frame(ddTF,ddGC)

ggplot(mydd, aes(x=ddGC)) +
    geom_histogram(aes(y = ..density..), 
                   binwidth=density(mydd$ddGC)$bw,
                   fill="blue", alpha = 0.5) +
    geom_density(fill="red", alpha = 0.2) +
#    theme_bw() +
    xlab('Distance (m)') +
    ylab('Probability')

# Trying to plot two kernels in same graph.
mydd<- data.frame(xx = c(ddTF,ddGC),
                  yy = rep(c("TF","GC"),each = 1000))

ggplot(mydd,aes(x=xx)) + 
    xlab('Distance (m)') +
    ylab('Probability') +
    geom_histogram(data=subset(mydd, yy == 'TF'),
                   aes(y = ..density..), 
                   binwidth=density(mydd$xx)$bw,
                   fill = "red", alpha = 0.2) +
#    geom_density(fill="red", alpha = 0.3) +
    geom_histogram(data=subset(mydd, yy == 'GC'),
                    aes(y = ..density..), 
                    binwidth=density(mydd$xx)$bw,
                    fill = "blue", alpha = 0.2) #+
#    geom_density(fill="blue", alpha = 0.3)

# The two smoothing functions, together.
p<- ggplot(mydd,aes(x=xx)) +
        theme_bw() +
        xlab('Distance (m)') +
        ylab('Probability') +
    geom_density(data=subset(mydd, yy == 'TF'),
                     fill="red", lty=0, alpha = 0.3) +
    geom_density(data=subset(mydd, yy == 'GC'),
                     fill="blue", lty=0,  alpha = 0.3)
p    
    
# These are resampled (randomized) values to visualize a confidence envelope.
kernelenv <- function (dd) {
  for (k in 1: 100) {
      di<- sample(dd, length(dd), replace = T, prob = NULL)
      ddi<-density(di, bw=20, from=0, to=max(dd)) # add density estimate
      lines(ddi, xlim=c(0, max(dd)), col="grey", lwd=0.2)
  }
  lines(d,xlim=c(0,max(dd)),col="blue") # Averaged rresampled kernel
}
dd<- subset(mydd, yy == 'TF')
kernelenv(dd$xx)
    

kernelenv <- function (dd) {
    for (k in 1: 100) {
        di<- sample(dd, length(dd), replace = T, prob = NULL)
        geom_density(data=di,
            col="grey", lwd=0.2, lty=0, alpha = 0.3)
    }
}

p+ kernelenv(dd)


    
    
    
    
    
