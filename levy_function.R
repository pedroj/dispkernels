#-----------------------------------------------------------------------------
# Function for Levy walk from source fixed point
# Pedro Jordano. 7 Apr 2009.
# Specify:
# R> levy(n,mu,l0)
# Where: n, is the number of events (default, n= 500)
#        mu, is the Levy exponent (default, mu= 2.0)
#        l0, is step size (default, l0= 1)
#-----------------------------------------------------------------------------
levy<-function (n= 500, mu= 2.0, l0= 1) 
{
    mu = 2
    l0 = 1
    d<-numeric(n)
    ang <- runif(n - 2, -pi, pi)
    v = l0 * (runif(n-1)^(1/(1 - mu)))
    ang = cumsum(c(runif(1, 0, 2 * pi), ang))
    x2<-v*cos(ang)
    y2<-v*sin(ang)
# Distances
    dist<-function(x1, y1, x2, y2)  sqrt((x2 - x1)^2 + (y2 - y1)^2)
    for (i in 1:n) {
        d<-0
        for (k in 1:100) d[k]<-dist(0,0,x2[k],y2[k])
    }
    par(mfrow=c(1,2))
    hist(d)
    plot(x2,y2, xlim=c(min(x2),max(x2)),ylim=c(min(y2),max(y2)))
 for (i in 1:n) 
     lines(c(0,x2[i]),c(0,y2[i])) # draw the lines
}
