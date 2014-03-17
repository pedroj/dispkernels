## fixed
clark2Dt <- function(x , p, u=1) {
    (p/(pi*u))/(1+(x^2/u))^(p+1)
}
newclark2Dt <- function(x , p, s=1, eps=1e-70) {
    d <- (1+(x/s)^2)
    r <- 1/d^(p+1)
    if (any(!is.finite(r))) browser()
    r
}

# It might be preferable to define this in terms of s=sqrt(u)
# instead (then s would be a scale parameter with the same units
#    as x, more easily interpretable ...
#    Sanity checks:        
    par(las=1,bty="l") ## personal preferences
    curve(clark2Dt(x,p=6),from=0,to=5)
    curve(clark2Dt(x,p=4),col=2,add=TRUE)
    curve(clark2Dt(x,p=2),col=4,add=TRUE)
    legend("topright",paste("p",c(6,4,2),sep="="),col=c(1,2,4),lty=1)

# - Test -------------------------------------
X <- as.data.frame(matrix(
    c(15,12,
        45,13,
        75,10,
        105,8,
        135,16,
        165,5,
        195,15,
        225,8,
        255,9,
        285,12,
        315,5,
        345,4,
        375,1,
        405,1,
        435,1,
        465,0,
        495,1,
        525,2,
        555,0,
        585,0,
        615,0,
        645,0,
        675,0),
    ncol=2,byrow=TRUE,
    dimnames=list(NULL,c("dist","count"))))

## assume these are traps/samples with unit size
## (if not, it will get absorbed into the "fecundity" constant

library(bbmle)
m1 <- mle2(count~ dnbinom(mu=f*clark2Dt(dist,p,u), size=k),
            data= X, start= list(f=20, u=10, p=5, k=2),
            lower= rep(0.002,4), method= "L-BFGS-B")

## we get a plausible-looking fit ...
with(X, plot(count~ dist, pch=16, las=1, bty="l"))
newdat <- data.frame(dist=1:700)  ## overkill but harmless
lines(newdat$dist,predict(m1,newdata=newdat))

## but the coefficients look funny, especially f

coef(m1)         

## tried resetting parscale but it's bogus (gets stuck at a worse likelihood)
m2 <- mle2(count~dnbinom(mu=f*clark2Dt(dist,p,u),size=k),
    data=X,start=list(f=20,u=10,p=5,k=2),
    control=list(parscale=abs(coef(m1))),
    lower=rep(0.002,4),method="L-BFGS-B")

m3 <- mle2(count~dnbinom(mu=exp(logf)*clark2Dt(dist,exp(logp),exp(logu)),
    size=exp(logk)),
    data=X,start=list(logf=log(20),logu=log(10),logp=log(5),
        logk=log(2)),
    method="Nelder-Mead")

exp(coef(m3))
coef(m1)
summary(m1)

## hmm.  Redefine in terms of s instead of u and (more importantly)
## with f = seed density at r=0 rather the

cov2cor(vcov(m1)) ## shows that f and u are horribly correlated         

newclark2Dt <- function(x , p, s=1, eps=1e-70) {
    d <- (1+(x/s)^2)
    r <- 1/d^(p+1)
    if (any(!is.finite(r))) browser()
    r
}

dnbinom_pen <- function(x,mu,size,pen=1000,log=TRUE) {
    mu <- rep(mu,length.out=length(x))
    logval <- ifelse(mu==0 && x==0,pen*x^2,dnbinom(x,mu=mu,size=size,log=TRUE))
    if (log) logval else exp(logval)
}

## needed for predict()
snbinom_pen <- snbinom

m4 <- mle2(count~dnbinom(mu=f*newclark2Dt(dist,p,s),size=k),
    data=X,start=list(f=20,s=10,p=5,k=2),
    lower=rep(0.002,4),method="L-BFGS-B")

m5 <- mle2(count~dnbinom_pen(mu=f*newclark2Dt(dist,1/(pinv),s),size=exp(logk)),
    data=X,start=list(f=15,s=10,pinv=100,logk=1),trace=TRUE,
    ##           control=list(parscale=c(200,0.002,1.66,3600)),
    lower=rep(0.002,4),method="L-BFGS-B")

with(X,plot(count~dist,pch=16,las=1,bty="l"))
newdat <- data.frame(dist=1:700)  ## overkill but harmless
lines(newdat$dist,predict(m1,newdata=newdat))
lines(newdat$dist,predict(m5,newdata=newdat),col=2)
