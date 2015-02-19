#GEV RETURN LEVEL CALCULATION
ret.level.gev<-function(mu,sigma,xi,period)
{
    ret.level<-mu-(sigma/xi)*(1-(-log(1-(1/period)))**(-xi))
    ret.level
}



#GPD RETURN LEVEL CALCULATION
ret.level.gpd<-function(sigma,xi,lambda,thresh,period)
{
    ret.level<-thresh+(sigma/xi)*(((period*lambda)^(xi))-1)
    ret.level
}

#GEV STANDARD ERRORS FOR Q
gev.ret<-function(data, period)
{
    lee<-gev.fit(data)
    
    V<-matrix(ncol=3,nrow=3)
    V[1,1]<-lee$cov[1,1]
    V[2,2]<-lee$cov[2,2]
    V[3,3]<-lee$cov[3,3]
    V[1,2]<-lee$cov[1,2]
    V[1,3]<-lee$cov[1,3]
    V[2,1]<-lee$cov[2,1]
    V[2,3]<-lee$cov[2,3]
    V[3,1]<-lee$cov[3,1]
    V[3,2]<-lee$cov[3,2]
    
    yp<--log(1-(1/period))
    
    diff1<-1
    diff2<--((lee$mle[3])**(-1))*(1-(yp**(-lee$mle[3])))
    diff3<-((lee$mle[2])*((lee$mle[3])**(-2))*(1-((yp)**(-lee$mle[3]))))-((lee$mle[2])*((lee$mle[3])**(-1))*((yp)**(-(lee$mle[3])))*log(yp))
    
    del.t<-matrix(ncol=3,nrow=1)
    del.t[1,1]<-diff1
    del.t[1,2]<-diff2
    del.t[1,3]<-diff3
    
    del<-matrix(ncol=1,nrow=3)
    del[1,1]<-diff1
    del[2,1]<-diff2
    del[3,1]<-diff3
    
    A<-matrix(ncol=3,nrow=1)
    A[1,1]<-del.t[1,1]*V[1,1]
    A[1,2]<-(del.t[1,2]*V[2,2])+(del.t[1,3]*V[3,2])
    A[1,3]<-(del.t[1,2]*V[2,3])+(del.t[1,3]*V[3,3])
    
    ret.var<-(A[1,1]*diff1) + (A[1,2]*diff2) + (A[1,3]*diff3)
    ret.se<-sqrt(ret.var)
    ret.level<-(lee$mle[1])-((lee$mle[2])/(lee$mle[3]))*(1-(-log(1-(1/period)))**(-(lee$mle[3])))
    
    cat(" ret.level= ",ret.level,"\n","ret.se= ",ret.se)
}

#GPD STANDARD ERRORS FOR Q)
gpd.ret<-function(data,threshold,period)
{
    lee<-gpd.fit(data,threshold)
    lambda<-length(data[data>threshold])/length(data)
    
    V<-matrix(ncol=3,nrow=3)
    V[1,1]<-lambda*(1-lambda)/length(data)
    V[1,2]<-0
    V[1,3]<-0
    V[2,1]<-0
    V[3,1]<-0
    V[2,2]<-(lee$cov[1,1])
    V[2,3]<-lee$cov[1,2]
    V[3,2]<-lee$cov[2,1]
    V[3,3]<-(lee$cov[2,2])
    
    diff1<-(lee$mle[1])*period^((lee$mle[2]))*lambda^((lee$mle[2])-1)
    diff2<-((lee$mle[2])^(-1))*(((period*lambda)^((lee$mle[2])))-1)
    diff3<--((lee$mle[1]))*(lee$mle[2])^(-2)*(((period*lambda)^((lee$mle[2])))-1) + (lee$mle[1])*((lee$mle[2])^(-1))*((period*lambda)^((lee$mle[2])))*log(period*lambda)
    
    
    del.t<-matrix(ncol=3,nrow=1)
    del.t[1,1]<-diff1
    del.t[1,2]<-diff2
    del.t[1,3]<-diff3
    
    del<-matrix(ncol=1,nrow=3)
    del[1,1]<-diff1
    del[2,1]<-diff2
    del[3,1]<-diff3
    
    A<-matrix(ncol=3,nrow=1)
    A[1,1]<-del.t[1,1]*V[1,1]
    A[1,2]<-(del.t[1,2]*V[2,2])+(del.t[1,3]*V[3,2])
    A[1,3]<-(del.t[1,2]*V[2,3])+(del.t[1,3]*V[3,3])
    
    ret.var<-(A[1,1]*diff1) + (A[1,2]*diff2) + (A[1,3]*diff3)
    ret.se<-sqrt(ret.var)
    ret.level<-threshold+((lee$mle[1])/(lee$mle[2]))*(((period*lambda)^((lee$mle[2])))-1)
    cat(" ret.level= ",ret.level,"\n","ret.se= ",ret.se)
}

#GEV - BAYES
gev.bayes<-function(n,dataset,mustart,sigmastart,xistart,errmu,errlogsigma,errxi,sdmu,sdlogsigma,sdxi)
{
    k<-length(dataset)
    
    mu<-mustart
    sigma<-sigmastart
    xi<-xistart
    eta<-log(sigma)
    
    canmu<-vector("numeric")
    caneta<-vector("numeric")
    canxi<-vector("numeric")
    
    canmu[1]<-mu
    caneta[1]<-eta
    canxi[1]<-xi
    
    w<-vector("numeric")
    x<-vector("numeric")
    y<-vector("numeric")
    
    aprobmu<-vector("numeric")
    aprobeta<-vector("numeric")
    aprobxi<-vector("numeric")
    
    w[1]<-canmu[1]
    x[1]<-caneta[1]
    y[1]<-canxi[1]
    
    loglik<-function(k,dataset,MU,ETA,XI)
    {
        m<-min((1+(XI*(dataset-MU)/exp(ETA))))
        if(m<0.00001)return(as.double(-1000000))
        if(exp(ETA)<0.00001)return(as.double(-1000000))
        loglik<--length(dataset)*ETA-(1/XI+1)*sum(log(1+(XI*(dataset-MU)/exp(ETA))))-sum((1+(XI*(dataset-MU)/exp(ETA)))**(-1/XI))
        loglik
    }
    
    for(i in 2:n){
        print(i)
        canmu[i]<-w[i-1]+rnorm(1,0,errmu)
        likely<-exp((loglik(k,dataset,canmu[i],x[i-1],y[i-1]))-(loglik(k,dataset,w[i-1],x[i-1],y[i-1])))
        aprobmu[i]<-min(1,(likely*((dnorm(canmu[i],0,sdmu))*(dnorm(x[i-1],0,sdlogsigma))*(dnorm(y[i-1],0,sdxi)))/((dnorm(w[i-1],0,sdmu))*(dnorm(x[i-1],0,sdlogsigma))*(dnorm(y[i-1],0,sdxi)))))
        u<-runif(1)
        if(u<aprobmu[i]){w[i]<-canmu[i]}
        if(u>=aprobmu[i]){w[i]<-w[i-1]}
        
        caneta[i]<-x[i-1]+rnorm(1,0,errlogsigma)
        likely2<-exp((loglik(k,dataset,w[i],caneta[i],y[i-1]))-(loglik(k,dataset,w[i],x[i-1],y[i-1])))
        aprobeta[i]<-min(1,(likely2*((dnorm(w[i],0,sdmu))*(dnorm(caneta[i],0,sdlogsigma))*(dnorm(y[i-1],0,sdxi)))/((dnorm(w[i],0,sdmu))*(dnorm(x[i-1],0,sdlogsigma))*(dnorm(y[i-1],0,sdxi)))))
        u<-runif(1)
        if(u<aprobeta[i]){x[i]<-caneta[i]}
        if(u>=aprobeta[i]){x[i]<-x[i-1]}
        
        
        canxi[i]<-y[i-1]+rnorm(1,0,errxi)
        likely3<-exp((loglik(k,dataset,w[i],x[i],canxi[i]))-(loglik(k,dataset,w[i],x[i],y[i-1])))
        aprobxi[i]<-min(1,(likely3*((dnorm(w[i],0,sdmu))*(dnorm(x[i],0,sdlogsigma))*(dnorm(canxi[i],0,sdxi)))/((dnorm(w[i],0,sdmu))*(dnorm(x[i],0,sdlogsigma))*(dnorm(y[i-1],0,sdxi)))))
        if(u<aprobxi[i]){y[i]<-canxi[i]}
        if(u>=aprobxi[i]){y[i]<-y[i-1]}
    }
    
    aprobmu<-aprobmu[!is.na(aprobmu)]
    aprobeta<-aprobeta[!is.na(aprobeta)]
    aprobxi<-aprobxi[!is.na(aprobxi)]
    mu<-w
    logsigma<-x
    xi<-y
    aproblogsigma<-aprobeta
    return(mu,logsigma,xi,aprobmu,aproblogsigma,aprobxi)
}


#GPD - BAYES
bayes5<-function(n,dataset,sigmastart,xistart,erreta,errxi,sdeta,sdxi)
{
    k<-length(dataset)
    sigma<-sigmastart
    xi<-xistart
    eta<-log(sigma)
    caneta<-vector("numeric")
    canxi<-vector("numeric")
    caneta[1]<-eta
    canxi[1]<-xi
    x<-vector("numeric")
    y<-vector("numeric")
    aprobeta<-vector("numeric")
    aprobxi<-vector("numeric")
    x[1]<-caneta[1]
    y[1]<-canxi[1]
    loglik<-function(k,dataset,ETA,XI)
    {
        m<-min(1+(dataset*(XI/exp(ETA))))
        if(m<0.00001)return(as.double(-1000000))
        if(exp(ETA)<0.00001)return(as.double(-1000000))
        loglik<--k*ETA-(1+(1/XI))*sum(log(1+((XI*dataset)/exp(ETA))))
        loglik
    }
    for(i in 2:n){
        caneta[i]<-x[i-1]+rnorm(1,0,erreta)
        likely<-exp((loglik(k,dataset,caneta[i],y[i-1]))-(loglik(k,dataset,x[i-1],y[i-1])))
        aprobeta[i]<-min(1,(likely*((dnorm(caneta[i],0,sdeta))*(dnorm(y[i-1],0,sdxi)))/((dnorm(x[i-1],0,sdeta))*(dnorm(y[i-1],0,sdxi)))))
        u<-runif(1)
        if(u<aprobeta[i]){x[i]<-caneta[i]}
        if(u>=aprobeta[i]){x[i]<-x[i-1]}
        canxi[i]<-y[i-1]+rnorm(1,0,errxi)
        likely2<-exp((loglik(k,dataset,x[i],canxi[i]))-(loglik(k,dataset,x[i],y[i-1])))
        aprobxi[i]<-min(1,(likely2*((dnorm(x[i],0,sdeta))*(dnorm(canxi[i],0,sdxi)))/((dnorm(x[i],0,sdeta))*(dnorm(y[i-1],0,sdxi)))))
        if(u<aprobxi[i]){y[i]<-canxi[i]}
        if(u>=aprobxi[i]){y[i]<-y[i-1]}
    }
    aprobeta<-aprobeta[!is.na(aprobeta)]
    aprobxi<-aprobxi[!is.na(aprobxi)]
    return(x,y,aprobeta,aprobxi)
}
