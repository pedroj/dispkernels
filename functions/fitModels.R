fitmodels <- function (x, funcs)
# Usage:
# funcs<- cbind("exponential","weibull","gamma")
# fitmodels(disp, funcs)
{
        n <- length(funcs)
# 	    funcs<-cbind("exponential", 
#                      "weibull", 
#                      "gamma", 
#                      "pearson",
#                      "lognormal", 
#                 #    "half-normal",
#                      "half-cauchy", 
#                      "half-t"); 
    i=0; zzz=NULL
for (i in 1:n) {
                zz<-fit.disp(x+1,fun=funcs[,i])
    zzz<- append(zzz, paste("\n","##### Model fitted:  ","\n"))
    zzz<- append(zzz,paste(funcs[,i],"\n"))
    zzz<- append(zzz,paste("dev= ", zz$dev,"\n",
                    "AIC= ", zz$aic,"\n",
                    "estimate= ", zz$estimate,"\n",
                    "se= ", zz$se,"\n","\n"))
            }
    cat(zzz)
}
