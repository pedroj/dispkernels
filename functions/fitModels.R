fitmodels <- function
    (ff)
{
        n <- length(funcs)
	    funcs<-cbind("exponential", "weibull", "gamma", "pearson","lognormal", "half-normal", "half-cauchy", "half-t"); i=0
for (i in 1:n) {
	            cat("\n","##### Model fitted:  ")
	            cat(funcs[,i],"\n")
                fit.disp(wpop.dist+1,fun=funcs[,i])
            }
}