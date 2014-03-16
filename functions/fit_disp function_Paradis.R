### fit.disp.R Emmanuel Paradis     24-1-2001
###
### This file contains an R function fit.disp() that fits 
### a probabilistic model to dispersal data with 3 different kinds 
### of precision (see Paradis et al.
### "Modelling large-scale dispersal distances").
###
### This program is similar to the F77 program dispers2.for except
### that the 95% CI is here computed both with the normal 
### approximation to the likelihood function, and with profile 
### likelihood (as in dispers2.for).
###
### The followings are accepted as data input (see below for the
### indicator vector y):
###
###    (i) a single numeric vector (x), the observations are thus 
###        taken as exact;
###   (ii) a numeric vector (x) and an indicator vector (y) as a
###        censoring ("e" or "c"), the observations are thus either 
###        exact or right-censored;
###  (iii) two numeric vectors (x and d2) and an indicator vector 
###        (y), the observations are thus of any precision;
###   (iv) a matrix or data.frame (x) containing three vectors 
###        as in (iii).
###
###   The indicator vector (y) can be either of mode character 
###       with three values allowed ("e", "i", or "c"), or of mode 
###       numeric with three different values:
###       the first one being equivalent to "e", the second one 
###       to "i", and the third one to "c".
###
### The other options are:
###   fun: the function assumed for the dispersal distance distribution.
###        Values allowed are: "exponential", "weibull", "gamma",
###        "pearson", "lognormal", "half-normal", "half-cauchy", 
###        "half-t", or "user" (a user-defined fonction). 
###        The default is to fit an exponential model.
###   pdf: the corresponding probability density function (can be 
###        omitted except if `fun = "user"').
###   CFD: the corresponding cumulative probability density function
###        (can be omitted except if `fun = "user"').
###    np: the number of parameters in the model (can be omitted 
###        except if `fun = "user"').
###     p: the initial values of the parameters used by the fitting
###        algorithm. (defaults to 0.5 for all parameters).
###
### The function returns a list with the following components:
###   dev        deviance
###   aic        Akaike information criterion
###   estimate   parameter estimates
###   se         standard-errors of the parameters
###   residuals  deviance residuals
###   interval   interval lengths
#
fit.disp <- function
    (x, d2 = NULL, y = NULL, fun = "exponential", pdf = NULL, 
        CDF = NULL, np = 1, p = rep(0.5, np))
{
    if (fun == "exponential") {
        dfun <- dexp
        pfun <- pexp
        np <- 1
    }
    if (fun == "weibull") {
        dfun <- dweibull
        pfun <- pweibull
        np <- 2
    }
    if (fun == "gamma") {
        dfun <- dgamma
        pfun <- pgamma
        np <- 2
    }
    if (fun == "pearson") {
        dfun <- dchisq
        pfun <- pchisq
        np <- 1
    }
    if (fun == "lognormal") {
        dfun <- dlnorm
        pfun <- plnorm
        np <- 2
    }
    if (fun == "half-normal") {
        dfun <- function(x, sigma) 2*dnorm(abs(x), 
                         mean = 0, sd = sigma)
        pfun <- function(x, sigma) 2*(pnorm(abs(x), 
                         mean = 0, sd = sigma) - .5)
        np <- 1
    }
    if (fun == "half-cauchy") {
        dfun <- function(x, sigma) 2*dcauchy(abs(x), 
                         location = 0, scale = sigma)
        pfun <- function(x, sigma) 2*(pcauchy(abs(x), 
                         location = 0, scale = sigma) - .5)
        np <- 1
    }
    if (fun == "half-t") {
        dfun <- function(x, df) 2*dt(abs(x), df)
        pfun <- function(x, df) 2*(pt(abs(x), df) - .5)
        np <- 1
    }
    if (fun == "user") {
        dfun <- pdf
        pfun <- CDF
        np <- length(formals(dfun)) - 1
    }

    if(is.null(d2) & is.null(y) & is.vector(x)) {
        d1 <- x
        n <- length(d1)
        intlen <- rep(0, n)

        ## prepare the arguments (as a character string) for the pdf function
        pdf.arg <- paste("d1,", paste("p[", 1:np, "]", 
                         sep = "", collapse=", "))

        ## prepare the pdf function (as an R expression)
        toto.exp <- parse(text=paste("dfun(", pdf.arg, ")", 
                          sep="", collapse=""))
    }

    if(is.null(d2) & is.vector(y) & is.vector(x)) {
        d1 <- x
        n <- length(d1)
        dum <- numeric(n)
        if(is.numeric(y)) {
            val.y <- as.numeric(names(table(y)))
            for (i in 1:n) ifelse(y[i] == val.y[1], 
                           dum[i] <- 0, dum[i] <- 1)
        }
        if(is.character(y)) for (i in 1:n) ifelse(y[i] == "e" | y[i] == "E", dum[i] <- 0, dum[i] <- 1)
        intlen <- -1*dum

        ## prepare the arguments (as a character string)
        ## for the pdf and the CDF functions
        fun.arg <- paste("d1,", paste("p[", 1:np, "]", 
                         sep = "", collapse=", "))

        ## prepare the R expression for the deviance function
        trm1 <- paste("(1 - dum) * dfun(", fun.arg, ")", 
                      sep="", collapse="")
        trm2 <- paste("dum * (1 - pfun(", fun.arg, "))", 
                      sep="", collapse="")
        toto.exp <- parse(text=paste(trm1, "+", trm2, 
                          sep="", collapse=""))
    }
    if(is.vector(d2) & is.vector(y) & is.vector(x) |
        is.matrix(x) | is.data.frame(x)) {
        if(is.matrix(x) | is.data.frame(x)) {
            y <- x[,3]
            d2 <- x[,2]
            d1 <- x[,1]
        }
        d1 <- x
        n <- length(d1)
        dum <- matrix(0, n, 3)
        if(is.numeric(y)) {
            val.y <- as.numeric(names(table(y)))
            for (i in 1:n) {
                if(y[i] == val.y[1]) dum[i,1] <- 1
                if(y[i] == val.y[2]) dum[i,2] <- 1
                if(y[i] == val.y[3]) dum[i,3] <- 1
            }
        }
        if(is.character(y)) {
            for (i in 1:n) {
                if (y[i] == "e" | y[i] == "E") dum[i,1] <- 1
                if (y[i] == "i" | y[i] == "I") dum[i,2] <- 1
                if (y[i] == "c" | y[i] == "C") dum[i,3] <- 1
            }
        }
        intlen <- dum[,2]*(d2 - d1) + dum[,3]*d1

        ## prepare the arguments (as a character string)
        ## for the pdf and the CDF functions
        fun.arg <- paste("d1,", paste("p[", 1:np, "]", 
                         sep = "", collapse=", "))
        fun.arg.bis <- paste("d2,", paste("p[", 1:np, "]", 
                             sep = "", collapse=", "))

        ## prepare the R expression for the deviance function
        trm1 <- paste("dum[,1] * dfun(", fun.arg, ")", 
                      sep="", collapse="")
        trm2 <- paste("dum[,2] * (pfun(", fun.arg.bis, ") - pfun(", 
                       fun.arg, "))", sep="", collapse="")
        trm3 <- paste("dum[,3] * (1 - pfun(", fun.arg, "))", 
                      sep="", collapse="")
        toto.exp <- parse(text=paste(trm1, "+", trm2, "+", trm3, 
                          sep="", collapse=""))
    }
    deviance <- function(p) -2*sum(log(eval(toto.exp)))
    out <- nlm(deviance, p = p, hessian = T)
    print(out)
    dev <- out$minimum
    aic <- dev + 2*np
    esti <- out$estimate
    p <- esti              ### needed to compute the residuals
    dev.res <- sqrt(-2*log(eval(toto.exp)))
    se <- sqrt(diag(solve(out$hessian)))
    step <- 0.001
    low.bd <- numeric(np)
    up.bd <- numeric(np)
    for (k in 1:np) {
        low <- esti
        low[k] <- esti[k] - step
        new.dev <- deviance(low)
        while((new.dev - dev) < 3.84) {
            low[k] <- low[k] - step
            new.dev <- deviance(low)
        }
        low.bd[k] <- low[k]
        up <- esti
        up[k] <- esti[k] + step
        new.dev <- deviance(up)
        while((new.dev - dev) < 3.84) {
            up[k] <- up[k] + step
            new.dev <- deviance(up)
        }
        up.bd[k] <- up[k]
    }
    cat("\n")
    print(data.frame(n=n,
                     Model=fun,
                     Dev=dev,
                     AIC=aic,
                     np=np))
    cat("\n")
    print(data.frame(estimate=esti,
                     SE=se,
                     CI.95.lower=esti - 1.96*se,
                     CI.95.upper=esti + 1.96*se))
    cat("\nProfile likelihood CI:\n")
    print(data.frame(lower.bound=low.bd,
                     upper.bound=up.bd))
    invisible(structure(list(dev=dev,
                             aic=aic,
                             estimate=esti,
                             se=se,
                             residuals=dev.res,
                             interval=intlen)))
}
