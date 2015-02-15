#
#
# put in the data
#
#
library(ismev)  
data(rain)
#
#
# gain the year vector! (This is fudged but is ok for appearences!!:) - the data do not
# have date info included!
#
#
l<-length(rain)
a<-seq(1:l)
f<-48
b<-((a-1)*f)/l
c<-b+1914
year<-c
#
#
# plot of the data:
#
#
postscript("rain_daily.ps",horizontal=FALSE)
par(mfrow=c(1,1))
plot(year,rain,ylab="Daily Rainfall (mm)",main="Daily Rainfall at a location in SW England (1914-1961)")
dev.off()
#
#
# plot with red threshold at u=30
#
#
postscript("rain_daily_thresh.ps",horizontal=FALSE)
par(mfrow=c(1,1))
plot(year,rain,ylab="Daily Rainfall (mm)",main="Daily Rainfall at a location in SW England (1914-1961)")
abline(h=30,col=2,lwd=3)
dev.off()
#
#
# fit of the threshold model and plot of goodness of fit: 
#
#
postscript("rain_thfit.ps",horizontal=TRUE)
par(mfrow=c(1,1))
fit<-gpd.fit(rain,30)
gpd.diag(fit)
dev.off()
#
#
# profile likelihood for threshold data:
#
#
postscript("rain_threshproflik.ps",horizontal=TRUE)
#  par(mfrow=c(1,1))
fit<-gpd.fit(rain,30)
gpd.prof(fit,100,79,200)
dev.off()
#
#
# mrl plot:
#
#
postscript("mrl_rain.ps",horizontal=FALSE)
par(mfrow=c(1,1))
mrl.plot(rain)
dev.off()
#
#
# mrl plot with chosen threshold:
#
#
postscript("mrl_rain_thresh.ps",horizontal=FALSE)
par(mfrow=c(1,1))
mrl.plot(rain)
abline(v=30,col=2,lwd=2)
dev.off()
#
#
# gpd stability plots:
#
#
postscript("rain_fitrange.ps",horizontal=TRUE)
#  par(mfrow=c(1,1))
gpd.fitrange(rain,30,50,nint=21)
dev.off()
#
#
# find the maxima data and year vec for the maxima:
#
#
maxima<-vector()
year1<-floor(year)
for(i in 1:48){
    maxima[i]<-max(rain[year1==1913+i])
}
year2<-as.numeric(levels(factor(year1)))
#
#
# maxima plot for the rain data:
#
#
postscript("rain_maxima.ps",horizontal=FALSE)
par(mfrow=c(1,1))
plot(year2,maxima,xlab="Year",ylab="Rainfall (mm)",main="Annual Maxima for Rain Data")
dev.off()
#
#
# fit and plot diagnostics for maxima data:
#
#
postscript("rain_fit.ps",horizontal=TRUE)
#  par(mfrow=c(1,1))
fit1<-gev.fit(maxima)
gev.diag(fit1)
dev.off()
#
#
# profile likelihood for maxima data:
#
#
postscript("rain_proflik.ps",horizontal=TRUE)
#  par(mfrow=c(1,1))
fit1<-gev.fit(maxima)
gev.prof(fit1,100,77,200)
dev.off()
########################################################
