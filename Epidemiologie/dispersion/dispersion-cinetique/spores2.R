library(lattice)

spores <- read.table("spores.txt", header=T,sep="\t")

spores <- subset(spores, rep!="4")
spores <- groupedData(spores~cat|rep, data=spores)
plot(spores, layout=c(2,2), aspect="fill")

### Rechercher distribuition des données

require(fitdistrplus)
descdist(spores$spores, boot=1001)
mle.fit <- fitdist(spores$spores,"lnorm",method="mle")
summary(mle.fit)
plot(mle.fit)
mle.fit <- fitdist(log(spores$spores),"norm",method="mle")
summary(mle.fit)
plot(mle.fit)

spores$log.spores <- log(spores$spores)

summary(aov(log.spores~cat+rep, data=spores))

### Weibull

weibfun <- function(cat,a,b,e) {log(abs(a*exp(-0.5*((log(cat)-log(abs(e)))/b)^2)))}
weib.pdf.nls.spo <- nls(log.spores~weibfun(cat,a,b,e), data=spores, start=c(a=400, e=0.4, b=-2))
summary(weib.pdf.nls.spo)
plot(weib.pdf.nls.spo, col="black")

require(nlme)
weib.list <- nlsList(log.spores~weibfun(cat,a,b,e), data=spores, start=c(a=400, e=0.4, b=-2))
summary(weib1.list)
nlmeweib1 <- nlme(weib1.list, groups=~rep, random=e~1)
summary(nlmeweib1)
plot(augPred(nlmeweib1,primary=~cat)) 
plot(ACF(nlmeweib1,alpha=0.05)) 

nlmeweib1.e <- nlme(ratio~weibfun(cat,a,b,e), data=spores, fixed=a+b+e~1, random=e~1, groups=~rep, start=c(a=0.35, b=-0.52, e=15))#, correlation=corAR1())


# Fitted vs Observed
graph.A <- xyplot(log(spores$spores)~fitted(weib.pdf.nls.spo ), lty=1, xlab=list("Fitted values"), ylab=list("Observed values"),
                  panel= function(x,y) {
                    grid.text("A", .07, .93, gp=gpar(fontsize=18, font=2))
                    panel.xyplot(x, y, col="black")
                    panel.abline(a=0,b=1)
                  })

# QQPlot
require(envelope)
resid.weib <- sort(residuals(weib.pdf.nls.spo ,type="pearson"))
resid.weib <- resid[!is.na(resid.weib[])]
#qqnorm(resid.weib)
#qqenvl(resid.weib)
envl.weib <- envl.plot(resid.weib,conf=95, plot.it=F)
theor.weib <- sort(envl.weib$quantiles)
low.band.weib <- envl.weib$low.band
high.band.weib <- envl.weib$high.band

graph.B <- xyplot(resid.weib~theor.weib, xlab=list("Theoretical normal quantiles"), ylab=list("Standardised residuals"),
                  panel= function(x,y) {
                    grid.text("Weib", .07, .93, gp=gpar(fontsize=18, font=2))
                    panel.polygon(c(theor, rev(theor)), c(high.band, rev(low.band)), col="grey85",border=F)
                    panel.points(theor.weib,low.band.weib,type="l",lty=2, col="black")
                    panel.points(theor.weib,high.band.weib,type="l",lty=2, col="black")
                    panel.xyplot(x,y, col="black", pch=16)
                    #panel.qqmathline(x,y, col="blue")
                    panel.curve(0+1*x,from=min(theor.weib),to=max(theor.weib))
                  })

# Combine graphs A and B on same page
postscript("fig-deriv-fitted-qqplot.eps", width=4, height=7, horizontal=FALSE, onefile=FALSE, paper="special", family="Helvetica")
print(graph.A, pos = c(0.0, 0.5, 1.0, 1.0), more = T)
print(graph.B, pos = c(0.0, 0.0, 1.0, 0.5), more = F)
dev.off()

postscript("fig-weibull-pdf.eps", width=4, height=4, horizontal=FALSE, onefile=FALSE, paper="special", family="Helvetica")
xyplot(log.spores~cat, data=spores, xlim=c(0,110), ylim=c(0,6), xlab=list("Number of impacting drops"), ylab=list("log(number of spores)"), panel=function(x,y){
  panel.points(x,y, col="black")
  panel.curve(coef(weib.pdf.nls.spo )[1]*exp(-0.5*((log(x)-log(coef(weib.pdf.nls.spo )[2]))/(coef(weib.pdf.nls.spo )[3]))^2))
  })
dev.off()


### Fréchet (wikipedia param)

#PDF

frech2.pdf <- function(cat,A,alpha,beta){ A*(alpha/beta)*(((cat-4)/beta)^(-1-alpha))*exp(-((cat-4)/beta)^-alpha) }
frech2.pdf.nls.spo <- nls(log.spores~frech2.pdf(cat,A,alpha,beta),data=spores, start=c(A=9000,alpha=0.2,beta=40000))
summary(frech2.pdf.nls.spo)
plot(frech2.pdf.nls.spo, col="black")

A <- coef(frech2.pdf.nls.spo)[1] ; alpha <- coef(frech2.pdf.nls.spo)[2] ; beta <- coef(frech2.pdf.nls.spo)[3]
A <- 200; alpha <- 1.5 ; beta <- 22.2 ; 

#postscript("fig-deriv-frechet.eps", width=4, height=4, horizontal=FALSE, onefile=FALSE, paper="special", family="Helvetica")
xyplot(log.spores~cat, data=spores, xlim=c(0,110), ylim=c(0,6), xlab=list("Number of impacting drops"), ylab=list("log(number of spores)"), panel=function(x,y){
  panel.points(x,y, col="black")
  panel.curve(coef(frech2.pdf.nls.spo)[1]*(coef(frech2.pdf.nls.spo)[2]/coef(frech2.pdf.nls.spo)[3])*(((x-0)/coef(frech2.pdf.nls.spo)[3])^(-1-coef(frech2.pdf.nls.spo)[2]))*exp(-((x-0)/coef(frech2.pdf.nls.spo)[3])^-coef(frech2.pdf.nls.spo)[2]), from=0, to=110)
  panel.curve(coef(weib.pdf.nls.spo)[1]*exp(-0.5*((log(x)-log(coef(weib.pdf.nls.spo )[2]))/(coef(weib.pdf.nls.spo )[3]))^2), lty=2)
  })
dev.off()

BIC(weib.pdf.nls.spo,frech2.pdf.nls.spo)

# Fitted vs Observed PDF
graph.A <- xyplot(spores$ratio~fitted(frech2.pdf.nls), lty=1, xlab=list("Fitted values"), ylab=list("Observed values"),
                  panel= function(x,y) {
                    grid.text("A", .07, .93, gp=gpar(fontsize=18, font=2))
                    panel.xyplot(x, y)
                    panel.abline(a=0,b=1)
                  })

# QQPlot PDF
require(envelope)
resid.frech <- sort(residuals(frech2.pdf.nls.spo,type="pearson"))
resid.frech <- resid[!is.na(resid.frech[])]
#qqnorm(resid.frech)
#qqenvl(resid.frech)
envl.frech <- envl.plot(resid.frech,conf=95, plot.it=F)
theor.frech <- sort(envl.frech$quantiles)
low.band.frech <- envl.frech$low.band
high.band.frech <- envl.frech$high.band

graph.B <- xyplot(resid.frech~theor.frech, xlab=list("Theoretical normal quantiles"), ylab=list("Standardised residuals"),
                  panel= function(x,y) {
                    grid.text("B", .07, .93, gp=gpar(fontsize=18, font=2))
                    panel.polygon(c(theor.frech, rev(theor.frech)), c(high.band.frech, rev(low.band.frech)), col="grey85",border=F)
                    panel.points(theor.frech,low.band.frech,type="l",lty=2, col="black")
                    panel.points(theor.frech,high.band.frech,type="l",lty=2, col="black")
                    panel.xyplot(x,y, col="black", pch=16)
                    #panel.qqmathline(x,y)
                    panel.curve(0+1*x,from=min(theor.frech),to=max(theor.frech))
                  })

# Combine graphs A and B on same page
postscript("fig-frechet-pdf-fitted-qqplot.eps", width=4, height=7, horizontal=FALSE, onefile=FALSE, paper="special", family="Helvetica")
print(graph.A, pos = c(0.0, 0.5, 1.0, 1.0), more = T)
print(graph.B, pos = c(0.0, 0.0, 1.0, 0.5), more = F)
dev.off()


#CDF

frech2.cdf <- function(cat,alpha,beta){ exp(-((cat-4)/beta)^-alpha) }
frech2.cdf.nls <- nls(cumul~frech2.cdf(cat,alpha,beta), data=spores, start=c(alpha=1.2571,beta=13.0926))
summary(frech2.cdf.nls)
alpha <- coef(frech2.cdf.nls)[1] ; beta <- coef(frech2.cdf.nls)[2]

postscript("fig-cumul-frechet.eps", width=4, height=4, horizontal=FALSE, onefile=FALSE, paper="special", family="Helvetica")
xyplot(cumul~cat, data=spores, xlim=c(0,110), ylim=c(0,1), xlab=list("Number of impacting drops"), ylab=list("Cumulated spore number"),
       panel=function(x,y){
         panel.xyplot(x,y)
         panel.curve(exp(-((x-4)/beta)^-alpha), from=0, to=100)
         panel.curve(exp(-exp(coef(cumfun1)[1]*(log(x)-log(coef(cumfun1)[2])))), from=0, to=100, lty=2)
       })
dev.off()

# Fitted vs Observed CDF
graph.A <- xyplot(spores$ratio~fitted(frech2.cdf.nls), lty=1, xlab=list("Fitted values"), ylab=list("Observed values"),
                  panel= function(x,y) {
                    grid.text("A", .07, .93, gp=gpar(fontsize=18, font=2))
                    panel.xyplot(x, y)
                    panel.abline(a=0,b=1)
                  })

# QQPlot CDF
require(envelope)
resid <- sort(residuals(frech2.cdf.nls,type="pearson"))
resid <- resid[!is.na(resid[])]
qqnorm(resid)
qqenvl(resid)
envl <- envl.plot(resid,conf=95, plot.it=F)
theor <- sort(envl$quantiles)
low.band <- envl$low.band
high.band <- envl$high.band

graph.B <- xyplot(resid~theor, xlab=list("Theoretical normal quantiles"), ylab=list("Standardised residuals"),
                  panel= function(x,y) {
                    grid.text("B", .07, .93, gp=gpar(fontsize=18, font=2))
                    #panel.polygon(c(theor, rev(theor)), c(high.band, rev(low.band)), col="grey85",border=F)
                    panel.points(theor,low.band,type="l",lty=2, col="black")
                    panel.points(theor,high.band,type="l",lty=2, col="black")
                    panel.xyplot(x,y, col="black")
                    panel.curve(0+1*x,from=min(theor),to=max(theor))
                  })

# Combine graphs A and B on same page
postscript("fig-frechet-cdf-fitted-qqplot.eps", width=4, height=7, horizontal=FALSE, onefile=FALSE, paper="special", family="Helvetica")
print(graph.A, pos = c(0.0, 0.5, 1.0, 1.0), more = T)
print(graph.B, pos = c(0.0, 0.0, 1.0, 0.5), more = F)
dev.off()


postscript("fig-deriv-weibull.eps", width=4, height=4, horizontal=FALSE, onefile=FALSE, paper="special", family="Helvetica")
xyplot(ratio~cat, data=spores, xlim=c(0,110), ylim=c(0,0.5), xlab=list("Number of impacting drops"), ylab=list("Cumulated spore number"), panel=function(x,y){
  panel.points(x,y)
  panel.curve(fixef(nlmeweib1)[1]*exp(-0.5*((log(x)-log(fixef(nlmeweib1)[2]))/(fixef(nlmeweib1)[3]))^2))
  #panel.curve(1 - exp(-exp(coef(cumfun2)[1]*(log(x)-log(coef(cumfun2)[2])))))
})
dev.off()


## Regression tree

library(partykit)
party.fit <- ctree(log.spores~cat+rep, data=spores, control=ctree_control(mincriterion=0.92))
party.plot <- plot(party.fit, main="Conditional Inference Tree for log(ratio)")
print(party.fit)
spores$log.spores.party.predict <- predict(party.fit)
xyplot(log.spores~cat, data=spores, xlim=c(0,110), ylim=c(0,6), xlab=list("Number of impacting drops"), ylab=list("log(number of spores)"), panel=function(x,y){
  panel.points(x,y, col="black")
  panel.lines(spores$log.spores.party.predict~spores$cat, type="l", col="black")
})

