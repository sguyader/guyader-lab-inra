library(nlme)
library(lattice) ; library(latticeExtra) ; library(grid)
library(Cairo)


disper2 <- read.table("disper2.txt", head=T,sep="\t")
disper2$sporedensity <- disper2$spores/(2.5*5.5)  # densité par cm2
disper2.g <- groupedData(sporedensity~distance|run, data=disper2)

#### Modèle linéarisé

#disper2.g[71,c(5,6)] <- NA # NA pour la ligne 153 qui a un compte de spores à 0
disper2.g <- disper2.g[disper2.g$sporedensity!=0,]
disper2.g <- disper2.g[disper2.g$run!=1,]
disper2.g <- disper2.g[disper2.g$run!=9,]
disper2.g$ln.sporedensity <- log(disper2.g$sporedensity)
disper2.g$ln.distance <- log(disper2.g$distance)

disper2.g <- data.frame(disper2.g, runlevel = factor(disper2.g$run, levels = unique(disper2.g$run))) # défini le run en facteur à niveaux
disper2.g <- groupedData(sporedensity~distance|runlevel, data=disper2.g)

## Modèle Pwer
postscript("fig1-modpower.eps", width=7, height=7, horizontal=FALSE, onefile=FALSE, paper="special", family="Helvetica")
xyplot(ln.sporedensity ~ ln.distance|runlevel, data=disper2.g, xlab="ln(distance en cm)", ylab=list(expression(bold(paste("ln(densité de spores par ",cm^2,")")))),
       panel=function(x,y){
         panel.xyplot(x,y)
         panel.abline(lm(y~ x), lty=1)}, 
       strip= strip.custom(factor.levels=c("R2","R3","R4","R5","R6","R7","R8","R10"), par.strip.text = list(cex=0.75)),
       between = list(x = 0.5, y = 0.5)
)
dev.off()

f.modpower <- function(distance,a,b) {a*(distance+c)^(-b)}

m.modpower1 <- lm(ln.sporedensity~ln.distance, data=disper2.g)
summary(m.modpower1)

m.modpower2 <- lme(fixed=ln.sporedensity~1, random=~1|run, data=disper2.g, method="ML") # unconditional means model
summary(m.modpower2)
VarCorr(m.modpower2)
# run = pdLogChol(1) 
#             Variance  StdDev   
# (Intercept) 8.513285e-09 9.226747e-05   # (intercept) = variance inter-run
# Residual    3.967689e+00 1.991906e+00   # Residual = variance intra-run
# Ce résultat montre que la variance inter-run est bien inférieure à la variance intra-run, il n'est donc pas nécessaire de procéder à un modèle effets mélangés
tau.sq <- as.numeric(VarCorr(m.modpower2)[1,1])
sigma.sq <- as.numeric(VarCorr(m.modpower2)[2,1])
tau.sq/(tau.sq+sigma.sq)
# [1] 2.145653e-09      # la corrélation entre observations d'un même run est nulle

m.modpower3 <- nls(sporedensity~f.modpower(distance,a,b), start=c(a=1.14e5,b=2.27), data=disper2.g) # random intercept model
(as.numeric(VarCorr(m.modpower2)[2,1]) - as.numeric(VarCorr(m.modpower3)[2,1]))/ as.numeric(VarCorr(m.modpower2)[2,1]) # Pseudo-R2 pour influence de distance sur niveau 1
# [1] 0.7834556 = Pseudo R2

anova(m.modpower2,m.modpower3)  # m.modpower3 est meilleur

#m.modpower4 <- lme(fixed=ln.sporedensity~1+log(distance), random=~1+distance|run, data=disper2.g, method="ML") # random intercept model
#anova(m.modpower3,m.modpower4)  # pas d'amélioration AIC, m.modpower3 préféré par BIC

BIC(m.modpower1, m.modpower3) # m.modpower3 préféré à m.modpower1

subj.specific.parms<-coef(m.modpower3)
pop.average.parms<-fixef(m.modpower3)
#create data frame of subject specific estimates and species names
ran.data<-data.frame(rownames(subj.specific.parms),subj.specific.parms)
colnames(ran.data)<-c('run','int','slope')
#add model estimates to the raw data
disper2.g2<-merge(disper2.g,ran.data)

moy.spod <- function(sstab){
  mean.ln.spod <- mean(sstab$ln.sporedensity[!is.na(sstab$ln.sporedensity)])
  stdev <- sqrt(var(sstab$ln.sporedensity[!is.na(sstab$ln.sporedensity)]))  #/sqrt(length(sstab$ratio[!is.na(sstab$ratio)]))
  upper <- mean.ln.spod + stdev
  lower <- mean.ln.spod - stdev
  data.frame(mean.ln.spod=mean.ln.spod, stdev=stdev, upper=upper, lower=lower, ln.distance=unique(sstab$ln.distance), run=unique(sstab$run)) }
tab.moy.spod <- do.call("rbind", by(disper2.g, list(disper2.g$ln.distance, disper2.g$run), moy.spod))

tab.moy.spod <- data.frame(tab.moy.spod, runlevel=factor(tab.moy.spod$run, levels=unique(tab.moy.spod$run))) # défini le run en facteur à niveaux


plot1 <- xyplot(mean.ln.spod ~ ln.distance, groups=factor(run, levels=c(2,3,4,5,6,7,8,10)), data=tab.moy.spod, type="b", main="",
                xlab=list("ln(distance en cm)"), ylab=list(expression(bold(paste("ln(densité de spores par ",cm^2,")")))), 
                key=list(text=list(as.character(sort(as.numeric(levels(tab.moy.spod$run))))), corner=c(0.85,0.95),
                         points=list(pch=1:8, col=1:8),
                         lines=list(lty=1:8,col=1:8)), 
                panel=function(x,y,...){
                  panel.xyplot(x,y,...)
                  grid.text("A", .95, .93, gp=gpar(fontsize=18, font=2))
                })

plot2 <- xyplot(ln.sporedensity~ ln.distance, data=disper2.g, xlab=list("ln(distance en cm)"), ylab=list(expression(bold(paste("ln(densité de spores par ",cm^2,")")))), panel=function(x,y,...)
{
  panel.xyplot(x,y,...)
  #subject-specific lines
  #panel.abline(c(disper2.g2$int[disper2.g2$run=="1"][1], disper2.g2$slope[disper2.g2$run=="1"][1]), lty=1, col="grey80")
  panel.abline(c(disper2.g2$int[disper2.g2$run=="2"][1], disper2.g2$slope[disper2.g2$run=="2"][1]), lty=1, col="grey80")
  panel.abline(c(disper2.g2$int[disper2.g2$run=="3"][1], disper2.g2$slope[disper2.g2$run=="3"][1]), lty=1, col="grey80")
  panel.abline(c(disper2.g2$int[disper2.g2$run=="4"][1], disper2.g2$slope[disper2.g2$run=="4"][1]), lty=1, col="grey80")
  panel.abline(c(disper2.g2$int[disper2.g2$run=="5"][1], disper2.g2$slope[disper2.g2$run=="5"][1]), lty=1, col="grey80")
  panel.abline(c(disper2.g2$int[disper2.g2$run=="6"][1], disper2.g2$slope[disper2.g2$run=="6"][1]), lty=1, col="grey80")
  panel.abline(c(disper2.g2$int[disper2.g2$run=="7"][1], disper2.g2$slope[disper2.g2$run=="7"][1]), lty=1, col="grey80")
  panel.abline(c(disper2.g2$int[disper2.g2$run=="8"][1], disper2.g2$slope[disper2.g2$run=="8"][1]), lty=1, col="grey80")
  #panel.abline(c(disper2.g2$int[disper2.g2$run=="9"][1], disper2.g2$slope[disper2.g2$run=="9"][1]), lty=1, col="grey80")
  panel.abline(c(disper2.g2$int[disper2.g2$run=="10"][1], disper2.g2$slope[disper2.g2$run=="10"][1]), lty=1, col="grey80")
  #population-average line
  panel.abline(c(pop.average.parms[1], pop.average.parms[2]), lty=1, lwd=2)
  grid.text("B", .95, .93, gp=gpar(fontsize=18, font=2))
})

postscript("fig2-modpower.eps", width=7, height=7, horizontal=FALSE, onefile=FALSE, paper="special", family="Helvetica")
c(plot2,plot1, layout=c(1,2), y.same=F, merge.legends=T)
dev.off()

postscript("fig3-modpower.eps", width=4.5, height=3, horizontal=FALSE, onefile=FALSE, paper="special", family="Helvetica")
plot(m.modpower3, xlab="Valeurs ajustées", ylab="Résidus standardisés",grid=F)
dev.off()

# Fitted vs Observed
graph.A <- xyplot(disper2.g$ln.sporedensity[!is.na(disper2.g$ln.sporedensity)]~fitted(m.modpower3), lty=1, xlab=list("Valeurs ajustées"), ylab=list("Valeurs observées"),
                  panel= function(x,y) {
                    grid.text("A", .07, .93, gp=gpar(fontsize=18, font=2))
                    panel.xyplot(x, y)
                    panel.abline(a=0,b=1)
                  })

# QQPlot
require(envelope)
resid <- residuals(m.modpower3,type="pearson")
resid <- resid[!is.na(resid[])]
qqnorm(resid)
envl <- envl.plot(resid,conf=95)
theor <- envl$quantiles
low.band <- envl$low.band
high.band <- envl$high.band

graph.B <- xyplot(sort(resid)~sort(theor), xlim=c(-3,3), ylim=c(-4,4), xlab=list("Quantiles théoriques (loi normale)"), ylab=list("Résidus standardisés"),
                  panel= function(x,y) {
                    grid.text("B", .07, .93, gp=gpar(fontsize=18, font=2))
                    #panel.polygon(c(theor, rev(theor)), c(high.band, rev(low.band)), col="grey85",border=F)
                    panel.points(theor,low.band,type="l",lty=2)
                    panel.points(theor,high.band,type="l",lty=2)
                    panel.xyplot(x,y)
                    panel.qqmathline(x,y)
                  })

# Combine graphs A and B on same page
postscript("fig4-modpower.eps", width=4, height=7, horizontal=FALSE, onefile=FALSE, paper="special", family="Helvetica")
print(graph.A, pos = c(0.0, 0.5, 1.0, 1.0), more = T)
print(graph.B, pos = c(0.0, 0.0, 1.0, 0.5), more = F)
dev.off()

# Calcul de la d50
params <- fixef(m.modpower3)
params
# (Intercept)    distance 
# 8.0020788  -0.1564117 
d50 <- log(0.5)/params[2]
d50
# distance 
# 4.431556   # Signifie que la densité de spores décroit de 50% tous les 4.4 cm


moy.spod2 <- function(sstab){
  mean.ln.spod <- mean(sstab$ln.sporedensity[!is.na(sstab$ln.sporedensity)])
  stdev <- sqrt(var(sstab$ln.sporedensity[!is.na(sstab$ln.sporedensity)]))  #/sqrt(length(sstab$ratio[!is.na(sstab$ratio)]))
  upper <- mean.ln.spod + stdev
  lower <- mean.ln.spod - stdev
  data.frame(mean.ln.spod=unique(mean.ln.spod), stdev=unique(stdev), upper=unique(upper), lower=unique(lower), distance=unique(sstab$distance)) }
tab.moy.spod2 <- do.call("rbind", by(disper2.g, list(disper2.g$distance), moy.spod2))

require(memisc)

postscript("fig5-modpower.eps", width=4, height=4, horizontal=FALSE, onefile=FALSE, paper="special", family="Helvetica")
xyplot(cbind(exp(mean.ln.spod),exp(lower),exp(upper))~distance, data=tab.moy.spod2, xlim=c(0,42), xlab="Distance (cm)", ylab=list(expression(bold(paste("Densité de spores par ",cm^2)))), panel=panel.errbars, ewidth=1, make.grid="none")
trellis.focus()
panel.curve(expr=exp(params[1])*x^(params[2]))
trellis.unfocus()
dev.off()

postscript("fig6-modpower.eps", width=4, height=4, horizontal=FALSE, onefile=FALSE, paper="special", family="Helvetica")
xyplot(cbind(mean.ln.spod,lower,upper)~log(distance), data=tab.moy.spod2, xlim=c(0,5), xlab="ln(distance (cm))", ylab=list(expression(bold(paste("ln(densité de spores par ",cm^2,")")))), panel=panel.errbars, ewidth=1, make.grid="none")
trellis.focus()
panel.curve(expr=params[1]+params[2]*x)
trellis.unfocus()
dev.off()

postscript("fig7-modpower.eps", width=4, height=4, horizontal=FALSE, onefile=FALSE, paper="special", family="Helvetica")
fig7.1.d2 <- xyplot(cbind(exp(mean.ln.spod),exp(lower),exp(upper))~distance, data=tab.moy.spod2, xlab=list("Distance (cm)"), xlim=c(0,46), ylim=c(0,1000), ylab=list(expression(bold(paste("Densité de spores par ",cm^2)))), panel=panel.errbars, ewidth=1, make.grid="none")
fig7.2.d2 <- xyplot(cbind(exp(mean.ln.spod),exp(lower),exp(upper))~distance, data=tab.moy.spod2, xlab=list("Distance (cm)"), xlim=c(0,46), ylim=c(0,1000), ylab=list(expression(bold(paste("Densité de spores par ",cm^2)))), panel=function(x,y){
  #subject-specific lines
  #panel.curve(exp(disper2.g2$int[disper2.g2$run=="1"][1])*exp(disper2.g2$slope[disper2.g2$run=="1"][1]*x), lty=1, col="grey80")
  panel.curve(exp(disper2.g2$int[disper2.g2$run=="2"][1])*exp(disper2.g2$slope[disper2.g2$run=="2"][1]*x), lty=1, col="grey80")
  panel.curve(exp(disper2.g2$int[disper2.g2$run=="3"][1])*exp(disper2.g2$slope[disper2.g2$run=="3"][1]*x), lty=1, col="grey80")
  panel.curve(exp(disper2.g2$int[disper2.g2$run=="4"][1])*exp(disper2.g2$slope[disper2.g2$run=="4"][1]*x), lty=1, col="grey80")
  panel.curve(exp(disper2.g2$int[disper2.g2$run=="5"][1])*exp(disper2.g2$slope[disper2.g2$run=="5"][1]*x), lty=1, col="grey80")
  panel.curve(exp(disper2.g2$int[disper2.g2$run=="6"][1])*exp(disper2.g2$slope[disper2.g2$run=="6"][1]*x), lty=1, col="grey80")
  panel.curve(exp(disper2.g2$int[disper2.g2$run=="7"][1])*exp(disper2.g2$slope[disper2.g2$run=="7"][1]*x), lty=1, col="grey80")
  panel.curve(exp(disper2.g2$int[disper2.g2$run=="8"][1])*exp(disper2.g2$slope[disper2.g2$run=="8"][1]*x), lty=1, col="grey80")
  #panel.curve(exp(disper2.g2$int[disper2.g2$run=="9"][1])*exp(disper2.g2$slope[disper2.g2$run=="9"][1]*x), lty=1, col="grey80")
  panel.curve(exp(disper2.g2$int[disper2.g2$run=="10"][1])*exp(disper2.g2$slope[disper2.g2$run=="10"][1]*x), lty=1, col="grey80")
  #population-average line
  panel.curve(exp(pop.average.parms[1])*exp(pop.average.parms[2]*x), lty=1, lwd=2)
  #grid.text("B", .95, .93, gp=gpar(fontsize=18, font=2))
})
fig7.1.d2 + as.layer(fig7.2.d2, under=T)
dev.off()