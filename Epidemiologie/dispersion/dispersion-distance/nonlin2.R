library(nlme)
library(lattice) ; library(latticeExtra) ; library(grid)
library(Cairo)

lattice.options(default.theme = modifyList(standard.theme(color=F),
                                           list(axis.text=list(cex=1,font=2), par.xlab.text=list(cex=1,font=2),par.ylab.text=list(cex=1,font=2),
                                                superpose.symbol=list(pch=1:12,col=1:12),
                                                superpose.line=list(lty=1:12,col=1:12))))


disper2 <- read.table("disper2.txt", head=T,sep="\t")
disper2$sporedensity <- disper2$spores/(2.5*5.5)  # densité par cm2
disper2.g <- groupedData(sporedensity~distance|run, data=disper2)

disper2.g <- disper2.g[disper2.g$sporedensity!=0,]
disper2.g <- disper2.g[disper2.g$run!=1,]
disper2.g <- disper2.g[disper2.g$run!=9,]

#### Modèle négatif exponentiel

plot(disper2.g, xlim=c(0,50), aspect=1)
xyplot(sporedensity~distance|run, groups=run, type="p", data=disper2, xlim= c(0,50))

#              distance <- c(0:45)
#              a=5000 ; b=0.15
#              y <- a*exp(-b*distance)
#              trellis.focus()
#              panel.points(distance,y,type="l",col="black",lty=1)
#              trellis.unfocus()


f.exp <- function(a,b,distance) { a*exp(-b*distance) }

m.exp <- nls(sporedensity~f.exp(a,b,distance), data=disper2.g, start=c(a=2852, b=0.145))
plot(m.exp, run~resid(.), abline=0)

m.exp.lis <- nlsList(sporedensity~f.exp(a,b,distance), data=disper2.g, start=c(a=2852, b=0.145))
plot(m.exp.lis, run~resid(.), abline=0)
plot(intervals(m.exp.lis))

m.exp.nlme1 <- nlme(m.exp.lis, random=a+b ~1, weights=varPower())
m.exp.nlme2 <- nlme(m.exp.lis, random=a ~1, weights=varPower())
m.exp.nlme3 <- nlme(m.exp.lis, random=b ~1, weights=varPower())

anova(m.exp.nlme1,m.exp.nlme2,m.exp.nlme3)

plot(m.exp.nlme3, id=0.05, adj=-1)
qqnorm(m.exp.nlme3)
plot(augPred(m.exp.nlme3, level=0:1), xlim=c(0,50), layout=c(3,3))

summary(m.exp.nlme3)

plot(m.exp.nlme3, sporedensity~fitted(.),abline=c(0,1))
