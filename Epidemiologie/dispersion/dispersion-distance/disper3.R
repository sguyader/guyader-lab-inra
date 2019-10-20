setwd("C:/Users/guyader/TRAVAIL/Essais/2012/Dispersion")


disper2 <- read.table("disper2.txt", head=T,sep="\t")
disper2$sporedensity <- disper2$spores/(2.5*5.5)  # densité par cm2
disper2.g <- groupedData(sporedensity~distance|run, data=disper2)

#### Modèle linéarisé

#disper2.g[71,c(5,6)] <- NA # NA pour la ligne 153 qui a un compte de spores à 0
disper2.g <- disper2.g[disper2.g$sporedensity!=0,]
disper2.g <- disper2.g[disper2.g$run!=1,]
disper2.g <- disper2.g[disper2.g$run!=9,]
disper2.g$ln.sporedensity <- log(disper2.g$sporedensity)

disper2.g <- data.frame(disper2.g, runlevel = factor(disper2.g$run, levels = unique(disper2.g$run))) # défini le run en facteur à niveaux
disper2.g <- groupedData(sporedensity~distance|runlevel, data=disper2.g)

xyplot(sporedensity ~ distance, data=disper2.g, xlab="Distance (cm)", ylab=list(expression(bold(paste("ln(spore density per ",cm^2,")")))),
       panel=function(x,y){
         panel.xyplot(x,y)
         #panel.abline(lm(y~ x), lty=1)
         }, 
       strip= strip.custom(factor.levels=c("R2","R3","R4","R5","R6","R7","R8","R10"), par.strip.text = list(cex=0.75)),
       between = list(x = 0.5, y = 0.5))

#### Modèle négatif exponentiel

plot(disper2.g, xlim=c(0,50), aspect=1)
xyplot(sporedensity~distance|run, groups=run, type="p", data=disper2.g, xlim= c(0,50))
#              distance <- c(0:45)
#              a=5000 ; b=0.15
#              y <- a*exp(-b*distance)
#              trellis.focus()
#              panel.points(distance,y,type="l",col="black",lty=1)
#              trellis.unfocus()

bwplot(sporedensity~factor(distance), data=disper2.g)

f.exp <- function(b1,b2,c,distance) { 3e3*(c*exp(-b1*distance) + (1-c)*exp(-b2*distance)) }

m.exp <- nls(sporedensity~f.exp(b1,b2,c,distance), data=disper2.g, start=c(b1=0.02, b2=0.1, c=0.06))
plot(m.exp, run~resid(.), abline=0)

names(m.exp)
coefs <- coef(m.exp)
xyplot(sporedensity~distance, data=disper2.g,
       panel=function(x,y){
         panel.xyplot(x,y)
         panel.curve(3e3*(coefs[3]*exp(-coefs[1]*x) + (1-coefs[3])*exp(-coefs[2]*x)), from=5, to=40)
         panel.curve(3e3*(coefs[3]*exp(-coefs[1]*x)), from=5, to=40, lty=2)
         panel.curve(3e3*((1-coefs[3])*exp(-coefs[2]*x)), from=5, to=40, lty=3)
       }
      )

