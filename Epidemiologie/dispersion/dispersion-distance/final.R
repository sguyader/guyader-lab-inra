library(nlme)
library(dplyr)


disper2 <- read.table("disper2.txt", head=T,sep="\t")
disper2$sporedensity <- disper2$spores/(2.5*5.5)  # densité par cm2
disper2 <- dplyr::filter(disper2, run > 1, run < 8)
disper2 <- groupedData(sporedensity~distance|run, data=disper2)
plot(disper2)
disper2 <- groupedData(sporedensity~distance|run, data=disper2)


### Modèles linéarisés

## exponentiel : intégrée : y = a*exp(-b*distance) ; linéarisée : log(y) = log(a) - b*distance
## power :                  y = a*distance^(-b)                   log(y) = log(a) - b*(log(distance))


## Modèle Power
pow.gls <- gls(log(sporedensity)~log(distance), data=disper2, weights=varPower())
my_qqplot(pow.gls)


# Graphe linéaire
ggplot(data=disper2, aes(x=log(distance), y=log(sporedensity))) +
  geom_point() +
  geom_abline(slope=(coef(pow.gls)[2]), intercept=(coef(pow.gls)[1])) +
  my_ggplot_theme()


## Modèle exponentiel
exp.gls <- gls(log(sporedensity)~distance*axis, data=disper2, weights=varPower())
my_qqplot(exp.gls)

# Graphe linéaire
ggplot(data=disper2, aes(x=distance, y=log(sporedensity))) +
  geom_point() +
  geom_abline(slope=(coef(exp.gls)[2]), intercept=(coef(exp.gls)[1])) +
  my_ggplot_theme()

## Graphe en coordonnées d'origine
ggplot(data=disper2, aes(x=distance, y=sporedensity)) +
  geom_jitter(width=1) +
  stat_function(fun=function(x){exp(coef(exp.gls)[1])*exp(coef(exp.gls)[2]*x)}, aes(linetype="Neg. exponential")) +
  stat_function(fun=function(x){exp(coef(pow.gls)[1])*x^((coef(pow.gls)[2]))}, aes(linetype="Inverse power")) +
  scale_linetype_manual("", values = c("Neg. exponential"="solid", "Inverse power"="dashed"), breaks=c("Neg. exponential","Inverse power")) +
  my_ggplot_theme() +
  theme(legend.position=c(0.8, 0.8))

## Tableau synthétique
tab.moy.spod <- disper2 %>% group_by(distance) %>% summarize(
  N=n(),
  mean=mean(sporedensity),
  sd=sd(sporedensity),
  se=sd/sqrt(N),
  ci.student=qt(0.975,df=N-1)*se
)

ggplot(data=tab.moy.spod, aes(x=distance, y=mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  stat_function(fun=function(x){exp(coef(exp.gls)[1])*exp(coef(exp.gls)[2]*x)}) +
  stat_function(fun=function(x){exp(coef(pow.gls)[1])*x^((coef(pow.gls)[2]))}, linetype="dashed") +
  xlab("Distance from source (cm)") + ylab("Density (spores per cm²)") +
  my_ggplot_theme()

### Using gnls

exp.fun <- function(a,b,distance) { a*exp(-b*distance) }
pow.fun <- function(a,b,distance) { a*distance^(-b) }

exp.gnls <- gnls(sporedensity~exp.fun(a,b,distance), data=disper2, start=c(a=2.852e3, b=0.145), weights=varPower()) #, weights=varFixed(~sporedensity*(1-sporedensity)))
pow.gnls <- gnls(sporedensity~pow.fun(a,b,distance), data=disper2, start=c(a=1.1e7, b=4.44), weights=varPower())

BIC(exp.gnls,pow.gnls) # le modèle exponentiel est préféré

ggplot(data=tab.moy.spod, aes(x=distance, y=mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  stat_function(fun=function(x){coef(exp.gnls)[1]*exp(-coef(exp.gnls)[2]*x)}, aes(linetype="Neg. exponential")) +
  stat_function(fun=function(x){coef(pow.gnls)[1]*x^(-coef(pow.gnls)[2])}, aes(linetype="Inverse power")) +
  scale_linetype_manual("", values = c("Neg. exponential"="solid", "Inverse power"="dashed"), breaks=c("Neg. exponential","Inverse power")) +
  xlab("Distance from source (cm)") + ylab("Density (spores per cm²)") +
  my_ggplot_theme() +
  theme(legend.position=c(0.8, 0.85))

grad.table <- signif(BIC(exp.gnls,pow.gnls), digits=4)

ggplot(data=disper2, aes(x=distance, y=sporedensity)) +
  geom_jitter(width=0.8) +
  stat_function(fun=function(x){coef(exp.gnls)[1]*exp(-coef(exp.gnls)[2]*x)}, aes(linetype="Neg. exponential")) +
  stat_function(fun=function(x){coef(pow.gnls)[1]*x^(-coef(pow.gnls)[2])}, aes(linetype="Inverse power")) +
  scale_linetype_manual("", values = c("Neg. exponential"="solid", "Inverse power"="dashed"), breaks=c("Neg. exponential","Inverse power")) +
  xlab("Distance from source (cm)") + ylab("Density (spores per cm²)") +
  annotation_custom(tableGrob(grad.table, theme=ttheme_minimal(base_size = 10)),
                    xmin=30, ymin=0) +
  my_ggplot_theme() +
  theme(legend.position=c(0.8, 0.85))

## calcul d50 :
coef(exp.gnls)
d50 <- log(0.5)/(-coef(exp.gnls)[2])
d50 # 4.86 cm




### Using DRC

# modèle exponentiel
exp2.drc <- drm(sporedensity~distance, data=disper2, fct=EXD.2())
plot(exp2.drc, log="", type="all")
summary(exp2.drc)  ## donne a=2304 b=1/e=0.130

# transformation boxcox pour normaliser les données
exp2.drc.bc <- boxcox(exp2.drc) # donne un lambda=0.25
plot(exp2.drc.bc, log="", type="all")
summary(exp2.drc.bc)  # donne a=2356 b=1/e=0.138

# modèle power
# définir une fonction perso

powfct <- function(x, parm){ parm[, 1]*x^((parm[, 2])) }
powfctSS <- function(cust.data) {
  lmfit <- lm(log(cust.data[,2])~log(cust.data[,1]))
  a <- exp(coef(lmfit)[1])
  b <- coef(lmfit)[2]
  return(c(a, b))
}
powNames <- c("a", "b")

pow.drc <- drm(sporedensity~distance, data=disper2, fct=list(powfct, powfctSS, powNames))
plot(pow.drc, log="", type="all")
summary(pow.drc) # donne a=74201 b=-2.07

# transformation boxcox pour normaliser les données
pow.drc.bc <- boxcox(pow.drc) #donne un lambda=0.25
plot(pow.drc.bc, log="", type="all")
summary(pow.drc.bc)  ## donne a=2.77e5 b=-2.635

# graphe avec Exponentiel + Power
ggplot(data=disper2, aes(x=distance, y=sporedensity)) +
  #geom_boxplot(aes(group=distance), width=1.5) +
  geom_jitter(width=0.4) +
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  stat_function(fun=function(x){coef(exp2.drc.bc)[1]*exp(-1/coef(exp2.drc.bc)[2]*x)}) +
  #stat_function(fun=function(x){coef(pow.drc.bc)[1]*x^(coef(pow.drc.bc)[2])}, linetype="dashed") +
  #stat_function(fun=function(x){exp(coef(exp.gls)[1])*exp(coef(exp.gls)[2]*x)}, colour="red") +
  stat_function(fun=function(x){coef(exp.gnls)[1]*exp(-coef(exp.gnls)[2]*x)}, colour="red", linetype="dashed") +
  my_ggplot_theme()

# graphe du meilleur modèle (exponentiel)
newdata <- data.frame(distance=10:40)
exp2.drc.bc.pred <- predict(exp2.drc.bc, newdata=newdata, interval="confidence")
newdata$drcp <- exp2.drc.bc.pred[,1]
newdata$drcmin <- exp2.drc.bc.pred[,2]
newdata$drcmax <- exp2.drc.bc.pred[,3]

newdata$glnspred <- exp(predict(exp.gnls, newdata=data.frame(distance=10:40), level=0))


ggplot(data=tab.moy.spod, aes(x=distance, y=mean)) +
  geom_jitter(width=0.4) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  #geom_ribbon(data=newdata, aes(x=distance, y=p, ymin=pmin, ymax=pmax), alpha=0.2) +
  stat_function(fun=function(x){coef(exp2.drc.bc)[1]*exp(-1/coef(exp2.drc.bc)[2]*x)}) +
  my_ggplot_theme()

########

plotdist(log(disper2$sporedensity), histo=T, demp=T)
descdist(log(disper2$sporedensity), boot=1001)
spo.fit <- fitdist(((disper2$sporedensity)^0.25-1)/0.25, "norm", method="mle")
summary(spo.fit)
plot(spo.fit)
