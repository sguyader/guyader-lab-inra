library(cowplot)

spores <- read.table("spores.txt", header=T,sep="\t", col.names=c("cat","rep","spores","total","ratio"))
spores <- transform(spores, cumul = ave(ratio, rep, FUN = cumsum))
spores$log.ratio <- log(spores$ratio)


### Analyse sur données log10(ratio)

require(fitdistrplus)
descdist(spores$log.ratio, boot=1001)
mle.fit <- fitdist(spores$log.ratio,"norm",method="mle")
summary(mle.fit)
plot(mle.fit)


### Analyse sur donnée ratio brutes ###

## Weibull
weib.pdf <- function(cat,a,b,e) {a*exp(-0.5*((log(cat)-log(abs(e)))/b)^2)}
weib <- nls(ratio~weib.pdf(cat,a,b,e), data=spores, start=c(a=0.35, e=15, b=-0.52))
summary(weib)

# Fitted vs Observed
weib.fit.obs <- ggplot(augment(weib), aes(x=.fitted, y=ratio)) +
  geom_point() +
  geom_line(aes(y=.fitted)) +
  xlab("Fitted") + ylab("Observed") +
  my_ggplot_theme()

# QQPlot
weib.qqplot <- my_qqplot(weib)

plot_grid(weib.fit.obs, weib.qqplot, ncol=1, labels="AUTO")


## Fréchet (wikipedia param)

frech.pdf <- function(cat,A,alpha,beta){ A*(alpha/beta)*(((cat-4)/beta)^(-1-alpha))*exp(-((cat-4)/beta)^-alpha) }
frech <- nls(ratio~frech.pdf(cat,A,alpha,beta),data=spores, start=c(A=9,alpha=1,beta=8.8))
summary(frech)


# Fitted vs Observed PDF
frech.fit.obs <- ggplot(augment(frech), aes(x=.fitted, y=ratio)) +
  geom_point() +
  geom_line(aes(y=.fitted)) +
  xlab("Fitted") + ylab("Observed") +
  my_ggplot_theme()

# QQPlot PDF
frech.qqplot <- my_qqplot(frech)

plot_grid(frech.fit.obs, frech.qqplot, ncol=1, labels="AUTO")


## Data + curve plot

weibfun <- function(x) {coef(weib)[1]*exp(-0.5*((log(x)-log(coef(weib)[2]))/(coef(weib)[3]))^2)}
frechfun <- function(x) {coef(frech)[1]*(coef(frech)[2]/coef(frech)[3])*(((x-4)/coef(frech)[3])^(-1-coef(frech)[2]))*exp(-((x-4)/coef(frech)[3])^-coef(frech)[2])}

# Summary table
moy.ratio <- spores %>% group_by(cat) %>% na.omit() %>% summarise(
  N=n(),
  mean=mean(ratio),
  sd=sd(ratio),
  se=sd/sqrt(N),
  ci.student=qt(0.975,df=N-1)*se
)

# Individual data + Weibull curve
ggplot(data=spores, aes(x=cat, y=ratio)) + 
  geom_point() +
  stat_function(fun=weibfun, xlim=c(1,100)) +
  stat_function(fun=frechfun, linetype="dashed", xlim=c(1,100)) +
  xlab("Number of impacting drops") + ylab("Ratio of released spores") +
  my_ggplot_theme()



BIC(weib,frech.pdf.nls)


#### Regression tree

library(partykit)
party.fit <- ctree(log(ratio)~cat, data=spores)
plot(party.fit, main="Conditional Inference Tree for log(ratio)")
print(party.fit)

spores$log.ratio.party.predict <- predict(party.fit)
spores$ratio.party.predict <- exp(spores$log.ratio.party.predict)
xyplot(ratio~cat, data=spores, xlim=c(0,110), xlab=list("Number of impacting drops"), ylab=list("Ratio of spores number)"), panel=function(x,y){
  panel.points(x,y, col="black")
  panel.lines(spores$ratio.party.predict~spores$cat, type="l", col="black")
})


### Mean data +/- SE  + curves

ggplot(data=moy.ratio, aes(x=cat, y=mean)) + 
  geom_point() +
  stat_function(fun=weibfun, xlim=c(1,100), linetype="dashed") +
  stat_function(fun=frechfun, xlim=c(1,100)) +
  geom_line(data=spores, aes(x=cat, y=ratio.party.predict, alpha=0.5)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  xlab("Number of impacting drops") + ylab("Ratio of released spores") +
  guides(alpha=FALSE) +
  my_ggplot_theme()