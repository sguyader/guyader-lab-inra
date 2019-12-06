library(DiceKriging)
library(DiceView)
library(DiceOptim)
library(DiceDesign)
source("../R_files/utilities_volcan_simple.R")

#### Question 1 ####
library(lhs)
X <- lhsDesign(n=100,dimension=5)$design
Y <- compute_wls(X)

#### Question 2 ####
m <- km(y~., design=data.frame(x=X), response=data.frame(y=Y), 
        covtype="gauss",
        coef.trend = NULL, coef.cov = NULL, coef.var = NULL,
        nugget=1e-8)

m <- km(y~1, design=data.frame(x=X), response=data.frame(y=Y), 
        covtype="matern5_2", # powexp semble être le mieux adapté
        coef.trend=NULL,
        nugget=1e-8)

logLik(m)

#### Question 3 ####
print(m)

m <- km(y~., design=data.frame(x=X), response=data.frame(y=Y), 
        covtype="matern5_2", # powexp semble être mieux adapté
        coef.trend=NULL,
        #lower=rep(0.05, 5),
        nugget=1e-8)
logLik(m)
print(m)
plot(m)

sectionview(m, type="UK")

#### Question 4 ####
m.loo <- leaveOneOut.km(m, type="UK")
q2 <- 1-(sum((Y - m.loo$mean)^2)/sum(m.loo$sd))
q2

#### Question 5 ####
err_std <- (Y-m.loo$mean)/m.loo$sd # calcul des résidus standardisés

# test des intervalles de confiance
x <- seq(-3, 3, 0.03)
hist(err_std, freq=FALSE, xlim=c(min(err_std, -3), max(err_std, 3)))
lines(x, dnorm(x))


sectionview(m, type="UK", center=rep(0.5, 5))

m1 <- km(y~1, design=data.frame(x=X), response=data.frame(y=Y),
         covtype="gauss", nugget=1e-8)
m2 <- km(y ~ .^2 + xs^2 + ys^2 + zs^2 + a^2 + p^2, design=data.frame(X),
         response=data.frame(y=Y),
         covtype="gauss", nugget=1e-8, lower=rep(0.05, 5))
m3 <- km(y~., design=data.frame(x=X), response=data.frame(y=Y),
         covtype="matern5_2", nugget=1e-8)
c(logLik(m1), logLik(m2), logLik(m3))

c(Q2m1, Q2m2, Q2m3)

#####################
## optimisation globale
#####################

#### Question 6 ####
res <- EGO.nsteps(model=m, fun=compute_wls, nsteps=50, lower=rep(0.01,5), upper=rep(1.5,5))

#### Question 7 ####
n <- length(Y)
p <- length(res$lastmodel@y)
best <- cummin(res$lastmodel@y)

plot(1:p, res$lastmodel@y)
lines(1:p, best)
abline(v=n+.5, lty=2)

#### Question 8 ####
cols=c(rep("black", n), rep("red", p))
cols[which(res$lastmodel@y < -1.6)] <- "blue"
X2 <- res$lastmodel@X
pairs(X2, col=cols)

#### Question 9 ####

#### Question 10 ####


#### Question 11 ####
library(lhs)
getmean <- function(newdata, m) {
  pred <- predict(object=m, newdata=newdata, type="UK")
  return(pred$mean)
}
X1 <- data.frame(randomLHS(10000,5))
X2 <- data.frame(randomLHS(10000,5))
colnames(X1) <- colnames(X2) <- colnames(m@X)
res2 <- soboljansen(model = getmean, X1=X1, X2=X2, nboot = 50, conf = 0.95, m=m)
plot(res2)

X1 <- data.frame(randomLHS(1000,5))
X2 <- data.frame(randomLHS(1000,5))
candidate <- data.frame(randomLHS(100,5))
colnames(X1) <- colnames(X2) <- colnames(candidate) <- colnames(m@X)
res <- sobolGP(model = m, type="UK", MCmethod="soboljansen",
               X1=X1, X2=X2, nsim = 20, nboot=50, sequential = TRUE, candidate=candidate)

plot(res)
