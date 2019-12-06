library(DiceKriging)
library(DiceView)

#### Question 1 ####
load(toy_data.Rdata)
plot(Y~X) #fonction semble régulière, sigmoïde

#### Question 2 ####
m <- km(y~1, design=data.frame(x=X), response=data.frame(y=Y),
        coef.trend = 0, nugget=1e-8,
        covtype="gauss", coef.var=4, coef.cov=.1)

x <- matrix(seq(-0.2, 1.2, 0.01))
Z <- simulate(m, 10, newdata=data.frame(x=x), cond=FALSE)
Z <- t(Z)

matplot(x, Z, type='l', col=1)

#### Question 3 ####
Zc <- simulate(m, 10, newdata=data.frame(x=x), cond=TRUE)
Zc <- t(Zc)

matplot(x, Zc, type='l', col=1)
points(X, Y)
sectionview(m)

#### Question 4 ####
m <- km(y~1, design=data.frame(x=X), response=data.frame(y=Y),
        coef.trend = 0, nugget=1e-8,
        covtype="gauss", coef.var=NULL, coef.cov=NULL)
print(m)

Zc <- simulate(m, 10, newdata=data.frame(x=x), cond=TRUE)
Zc <- t(Zc)

matplot(x, Zc, type='l', col=1)
points(X, Y)
sectionview(m, xlim=c(-0.2,5), ylim=c(-5,6))

#### Question 5 ####
mloo <- leaveOneOut.km(m, type="SK")
res_std <- (Y-mloo$mean)/mloo$sd # à vous de jouer !

hist(res_std, freq=FALSE, xlim=c(min(X, -3), max(X, 3)))
x <- seq(-3, 3, 0.03)
lines(x, dnorm(x))

#### Question 6 ####
pred <- predict(m, newdata=data.frame(x=4), type="SK")
print(pred)
sectionview(m, xlim=c(-0.2,4), ylim=c(-18, 22))

# modèle de krigeage ordinaire (estimation tendance constante)
m <- km(y~1, design=data.frame(x=X), response=data.frame(y=Y), 
        covtype="gauss", nugget=1e-8)
sectionview(m, xlim=c(-0.2, 4), ylim=c(-18, 22))

m <- km(y~x, design=data.frame(x=X), response=data.frame(y=Y), 
        covtype="gauss", nugget=1e-8)
sectionview(m, xlim=c(-0.2, 4), ylim=c(-18, 22))

#### Question 7 ####

#### Question 8 ####

#### Question 9 ####

# test de la moyenne
res <-leaveOneOut.km(m, type='SK')
Q2 <- 1 - sum((Y - res$mean)^2) / sum((Y - mean(Y))^2)

#### Question 10 ####
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