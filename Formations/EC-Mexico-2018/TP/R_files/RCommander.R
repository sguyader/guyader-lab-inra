source("mainScript_DatascienceClass.R")
#install.packages("R.matlab")
xmin2 = xmin
xmax2 = xmax
xmin2[1] = 365000 ; xmax2[1] = 367000
xmin2[2] = 7649500 ; xmax2[2] = 7650500
xmin2[3] =  -2000 ; xmax2[3] = 0

valfac = lapply(n2i,function(ll) seq(xmin[ll],xmax[ll],length.out=5))
for (i in 1:3) valfac[[i]] = seq(xmin2[[i]],xmax2[[i]],length.out=5)

planX = expand.grid(valfac)
summary(planX)

Y = apply(planX,1,compute_wls)
XY = cbind(planX,Y)

plot(as.data.frame(XY))

#install.packages("scatterplot3d")

library(scatterplot3d)

planT = expand.grid(valfac[c("xs","a","p")])

pdf("FiguresCours/planfactoriel.pdf")
toto= scatterplot3d(planX[1:125,], type="p")
toto$points3d

for(i in 2:4) toto$points3d (rep(valfac$xs[1],5), valfac$ys, rep(valfac$zs[i],5),type="l", lty=2, col="grey")
for(i in 2:4) toto$points3d (rep(valfac$xs[5],5), valfac$ys, rep(valfac$zs[i],5),type="l", lty=2, col="grey")
for(j in 2:4) { for(i in 2:5) toto$points3d (rep(valfac$xs[j],5), valfac$ys, rep(valfac$zs[i],5),type="l", lty=2, col="grey")}

for(i in 2:4) toto$points3d (valfac$xs, rep(valfac$ys[1],5), rep(valfac$zs[i],5),type="l", lty=2, col="grey")
for(i in 2:4) toto$points3d (valfac$xs, rep(valfac$ys[5],5), rep(valfac$zs[i],5),type="l", lty=2, col="grey")
for(j in 2:4) { for(i in 2:5) toto$points3d (valfac$xs, rep(valfac$ys[j],5),rep(valfac$zs[i],5),type="l", lty=2, col="grey")}

for(i in 2:4) toto$points3d (rep(valfac$xs[i],5), rep(valfac$ys[1],5), valfac$zs,type="l", lty=2, col="grey")
for(i in 2:4) toto$points3d (rep(valfac$xs[i],5), rep(valfac$ys[5],5), valfac$zs,type="l", lty=2, col="grey")
for(j in 2:4) { for(i in 1:5) toto$points3d (rep(valfac$xs[i],5), rep(valfac$ys[j],5),valfac$zs,type="l", lty=2, col="grey")}
dev.off()

planZ = as.data.frame(planX)

planZ = apply(planZ, 2, as.factor)
ZY = cbind.data.frame(planZ,Y)
summary(ZY)
summary(aov(Y~xs*a*p,data = ZY))
res.aov = aov(Y~xs*a*p, data =ZY)
summary(res.aov)
uu = summary(res.aov)
uu[[1]][,2]/ sum(uu[[1]][,2]) * 100

resComplet.aov = aov(Y~xs*ys*zs*p*a, data=ZY)

uuC = summary(resComplet.aov)
uuC[[1]][,2]/ sum(uuC[[1]][,2]) * 100

install.packages("lhs")
library(lhs)
set.seed(2018)
x.lhs = improvedLHS(1000,5,3)
z.lhs = x.lhs
for(i in 1:5) z.lhs[,i] = xmin[i] + x.lhs[,i]*(xmax[i]-xmin[i])

apply(z.lhs,2,summary)
Y.lhs = apply(z.lhs,1,compute_wls)
plot(as.data.frame(cbind(z.lhs,Y.lhs)))
plot(z.lhs[,5],Y.lhs)

