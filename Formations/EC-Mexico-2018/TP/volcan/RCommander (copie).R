source("mainScript_DatascienceClass.R")

valfac = lapply(n2i,function(ll) seq(xmin[ll],xmax[ll],length.out=5))

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

planX = apply(planZ, 2, as.factor)
ZY = cbind(planZ,Y)
summary(ZY)
summary(aov(Y~xs*p*a, data=ZY))
dim(ZY)

