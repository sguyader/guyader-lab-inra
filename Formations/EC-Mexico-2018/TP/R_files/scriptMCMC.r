setwd("~/Documents/Documents/rapport_presentation/presentations/EC_Mexico/td/codeglobal/")

##-----##
##  0  ##
##-----##

library(R2OpenBUGS)
library(coda)
library(HDInterval)

don <- read.table("donnees.txt",header=T)

##-----##
##  1  ##
##-----##

##--------------##
##  1.1 et 1.2  ##
##--------------##

## cf fonction dans  funMCMC.r
source("funMCMC.r")

##-------##
##  1.3  ##
##-------##
alpha <- 0
beta <- 1

dataTime <- runif(100,0,5)
dataSim <- rpois(100,exp(alpha+beta*dataTime))
plot(dataTime,dataSim)

mcmc.MH(n=10^5, Y=dataSim, parinit=c(-0.1,2),
	dynamics=function(par) exp(par[1]+par[2]*dataTime),
        obsprocess=list(ddistrib=dpois,parfunction=function(u){ u }),
        prior=list(ddistrib=dnorm, parfunction=c(function(){c(0,0)}, function(){c(10,10)})),
        proposal=list(ddistrib=dnorm, rdistrib=rnorm,parfunction=c(function(u){u},function(u){c(0.1,0.1)})),
        subsample=100)

temp2 <- read.table("save.txt")

par(mfrow=c(3,2))
plot(temp2[,1],ylim=range(temp2[,1]),type="l",main="alpha")
hist(temp2[-(1:100),1])
abline(v=alpha,col=2)
plot(temp2[,2],ylim=range(temp2[,2]),type="l",main="beta")
hist(temp2[-(1:100),2])
abline(v=beta,col=2)
plot(temp2[,5],ylim=range(temp2[,5]),type="l",main="vraisemblance")
plot(temp2[-(1:100),5],ylim=range(temp2[-(1:100),5]),type="l")

##-----##
##  2  ##
##-----##

##-------##
##  2.1  ##
##-------##

plot(x=don$Year,don$Lynx,type="b",col=1,ylim=range(don[,2:3]),xlab="Year",ylab="Pelts (thousands)")
lines(x=don$Year,don$Hare,type="b",col=4)
legend(x=1918,y=70,col=c(1,4),legend=c("Lynx","Hare"),lty=1)

##------------------------##
##  2.2, 2.3, 2.4 et 2.5  ##
##------------------------##

dir.create("lognormal")

sink(paste0(getwd(),"/lognormal/test_ode.txt"))
cat("
    model{
        ## system of ODEs resolution
        solution[1:ntimes,1:Nesp] <- ode(VALinits[1:Nesp], 
                                   tgrid[1:ntimes], 
                                   D(C[1:Nesp], s1), 
                                   origin, 
                                   tol) 
        ## Lokta-voltera model
        D(C[1], s1) <- alpha*C[1] - beta*C[1]*C[2]
        D(C[2], s1) <- -gamma*C[2] + delta*C[1]*C[2]

        ## observation
        ### initial conditions
        Lynxinit ~ dlnorm(VALinits[2],tauLynx)
        Lievreinit ~ dlnorm(VALinits[1],tauLievre)
        ### loop over time
        for (t in 1:Nyear){
            Lynx[t] ~ dlnorm(solution[t,2],tauLynx)
            Lievre[t] ~ dlnorm(solution[t,1],tauLievre)
            #repeted data
            Lynxrep[t] ~ dlnorm(solution[t,2],tauLynx) 
            Lievrerep[t] ~ dlnorm(solution[t,1],tauLievre)
            #chi2 distance
            evalLynx[t] <- solution[t,2]
            ELynx[t] <- pow(Lynx[t]-evalLynx[t],2)/(evalLynx[t]+0.5)
            ELynxrep[t] <- pow(Lynxrep[t]-evalLynx[t],2)/(evalLynx[t]+0.5)
            evalLievre[t] <- solution[t,2]
            ELievre[t] <- pow(Lievre[t]-evalLievre[t],2)/(evalLievre[t]+0.5)
            ELievrerep[t] <- pow(Lievrerep[t]-evalLievre[t],2)/(evalLievre[t]+0.5)
        }
        #fit variables
        fitLynx <- sum(ELynx[])
        fitLynxrep <- sum(ELynxrep[])
        fitLievre <- sum(ELievre[])
        fitLievrerep <- sum(ELievrerep[])

        ## a priori
        tauLynx <- 1/(sigLynx*sigLynx)
        sigLynx ~ dunif(0,4)
        tauLievre <- 1/(sigLievre*sigLievre)
        sigLievre ~ dunif(0,4)
        VALinits[1] ~ dunif(0,100)
        VALinits[2] ~ dunif(0,100)
        alpha ~ dunif(0,1)
        beta ~ dunif(0,1)
        gamma ~ dunif(0,1)
        delta ~ dunif(0,1)
    }
")
sink()

##-------##
##  2.6  ##
##-------##

data <- list(
Nyear=nrow(don)-1,
origin = 0,
tgrid = seq(1,nrow(don)-1,by=1),
ntimes=length(seq(1,nrow(don)-1,by=1)),
tol = 1.0E-3, 
Lynxinit = don[1,2],
Lievreinit = don[1,3],
Lynx=don[2:nrow(don),2],
Lievre=don[2:nrow(don),3],
Nesp=2)

deb <- Sys.time()
RES <- bugs(data=data,inits=NULL,
            parameters.to.save=c("sigLynx","sigLievre","VALinits","alpha","beta","gamma","delta","solution","fitLynx","fitLynxrep","fitLievre","fitLievrerep","Lievrerep","Lynxrep"),
            n.iter=5000, n.chains=2, n.burnin=3000, n.thin=1,
            model.file="test_ode.txt", debug=FALSE, codaPkg=TRUE,
            working.directory=paste0(getwd(),"/lognormal"))
fin <- Sys.time()
fin-deb

##-------##
##  2.7  ##
##-------##

chain1 <- RES[1]
chain2 <- RES[2]
res <- read.bugs(c(chain1, chain2))

paramNOM <- "alpha"
param <- cbind(res[[1]][,paramNOM],res[[2]][,paramNOM])
par(mfrow=c(1,2))
plot(param[,1],type="l",ylim=range(param),ylab=paramNOM)
lines(param[,2],lty=2)
plot(density(as.vector(param)),main="")

paramNOM <- "alpha"
param1 <- cbind(res[[1]][,paramNOM],res[[2]][,paramNOM])
paramNOM <- "beta"
param2 <- cbind(res[[1]][,paramNOM],res[[2]][,paramNOM])
plot(param1,param2,pch=16)

Rhat <- gelman.diag(res)
par(mar=c(10,4,1,1))
plot(Rhat$psrf[,1],axes=F,xlab="",ylab="Rhat",pch=16)
axis(1,at=1:length(Rhat$psrf[,1]),labels=row.names(Rhat$psrf),las=2)
axis(2)

par(mfrow=c(1,2))
paramNOM <- "fitLynxrep"
param1 <- cbind(res[[1]][,paramNOM],res[[2]][,paramNOM])
paramNOM <- "fitLynx"
param2 <- cbind(res[[1]][,paramNOM],res[[2]][,paramNOM])
plot(param1,param2,pch=16,main=paste0("Ajustement Lynx : ",round(sum(param1>param2)/length(param1),2)))
abline(a=0,b=1,col=4,lwd=2,lty=2)
paramNOM <- "fitLievrerep"
param1 <- cbind(res[[1]][,paramNOM],res[[2]][,paramNOM])
paramNOM <- "fitLievre"
param2 <- cbind(res[[1]][,paramNOM],res[[2]][,paramNOM])
plot(param1,param2,pch=16,main=paste0("Ajustement Lièvre : ",round(sum(param1>param2)/length(param1),2)))
abline(a=0,b=1,col=4,lwd=2,lty=2)

##-------##
##  2.8  ##
##-------##

resstat <- NULL
for (paramNOM in c("alpha","beta","delta","gamma")){
    paramChaine <- c(res[[1]][,paramNOM],res[[2]][,paramNOM])
    resstat <- cbind(resstat,quantile(paramChaine,probs=c(0.025,0.5,0.975)))
}
par(mar=c(10,4,1,1))
plot(1:4,resstat[2,],ylim=range(resstat),pch=16,axes=F,xlab="",ylab="")
segments(x0=1:4,x1=1:4,y0=resstat[1,],y1=resstat[3,])
axis(1,at=1:4,labels=c("alpha","beta","delta","gamma"),las=2)
axis(2)

paramNOM <- "alpha"
paramChaine <- c(res[[1]][,paramNOM],res[[2]][,paramNOM])
hdiD <- hdi(density(paramChaine),allowSplit=T,credMass=0.95)
ht <- attr(hdiD, "height")
plot(density(paramChaine),main=paste(paramNOM, " - HPD95 =",round(hdiD[, 1],2),",",round(hdiD[, 2],2), sep=""))
segments(hdiD[, 1], ht, hdiD[, 2], ht, lwd=3, col='blue')

paramNOM1 <- "sigLynx"
paramNOM2 <- "sigLievre"
param1 <- cbind(res[[1]][,paramNOM1],res[[2]][,paramNOM1])
param2 <- cbind(res[[1]][,paramNOM2],res[[2]][,paramNOM2])
proba <- sum(param1>param2)/length(param1)
densparam1 <- density(param1)
densparam2 <- density(param2)
plot(densparam1,xlim=range(c(densparam1$x,densparam2$x)),ylim=range(c(densparam1$y,densparam2$y)),main=paste0("pr(",paramNOM1,">",paramNOM2,")=",round(proba,2)))
lines(densparam2,lty=2)
legend(x=0.5,y=8,legend=c(paramNOM1,paramNOM2),lty=c(1,2))



paramNOM <- "VALinits[1]"
Lievre <- c(res[[1]][,paramNOM],res[[2]][,paramNOM])
for (i in 1:20){
    paramNOM <- paste0("solution[",i,",1]")
    Lievre <- cbind(Lievre,c(res[[1]][,paramNOM],res[[2]][,paramNOM]))
}
Lievre.stat <- apply(Lievre,2,quantile,probs=c(0.025,0.5,0.975))
    
plot(NULL,NULL,xlim=c(0,21),ylim=range(c(range(Lievre.stat),range(log(don[,2])))),xlab="Years",ylab="Pelts (thousands)",cex.lab=1.2)
polygon(x=c(1:21,21:1),y=c(Lievre.stat[1,],rev(Lievre.stat[3,])),col="grey",border=NA)
lines(1:21,Lievre.stat[2,],lwd=1,col="blue")
points(x=1:21,y=log(don[,3]),col=2,pch=16)

paramNOM <- "VALinits[2]"
Lynx <- c(res[[1]][,paramNOM],res[[2]][,paramNOM])
for (i in 1:20){
    paramNOM <- paste0("solution[",i,",2]")
    Lynx <- cbind(Lynx,c(res[[1]][,paramNOM],res[[2]][,paramNOM]))
}
Lynx.stat <- apply(Lynx,2,quantile,probs=c(0.025,0.5,0.975))
    
plot(NULL,NULL,xlim=c(0,21),ylim=range(c(range(Lynx.stat),range(log(don[,2])))),xlab="Years",ylab="Pelts (thousands)",cex.lab=1.2)
polygon(x=c(1:21,21:1),y=c(Lynx.stat[1,],rev(Lynx.stat[3,])),col="grey",border=NA)
lines(1:21,Lynx.stat[2,],lwd=1,col="blue")
points(x=1:21,y=log(don[,2]),col=2,pch=16)



Lievre <- NULL
for (i in 1:20){
    paramNOM <- paste0("Lievrerep[",i,"]")
    Lievre <- cbind(Lievre,c(res[[1]][,paramNOM],res[[2]][,paramNOM]))
}
Lievre.stat <- apply(Lievre,2,quantile,probs=c(0.025,0.5,0.975))
    
plot(NULL,NULL,xlim=c(0,20),ylim=range(c(range(Lievre.stat),range(don[,2]))),xlab="Years",ylab="Pelts (thousands)",cex.lab=1.2)
polygon(x=c(1:20,20:1),y=c(Lievre.stat[1,],rev(Lievre.stat[3,])),col="grey",border=NA)
lines(1:20,Lievre.stat[2,],lwd=1,col="blue")
points(x=1:20,y=don[-1,3],col=2,pch=16)


##-------##
##  2.9  ##
##-------##

dir.create("poisson")

sink(paste0(getwd(),"/poisson/test_ode.txt"))
cat("
    model{
        ## system of ODEs resolution
        solution[1:ntimes,1:Nesp] <- ode(VALinits[1:Nesp], 
                                   tgrid[1:ntimes], 
                                   D(C[1:Nesp], s1), 
                                   origin, 
                                   tol) 
        ## abundance model

        D(C[1], s1) <- alpha*C[1] - beta*C[1]*C[2]
        D(C[2], s1) <- -gamma*C[2] + delta*C[1]*C[2]

        ## observation
        Lynxinit ~ dpois(VALinits[2])
        Lievreinit ~ dpois(VALinits[1])
        for (t in 1:Nyear){
             Lynx[t] ~ dpois(solution[t,2])
             Lynxrep[t] ~ dpois(solution[t,2])
             Lievre[t] ~ dpois(solution[t,1])
             Lievrerep[t] ~ dpois(solution[t,1])
             evalLynx[t] <- solution[t,2]
             ELynx[t] <- pow(Lynx[t]-evalLynx[t],2)/(evalLynx[t]+0.5)
             ELynxrep[t] <- pow(Lynxrep[t]-evalLynx[t],2)/(evalLynx[t]+0.5)
             evalLievre[t] <- solution[t,1]
             ELievre[t] <- pow(Lievre[t]-evalLievre[t],2)/(evalLievre[t]+0.5)
             ELievrerep[t] <- pow(Lievrerep[t]-evalLievre[t],2)/(evalLievre[t]+0.5)
        }

        fitLynx <- sum(ELynx[])
        fitLynxrep <- sum(ELynxrep[])
        fitLievre <- sum(ELievre[])
        fitLievrerep <- sum(ELievrerep[])
        
        ## a priori
        VALinits[1] ~ dunif(0,100)
        VALinits[2] ~ dunif(0,100)
        alpha ~ dunif(0,1)
        beta ~ dunif(0,1)
        gamma ~ dunif(0,1)
        delta ~ dunif(0,1)
    }
")
sink()

##--------##
##  2.10  ##
##--------##


dir.create("bloc")

sink(paste0(getwd(),"/bloc/test_ode.txt"))
cat("
    model{
        ## system of ODEs resolution
        solution[1:ntimes,1:Nesp] <- ode(VALinits[1:Nesp], 
                                   tgrid[1:ntimes], 
                                   D(C[1:Nesp], s1), 
                                   origin, 
                                   tol) 
        ## Lokta-voltera model
        D(C[1], s1) <- alpha*C[1] - beta*C[1]*C[2]
        D(C[2], s1) <- -gamma*C[2] + delta*C[1]*C[2]

        ## observation
        ### initial conditions
        Lynxinit ~ dlnorm(VALinits[2],tauLynx)
        Lievreinit ~ dlnorm(VALinits[1],tauLievre)
        ### loop over time
        for (t in 1:Nyear){
            Lynx[t] ~ dlnorm(solution[t,2],tauLynx)
            Lievre[t] ~ dlnorm(solution[t,1],tauLievre)
            #repeted data
            Lynxrep[t] ~ dlnorm(solution[t,2],tauLynx) 
            Lievrerep[t] ~ dlnorm(solution[t,1],tauLievre)
            #chi2 distance
            evalLynx[t] <- solution[t,2]
            ELynx[t] <- pow(Lynx[t]-evalLynx[t],2)/(evalLynx[t]+0.5)
            ELynxrep[t] <- pow(Lynxrep[t]-evalLynx[t],2)/(evalLynx[t]+0.5)
            evalLievre[t] <- solution[t,2]
            ELievre[t] <- pow(Lievre[t]-evalLievre[t],2)/(evalLievre[t]+0.5)
            ELievrerep[t] <- pow(Lievrerep[t]-evalLievre[t],2)/(evalLievre[t]+0.5)
        }
        #fit variables
        fitLynx <- sum(ELynx[])
        fitLynxrep <- sum(ELynxrep[])
        fitLievre <- sum(ELievre[])
        fitLievrerep <- sum(ELievrerep[])

        ## a priori
        tauLynx <- 1/(sigLynx*sigLynx)
        sigLynx ~ dunif(0,4)
        tauLievre <- 1/(sigLievre*sigLievre)
        sigLievre ~ dunif(0,4)
        VALinits[1] ~ dunif(0,100)
        VALinits[2] ~ dunif(0,100)

        moy[1] <- 0
        moy[2] <- 0
        moy[3] <- 0
        moy[4] <- 0
        tau[1,1] <- 1
        tau[1,2] <- 0
        tau[1,3] <- 0
        tau[1,4] <- 0
        tau[2,1] <- 0
        tau[2,2] <- 1
        tau[2,3] <- 0
        tau[2,4] <- 0
        tau[3,1] <- 0
        tau[3,2] <- 0
        tau[3,3] <- 1
        tau[3,4] <- 0
        tau[4,1] <- 0
        tau[4,2] <- 0
        tau[4,3] <- 0
        tau[4,4] <- 1
        paramBLOC ~ dmnorm(moy[],tau[,])

        logit(alpha) <- paramBLOC[1]
        logit(beta) <- paramBLOC[2]
        logit(gamma) <- paramBLOC[3]
        logit(delta) <- paramBLOC[4]
    }
")
sink()


data <- list(
Nyear=nrow(don)-1,
origin = 0,
tgrid = seq(1,nrow(don)-1,by=1),
ntimes=length(seq(1,nrow(don)-1,by=1)),
tol = 1.0E-3, 
Lynxinit = don[1,2],
Lievreinit = don[1,3],
Lynx=don[2:nrow(don),2],
Lievre=don[2:nrow(don),3],
Nesp=2)

deb <- Sys.time()
RES <- bugs(data=data,inits=NULL,
            parameters.to.save=c("sigLynx","sigLievre","VALinits","paramBLOC","solution","fitLynx","fitLynxrep","fitLievre","fitLievrerep"),
            n.iter=5000, n.chains=2, n.burnin=3000, n.thin=1,
            model.file="test_ode.txt", debug=FALSE, codaPkg=TRUE,
            working.directory=paste0(getwd(),"/bloc"))
fin <- Sys.time()
fin-deb
