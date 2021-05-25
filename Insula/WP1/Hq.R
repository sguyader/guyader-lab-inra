####Traitement herbacee


##Etape##
#E1 :  Observer les données en fonction des variables
## cad. Retirer les données d'intéret pour les comparer par quadrat / par site / par biotope / par etat
##  par sp / par abondance (type boxplot)
#E2 : Regarder si l'effort d'échantillonage est suffisant : 
## SAC
##
# E3 : Faire les test pour pouvoir pooler les données :
## cad par site / par biotope /
## Etudier les variances à la moyenne 
## H0 nos données ont la même variance, les même sp également ? on peut les pooler
# E4 : Test d'equitabilité : indice de PIELOU
## Par site ? Par échantillon ? Par état_biotope ?
# E5 : Test Shannon / fisher et représentativité 
## Par site ? Par échantillon ? Par état_biotope ?


# E1 Observation du jeux de données :
##Chargement du jeux de données :
library(readr)
hq <- read_delim("DATA/herb_quadrats.csv", ";", escape_double = FALSE, trim_ws = TRUE)
View(hq)
str(hq)
hq2 <-hq
hq2<-as.data.frame(unclass(hq2))    # changer le type de facteur
str(hq2)
## Observation du jeux de données

summary(hq2)#Observation générales du jeu de données selon le biotope
ggplot(hq2, aes(x = "Id_quadrats")) +   #Observation général du jeu de données selon le biotope / a revoir/ faux...
  geom_bar() +
  facet_grid(~ Id_site2) +
  theme(axis.text.x = element_text(angle = 100, hjust = 1))

barplot(table(hq2$Biotope), #Observation du jeux de données selon le biotope
        horiz = TRUE,las=1, 
        col = "blue", 
        border = "white",
        xlab = "Effectifs")

barplot(table(hq2$Id_site2), #Observation du jeux de données selon les sites 
        horiz = TRUE,las=1, 
        col = "orange", 
        border = "white",
        xlab = "Effectifs")

E1
E1<-hq2[hq2$Id_site2=="A1TR",c(2,3,4,8)]
summary(E1)
str(E1)
barplot(table(E1$Id_quadrats), #Observation du jeux de données selon les quadrat pour le site 1 
        horiz = TRUE,las=1, 
        col = "green", 
        border = "white",
        xlab = "Effectifs")

sort(table(hq2$Nom_sc), decreasing = TRUE) #Observation du jeux de données selon le Biotope

###Observation par quadrat dans chaque site :
####Pour le site : 


###Observation des données par site :
eff<-
boxplot(hq2$Nom_sc,hq2$Biotope)
table(hq2$Nom_sc,hq2$Biotope)




#E2SAC
library("EntropyEstimation")
library(ggplot2)
###Par biotope
EXTA
EXTA<-hq2[hq2$Biotope=="A",c(2,3,4,8)]
NsA<-table(EXTA$Nom_sc)
gsValuesA <- sapply(1:(sum(NsA)-1), function(r) GenSimp.z(NsA, r))
SACA <- cumsum(gsValuesA)
ggplot(data.frame(x = 0:(sum(NsA)-1), 
                  y = c(0, SACA)), 
       aes(x = x, y = y)) +
  geom_line() +
  labs(x = "Nombre d'individus", y = "Nombre d'espèces")

EXTB
EXTB<-hq2[hq2$Biotope=="B",c(2,3,4,8)]
NsB<-table(EXTB$Nom_sc)
gsValuesB <- sapply(1:(sum(NsB)-1), function(r) GenSimp.z(NsB, r))
SACB <- cumsum(gsValuesB)
ggplot(data.frame(x = 0:(sum(NsB)-1), 
                  y = c(0, SACB)), 
       aes(x = x, y = y)) +
  geom_line() +
  labs(x = "Nombre d'individus", y = "Nombre d'espèces")

EXTC
EXTC<-hq2[hq2$Biotope=="C",c(2,3,4,8)]
NsC<-table(EXTC$Nom_sc)
gsValuesC <- sapply(1:(sum(NsC)-1), function(r) GenSimp.z(NsC, r))
SACC <- cumsum(gsValuesC)
ggplot(data.frame(x = 0:(sum(NsC)-1), 
                  y = c(0, SACC)), 
       aes(x = x, y = y)) +
  geom_line() +
  labs(x = "Nombre d'individus", y = "Nombre d'espèces")


#E4 : Test d'equitabilité : indice de PIELOU
E1
E1<-hq2[hq2$Id_site2=="A1TR",c(2,3,4,8)]
summary(E1)
str(E1)
table(E1)
sp_liste<-unique(E1$Nom_sc);sp_liste
S=length(sp_liste);S
sp=matrix(ncol=1,nrow=S,NA)
ni=matrix(ncol=1,nrow=S,0)
for(i in 1:S){
  E1a<-E1[E1$Nom_sc==sp_liste[i],]
  sp[i] <-as.factor(sp_liste[i])
  ni[i] <-as.numeric(E1a)
}
FIN<-data.frame(cbind(sp,ni))
colnames(FIN)=c("sp","ni");FIN
N=sum(ni);N
pi=ni/N;pi
sum(pi)

S = length(pi)
SH=-sum(pi*log(pi)) ; SH

SH2=-sum(pi*log(pi,2)) ; SH2

# Le calcul de l'?quitabilit?, parfois
# appel?e "indice de Pi?lou" :
PIELOU=SH2/log(S,2)
PIELOU
# Indice de Pielou en base e :
SH/log(S)

library(vegan)
diversity(ni,index="shannon")
diversity(ni,index="shannon",base=2) # ici on lui impose la base 2
fisher.alpha(ni)

library(entropart)
Tsallis(ni,q) # entropie HCDT
q.seq <-seq(0, 2, .1)
DIV<-CommunityProfile(Diversity, ni, q.seq)
DIV
plot(DIV$x,DIV$y,type='l',col='blue')
autoplot(DIV, xlab = "q", ylab = "Diversit?")