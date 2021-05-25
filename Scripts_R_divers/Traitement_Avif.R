###Calculs Diversity AVIFAUNE###

##Etape##
#E1 :  Observer les donn?es en fonction des variables
  ## cad. Retirer les donn?es d'int?ret pour les comparer par etat_biotope / 
  ## par biotope / par sp / par abondance (type boxplot)
# E2 : Mesurer la biodiversit? ALPHA par site (/ ou par point d'?coute?) : 
  ## Mesurer l'Indice de Shannon
  ## Mesurer l'?quitabilit? : indice de Pielou 
  ###Stat R : trouver un algorithme pour tous les r?aliser en m?me temps 
  ### Repr?sentation des r?sultats
# E3 : Mesurer la diversit? par biotope / ou site d?grad? / nn deg
  ## Mesurer l'indice de Jacquard entre biotope_etat ou encore Shannon ? 
  ###repr?sentation des r?sultats


#Faire les test pour pouvoir pooler les donn?es :
  ## cad par etat_biotope / par biotope / par sp / par abondance
  ## Etudier les variances ? la moyenne 
  ## H0 nos donn?es ont la m?me variance, les m?me sp ?galement ? on peut les pooler





# E1 Observation du jeux de donn?es :
##Chargement du jeux de donn?es :
install.packages("vegan")
install.packages("entropart")
install.packages("tidyverse")
library(readr)
setwd("~/Downloads/Insula_data")
library(readr)
AVIF <- read_delim("AVIF.csv", ";", escape_double = FALSE, trim_ws = TRUE)
View(AVIF)
D <-AVIF %>%
  unclass()%>%
  as.data.frame()    # changer le type de facteur
##S?lection des colonnes d'int?ret : 
D<-D[c(4,6,7,11,19,22)]
View(D)
str(D)
##Observation des data (abandonce)
summary(D)

eff <- D %>%
    group_by(Id_ecoute) %>%
     count(Nom_sc)

> boxplot(eff$n, eff$Id_ecoute)###Observation par pt d'ecoute : # On a un probl?me : comme les inventaires ont ?t? s?par? par phase de 5 min et qu'on les regroupe, on a la colonne Count qui comptabilise le nombre d'individu r?pertorier par observation. On souhaite supprimer la colonne count et que ces donn?es apparaissent en lignes (ex pour Orthorhyncus cristatus on veut que cette ligne apparaisse une fois sans la colonne count)
# Utilisiation de separat_rows ?? Je n'arrive pas ? avoir le r?sultat


 
table(D$Nom_sc)
table(D$Count ~D$Id_ecoute)
boxplot(D$Count,D$Id_ecoute)


# E2 : Mesurer la biodiversit? ALPHA par site (/ ou par point d'?coute?) : A r?p?ter par site ??? et voir les erreurs (types pool site avec pt-ecoute)
head(D)
unique(D$Id_site)
as.character(D$Id_site)
E1
E1<-D[D$Id_site=="B0VB",c(5,6)]
table(E1)
sp_liste<-unique(E1$Nom_sc);sp_liste
S=length(sp_liste);S
sp=matrix(ncol=1,nrow=S,NA)
ni=matrix(ncol=1,nrow=S,0)
for(i in 1:S){
  E1b<-E1[E1$Nom_sc==sp_liste[i],]
  sp[i] <-as.character(sp_liste[i])
  ni[i] <-as.numeric(as.character(sum(E1b$Count)))
}

## Mesurer l'Indice de Shannon

FIN<-data.frame(cbind(sp,ni))
colnames(FIN)=c("sp","ni");FIN
N=sum(ni);N
pi=ni/N;pi
sum(pi)

S = length(pi)
SH=-sum(pi*log(pi)) ; SH

SH2=-sum(pi*log(pi,2)) ; SH2

## Mesurer l'?quitabilit? : indice de Pielou 
PIELOU=SH2/log(S,2)
PIELOU

SH/log(S) # Indice de Pielou en base e :

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



# E3. Mesurer la diversit? par biotope / ou site d?grad? / nn deg

head(D)
unique(D$Biotope, D$Etat)
as.character(D$Biotope)
as.character(D$Etat)
B0
B0<-D %>%

  
  jaccard <- function(a, b) {
    intersection = length(intersect(a, b))
    union = length(a) + length(b) - intersection
    return (intersection/union)
  }

vegdist(test, method="jaccard", binary=TRUE)
jac
  
jaccard(a, b)


