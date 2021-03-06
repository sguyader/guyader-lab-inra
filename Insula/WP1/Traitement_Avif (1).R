###Calculs Diversity AVIFAUNE###

##Etape##
#E1 :  Observer les donn�es en fonction des variables
  ## cad. Retirer les donn�es d'int�ret pour les comparer par etat_biotope / 
  ## par biotope / par sp / par abondance (type boxplot)
# E2 : Mesurer la biodiversit� ALPHA par site (/ ou par point d'�coute?) : 
  ## Mesurer l'Indice de Shannon
  ## Mesurer l'�quitabilit� : indice de Pielou 
  ###Stat R : trouver un algorithme pour tous les r�aliser en m�me temps 
  ### Repr�sentation des r�sultats
# E3 : Mesurer la diversit� par biotope / ou site d�grad� / nn deg
  ## Mesurer l'indice de Jacquard entre biotope_etat ou encore Shannon ? 
  ###repr�sentation des r�sultats


#Faire les test pour pouvoir pooler les donn�es :
  ## cad par etat_biotope / par biotope / par sp / par abondance
  ## Etudier les variances � la moyenne 
  ## H0 nos donn�es ont la m�me variance, les m�me sp �galement ? on peut les pooler






## Chargement des packages
install.packages("vegan")
install.packages("entropart")
install.packages("tidyverse")
library(tidyverse)
library(entropart)
library(vegan)
library(magrittr)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

##Chargement du jeu de donn�es :
setwd("D:/03_stat/R/DATA")
av <- read_delim("BD_AV.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>%
        rename_all(tolower) %>%
          unclass() %>%
        as.data.frame() %>% # changer le type de facteur
        select(etat, biotope, id_ecoute, nom_sc, count) # s�l�ction des variables d'int�ret 

str_to_lower(av$nom_sc)
View (av)

##E1 cr�ation de la matrice : 
matrice_av <- av %>%
            select(id_ecoute,count,nom_sc) %>%
            pivot_wider(names_from = nom_sc, values_from = count) %>%
            as.data.frame() 

matrice_av[is.na(matrice_av)] <- 0   #remplacer na par 0
rownames(matrice_av)<-matrice_av[,1]
matrice_av<-matrice_av[,-1]         # colonne 1 des sites en t�te et supprimer colonne 1
View(matrice_av)

## E2 richesse et autre 
### calculer la richesse spécifique par quadrat / la richesse locale par site / la richesse globale par site et par biotope / la richesse originale par site et par biotope 
###  calculer la richesse spécifique avec les données de la modalite total de la variable inventaire 



specnumber(matrice_av) # Richesse sp�cifique


# E3 :indice de biodiversité : 
## Shannon, simpson, insimpson 
H <- diversity(matrice_av) # Indices de biodiv / si rien = shannon / insimpson 
S <- diversity(matrice_av, index = "simpson", base =2)
I <- diversity(matrice_av, index = "inv", base =2)
unbias.simp <- rarefy(matrice_av, 2) - 1 ## Unbiased Simpson (Hurlbert 1971, eq. 5) 
## repr�sentation des indices en fonction des sites
pairs(cbind(H, S, I, unbias.simp), pch="+", col="blue")

## Equitabilit�
Pi<- H/ log(specnumber(matrice_av)) # Indice de Pielou avec H shannon

# E4  : rarefaction 
rs <- rowSums(matrice_av)#Rarefaction (nombre d'sp total en fonction de l'effort ) observer l'effort d'échantillonage
quantile(rs)
Srar <-rarefy(matrice_av, min(rs))
rarecurve(matrice_av, sample= min(rs))

# E5 : dissimilarité : 
## déterminer l'indice le plus pertinent 
euc_dis <-vegdist(matrice_av, method = "euclidean") # avec Qmoy
bc_dis <-vegdist(matrice_av)
hell_dis <- vegdist(decostand(matrice_av, method="hellinger"),
                    method="euclidean")# avec Qmoy

jac_dis<- vegdist(matrice_av, method = "jaccard")#presence absence data (0;1:...dissimilarité comme jaccard vérifier si besoin matrice_pa nécessaire) 
## Ward mesure de distance � chercher 
###repr�sentation euclideau
tree_euc <- (spantree(euc_dis))
cle <- as.hclust(tree_euc)
plot(cle)

plot(tree_euc, col = cutree(cle, 3), pch=16)
###repr�sentation hellinger
tree_bc <- (spantree(bc_dis))
clb <- as.hclust(tree_bc)
plot(clb)

plot(tree_bcl, col = cutree(clb, 3), pch=16)
###repr�sentation hellinger
tree_hell <- (spantree(hell_dis))
clh <- as.hclust(tree_hell)
plot(clh)

plot(tree_hell, col = cutree(clh, 3), pch=16)
###representation Jaccard
tree_jac <- (spantree(jac_dis))
clj <- as.hclust(tree_jac)
plot(clj)

plot(tree_jac, col = cutree(clj, 3), pch=16)

#E6 : Ordination  ## ADE4 : écology ; regarder le package  voir exemple / interface graphique ADE4 TKGUI
##  PCA : methode linéaire petit gradient / 
pca <- rda(decostand(matrice_av, method ="hellinger"), scale =TRUE) ##centrer et scaler
biplot(pca,scaling ="symmetric")
screeplot(pca,bstick = TRUE, type = "l",main = NULL)

## HE = CA correspondence analysis : unimodal (quand data pas linéaire) / marche avec présence absence
ca <- cca(matrice_av)
plot(ca, scaling = "sites") # ou "sp" ou "symmetric")

disp <-"species"
scl <- "symmetric"
plot(pca,dispay = disp, 
     scaling= scl, type ="n")
points(pca, display= disp,
       scaling=scl, pch= 19)
set.seed(10)
ordipointlabel(pca, displa=disp, scaling =scl,add=TRUE)








