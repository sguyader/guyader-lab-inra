####Traitement herbacee


##Etape##
library(tidyverse)
library(vegan)
library(magrittr)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)


he <- read_delim("data/BD_HE.csv", ";", escape_double = FALSE, trim_ws = TRUE)%>%
  rename_all(tolower) %>%
  unclass()%>%
  as.data.frame()%>%
  filter(inventaire  == "quadrat") %>% # selectionner uniquement les données ayant pour modalite "quadrat" de la variable inventaire 
  select(id_site, etat, biotope, id_quadrats, nom_sc, qmoy) 

str_to_lower(he$nom_sc)
 
View(he)
str(he)

  
#E1 : MATRICE HE quadrat   
    #E1' : matrice pr?sence absence 0/1

hefreq <- as.data.frame(table(he$id_quadrats,he$nom_sc))
colnames(hefreq) <- c("id_quadrats", "nom_sc", "freq")



matrice_pa<-   hefreq %>%
               pivot_wider(names_from = nom_sc, values_from = freq) ## erreur ? v?rifier, pourquoi 117 variables

  #E1' : matrice avec q moy de Maarel

matrice_a<-   he %>%
      select(id_quadrats,qmoy,nom_sc) %>%  
      pivot_wider(names_from = nom_sc, values_from = qmoy) %>%
      as.data.frame() 
    

matrice_a[is.na(matrice_a)] <- 0
rownames(matrice_a)<-matrice_a[,1]
matrice_a<-matrice_a[,-1]
View(matrice_a)


ggplot(data = he, aes(x=id_site, y=qmoy)) +
  geom_boxplot() +
  #geom_jitter() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(.~biotope) +
  theme_bw()

###### cpglmmm
library(cplm)
test <- cpglmm(qmoy ~ biotope + (biotope|id_site), data=he)
test2 <- cpglm(qmoy ~ biotope + id_site, data=he)
anova(test,test2)
summary(test)
fixef(test)
ranef(test)

plot(fitted(test)~test$y)
plot(fitted(test2)~test2$y)
plot(resid(test)~fitted(test))


#E2 : Représentation : 
### calculer le coefficient de recouvrement par espèce : CoefR = (somme des Qmoy / N)/100 avec N le nombre total de relevés
N<- 63 ## N le nombre total de relevés
Sqmoy <- colSums(matrice_a) ## ? voir avec S; comment ne pas prendre en compte les headers : 

CoefR <- (Sqmoy/N)/100
###repr?sentation :ggplot coefR en fonction des SP / et classer et visualiser les biotopes  
  

  
### calculer la richesse spécifique par quadrat / la richesse locale par site / la richesse globale par site et par biotope / la richesse originale par site et par biotope 
###  calculer la richesse spécifique avec les données de la modalite total de la variable inventaire 

specnumber(matrice_a) # Richesse spécifique


# E3 : Richesse et indice de biodiversité : 
## Shannon, simpson, insimpson 
H <- diversity(matrice_a) # Indices de biodiv / si rien = shannon / insimpson 
S <- diversity(matrice_a, index = "simpson", base =2)
I <- diversity(matrice_a, index = "inv", base =2)
unbias.simp <- rarefy(matrice_a, 2) - 1 ## Unbiased Simpson (Hurlbert 1971, eq. 5) 
## repr?sentation des indices en fonction des sites
pairs(cbind(H, S, I, unbias.simp), pch="+", col="blue")

## Equitabilit?
Pi<- H/ log(specnumber(matrice_a)) # Indice de Pielou avec H shannon

# E4  : rarefaction 
rs <- rowSums(matrice_a)#Rarefaction (nombre d'sp total en fonction de l'effort ) observer l'effort d'échantillonage
quantile(rs)
Srar <-rarefy(matrice_a, min(rs))
rarecurve(matrice_a, sample= min(rs))

# E5 : dissimilarité : 
## déterminer l'indice le plus pertinent 
euc_dis <-vegdist(matrice_a, method = "euclidean") # avec Qmoy
bc_dis <-vegdist(matrice_a)
hell_dis <- vegdist(decostand(matrice_a, method="hellinger"),
                      method="euclidean")# avec Qmoy
jac_dis<- vegdist(matrice_a, method = "jaccard")#presence absence data (0;1:...dissimilarité comme jaccard vérifier si besoin matrice_pa nécessaire) 
## Ward mesure de distance à chercher 
##repr?entation des indices : 

###repr?sentation euclideau
tree_euc <- (spantree(euc_dis))
cle <- as.hclust(tree_euc)
plot(cle)

plot(tree_euc, col = cutree(cle, 3), pch=16)
###repr?sentation hellinger
tree_bc <- (spantree(bc_dis))
clb <- as.hclust(tree_bc)
plot(clb)

plot(tree_bcl, col = cutree(clb, 3), pch=16)
###repr?sentation hellinger
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
pca <- rda(decostand(matrice_a, method ="hellinger"), scale =TRUE) ##centrer et scaler
biplot(pca,scaling ="symmetric")
screeplot(pca,bstick = TRUE, type = "l",main = NULL)

## HE = CA correspondence analysis : unimodal (quand data pas linéaire) / marche avec présence absence
ca <- cca(matrice_a)
plot(ca, scaling = "sites") # ou "sp" ou "symmetric")

disp <-"species"
scl <- "symmetric"
plot(pca,dispay = disp, 
     scaling= scl, type ="n")
points(pca, display= disp,
       scaling=scl; pch= 19)
set.seed(10)
ordipointlabel(pca, displa=disp, scaling =scl,add=TRUE)



