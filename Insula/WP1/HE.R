####Traitement herbacee


##Etape##
library(tidyverse)
library(vegan)
library(magrittr)
library(readr)
library(dplyr)
library(tidyr)

setwd("./WP1")
he <- read_delim("data/DATA_HE.csv", ";", escape_double = FALSE, trim_ws = TRUE)%>%
  rename_all(tolower) %>%
  unclass()%>%
  as.data.frame()%>%
  filter(inventaire  == "quadrat") %>% # selectionner uniquement les données ayant pour modalite "quadrat" de la variable inventaire 
  select(id_site, etat, biotope, id_quadrats, nom_sc, qmoy) 
 
View(he)
str(he)

  
#E1 : MATRICE HE quadrat   
    #E1' : matrice présence absence 0/1

matrice_pa <- he %>%
  select(id_quadrats,qmoy,nom_sc) %>%  
  pivot_wider(names_from = nom_sc, values_from = qmoy) %>%
  as.data.frame()

matrice_pa[matrice_pa == "NULL"] <- 0

ggplot(data = he, aes(x=id_site, y=qmoy)) +
  geom_boxplot() +
  #geom_jitter() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(.~biotope) +
  theme_bw()

test <- cpglmm(qmoy ~ biotope + (biotope|id_site), data=he)
test2 <- cpglm(qmoy ~ biotope, data=he)
summary(test)
fixef(test)
ranef(test)

    ### changer en 1 tout sauf 0 
    #E1'' : Mettre l'abondance  des sp en variable / les sites en données
  
      ### créer une nouvelle variable "Qmoy"  : transformer le coefficent d'abondance dominance de BB au coefficient de recouvrement de Van der Maarel 
                ## c(r,+, 1, 2, 3, 4, 5) en c(1,2,3,5,7,8,9)
    matrice_a<-   he %>%
      select(id_quadrats,qmoy,nom_sc) %>%  
      pivot_wider(names_from = nom_sc, values_from = qmoy) %>%
      mutate(he= ifelse(he == "NULL", 0, he) %>%  ### convertir les valeurs NULL par 0 
               as.numeric
  
#E2 : Représentation : 
### calculer le coefficient de recouvrement par espèce : CoefR = (somme des Qmoy / N)/100 avec N le nombre total de relevés
N<- 63 ## N le nombre total de relevés
Sqmoy <- qmoydes sp ##

  
### calculer la richesse spécifique par quadrat / la richesse locale par site / la richesse globale par site et par biotope / la richesse originale par site et par biotope 
###  calculer la richesse spécifique avec les données de la modalite total de la variable inventaire 

specnumber(matrice_a) # Richesse spécifique



# E3 : Richesse et indice de biodiversité : 
## Shannon, simpson, insimpson 
H <- diversity(matrice_a) # Indices de biodiv / si rien = shannon / insimpson 
S <- diversity(matrice_a, index = "simpson", base =2)
I <- diversity(matrice_a, index = "invsimpson", base =2)
### représentation des indices en fonction des sites

## Equitabilité
Pi<- H/ log(specnumber(matrice_a)) # Indice de Pielou avec H shannon


k <-sample((nrow((matrice_a),6)))
R <- renyi(matrice_a[k,]) #Entropy de rényi à comprendre

# E4  : rarefaction 
rs <- rowSums(database)#Rarefaction (nombre d'sp total en fonction de l'effort ) observer l'effort d'échantillonage
quantile(rs)
Srar <-rarefy(database, min(rs))
rarecurve(database, sample= min(rs))

# E5 : dissimilarité : 
## déterminer l'indice le plus pertinent 
data (database)
euc_dis <-vegdist(database, method = "euclidean") # avec Qmoy
bc_dis <-vegdist(database)
  hell_dis <- vegdist(decostand(database, method="hellinger"),
                      method="euclidean")# avec Qmoy
jac_dis<- vegdist(database_pr_abs, method = "jaccard")#presence absence data (0;1:...dissimilarité comme jaccard vérifier si besoin matrice_pa nécessaire) 
## Ward mesure de distance à chercher 
##représentation des indices par arbre ? 

#E6 : Ordination  ## ADE4 : écology ; regarder le package  voir exemple / interface graphique ADE4 TKGUI
##  PCA : methode linéaire petit gradient / 
pca <- rda(decostand(datasp, method ="hellinger"), scale =TRUE) ##centrer et scaler
biplot(pca,scaling ="symmetric")
eigenvals(pca),5) ## eigenvalues inertie de chaque axe, la somme des variance
screeplot(pca,bstick = TRUE, type = "l",main = NULL)

## HE = CA correspondence analysis : unimodal (quand data pas linéaire) / marche avec présence absence
ca <- cca(datasp)
plot(ca, scaling = "sites") # ou "sp" ou "symmetric")

disp <-"species"
scl <- "symmetric"
plot(pca,dispay = disp, 
     scaling= scl, type ="n")
points(pca, display= disp,
       scaling=scl; pch= 19)
set.seed(10)
ordipointlabel(pca, displa=disp, scaling =scl,add=TRUE)



