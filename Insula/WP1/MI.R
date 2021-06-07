##### Traitement MIcro mammifère #####



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

##Chargement du jeu de données 

##Chargement du jeu de données :
setwd("D:/03_stat/R/DATA")
mi <- read_delim("BD_MI.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>%
  rename_all(tolower) %>%
  unclass() %>%
  as.data.frame() %>% # changer le type de facteur
  select(id_site,etat,biotope,nom_sc) # séléction des variables d'intéret 

str_to_lower(mi$nom_sc)
View (mi)

##E1 création de la matrice freq: 
mi_table <-  as.data.frame(table(mi$id_site, mi$nom_sc))
colnames(mi_table) <- c("id_site", "nom_sc", "freq")

matrice_mi <- mi_table %>%
            pivot_wider(names_from = nom_sc, values_from = freq) %>%
            as.data.frame() 

matrice_mi[is.na(matrice_mi)] <- 0   #remplacer na par 0
rownames(matrice_mi)<-matrice_mi[,1]
matrice_mi<-matrice_mi[,-1]         # colonne 1 des sites en tête et supprimer colonne 1
View(matrice_mi)

##E1 création de la matrice presence/absence 

matrice_pa <- matrice_mi### à revoir : j'ai mi qui devient pa et pa qui devient mi
              matrice_mi[matrice_mi >= 1] <- 1
              
##E2 :Biodiversité : indice et richesse
specnumber(matrice_mi)  ##richesse par site 
specnumber(matrice_pa)
# E3 : Richesse et indice de biodiversitÃ© : 
## Shannon, simpson, insimpson 
H <- diversity(matrice_pa) # Indices de biodiv / si rien = shannon / insimpson 
S <- diversity(matrice_pa, index = "simpson", base =2)
I <- diversity(matrice_pa, index = "inv", base =2)
unbias.simp <- rarefy(matrice_pa, 2) - 1 ## Unbiased Simpson (Hurlbert 1971, eq. 5) 
## représentation des indices en fonction des sites
pairs(cbind(H, S, I, unbias.simp), pch="+", col="blue")

....
                            
              
              