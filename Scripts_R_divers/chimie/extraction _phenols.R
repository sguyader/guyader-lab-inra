library(tidyverse) # pour travailler sur les données + graphiques de base
library(rsm) # pour le graphique de surface 3D


setwd("~/TRAVAIL/git-repo/guyader-lab-inra/Scripts_R_divers/chimie")
data <- read_delim("extraction_oriental.csv", "\t", escape_double = FALSE, trim_ws = TRUE)


#### Traitement incubation et température ####


# coder les variables explicatives en facteurs pour les graphiques de base
data$température <- as.factor(data$température)
data$durée <- as.factor(data$durée)

# extraire les données pour le traitement durée*température
data_temp_durée <- data %>% filter(traitement=="effet_température_durée")

# faire un tableau résumé
data_temp_durée_resum <- data_temp_durée %>%
  group_by(température, durée) %>%
  summarise(concentration_moyenne=mean(concentration), # moyenne de la série
            se=sd(concentration), # écart type de la série
            N=n()) # nombre d'échantillons de la série

# graphique concentration en fonction de la température ;
# couleur et forme des points en fonction de la durée, avec les données brutes (3 mesures)
ggplot(data=data_temp_durée, aes(x=température, y=concentration)) +
  geom_jitter(aes(color=durée, shape=durée), width=0.08, size=2) +
  xlab("Température") +
  ylab("Concentration") +
  theme_bw()

# graphique concentration en fonction de la température, couleur de la ligne en fonction du temps d'incubation, à partir de la valeur moyenne de chaque traitement
ggplot(data=data_temp_durée_resum, aes(x=température, y=concentration_moyenne, group=durée)) +
  geom_line(aes(color=durée)) +
  xlab("Température") +
  ylab("Concentration") +
  theme_bw()

# graphique en carte de chaleur croisant température et temps d'incubation, à partir des valeurs moyennes
ggplot(data_temp_durée_resum, aes(x=température, y=durée, fill=concentration_moyenne)) +
  geom_tile() +
  xlab("Température") +
  ylab("Durée") +
  scale_fill_gradient(name = "Concentration", low = "blue", high = "green") +
  theme_bw()


### modèle linéaire + quadratique

# reconvertir les variables explicatives en numériques
data_temp_durée$température <- as.numeric(data_temp_durée$température)
data_temp_durée$durée <- as.numeric(data_temp_durée$durée)

# modèle 1 : effets principaux + interaction
mod.1 <- lm(concentration ~ durée + température + durée:température, data=data_temp_durée)
anova(mod.1)
plot(mod.1)

# modèle 2 : ajout des termes quadratiques sur température et durée
mod.2 <- lm(concentration ~ durée + température + durée:température + I(durée^2) + I(température^2), data=data_temp_durée)
anova(mod.2) # montre que le terme quadratique pour durée est négligeable
plot(mod.2)

# comparaison des modèles
anova(mod.1, mod.2) # montre que le modèle avec termes quadratiques est meilleur statistiquement que sans termes quadratiques

# modèle 3 : affinons le modèle 2 en retirant le terme quadratique sur la durée qui est le plus négligeable (on garde l'interaction température*durée)
mod.3 <- lm(concentration ~ durée + température + durée:température + I(température^2), data=data_temp_durée)
anova(mod.3)
plot(mod.3)

# comparaison modèles 2 et 3
anova(mod.2, mod.3) # il n'y a pas de différence significative entre les 2 modèles, donc on pourrait garder le plus simple : mod.3

#### Diagramme de Pareto

# extraction de effets dans un nouveau tableau "dat"
coeff.full <- coef(mod.2)[-1]
coeff.full <- na.omit(coeff.full)
coeff.abs <- unname(abs(coeff.full))
coeff <- sort(coeff.abs, index.return = TRUE, method = "shell", decreasing = T)
grouping <- unname((coeff.full > 0)[coeff$ix])
grouping[grouping == FALSE] <- "Négatif"
grouping[grouping == TRUE] <- "Positif"
fnames <- factor(names(coeff.full)[coeff$ix], levels = names(coeff.full)[coeff$ix], ordered = TRUE)
dat <- data.frame(label = fnames, value = abs(coeff$x), group = grouping) # pour les barres d'effets
dat <- dat %>% mutate(cs=cumsum(value)/sum(value)*100) # pour la courbe de pourcentage cumulé
label <- value <- group <- NULL

# diagramme
ggplot(dat, aes(x = label, y = value)) +
  geom_bar(stat = "identity", aes(fill = group)) + 
  geom_line(aes(x=label, y=cs*2, group=1)) +
  geom_point(aes(x=label, y=cs*2, group=1)) +
  scale_fill_manual(values = setNames(c("grey", "black"), c("Négatif", "Positif")),
                    name = "Signe du coefficient") + 
  scale_y_continuous(name = "Magnitude du coefficient (valeur absolue)", 
    sec.axis = sec_axis(~./5, name = "Pourcentage cumulé", 
    labels = function(b) { paste0(round(b * 2.5, 0), "%")})) +
  xlab("Effet") +
  ggtitle("Diagramme de Pareto") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# Surface de réponse
persp(mod.2, durée ~ température, contours="col", xlab=c("Température","Durée"), zlab="Concentration")

      