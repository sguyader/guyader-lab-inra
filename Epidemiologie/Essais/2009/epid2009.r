## Code pour faire des grilles colorés selon le niveau d'un facteur mesuré ("levelplots") ##


#setwd('C:/TRAVAIL/Essais/Essai Duclos 2009/') # emplacement du dossier de travail

library(lattice) # package chargé de construire le graphique
library(grDevices) # package chargé de définir la palette de couleurs

data.2009 <- read.table("Notations2009.txt", strip.white=T, header=T) # lit les données, les convertit sous format vecteur long
data.2009[is.na(data.2009)] <- 10 # remplace valeurs manquantes par 10 (selon l'échelle, spécifier 1 point au-dessus du max de l'échelle)
data.2009.nt <- as.data.frame(data.2009[data.2009$treatment=="plat",])
data.2009.t <- as.data.frame(data.2009[data.2009$treatment=="tuteur",])
JAP.2009 <- c(112,120,126,133,141,147,154,161,171,176,183,190,198) # dates en JAP

head(data.2009) # lit les 6 premières lignes de la table de données, juste pour vérification

colfunc <- colorRampPalette(c("white","yellow","red"), bias=1)   # fonction pour générer la palette continue de couleurs blanc à rouge, avec jaune en intermédiaire
cols <- c(colfunc(10),"black") # définit 10 couleurs discrètes pour échelle allant de 0 à 9 + noir pour valeurs manquantes

nt.plot.2009 <- levelplot(date01+date02+date03+date04+date05+date06+date07+date08+date09+date10+date11+date12+date13   # place plusieurs graphes sur la même page
            ~ x+y, # coordonnées
            data = data.2009.nt, aspect="iso",
            as.table=T,  # si plusieurs graphes sur une même page, remplit la page de haut en bas (de bas en haut par défaut)
            col.regions=cols, at=c(-0.5:10.5), # distribue les couleurs, centrées sur les valeurs entières de "level"
            colorkey=list(at=c(-0.5:10.5), labels=list(at=c(0:10),lab=c(as.character(c(0:9)),"NA"))), # ajoute la clé à droite graphe
            main="A plat",
            xlab="Rangs", ylab="Quadrats", # personnalise les labels des axes x et y
            xlim=c(min(data.2009.nt$x)-0.5,max(data.2009.nt$x)+0.5), ylim=c(min(data.2009.nt$y)-0.5,max(data.2009.nt$y)+0.5), # ajuste les limites des axes
            #scales=list(x=list(at=c(min(data$x):max(data$x))), y=list(at=c(min(data$x):max(data$y)))), # pour forcer les graduations de 1 en 1
            panel=function(x,y,subscripts,cols,...) {  # fonction nécessaire uniquement pour déssiner les lignes de la grille
              panel.levelplot(x,y,subscripts,...)
              hloc=seq(min(data.2009.nt$y):(max(data.2009.nt$y)-1))+0.5
              vloc=seq(min(data.2009.nt$x):(max(data.2009.nt$x)-1))+0.5
              panel.abline(h=hloc, v=vloc, col="darkgrey")
            },
            strip = strip.custom(factor.levels = c("112","120","126","133","141","147","154","161","171","176","183","190","198"), bg="white")
            #shrink=c(0.9,0.9) # ajoute un espace blanc entre chaque "pixel"
            )

t.plot.2009 <- levelplot(date01+date02+date03+date04+date05+date06+date07+date08+date09+date10+date11+date12+date13   # place plusieurs graphes sur la même page
            ~ x+y, # coordonnées
            data = data.2009.t, aspect="iso",
            as.table=T,  # si plusieurs graphes sur une même page, remplit la page de haut en bas (de bas en haut par défaut)
            col.regions=cols, at=c(-0.5:10.5), # distirbue les couleurs, centrées sur les valeurs entières de "level"
            colorkey=list(at=c(-0.5:10.5), labels=list(at=c(0:10),lab=c(as.character(c(0:9)),"NA"))), # ajoute la clé à droite graphe
            main="Tuteuré",
            xlab="Rangs", ylab="Quadrats", # personnalise les labels des axes x et y
            xlim=c(min(data.2009.t$x)-0.5,max(data.2009.t$x)+0.5), ylim=c(min(data.2009.t$y)-0.5,max(data.2009.t$y)+0.5), # ajuste les limites des axes
            #scales=list(x=list(at=c(min(data$x):max(data$x))), y=list(at=c(min(data$x):max(data$y)))), # pour forcer les graduations de 1 en 1
            panel=function(x,y,subscripts,cols,...) {  # fonction nécessaire uniquement pour dessiner les lignes de la grille
              panel.levelplot(x,y,subscripts,...)
              hloc=seq(min(data.2009.t$y):(max(data.2009.t$y)-1))+0.5
              vloc=seq(min(data.2009.t$x):(max(data.2009.t$x)-1))+0.5
              panel.abline(h=hloc, v=vloc, col="darkgrey")
            },
            strip = strip.custom(factor.levels = c("112","120","126","133","141","147","154","161","171","176","183","190","198"), bg="white")
            #shrink=c(0.9,0.9) # ajoute un espace blanc entre chaque "pixel"
            )

postscript("epid2009.eps", width = 10, height = 7, horizontal = F, onefile=F, paper = "special", family = "Helvetica")
print(nt.plot.2009, position = c(0, 0, 0.5, 1), more = TRUE)
print(t.plot.2009, position = c(0.5, 0, 1, 1))            
dev.off() # ferme et écrit l'image sur le disque

## Graphe sévérité moyenne ##
sever.2009 <- data.frame(plat=numeric(0), tuteur=numeric(0))
for (i in 1:13) {
sever.2009[i,] <- tapply(data.2009[,i+3], data.2009[,1], mean)
}
# Transformer les notes en % de surface nécrosée selon l'échelle :
#1 -> 1.5 
#2 -> 4.5 
#3 -> 9.0 
#4 -> 18.5 
#5 -> 37.5 
#6 -> 62.5 
#7 -> 81.5 
#8 -> 93.5 
#9 -> 100
library(Deducer)
data.2009.sev <- data.2009

data.2009.sev <- recode.variables(data.2009.sev[c("date01","date02","date03","date04","date05","date06","date07","date08","date09","date10","date11","date12","date13")] , "9->100; 8->93.5; 7->81.5; 6->62.5; 5->37.5; 4->18.5; 3->9.0; 2->4.5; 1->1.5;")
colnames(data.2009.sev) <- c("date01","date02","date03","date04","date05","date06","date07","date08","date09","date10","date11","date12","date13")
data.2009.sev <- cbind(data.2009[c("treatment","x","y")],data.2009.sev)
head(data.2009.sev)

moy.sever.2009 <- data.frame(plat=numeric(0), tuteur=numeric(0))
for (i in 1:13) {
moy.sever.2009[i,] <- tapply(data.2009.sev[,i+3], data.2009.sev[,1], mean)
}
moy.sever.2009 <- cbind(JAP.2009, moy.sever.2009)
fix(moy.sever.2009)
logis.nt.2009 <- nls(plat ~ 100/(1+exp((xmid-JAP.2009)/ scal)), data = moy.sever.2009, start=c(xmid=150,scal=10))
logis.t.2009 <- nls(tuteur ~ 100/(1+exp((xmid-JAP.2009)/ scal)), data = moy.sever.2009, start=c(xmid=150,scal=10))

tt <- seq(0, 200, length=201)

postscript("logis2009.eps", width = 4, height = 4, horizontal = F, onefile=F, paper = "special", family = "Helvetica")
xyplot(plat+tuteur~JAP.2009, data=moy.sever.2009, main="2009", pch=c(1,4), col="black", scales=list(y=list(at=c(0,20,40,60,80,100)), font=2), xlab=list("Jours après plantation", font=2), ylab=list("Sévérité (% surf.)", font=2), ylim=c(0,100), xlim=c(80,200))
trellis.focus(highlight=F)
llines(tt, predict(logis.nt.2009, list(JAP.2009 = tt)), col="black", lty=1)
llines(tt, predict(logis.t.2009, list(JAP.2009 = tt)), col="black", lty=2)
trellis.unfocus()
dev.off()