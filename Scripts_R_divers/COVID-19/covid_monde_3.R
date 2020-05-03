library(tidyverse)
library(rnaturalearth)
library(gganimate)
library(sf)
library(minpack.lm)
theme_set(theme_bw())

##############################################
### Obtention et mise en forme des Données ###
##############################################

# Télécharger les données de la Johns Hopkins University
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", destfile = "data/covid19_confirmed.csv")
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", destfile = "data/covid19_deaths.csv")
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", destfile = "data/covid19_recovered.csv")

# Charger les données en mémoire
covid_confirmed <- read_csv("data/covid19_confirmed.csv")
covid_deaths <- read_csv("data/covid19_deaths.csv")
covid_recovered <- read_csv("data/covid19_recovered.csv")

# Convertir les données au format "long"
covid_confirmed_long <- gather(covid_confirmed, 5:ncol(covid_confirmed), key=date, value=confirmed)
covid_deaths_long <- gather(covid_deaths, 5:ncol(covid_deaths), key=date, value=deaths)
covid_recovered_long <- gather(covid_recovered, 5:ncol(covid_recovered), key=date, value=recovered)

# Joindre les 3 types de données en un seul tableau
jhu.covid <- full_join(covid_confirmed_long, covid_deaths_long)
jhu.covid <- left_join(jhu.covid, covid_recovered_long)

# Retirer les données correspondant à 2 paquebots de croisière (coordonnées Long/Lat 0/0)
jhu.covid <- filter(jhu.covid, Lat != 0)

# Convertir les dates du format "character" vers le format "Date"
jhu.covid$date <- as.Date(jhu.covid$date, format="%m/%d/%y")

# Renommer les 2 premières colonnes pour retirer le caractère "/" qui complique le filtrage
jhu.covid <- jhu.covid %>% rename("Province.State" = "Province/State") %>% rename("Country.Region" = "Country/Region")

# Créer une variable identité unique "geounit" qui combine les 2 premières colonnes Province.State et Country.Region
jhu.covid <- unite(jhu.covid, geounit, c(Country.Region, Province.State), sep="_", remove=F, na.rm=T)

### Cas particulier de l'Australie, le Cananda et la Chine : les données sont uniquement par région,
### il faut donc faire la somme des régions pour avoir la dynamique au niveau pays

jhu.covid.2 <- filter(jhu.covid, Country.Region != "Australia" & Country.Region != "Canada" & Country.Region != "China")

# Filtrage et préparation des données
australia <- filter(jhu.covid, Country.Region == "Australia") %>% group_by(date) %>% mutate(confirmed = sum(confirmed), deaths=sum(deaths), recovered=sum(recovered)) %>% filter(row_number() %% length(unique(.$date)) == 1)
australia[,1] <- "Australia"
australia[,2] <- NA
australia[,4] <- -25.2743988
australia[,5] <- 133.7751312

canada <- filter(jhu.covid, Country.Region == "Canada") %>% group_by(date) %>% mutate(confirmed = sum(confirmed), deaths=sum(deaths), recovered=sum(recovered)) %>% filter(row_number() %% length(unique(.$date)) == 1)
canada[,1] <- "Canada"
canada[,2] <- NA
canada[,4] <- 56.1303673
canada[,5] <- -106.3467712

china <- filter(jhu.covid, Country.Region == "China") %>% group_by(date) %>% mutate(confirmed = sum(confirmed), deaths=sum(deaths), recovered=sum(recovered)) %>% filter(row_number() %% length(unique(.$date)) == 1)
china[,1] <- "China"
china[,2] <- NA
china[,4] <- 34.231011
china[,5] <- 103.614978

# joindre avec le premier tableau et classer par "geounit"
jhu.covid.3 <- bind_rows(jhu.covid.2, australia, canada, china) %>% arrange(geounit)

############################################
### Cartographie à l'avant-dernière date ###
############################################

# Fond de carte mondiale, sans l'Antarctique
World <- ne_countries(scale = "large", type="countries", returnclass="sf") %>% filter(admin != "Antarctica") %>% fortify()

# Filtrer les colonnes intéressantes
World <- select(World, c(geounit,gu_a3,iso_a2, iso_a3, economy, income_grp, continent, region_un, subregion, region_wb))

# nombre de pays/regions uniques
nbcountries <- length(unique(jhu.covid.3$geounit))
# avant-dernière date
bflast <- jhu.covid.3$date[(length(unique(jhu.covid.3$date))-1)]

# données de l'avant-dernière date
dat.bflast <- dplyr::filter(jhu.covid.3, date == bflast)

worldmap <- ggplot(World) +
  geom_sf(aes(fill=geounit), alpha=0.4, color="White", size=0.3) +
  scale_fill_discrete(guide=NULL) +
  geom_point(data=dat.bflast, aes(x=Long, y=Lat, size=confirmed, colour=deaths, stroke=0), alpha=0.6) +
  scale_colour_gradient(low="#3333CC", high="red") +
  scale_size(range=c(0.5,15)) +
  labs(title = paste("Répartition mondiale des cas avérés de COVID-19 au", bflast), x = "Longitude", y = "Latitude") +
  coord_sf(expand=F)

worldmap

########################################################################
### Génération des cartes de la dynamique temporelle par pays/région ###
########################################################################

# créer un tableau vide dans le quel les paramètres des courbes ajustées seront écrits
results <- tibble(params=c("b","Asym","xmid", "P(b)", "P(Asym)", "P(xmid)", "deviance", "converged", "nbiter"))

# formule du modèle de gompertz
gomp.fn <- confirmed ~ Asym*exp(-exp(-b*(as.numeric(date)-xmid)))

# boucle d'ajustements
for(i in unique(jhu.covid.3$geounit)) {
  tryCatch({ # fonction pour intercepter les erreurs et laisser la boucle de continuer
    dat <- filter(jhu.covid.3, geounit == i) # filtrer par région/pays
    model1 <- nlsLM(gomp.fn, data=dat, start=list(b=0.1, Asym=4e4, xmid=1.83e4))  # ajustement nonliéaire avec algorithme Levenberg-Marquardt
    res <- tibble(coef(model1))
    names(res) <- i
    res <- rbind(res, summary(model1)$coefficients[10], summary(model1)$coefficients[11], summary(model1)$coefficients[12], deviance(model1), model1$convInfo$isConv, model1$convInfo$finIter
)
    results <- cbind(results, res) # ajouter les paramètres au tableau
    newdata = expand.grid(date=as.Date(dat$date)) # créer un jeu de dates servant de valeurs explicatives au modèle pour générer les courbes
    pred <- predict(model1, newdata=newdata) # générer les valeurs prédites par le modèle
    p <- ggplot(dat) + # générer le graphique confirmed = f(date) et le stocker en mémoire
      geom_point(aes(x=as.Date(date), y=confirmed)) + # points de données réelles
      geom_line(aes(x=as.Date(date), y=pred), color="red") + # courbe ajustée
      scale_x_date(date_breaks="1 week", labels=scales::date_format("%d-%m-%Y")) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(title = paste("Evolution des cas officiels de COVID-19 en", i), x="date", y="nombre de cas confirmés")
    ggsave(p, filename=paste("plot_",i,".png"),
           path="dynplots", width=8, height=5, units="in", dpi=72) # sauver le graphique
  }, error=function(e){cat("ERROR :",i, "\n")}) # afficher la source si erreur
}


# Tableau de résultats des ajustements

gompertz_param_table <- results %>% pivot_longer(-params, names_to = "Country.Region", values_to = "value") %>% pivot_wider(names_from=params, values_from=value)

write_excel_csv(gompertz_param_table, "gompertz_param_table_25-04-2020.csv")
write_excel_csv(jhu.covid.3, "JHU_covid19_data_25-04-2020.csv")


### Liste des pays
World2 <- select(as.data.frame(World), -geometry) # extract countries without sf geometry
World2 <- cbind(World2, st_coordinates(st_point_on_surface(World))) # extract x y coordinates and append to countries
colnames(World2)[11] <- "Long"
colnames(World2)[12] <- "Lat"

# Ajouter les codes iso_a3 manquants pour Norvège, Kosovo et France
World2[23,4] <- "FRA"
World2[54,4] <- "NOR"
World2[66,4] <- "XKX"

# retirer les petits territoires sans code iso
World2 <- filter(World2, !is.na(iso_a3))

# classer par code pays iso_a3 croissant
World2 <- arrange(World2, iso_a3)

### Température moyenne mensuelle

library(raster)
library(rgdal)

#GDALinfo("data/2020_03_data.tiff")

raster201912 <- readGDAL("data/2019_12_data.tiff")
raster202001 <- readGDAL("data/2020_01_data.tiff")
raster202002 <- readGDAL("data/2020_02_data.tiff")
raster202003 <- readGDAL("data/2020_03_data.tiff")

raster201912$band1 <- raster201912$band1 -273.15
raster202001$band1 <- raster202001$band1 -273.15
raster202002$band1 <- raster202002$band1 -273.15
raster202003$band1 <- raster202003$band1 -273.15

raster201912$band1[raster201912$band1 > 1000] <- NA
raster202001$band1[raster202001$band1 > 1000] <- NA
raster202002$band1[raster202002$band1 > 1000] <- NA
raster202003$band1[raster202003$band1 > 1000] <- NA

raster201912 <- raster201912 %>% raster() %>% rotate()
raster202001 <- raster202001 %>% raster() %>% rotate()
raster202002 <- raster202002 %>% raster() %>% rotate()
raster202003 <- raster202003 %>% raster() %>% rotate()

coords <- SpatialPoints(coords = data.frame(World2$Long, World2$Lat),
                        proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


temperatures_mensuelles <- tibble(temp_dec19 = raster201912[cellFromXY(raster201912, xy = coords)],
                                  temp_jan20 = raster202001[cellFromXY(raster202001, xy = coords)],
                                  temp_fev20 = raster202002[cellFromXY(raster202002, xy = coords)],
                                  temp_mar20 = raster202003[cellFromXY(raster202003, xy = coords)]
                                  )

temperatures_mensuelles <- mutate(temperatures_mensuelles, temp_mean=rowMeans(temperatures_mensuelles))

World2 <- bind_cols(World2, temperatures_mensuelles)

write_excel_csv(World2, "natural_earth_data.csv")

worldmap + geom_point(data=as.data.frame(coords), aes(x=World2.Long, y=World2.Lat), shape=1, fill=NA)
