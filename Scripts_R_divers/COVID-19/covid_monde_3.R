library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(gganimate)
library(sf)
library(minpack.lm)
theme_set(theme_bw())


### Obtention et mise en forme des Données ###

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
jhu.covid <- unite(jhu.covid, geounit, c(Province.State, Country.Region), sep="_", remove=F, na.rm=T)

############################################
### Cartographie à l'avant-dernière date ###
############################################

# Fond de carte mondiale, sans l'Antarctique
World <- ne_countries(scale = "medium", type="countries", returnclass="sf") %>% filter(admin != "Antarctica") %>% fortify()

# nombre de pays/regions uniques
nbcountries <- length(unique(jhu.covid$geounit))
# avant-dernière date
bflast <- jhu.covid$date[(length(unique(jhu.covid$date))-1) * nbcountries]

# données de l'avant-dernière date
dat <- dplyr::filter(jhu.covid, date == bflast)


ggplot(World) +
  geom_sf(aes(fill=admin), alpha=0.4, color="White", size=0.3) +
  scale_fill_discrete(guide=NULL) +
  geom_point(data=dat, aes(x=Long, y=Lat, size=confirmed, colour=deaths, stroke=0), alpha=0.6) +
  scale_colour_gradient(low="#3333CC", high="red") +
  scale_size(range=c(0.5,15)) +
  labs(title = "Répartition mondiale des cas avérés de COVID-19", x = "Longitude", y = "Latitude") +
  coord_sf(expand=F)

geom_map() +
  geom_point(data=jhu.covid, aes(x=Long, y=Lat, size=confirmed, colour=deaths, stroke=0), alpha=0.6) +
  scale_colour_gradient(low="#3333CC", high="red") +
  scale_size(range=c(0,15)) +
  coord_quickmap(xlim=c(-160,180), ylim=c(-55,80)) +
  labs(title = "Date: {frame_time}", x = "Longitude", y = "Latitude") +
  theme_bw() +
  guides(colour = guide_colourbar(label.vjust=0.5, label.theme = element_text(angle = 90)),
         size = guide_legend(nrow=1)) +
  theme(legend.position="bottom") +
  transition_time(date)

########################################################################
### Génération des cartes de la dynamique temporelle par pays/région ###
########################################################################

results = tibble(.rows=3)
results[,1] <- c("b","Asym","xmid")
formule <- function(Asym,b,date,xmid) { Asym*exp(-exp(-b*(as.numeric(date)-xmid))) }
newdata = expand.grid(date=as.Date(jhu.covid$date))

for(i in unique(jhu.covid$geounit)) {
  tryCatch({ # fonction pour intercepter les messages d'erreur et permettre à la boucle de continuer
    dat <- filter(jhu.covid, geounit == i)
    model1 <- nls(confirmed ~ formule(Asym,b,date,xmid), data=dat, start=list(b=0.1, Asym=140000, xmid=18350)) 
    results <- cbind(results, tibble(coef(model1)))
    newdata = expand.grid(date=as.Date(dat$date))
    pred <- predict(model1, newdata=newdata)
    p <- ggplot(dat) +
      geom_point(aes(x=as.Date(date), y=confirmed)) +
      geom_line(aes(x=as.Date(date), y=pred), color="red") +
      scale_x_date(date_breaks="1 week", labels=scales::date_format("%d-%m-%Y")) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(title = paste("Evolution des cas officiels de COVID-19 en", i), x="date", y="nombre de cas confirmés")
    ggsave(p, filename=paste("plot_",i,".png"), path="dynplots")
  }, error=function(e){cat("ERROR :",i, "\n")}) # affiche le nom du pays/région qui pose problème
}
  
results

