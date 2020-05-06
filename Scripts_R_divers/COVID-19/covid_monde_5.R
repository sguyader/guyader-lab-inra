library(tidyverse)
library(rnaturalearth)
#library(gganimate)
library(sf)
library(minpack.lm)

library(raster)
library(rgdal)

theme_set(theme_bw())

##############################################
### Obtention et mise en forme des Données ###
##############################################

# Télécharger les données de la Johns Hopkins University
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", destfile = "data/covid19_confirmed_03mai2020.csv")
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", destfile = "data/covid19_deaths_03mai2020.csv")
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", destfile = "data/covid19_recovered_03mai2020.csv")
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv", destfile = "data/UID_ISO_FIPS_LookUp_Table_03mai2020.csv")

# Charger les identifiants
covid_uid <- read_csv("data/UID_ISO_FIPS_LookUp_Table_03mai2020.csv") %>%
  rename("Province.State" = "Province_State") %>% rename("Country.Region" = "Country_Region") %>%
  slice(1:288) %>%
  filter(!row_number() %in% 156:174) %>%
  filter(!Country.Region %in% c("Comoros", "Tajikistan"))

# Filtrer les identifiants pour pays à conserver
covid_uid <- filter(covid_uid, !UID %in% c(9999,8888,535,3601:3608,12401:12415,15601:15631,344,446)) %>%
  arrange(Combined_Key)
# Changer les codes ISO pour les îles Anglo Normandes
covid_uid[41,2] <- NA
covid_uid[41,3] <- "CHI"

# Confirmés
covid_confirmed <- read_csv("data/covid19_confirmed_03mai2020.csv") %>%
  rename("Province.State" = "Province/State") %>% rename("Country.Region" = "Country/Region") %>%
  filter(!Country.Region %in% c("Comoros", "Tajikistan"))

covid_confirmed <- unite(covid_confirmed, Country, c(Province.State, Country.Region), sep=", ", remove=F, na.rm=T)

covid_confirmed_long <- gather(covid_confirmed, 6:ncol(covid_confirmed), key=date, value=confirmed)
covid_confirmed_long$date <- as.Date(covid_confirmed_long$date, format="%m/%d/%y")
covid_confirmed_long <- filter(covid_confirmed_long, date < "2020-05-01")

australia <- filter(covid_confirmed_long, Country.Region == "Australia") %>% group_by(date) %>%
  mutate(confirmed = sum(confirmed)) %>% filter(row_number() %% length(unique(.$date)) == 1)
australia[,1] <- "Australia"
australia[,2] <- NA
australia[,4] <- -25.2743988
australia[,5] <- 133.7751312

canada <- filter(covid_confirmed_long, Country.Region == "Canada") %>% group_by(date) %>%
  mutate(confirmed = sum(confirmed)) %>% filter(row_number() %% length(unique(.$date)) == 1)
canada[,1] <- "Canada"
canada[,2] <- NA
canada[,4] <- 56.1303673
canada[,5] <- -106.3467712

china <- filter(covid_confirmed_long, Country.Region == "China") %>% group_by(date) %>%
  mutate(confirmed = sum(confirmed)) %>% filter(row_number() %% length(unique(.$date)) == 1)
china[,1] <- "China"
china[,2] <- NA
china[,4] <- 34.231011
china[,5] <- 103.614978

covid_confirmed_long <- filter(covid_confirmed_long, !Country.Region %in% c("Australia", "Canada", "China", "MS Zaandam", "Diamond Princess"), Country != "Bonaire, Sint Eustatius and Saba, Netherlands")

covid_confirmed_long <- bind_rows(covid_confirmed_long, australia, canada, china) %>% arrange(Country)

length(unique(covid_confirmed_long$Country)) # doit faire 208


# Décédés
covid_deaths <- read_csv("data/covid19_deaths_03mai2020.csv") %>%
  rename("Province.State" = "Province/State") %>% rename("Country.Region" = "Country/Region") %>%
  filter(!Country.Region %in% c("Comoros", "Tajikistan"))

covid_deaths <- unite(covid_deaths, Country, c(Province.State, Country.Region), sep=", ", remove=F, na.rm=T)

covid_deaths_long <- gather(covid_deaths, 6:ncol(covid_deaths), key=date, value=deaths)
covid_deaths_long$date <- as.Date(covid_deaths_long$date, format="%m/%d/%y")
covid_deaths_long <- filter(covid_deaths_long, date < "2020-05-01")

australia <- filter(covid_deaths_long, Country.Region == "Australia") %>% group_by(date) %>%
  mutate(deaths = sum(deaths)) %>% filter(row_number() %% length(unique(.$date)) == 1)
australia[,1] <- "Australia"
australia[,2] <- NA
australia[,4] <- -25.2743988
australia[,5] <- 133.7751312

canada <- filter(covid_deaths_long, Country.Region == "Canada") %>% group_by(date) %>%
  mutate(deaths = sum(deaths)) %>% filter(row_number() %% length(unique(.$date)) == 1)
canada[,1] <- "Canada"
canada[,2] <- NA
canada[,4] <- 56.1303673
canada[,5] <- -106.3467712

china <- filter(covid_deaths_long, Country.Region == "China") %>% group_by(date) %>%
  mutate(deaths = sum(deaths)) %>% filter(row_number() %% length(unique(.$date)) == 1)
china[,1] <- "China"
china[,2] <- NA
china[,4] <- 34.231011
china[,5] <- 103.614978

covid_deaths_long <- filter(covid_deaths_long, !Country.Region %in% c("Australia", "Canada", "China", "MS Zaandam", "Diamond Princess"), Country != "Bonaire, Sint Eustatius and Saba, Netherlands")

covid_deaths_long <- bind_rows(covid_deaths_long, australia, canada, china) %>% arrange(Country)

length(unique(covid_deaths_long$Country)) # doit faire 208

# Guéris
covid_recovered <- read_csv("data/covid19_recovered_03mai2020.csv") %>%
  rename("Province.State" = "Province/State") %>% rename("Country.Region" = "Country/Region") %>%
  filter(!Country.Region %in% c("Comoros", "Tajikistan"))

covid_recovered <- unite(covid_recovered, Country, c(Province.State, Country.Region), sep=", ", remove=F, na.rm=T)


covid_recovered_long <- gather(covid_recovered, 6:ncol(covid_recovered), key=date, value=recovered)
covid_recovered_long$date <- as.Date(covid_recovered_long$date, format="%m/%d/%y")
covid_recovered_long <- filter(covid_recovered_long, date < "2020-05-01")

australia <- filter(covid_recovered_long, Country.Region == "Australia") %>% group_by(date) %>%
  mutate(recovered = sum(recovered)) %>% filter(row_number() %% length(unique(.$date)) == 1)
australia[,1] <- "Australia"
australia[,2] <- NA
australia[,4] <- -25.2743988
australia[,5] <- 133.7751312

canada <- filter(covid_recovered_long, Country.Region == "Canada") %>% group_by(date) %>%
  mutate(recovered = sum(recovered)) %>% filter(row_number() %% length(unique(.$date)) == 1)
canada[,1] <- "Canada"
canada[,2] <- NA
canada[,4] <- 56.1303673
canada[,5] <- -106.3467712

china <- filter(covid_recovered_long, Country.Region == "China") %>% group_by(date) %>%
  mutate(recovered = sum(recovered)) %>% filter(row_number() %% length(unique(.$date)) == 1)
china[,1] <- "China"
china[,2] <- NA
china[,4] <- 34.231011
china[,5] <- 103.614978

covid_recovered_long <- filter(covid_recovered_long, !Country.Region %in% c("Australia", "Canada", "China", "MS Zaandam", "Diamond Princess"), Country != "Bonaire, Sint Eustatius and Saba, Netherlands")

covid_recovered_long <- bind_rows(covid_recovered_long, australia, canada, china) %>% arrange(Country)

length(unique(covid_recovered_long$Country)) # doit faire 208

# Joindre les 3 types de données en un seul tableau
jhu.covid <- cbind(covid_confirmed_long, deaths = covid_deaths_long$deaths, recovered = covid_recovered_long$recovered)

# joindre avec le premier tableau et classer par "geounit"
jhu.covid.2 <- jhu.covid %>% separate(Country, into = c("Country", "to_remove"), sep = ", ") %>%
  dplyr::select(-to_remove)

jhu.covid.3 <- jhu.covid.2 %>% 
  mutate(Country = replace(Country, which(Country == "Bahamas"), "Bahamas, The"))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Brunei"), "Brunei Darussalam"))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Burma"), "Myanmar"))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Congo (Brazzaville)"), "Congo, Rep."))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Congo (Kinshasa)"), "Congo, Dem. Rep."))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Egypt"), "Egypt, Arab Rep."))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "St Martin"), "St. Martin (French part)"))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Gambia"), "Gambia, The"))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Holy See"), "Vatican"))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Iran"), "Iran, Islamic Rep."))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Korea"), "Korea, Rep."))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Kyrgyzstan"), "Kyrgyz Republic"))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Laos"), "Lao PDR"))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Sint Maarten"), "Sint Maarten (Dutch part)"))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Russia"), "Russian Federation"))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Saint Kitts and Nevis"), "St. Kitts and Nevis"))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Saint Lucia"), "St. Lucia"))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Saint Vincent and the Grenadines"), "St. Vincent and the Grenadines"))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Slovakia"), "Slovak Republic"))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Syria"), "Syrian Arab Republic"))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Taiwan*"), "Taiwan"))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Falkland Islands (Malvinas)"), "Falkland Islands"))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "US"), "United States"))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Venezuela"), "Venezuela, RB"))
jhu.covid.3 <- jhu.covid.3 %>% 
  mutate(Country = replace(Country, which(Country == "Yemen"), "Yemen, Rep."))

nb_dates <- length(unique(jhu.covid.3$date))
iso_a2 <- rep(covid_uid$iso2, each=nb_dates)
iso_a3 <- rep(covid_uid$iso3, each=nb_dates)

jhu.covid.3 <- cbind(ISO_A3=iso_a3, ISO_A2=iso_a2, jhu.covid.3)

jhu.covid.3 <- jhu.covid.3 %>% dplyr::select(-Province.State, -Country.Region)
jhu.covid.3 <- jhu.covid.3 %>% arrange(ISO_A3)

### Remplacer les valeurs "confirmed" par les valeurs issues de l'ECDC

download.file("https://open-covid-19.github.io/data/data_minimal.csv", destfile="data/covid_ecdc.csv")

covid_fr_ecdc <- read_csv("data/covid_ecdc.csv")
covid_fr_ecdc <- filter(covid_fr_ecdc, Key == "FR" & Date > "2020-01-21" & Date < "2020-05-01")

for(i in 1:length(filter(jhu.covid.3, Country =="France")$confirmed)){
  jhu.covid.3[(6600+i), "confirmed"] <- covid_fr_ecdc[i, "Confirmed"]
  print(i)
}

plot(filter(jhu.covid.3, Country =="France")$date, filter(jhu.covid.3, Country =="France")$confirmed)

write_excel_csv(jhu.covid.3, "JHU_covid19_data_03mai2020.csv")

############################################
### Cartographie à la dernière date ###
############################################

# Fond de carte mondiale, sans l'Antarctique
World <- ne_download(scale = "large", type="countries", returnclass="sf") %>%
  filter(GEOUNIT != "Antarctica") %>% fortify()

# Filtrer les colonnes intéressantes
World <- dplyr::select(World, c(ISO_A3, ISO_A2, GEOUNIT, ECONOMY, INCOME_GRP, CONTINENT, REGION_UN, SUBREGION, REGION_WB))

# nombre de pays/regions uniques
nbcountries <- length(unique(jhu.covid.3$Country))
# dernière date
last_date <- jhu.covid.3$date[length(unique(jhu.covid.3$date))]

# données covid de la dernière date
dat.last <- dplyr::filter(jhu.covid.3, date == last_date) %>%
  rename(confirmed_30avr = confirmed, deaths_30avr = deaths, recovered_30avr = recovered) %>%
  select(-date) %>%
  arrange(ISO_A3)

worldmap <- ggplot(World) +
  geom_sf(aes(fill=GEOUNIT), alpha=0.4, color="White", size=0.3) +
  scale_fill_discrete(guide=NULL) +
  geom_point(data=dat.last, aes(x=Long, y=Lat, size=confirmed_30avr, colour=deaths_30avr, stroke=0), alpha=0.6) +
  scale_colour_gradient(low="#3333CC", high="red") +
  scale_size(range=c(0.5,15)) +
  labs(title = paste("Répartition mondiale des cas avérés de COVID-19 au", last_date), x = "Longitude", y = "Latitude") +
  coord_sf(expand=F)

worldmap

########################################################################
### Génération des cartes de la dynamique temporelle par pays/région ###
########################################################################

##### confirmés

# formule du modèle de gompertz
gomp.confirmed.fun <- confirmed ~ Asym*exp(-exp(-b*(as.numeric(date)-xmid)))

# créer un tableau vide dans le quel les paramètres des courbes ajustées seront écrits
results.confirmed <- tibble(params=c("b_confirmed","Asym_confirmed","xmid_confirmed", "P(b)_confirmed", "P(Asym)_confirmed", "P(xmid)_confirmed", "converged_confirmed", "nbiter_confirmed"))

# boucle d'ajustements
for(i in unique(jhu.covid.3$Country)) {
  tryCatch({ # fonction pour intercepter les erreurs et laisser la boucle de continuer
    dat <- filter(jhu.covid.3, Country == i) # filtrer par région/pays
    model1 <- nlsLM(gomp.confirmed.fun, data=dat, start=list(b=0.1, Asym=5e4, xmid=1.83e4), control = nls.lm.control(maxiter=100))  # ajustement nonliéaire avec algorithme Levenberg-Marquardt
    res <- tibble(coef(model1))
    names(res) <- i
    res <- rbind(res, summary(model1)$coefficients[10], summary(model1)$coefficients[11], summary(model1)$coefficients[12], model1$convInfo$isConv, model1$convInfo$finIter
)
    results.confirmed <- cbind(results.confirmed, res) # ajouter les paramètres au tableau
    # newdata = expand.grid(date=as.Date(dat$date)) # créer un jeu de dates servant de valeurs explicatives au modèle pour générer les courbes
    # pred <- predict(model1, newdata=newdata) # générer les valeurs prédites par le modèle
    # p <- ggplot(dat) + # générer le graphique confirmed = f(date) et le stocker en mémoire
    #   geom_point(aes(x=as.Date(date), y=confirmed)) + # points de données réelles
    #   geom_line(aes(x=as.Date(date), y=pred), color="red") + # courbe ajustée
    #   scale_x_date(date_breaks="1 week", labels=scales::date_format("%d-%m-%Y")) +
    #   theme(axis.text.x = element_text(angle = 90)) +
    #   labs(title = paste("Evolution des cas officiels de COVID-19 en", i), x="date", y="nombre de cas confirmés")
    # ggsave(p, filename=paste("dyn_confirmed_",i,".png"),
    #        path="dynplots", width=8, height=5, units="in", dpi=72) # sauver le graphique
  }, error=function(e){cat("ERROR :",i, "\n")}) # afficher la source si erreur
}

# Tableau de résultats des ajustements

gompertz_param_table.confirmed <- results.confirmed %>%
  pivot_longer(-params, names_to = "Country", values_to = "value") %>%
  pivot_wider(names_from=params, values_from=value) %>%
  arrange(Country)

##### décès

# formule du modèle de gompertz
gomp.deaths.fun <- deaths ~ Asym*exp(-exp(-b*(as.numeric(date)-xmid)))

# créer un tableau vide dans le quel les paramètres des courbes ajustées seront écrits
results.deaths <- tibble(params=c("b_deaths","Asym_deaths","xmid_deaths", "P(b)_deaths", "P(Asym)_deaths", "P(xmid)_deaths", "converged_deaths", "nbiter_deaths"))

# boucle d'ajustements
for(i in unique(jhu.covid.3$Country)) {
#for(i in unique(filter(jhu.covid.3, Country %in% c(c("Benin", "Brunei Darussalam", "Curacao", "Cayman Islands", "French Guiana", "Montserrat", "British Virgin Islands", "Yemen, Rep.")))[[3]])) {
  ##### start=list(b=0.1, Asym=5e4, xmid=1.835e4)
  ##### start=list(b=3, Asym=2, xmid=1.83e4)
  tryCatch({ # fonction pour intercepter les erreurs et laisser la boucle de continuer
    dat <- filter(jhu.covid.3, Country == i) # filtrer par région/pays
    model1 <- nlsLM(gomp.deaths.fun, data=dat, start=list(b=0.1, Asym=5e4, xmid=1.835e4), control = nls.lm.control(maxiter=100))  # ajustement nonliéaire avec algorithme Levenberg-Marquardt
    res <- tibble(coef(model1))
    names(res) <- i
    res <- rbind(res, summary(model1)$coefficients[10], summary(model1)$coefficients[11], summary(model1)$coefficients[12], model1$convInfo$isConv, model1$convInfo$finIter
    )
    results.deaths <- cbind(results.deaths, res) # ajouter les paramètres au tableau
    # newdata = expand.grid(date=as.Date(dat$date)) # créer un jeu de dates servant de valeurs explicatives au modèle pour générer les courbes
    # pred <- predict(model1, newdata=newdata) # générer les valeurs prédites par le modèle
    # p <- ggplot(dat) + # générer le graphique confirmed = f(date) et le stocker en mémoire
    #   geom_point(aes(x=as.Date(date), y=deaths)) + # points de données réelles
    #   geom_line(aes(x=as.Date(date), y=pred), color="red") + # courbe ajustée
    #   scale_x_date(date_breaks="1 week", labels=scales::date_format("%d-%m-%Y")) +
    #   theme(axis.text.x = element_text(angle = 90)) +
    #   labs(title = paste("Evolution des décès dus COVID-19 en", i), x="date", y="nombre de décès")
    # ggsave(p, filename=paste("dyn_deaths",i,".png"),
    #        path="dynplots", width=8, height=5, units="in", dpi=72) # sauver le graphique
  }, error=function(e){cat("ERROR :",i, "\n")}) # afficher la source si erreur
}

# Tableau de résultats des ajustements

gompertz_param_table.deaths <- results.deaths %>%
  pivot_longer(-params, names_to = "Country", values_to = "value") %>%
  pivot_wider(names_from=params, values_from=value) %>%
  arrange(Country)

##### guéris

# formule du modèle de gompertz
gomp.recovered.fun <- recovered ~ Asym*exp(-exp(-b*(as.numeric(date)-xmid)))

# créer un tableau vide dans le quel les paramètres des courbes ajustées seront écrits
results.recovered <- tibble(params=c("b_recovered","Asym_recovered","xmid_recovered", "P(b)_recovered", "P(Asym)_recovered", "P(xmid)_recovered", "converged_recovered", "nbiter_recovered"))

# boucle d'ajustements
for(i in unique(jhu.covid.3$Country)) {
#for(i in unique(filter(jhu.covid.3, Country %in% c("Western Sahara"))[[3]])) {
  ### start=list(b=0.1, Asym=1e5, xmid=1.83e4))
  ### start=list(b=0.1, Asym=5, xmid=1.83e4))
  tryCatch({ # fonction pour intercepter les erreurs et laisser la boucle de continuer
    dat <- filter(jhu.covid.3, Country == i) # filtrer par région/pays
    model1 <- nlsLM(gomp.recovered.fun, data=dat, start=list(b=0.1, Asym=1e5, xmid=1.83e4), control = nls.lm.control(maxiter=100))  # ajustement nonliéaire avec algorithme Levenberg-Marquardt
    res <- tibble(coef(model1))
    names(res) <- i
    res <- rbind(res, summary(model1)$coefficients[10], summary(model1)$coefficients[11], summary(model1)$coefficients[12], model1$convInfo$isConv, model1$convInfo$finIter
    )
    results.recovered <- cbind(results.recovered, res) # ajouter les paramètres au tableau
    # newdata = expand.grid(date=as.Date(dat$date)) # créer un jeu de dates servant de valeurs explicatives au modèle pour générer les courbes
    # pred <- predict(model1, newdata=newdata) # générer les valeurs prédites par le modèle
    # p <- ggplot(dat) + # générer le graphique confirmed = f(date) et le stocker en mémoire
    #   geom_point(aes(x=as.Date(date), y=recovered)) + # points de données réelles
    #   geom_line(aes(x=as.Date(date), y=pred), color="red") + # courbe ajustée
    #   scale_x_date(date_breaks="1 week", labels=scales::date_format("%d-%m-%Y")) +
    #   theme(axis.text.x = element_text(angle = 90)) +
    #   labs(title = paste("Evolution des guérisons du COVID-19 en", i), x="date", y="nombre de guéris")
    # ggsave(p, filename=paste("dyn_recovered",i,".png"),
    #        path="dynplots", width=8, height=5, units="in", dpi=72) # sauver le graphique
  }, error=function(e){cat("ERROR :",i, "\n")}) # afficher la source si erreur
}

# Tableau de résultats des ajustements

gompertz_param_table.recovered <- results.recovered %>%
  pivot_longer(-params, names_to = "Country", values_to = "value") %>%
  pivot_wider(names_from=params, values_from=value) %>%
  arrange(Country)

####
gompertz_param_table_all <- cbind(gompertz_param_table.confirmed, select(gompertz_param_table.deaths, -Country), select(gompertz_param_table.recovered, -Country)) %>%
  arrange(Country)

covid_result_table <- bind_cols(arrange(dat.last, Country), select(gompertz_param_table_all, -Country))

#write_excel_csv(gompertz_param_table_all, "gompertz_param_table_03mai2020.csv")


### Liste des pays
World2 <- dplyr::select(as.data.frame(World), -geometry) # extract countries without sf geometry
World2 <- cbind(World2, st_coordinates(st_point_on_surface(World))) # extract x y coordinates and append to countries
colnames(World2)[10] <- "Long"
colnames(World2)[11] <- "Lat"

# Ajouter les codes iso_a3 manquants pour Norvège, Kosovo et France
World2[23,1] <- "FRA"
World2[23,2] <- "FR"
World2[54,1] <- "NOR"
World2[54,2] <- "NO"
World2[66,1] <- "XKX"
World2[66,2] <- "XK"

# retirer les petits territoires sans code iso et terres australes et antarctiques françaises
# et classer par code pays ISO_A3 croissant
World2 <- World2 %>% filter(!is.na(ISO_A3), ISO_A3 != "ATF", GEOUNIT != "Aland", GEOUNIT != "American Samoa", GEOUNIT != "Cook Islands", GEOUNIT != "Comoros", GEOUNIT != "Federated States of Micronesia", GEOUNIT != "Guam", GEOUNIT != "Guernsey", GEOUNIT != "Hong Kong S.A.R.", GEOUNIT != "Heard Island and McDonald Islands", GEOUNIT != "British Indian Ocean Territory", GEOUNIT != "Jersey", GEOUNIT != "Kiribati", GEOUNIT != "Lesotho", GEOUNIT != "Macao S.A.R", GEOUNIT != "Marshall Islands", GEOUNIT != "Northern Mariana Islands", GEOUNIT != "Norfolk Island", GEOUNIT != "Niue", GEOUNIT != "Nauru", GEOUNIT != "Pitcairn Islands", GEOUNIT != "Palau", GEOUNIT != "Puerto Rico", GEOUNIT != "North Korea", GEOUNIT != "South Georgia and the Islands", GEOUNIT != "Saint Helena", GEOUNIT != "Solomon Islands", GEOUNIT != "Tajikistan", GEOUNIT != "Turkmenistan", GEOUNIT != "Tuvalu", GEOUNIT != "United States Minor Outlying Islands", GEOUNIT != "United States Virgin Islands", GEOUNIT != "Vanuatu", GEOUNIT != "Wallis and Futuna", GEOUNIT != "Samoa", GEOUNIT != "Tonga")

World2 <- World2 %>% add_row(ISO_A3=c("CHI", "GLP", "GUF", "MTQ", "MYT", "REU"), GEOUNIT=c("Channel Islands","Guadeloupe", "French Guiana", "Martinique", "Mayotte", "Reunion"), Long=c(-2.12238430068449, -61.565542,-53.09714,-60.997992,45.153713,55.550615), Lat=c(49.218329169, 16.253345,4.097024,14.652649,-12.835302,-21.140234))

World2 <- World2 %>% arrange(ISO_A3)


### Température moyenne mensuelle

#GDALinfo("data/2020_03_data.tiff")

raster201912 <- readGDAL("data/temp_2019_12_data.tiff")
raster202001 <- readGDAL("data/temp_2020_01_data.tiff")
raster202002 <- readGDAL("data/temp_2020_02_data.tiff")
raster202003 <- readGDAL("data/temp_2020_03_data.tiff")

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

coords <- SpatialPoints(coords = data.frame(Long=World2$Long, Lat=World2$Lat),
                        proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


temperatures_mensuelles <- tibble(temp_dec19 = raster201912[cellFromXY(raster201912, xy = coords)],
                                  temp_jan20 = raster202001[cellFromXY(raster202001, xy = coords)],
                                  temp_fev20 = raster202002[cellFromXY(raster202002, xy = coords)],
                                  temp_mar20 = raster202003[cellFromXY(raster202003, xy = coords)]
                                  )

temperatures_mensuelles <- mutate(temperatures_mensuelles, temp_mean=rowMeans(temperatures_mensuelles))

World2 <- bind_cols(World2, temperatures_mensuelles)

# identifier les pays avec températures manquantes
World2[which(is.na(World2$temp_mean)), 3]

# Chercher les températures manquantes pour 
library(riem)
riem_net <- riem_networks()
riem_net_stations <- riem_networks() %>% filter(str_detect(name, "Bermuda|Polynesia"))

Berm <- riem_stations(riem_net_stations[1,1])[[1]][1]
Berm_temp_dec19 <- riem_measures(Berm, date_start = "2019-12-01", date_end="2019-12-31")
Berm_temp_jan20 <- riem_measures(Berm, date_start = "2020-01-01", date_end="2020-01-31")
Berm_temp_fev20 <- riem_measures(Berm, date_start = "2020-02-01", date_end="2020-02-29")
Berm_temp_mar20 <- riem_measures(Berm, date_start = "2020-03-01", date_end="2020-03-31")

Poly <- riem_stations(riem_net_stations[2,1])[[1]][1]
Poly_temp_dec19 <- riem_measures(Poly, date_start = "2019-12-01", date_end="2019-12-31")
Poly_temp_jan20 <- riem_measures(Poly, date_start = "2020-01-01", date_end="2020-01-31")
Poly_temp_fev20 <- riem_measures(Poly, date_start = "2020-02-01", date_end="2020-02-29")
Poly_temp_mar20 <- riem_measures(Poly, date_start = "2020-03-01", date_end="2020-03-31")

# fonction pour convertir les températures Fahrenheit vers Celsius
F_to_C <- function(x){(x-32)/1.8}

World2[26,"temp_dec19"] <- mean(F_to_C(Berm_temp_dec19$tmpf), na.rm=T)
World2[26,"temp_jan20"] <- mean(F_to_C(Berm_temp_jan20$tmpf), na.rm=T)
World2[26,"temp_fev20"] <- mean(F_to_C(Berm_temp_fev20$tmpf), na.rm=T)
World2[26,"temp_mar20"] <- mean(F_to_C(Berm_temp_mar20$tmpf), na.rm=T)
World2[26,"temp_mean"] <- mean(World2[[26,"temp_dec19"]], World2[[26,"temp_jan20"]], World2[[26,"temp_fev20"]], World2[[26,"temp_mar20"]])

World2[158,"temp_dec19"] <- mean(F_to_C(Poly_temp_dec19$tmpf), na.rm=T)
World2[158,"temp_jan20"] <- mean(F_to_C(Poly_temp_jan20$tmpf), na.rm=T)
World2[158,"temp_fev20"] <- mean(F_to_C(Poly_temp_fev20$tmpf), na.rm=T)
World2[158,"temp_mar20"] <- mean(F_to_C(Poly_temp_mar20$tmpf), na.rm=T)
World2[158,"temp_mean"] <- mean(World2[[158,"temp_dec19"]], World2[[158,"temp_jan20"]], World2[[158,"temp_fev20"]], World2[[158,"temp_mar20"]])

###
covid_data <- bind_cols(arrange(World2, ISO_A3), arrange(covid_uid, iso3)[12], arrange(covid_result_table, ISO_A3)[c(3,6:32)]) %>%
  select(-GEOUNIT) %>%
  relocate(Country, .after=ISO_A2)

covid_data$condirmed_p_million <- covid_data$confirmed_30avr / covid_data$Population * 1e6
covid_data$deaths_p_million <- covid_data$deaths_30avr / covid_data$Population * 1e6
covid_data$recovered_p_million <- covid_data$recovered_30avr / covid_data$Population * 1e6

covid_data <- covid_data %>% relocate(condirmed_p_million, .after=recovered_30avr) %>%
  relocate(deaths_p_million, .after=condirmed_p_million) %>%
  relocate(recovered_p_million, .after=deaths_p_million)

#write_excel_csv(World2, "natural_earth_data.csv")


#########################################
### inclure les données sur les tests ###
#########################################


download.file("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv", destfile = "data/covid-testing-all-observations_03mai2020.csv")

covid_testing <- read_csv("data/covid-testing-all-observations_03mai2020.csv")
covid_testing <- separate(covid_testing, col=Entity, into=c("Country", "type_test"), sep=" - ") %>%
  select(Country, "Cumulative total", type_test, Date) %>%
  rename(cumul_tests="Cumulative total")

last_tests <- covid_testing %>%
  group_by(Country) %>%
  arrange(Date) %>%
  filter(row_number()==n()) %>%
  arrange(Country) %>%
  filter(Country != "Hong Kong")

last_tests$Country[16] <- "Czechia"
last_tests$Country[31] <- "Iran, Islamic Rep"
last_tests$Country[59] <- "Russian Federation"
last_tests$Country[64] <- "Slovak Republic"
last_tests$Country[67] <- "Korea, Rep."


first_cases <- jhu.covid.3 %>%
  group_by(Country) %>%
  arrange(date) %>%
  filter(confirmed > 0 ) %>%
  filter(row_number() == 1) %>%
  arrange(Country) %>%
  select(Country, date) %>%
  rename(start_epid=date)

last_cases <- jhu.covid.3 %>%
  group_by(Country) %>%
  arrange(date) %>%
  filter(confirmed > 0 ) %>%
  filter(row_number() == n()) %>%
  arrange(Country) %>%
  select(Country, date) %>%
  rename(last_epid=date)

testing <- bind_cols(first_cases, last_cases[2]) %>%
  left_join(last_tests) %>%
  mutate(epid_duration = last_epid-start_epid) %>%
  relocate(epid_duration, .after=last_epid) %>%
  rename(date_testing = Date)


########

covid_data_all <- cbind(arrange(covid_data, Country), testing[c(2,4:7)])
covid_data_all$cumul_tests_p_million <- covid_data_all$cumul_tests / covid_data_all$Population * 1e6
covid_data_all <- covid_data_all %>% relocate(cumul_tests_p_million, .after=cumul_tests) %>%
  arrange(ISO_A3)

write_excel_csv(covid_data_all, "covid_data_all.csv")
