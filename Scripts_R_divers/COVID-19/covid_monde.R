library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggthemes)
library(gganimate)

#library(rnaturalearth)
#library(rnaturalearthdata)

theme_set(theme_bw())
setwd("~/TRAVAIL/github/guyader-lab-inra/Scripts_R_divers/COVID-19")

# download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", destfile = "covid19_confirmed.csv")
# download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", destfile = "covid19_deaths.csv")
# download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", destfile = "covid19_recovered.csv")
# 
# covid_confirmed <- read_csv("covid19_confirmed.csv")
# covid_deaths <- read_csv("covid19_deaths.csv")
# covid_recovered <- read_csv("covid19_recovered.csv")
# 
# covid_confirmed_long <- gather(covid_confirmed, 5:ncol(covid_confirmed), key=date, value=confirmed)
# covid_deaths_long <- gather(covid_deaths, 5:ncol(covid_deaths), key=date, value=deaths)
# covid_recovered_long <- gather(covid_recovered, 5:ncol(covid_recovered), key=date, value=recovered)
# 
# covid_join <- full_join(covid_confirmed_long, covid_deaths_long)
# covid_join2 <- full_join(covid_join, covid_recovered_long)

# autres sources, toutes les 2 de l'ECDC :
open.covid <- read_csv("https://open-covid-19.github.io/data/data.csv", col_types = cols(RegionCode = col_character(), RegionName = col_character()))
#  ecdc_covid <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

last <- open.covid %>% filter(Date=="2020-04-08" & !is.na(Latitude)) %>% st_as_sf(coords=c("Longitude","Latitude"), crs=4326) %>% fortify()
last_countries <- filter(last, is.na(RegionCode))
last_regions <- filter(last, !is.na(RegionCode))

#WorldData <- ne_download( scale = 50, type = 'countries', returnclass="sf")
#st_is_valid(world)

#world_countries <- st_coordinates(st_centroid(world))

#world <- cbind(world, world_countries)

WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify()

ggplot(filter(last, Date=="2020-04-08")) +
  geom_map(data=WorldData, map = WorldData, aes(map_id=region, fill=region), color="white", size=0.2) +
  geom_sf(data=last, aes(size=Confirmed)) +
  scale_colour_gradient(low = "#3333CC", high = "red") +
  scale_fill_discrete(guide=NULL) +
  scale_size(range=c(1,15)) +
  coord_sf(xlim=c(-180,180), ylim=c(-60,90), expand=F) +
  #coord_map("mercator", xlim=c(-160,180), ylim=c(-50,120)) +
  theme_bw()


#####
WorldData <- ne_countries(scale = "medium", type='countries', returnclass="sf") %>% filter(admin != "Antarctica") %>% fortify()

last <- open.covid %>% filter(Date=="2020-04-08" & !is.na(Latitude)) %>% st_as_sf(coords=c("Longitude","Latitude"), crs=4326) %>% fortify()
last_countries <- filter(last, is.na(RegionCode))
last_regions <- filter(last, !is.na(RegionCode))


ggplot(WorldData) +
  geom_sf(aes(fill=admin), color="white", size=0.2, alpha=0.3) +
  scale_fill_discrete(guide=NULL) +
  geom_sf(data=last, aes(size=Confirmed, colour=Deaths)) +
  scale_colour_gradient(low="#3333CC", high="red") +
  scale_size(range=c(1,15)) +
  coord_sf(xlim=c(-180,180), ylim=c(-60,90), expand=F) +
  theme_bw()

# ####
# jhu.covid <- covid_join2 %>% st_as_sf(coords=c("Long","Lat"), crs=4326) %>% fortify()
# 
# WorldData <- ne_countries(scale = "medium", type='countries', returnclass="sf") %>% filter(admin != "Antarctica") %>% fortify()
# 
# ggplot(WorldData) +
#   geom_sf(aes(fill=admin), color="white", size=0.2, alpha=0.3) +
#   scale_fill_discrete(guide=NULL) +
#   geom_sf(data=filter(jhu.covid, date=="4/8/20"), aes(size=confirmed, colour=deaths)) +
#   scale_colour_gradient(low="#3333CC", high="red") +
#   scale_size(range=c(1,15)) +
#   coord_sf(xlim=c(-180,180), ylim=c(-60,90), expand=F) +
#   theme_bw()

####
# j=1
# for (i in unique(jhu.covid$date))
#   {
#   j=j 
#   data.i <- jhu.covid %>% filter(date==i)
#    ggplot(WorldData) +
#      geom_sf(aes(fill=admin), color="white", size=0.2, alpha=0.3) +
#      scale_fill_discrete(guide=NULL) +
#      geom_sf(data=data.i, aes(size=confirmed, colour=deaths)) +
#      scale_colour_gradient(low="#3333CC", high="red") +
#      scale_size(range=c(1,15)) +
#      coord_sf(xlim=c(-180,180), ylim=c(-60,90), expand=F) +
#      labs(title = paste("Date: ", i)) +
#      theme_bw()
#    ggsave(paste("data.", j,".png", sep=""))
#    j=j+1
#    }

