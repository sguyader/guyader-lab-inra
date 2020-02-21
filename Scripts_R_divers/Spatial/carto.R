library(tidyverse)

# get bounding box
library(osmdata)
bbox <- getbb("guadeloupe")

# Avec "osmdata"
## création de la requête overpass sur le serveur de cartes OpenStreetMap
gpe_opq <- opq(bbox) %>% add_osm_feature(key="boundary", value="administrative") %>%
  #add_osm_feature(key="place", value="archipelago") %>%
  add_osm_feature(key="admin_level", value="8")

## envoi de la requête et enregistrement des données comme liste d'objets "sf"
gpe_osmdata <- osmdata_sf(gpe_opq) # (peut nécessiter plusieurs essais si le serveur ne répond pas)
gpe_osmdata
## l'objet "osm_multipolygons" contient les données des 32 communes
names(gpe_osmdata$osm_multipolygons)

## selection des variables d'intérêt
## et extraction des données communales au format "sf"
gpe_sf <- dplyr::select(gpe_osmdata$osm_multipolygons, osm_id, name, addr.postcode, population)
##convertir la variable "population" en numérique
gpe_sf$population <- as.numeric(gpe_sf$population)
## retirer les dépendances (Désirade, Mari Galante, Les Saintes) qui sont à la fin du tableau
gpe_sf_continent <- dplyr::slice(gpe_sf, 1:26)
## localisation des bourgs
communes_sf <- dplyr::filter(gpe_osmdata$osm_points, !is.na(name.fr)) %>% dplyr::select(osm_id, name.fr, addr.postcode)

# carte avec "sf" et "cartography"
library(sf)
library(cartography)

# charger les tuiles OSM
tilesLayer(getTiles(gpe_sf, type="osm", zoom=10, crop=T))

# superposer les contours ("géométrie")
plot(st_geometry(gpe_sf)) # ajouter "add=T pour superposer aux tuiles OSM

# choropleth
choroLayer(gpe_sf, var="population",
           method = "quantile", nclass=5, col = carto.pal(pal1 = 'blue.pal', n1 = 5), add=T)

# calcul des surfaces des communes en km2
gpe_sf$superficie <- units::set_units(st_area(gpe_sf), km^2)
# calcul de la densité de poppulation par km2
gpe_sf$pop.density <- gpe_sf$population/gpe_sf$superficie

choroLayer(gpe_sf, var="superficie",
           method = "quantile", nclass=5, col = carto.pal(pal1 = 'blue.pal', n1 = 5), add=F)

# isopleth
library(SpatialPosition)
smoothLayer(
  x = gpe_sf_sel, 
  var = 'population',
  typefct = "exponential",
  span = 5000,
  beta = 2,
  nclass = 12,
  col = carto.pal(pal1 = 'brown.pal', n1 = 12),
  border = "red",
  lwd = 0.1, 
  mask = gpe_sf$osm_multipolygons, 
  legend.values.rnd = -3,
  legend.title.txt = "Population\nPotential",
  legend.pos = "topright", 
  add=F
)
plot(st_geometry(gpe_sf$osm_multipolygons), col = NA, border = "white", bg = "lightblue1", add=T)

## ajouter la localisation des bourgs des communes
plot(st_geometry(communes_sel), pch=16, add=T)

labelLayer(gpe_sf_sel, txt="name", halo=T)

# ou plus simplement :
plot(gpe_sf_sel["population"], breaks="quantile")
# avec ggplot :
ggplot(gpe_sf_sel) +
  geom_sf(aes(fill=population)) +
  geom_sf(data=communes_sel, col="red") +
  labs(x="lon",y="lat") +
  theme_bw()

#####################################################################

## carte avec "ggmap"
library(ggmap)

base_map <- get_stamenmap(bbox, maptype="toner-lite", zoom = 11)

ggmap(base_map) +
  geom_sf(data=gpe_sf$osm_multipolygons,
          inherit.aes =FALSE,
          colour="black",
          alpha=.5,
          size=0.5)

#####################################################################

# obtention de données carto avec osmplotr
library(osmplotr)

## envoi de la requête : "admin_level=8" permet d'extraire les données au niveau de la commune
## les données sont enregistrées au format "sf"
bound <- extract_osm_objects(key="boundary", value="administrative",
            extra_pairs=c("admin_level", "8"), return_type="line", bbox = bbox)

ggmap(base_map) +
  geom_sf(data=bound,
          inherit.aes =FALSE,
          colour="black",
          alpha=.5,
          size=0.5)+
  labs(x="",y="")
