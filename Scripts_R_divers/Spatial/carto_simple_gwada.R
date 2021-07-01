library(tidyverse)
library(sf)
library(osmdata)
library(ggspatial)


## définir la zone d'intérêt (bounding box)
bbox <- getbb("guadeloupe")

## création de la requête overpass sur le serveur de cartes OpenStreetMap
gpe_query <- opq(bbox) %>% add_osm_feature(key="boundary", value="administrative") %>%
  add_osm_feature(key="admin_level", value="8") # admin_level=8 donne le découpage communal français

## envoi de la requête, et enregistrement des données au format "osmdata" (liste d'objets "sf")
gpe_osmdata <- osmdata_sf(gpe_query) # (peut nécessiter plusieurs essais si le serveur ne répond pas)
gpe_osmdata # l'objet "osm_multipolygons" contient les polygones (territoires) des 32 communes
# l'objet "osm_points" contient entre autres les localisations des bourgs des communes

## extraction des territoires communeaux au format "sf"
gpe_sf <- dplyr::select(gpe_osmdata$osm_multipolygons, name) %>% arrange(name)

## rendre toutes les géométries valides
gpe_sf <- st_make_valid(gpe_sf)

## requête pour le contour du département
gpe_query2 <- opq(bbox) %>% add_osm_feature(key="boundary", value="administrative") %>%
  add_osm_feature(key="admin_level", value="6") # admin_level=8 donne le découpage du département
gpe_osmdata2 <- osmdata_sf(gpe_query2) # (peut nécessiter plusieurs essais si le serveur ne répond pas)
gpe_osmdata2 # l'objet "osm_multipolygons" contient le multipolygone du département (Gpe + dépendances)

## extraction et validation du contour
border <- dplyr::select(gpe_osmdata2$osm_multipolygons, name)
border <- st_make_valid(border)

ggplot(gpe_sf) +
  layer_spatial(gpe_sf, alpha=0.25) + # dessine les limites des communes
  layer_spatial(border, alpha=0.25) + # ajoute le contour de la Guadeloupe + dépendances
  annotation_scale(location = "br") + # ajoute l'échelle de distances
  annotation_north_arrow(location = "tr", which_north = "true", style=north_arrow_fancy_orienteering(line_width = 0.5)) + # ajoute la direction Nord
  theme_bw()