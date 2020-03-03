library(tidyverse)
library(osmdata)
library(sf)
library(raster)
library(lwgeom)
library(tmap)
library(tmaptools)
library(RColorBrewer)

## définir la zone d'intérêt (bounding box)
bbox <- getbb("guadeloupe")

## création de la requête overpass sur le serveur de cartes OpenStreetMap
gpe_query <- opq(bbox) %>% add_osm_feature(key="boundary", value="administrative") %>%
  add_osm_feature(key="admin_level", value="8") # admin_level=8 donne le découpage communal français

## envoi de la requête, et enregistrement des données au format "osmdata" (liste d'objets "sf")
gpe_osmdata <- osmdata_sf(gpe_query) # (peut nécessiter plusieurs essais si le serveur ne répond pas)
gpe_osmdata # l'objet "osm_multipolygons" contient les polygones (territoires) des 32 communes
            # l'objet "osm_points" contient entre autres les localisations des bourgs des communes

## sélection des variables d'intérêt
## et extraction des territoires communeaux au format "sf"
names(gpe_osmdata$osm_multipolygons)
gpe_sf <- dplyr::select(gpe_osmdata$osm_multipolygons, osm_id, name, addr.postcode, population) %>% arrange(name)

## vérification de la validité de l'objet "gpe_sf"
st_is_valid(gpe_sf, reason = T)

## rendre toutes les géométries valides
gpe_sf <- st_make_valid(gpe_sf)

## convertir la variable "population" en numérique
is.numeric(gpe_sf$population)
gpe_sf$population <- as.numeric(gpe_sf$population)

## calcul des surfaces des communes en km2
gpe_sf$superficie <- as.numeric(units::set_units(st_area(gpe_sf), km^2))

## calcul de la densité de population par km2
gpe_sf$population_density <- gpe_sf$population/gpe_sf$superficie

## retirer les dépendances (Désirade, Marie-Galante, Les Saintes) qui sont à la fin du tableau
# gpe_sf_continent <- dplyr::slice(gpe_sf, 1:26)

## extraction des localisations des bourgs en se basant sur la variable "name.fr" 
## (la variable "name" contient d'autres points d'intérêt)
gpe_sf_communes <- dplyr::filter(gpe_osmdata$osm_points, !is.na(name.fr)) %>% dplyr::select(osm_id, name.fr, addr.postcode) %>% arrange(name.fr)
gpe_sf_communes$population <- gpe_sf$population
gpe_sf_communes$population_density <- gpe_sf$population_density


## téléchargement des données d'élévation SRTM 4 au format raster GeoTIFF (http://srtm.csi.cgiar.org)
srtm_url = "http://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/srtm_24_09.zip"
srtm_file = "srtm_24_09.zip"
if (!file.exists("./srtm/srtm_24_09.tif")) {
  download.file(srtm_url, srtm_file, method="auto")
  unzip(srtm_file, files="srtm_24_09.tif", exdir="./srtm")
  file.remove("srtm_24_09.zip")
}

## chargement du GeoTIFF (tuile de 5°x5° contenant la Guadeloupe et certaines îles au nord)
gpe_elev_raster <- raster("./srtm/srtm_24_09.tif")

## infos sur la couche raster chargée
gdalUtils::gdalinfo("./srtm/srtm_24_09.tif")
crs(gpe_elev_raster)
nlayers(gpe_elev_raster)

## conserver uniquement la zone d'intérêt (Guadeloupe)
gpe_elev_raster <- crop(gpe_elev_raster, extent(bbox))

## calcul des lignes de niveau d'altitude, tous les 100m de 100m à l'altitude max
gpe_elev_contour <- rasterToContour(gpe_elev_raster, maxpixels=length(gpe_elev_raster),
                                    levels = seq(100, gpe_elev_raster@data@max, 100))

## graphiques rapides
plot(gpe_elev_raster)
plot(gpe_elev_contour, col="gray50", add=T)

####################################
## Cartes thématiques avec "tmap" ##
####################################

# Carte avec territoires des communes, choropleth et isopleth

## estimation de densité par kernel 2D
require(SpatialPosition)
k2de <-  quickStewart(x = gpe_sf_communes, var="population",
             typefct = "exponential", span=5000, beta=2, nclass=12,
             mask=gpe_sf, returnclass = "sf")

## carte + surcouches 
tm_shape(gpe_sf) + tm_borders() +                                               # carte communes
  tm_shape(gpe_sf) + tm_polygons(col="population", border.col=NA, alpha=0.5) +  # choropleth
  tm_shape(k2de) + tm_polygons(col="center", n=nrow(k2de), alpha=0.5,
                             palette="YlOrRd", border.alpha=0.5) +              # isopleth
  tm_style("natural") +
  tm_grid(alpha=0.3) +
  tm_legend(show=F)


## carte avec fond OSM/Stamen

## tmap_mode("view") : pour afficher des tuiles de fond de carte en mode interactif
## tm_basemap(leaflet::providers$Stamen.TerrainBackground) +

# créer un fond de carte à partir de tuiles téléchargées
# par exemple charger les tuiles stamen terrain brutes avec url custom :
# sinon remplacer type="osm" par exemple
base_map <- read_osm(bbox, type="http://tile.stamen.com/terrain-background/{z}/{x}/{y}.jpg", zoom=11)
base_map2 <- read_osm(getbb("france"), type="hillshade", zoom=4)

tm_shape(base_map, projection=4326) + tm_rgb() +
  tm_shape(gpe_elev_raster) + tm_raster(palette=terrain.colors(3), alpha=0.5, style="cont") + # fond élévation
  tm_shape(gpe_elev_contour) + tm_iso(col="black", alpha=0.3) +             # ajout des courbes de niveau
  tm_shape(gpe_sf) + tm_borders(alpha=0.2) +                                # ajout des limites communales
  tm_shape(gpe_sf_communes) + tm_dots(size=0.2, col="black") +              # ajout des centres des communes
  tm_text(text="name.fr", shadow=T, size=0.6, xmod=0.5, ymod=0.5) +         # ajout des noms des communes
  tm_graticules(alpha=0.3) +                                                # ajout de la grille lat/lat (ou "tm_grid')
  tm_style("natural") +                                                     # style de base avec mer en bleu
  tm_legend(show=F)                                                         # retirer la légende du fond d'élévation

