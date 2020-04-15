library(tidyverse)
library(osmdata)
library(sf)
library(raster)
library(lwgeom)
library(tmap)
library(tmaptools)
library(RColorBrewer)

setwd("~/TRAVAIL/github/guyader-lab-inra/Scripts_R_divers/Spatial")

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
str(gpe_osmdata$osm_multipolygons)
## et extraction des territoires communeaux au format "sf"
gpe_sf <- dplyr::select(gpe_osmdata$osm_multipolygons, osm_id, name, addr.postcode, population) %>% arrange(name)

## vérification de la validité de l'objet "gpe_sf"
st_is_valid(gpe_sf, reason = T)
## rendre toutes les géométries valides
gpe_sf <- st_make_valid(gpe_sf)

## convertir la variable "population" en numérique, car le type est "character"
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

## requête pour le contour du département
gpe_query2 <- opq(bbox) %>% add_osm_feature(key="boundary", value="administrative") %>%
  add_osm_feature(key="admin_level", value="6") # admin_level=8 donne le découpage du département
gpe_osmdata2 <- osmdata_sf(gpe_query2) # (peut nécessiter plusieurs essais si le serveur ne répond pas)
gpe_osmdata2 # l'objet "osm_multipolygons" contient le multipolygone du département (Gpe + dépendances)

## extraction et validation du contour
border <- dplyr::select(gpe_osmdata2$osm_multipolygons, osm_id, name)
border <- st_make_valid(border)

## téléchargement des données d'élévation SRTM 4 au format raster GeoTIFF (http://srtm.csi.cgiar.org)
srtm_url1 = "http://viewfinderpanoramas.org/dem3/D20.zip"
srtm_file1 = "D20.zip"

srtm_url2 = "http://viewfinderpanoramas.org/dem3/E20.zip"
srtm_file2 = "E20.zip"

if (!file.exists("./srtm/D20/N15W062.hgt")) {
  download.file(srtm_url1, srtm_file1, method="auto")
  unzip(srtm_file1, files="D20/N15W062.hgt", exdir="./srtm")
  file.remove("D20.zip")
}
if (!file.exists("./srtm/E20/N16W062.hgt")) {
  download.file(srtm_url2, srtm_file2, method="auto")
  unzip(srtm_file2, files="E20/N16W062.hgt", exdir="./srtm")
  file.remove("E20.zip")
}

## chargement du GeoTIFF (tuile de 5°x5° contenant la Guadeloupe et certaines îles au nord)
N15W062 <- raster("./srtm/D20/N15W062.hgt")
N16W062 <- raster("./srtm/E20/N16W062.hgt")
gpe_elev_raster <- raster::merge(N15W062, N16W062)

## infos sur la couche raster chargée
# gdalUtils::gdalinfo("./srtm/srtm_24_09.tif")
# nlayers(gpe_elev_raster)
crs(gpe_elev_raster)

## conserver uniquement la zone d'intérêt (Guadeloupe)
gpe_elev_raster <- crop(gpe_elev_raster, extent(bbox))

## identifier les valeurs NA et les convertir à 0
which(is.na(gpe_elev_raster@data@values))
gpe_elev_raster[is.na(gpe_elev_raster)] <- 0

## suréchantillonner le raster
# gpe_elev_raster2 <- raster::disaggregate(gpe_elev_raster, fact=3, method="bilinear")

## générer un fond "hillshade" à partir de la couche élévation
slope <- terrain(gpe_elev_raster, opt='slope')
aspect <- terrain(gpe_elev_raster, opt='aspect')
gpe_hillshade_raster <- hillShade(slope, aspect, 45, 135)

## masquer les valeurs en dehors des contours du département
gpe_elev_raster <- raster::mask(gpe_elev_raster, border)
gpe_hillshade_raster <- raster::mask(gpe_hillshade_raster, border)

## calcul des lignes de niveau d'altitude, tous les 100m de 100m à l'altitude max
iso20 <- seq(20,1500,20)
iso100 <- seq(100,1500,100)
iso20 <- iso20[! iso20 %in% iso100]

gpe_elev_contour_20 <- rasterToContour(gpe_elev_raster, maxpixels=length(gpe_elev_raster),
                                    levels = iso20)
gpe_elev_contour_100 <- rasterToContour(gpe_elev_raster, maxpixels=length(gpe_elev_raster),
                                       levels = iso100)

## graphiques rapides
plot(gpe_elev_raster)
plot(gpe_hillshade_raster)

####################################
## Cartes thématiques avec "tmap" ##
####################################

# Carte générale avec différentes couches
tmap_mode("plot")
tm_basemap(leaflet::providers$Hydda.Base) +
  tm_shape(border, xlim=c(-61.8,-61.7), ylim=c(16.0,16.1)) + tm_borders() + # xlim=c(-61.8,-61.7), ylim=c(16.0,16.1)
  tm_shape(gpe_elev_raster) + tm_raster(palette=terrain.colors(3), alpha=0.5, style="cont") +
  #tm_shape(gpe_hillshade_raster) + tm_raster(palette="Greys", alpha=0.5, style="cont") +
  tm_shape(gpe_elev_contour_20) + tm_iso(col="orange", alpha=0.5, lwd=0.6) +
  tm_shape(gpe_elev_contour_100) + tm_iso(along.lines=T, overwrite.lines=T, col="orange", alpha=0.5, lwd=1.5) +
  tm_style("natural") +
  tm_grid(alpha=0.3) +
  tm_legend(show=F)

# Carte avec territoires des communes, choropleth et isopleth

## estimation de densité par kernel 2D
require(SpatialPosition)
density <-  quickStewart(x = gpe_sf_communes, var="population",
             typefct = "exponential", span=5000, beta=2, nclass=12,
             mask=gpe_sf, returnclass = "sf")

## carte + surcouches 
tmap_mode("view")
tm_basemap(leaflet::providers$Hydda.Base) +
tm_shape(gpe_sf) + tm_borders() +                                               # carte communes
  tm_shape(gpe_sf) + tm_polygons(col="population", border.col=NA, alpha=0.5) +  # choropleth
  tm_shape(gpe_hillshade_raster) + tm_raster(palette="Greys", alpha=0.5, style="cont") +
  tm_shape(density) + tm_polygons(col="center", n=nrow(density), alpha=0.5,                             palette="YlOrRd", border.alpha=0.2, lty="dashed") +        # isopleth
  tm_shape(gpe_sf_communes) + tm_dots(size=0.2, col="black") + # ajout des centres des communes
  tm_text(text="name.fr", shadow=T, size=0.7, xmod=0.5, ymod=0.5) + # ajout des noms des communes
  tm_style("natural") +
  tm_grid(alpha=0.3) +
  tm_legend(show=F) +
  tm_view(set.bounds=T, dot.size.fixed=T)


## carte avec fond OSM/Stamen

## tmap_mode("view") : pour afficher des tuiles de fond de carte en mode interactif
## tm_basemap(leaflet::providers$Stamen.TerrainBackground) +

## créer un fond de carte à partir de tuiles téléchargées
## par exemple charger les tuiles stamen terrain brutes avec url custom :
## (sinon remplacer type="osm" par exemple)

# base_map <- read_osm(bbox, type="http://tile.stamen.com/terrain-background/{z}/{x}/{y}.jpg", zoom=11)

# tm_shape(base_map, projection=4326) + tm_rgb() +
tm_shape(gpe_hillshade_raster2) + tm_raster(palette="Greys", alpha=1, style="cont") +
  tm_shape(gpe_elev_raster2) + tm_raster(palette=terrain.colors(3), alpha=0.7, style="cont") + # fond élévation
  tm_shape(gpe_elev_contour) + tm_iso(col="black", alpha=0.2) +             # ajout des courbes de niveau
  tm_shape(gpe_sf) + tm_borders(alpha=0.2) +                                # ajout des limites communales
  tm_shape(gpe_sf_communes) + tm_dots(size=0.2, col="black") +              # ajout des centres des communes
  tm_text(text="name.fr", shadow=T, size=0.6, xmod=0.5, ymod=0.5) +         # ajout des noms des communes
  tm_graticules(alpha=0.3) +                                                # ajout de la grille lat/lat (ou "tm_grid')
  tm_style("natural") +                                                     # style de base avec mer en bleu
  tm_legend(show=F)                                                         # retirer la légende du fond d'élévation

