library(tidyverse)
library(sf)
library(osmdata)
library(ggspatial)
library(scatterpie)

library(lwgeom)
library(RColorBrewer)
library(patchwork)
library(ggrepel)
library(OpenStreetMap)

theme_set(theme_bw())

#base_map <- tmaptools::read_osm(bbox, type="http://tile.stamen.com/terrain-background/{z}/{x}/{y}.jpg", zoom=11)

## définir la zone d'intérêt (bounding box)
bbox <- getbb("guadeloupe")

bbox_GT <- getbb("grande-terre, guadeloupe")

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
gpe_sf <- dplyr::select(gpe_osmdata$osm_multipolygons, name) %>% arrange(name)

## vérification de la validité de l'objet "gpe_sf"
st_is_valid(gpe_sf, reason = T)
## rendre toutes les géométries valides
gpe_sf <- st_make_valid(gpe_sf)

## extraction des coordonnées des bourgs en se basant sur la variable "name.fr" 
## (la variable "name" contient d'autres points d'intérêt)
gpe_sf_communes <- dplyr::filter(gpe_osmdata$osm_points, !is.na(name.fr)) %>% dplyr::select(name.fr, addr.postcode) %>% arrange(name.fr)

## requête pour le contour du département
gpe_query2 <- opq(bbox) %>% add_osm_feature(key="boundary", value="administrative") %>%
  add_osm_feature(key="admin_level", value="6") # admin_level=8 donne le découpage du département
gpe_osmdata2 <- osmdata_sf(gpe_query2) # (peut nécessiter plusieurs essais si le serveur ne répond pas)
gpe_osmdata2 # l'objet "osm_multipolygons" contient le multipolygone du département (Gpe + dépendances)

## extraction et validation du contour
border <- dplyr::select(gpe_osmdata2$osm_multipolygons, name)
border <- st_make_valid(border)

## transformer l'objet gpe_sf_communes en "spatial dataframe" pour ggplot2
gpe_sf_communes_df <- df_spatial(gpe_sf_communes)

## préparer les données de prévalence
preval_carto <- read.delim("Donnees_pour_carto_prevalence_2019.csv")

p <- ggplot(gpe_sf) +
  layer_spatial(gpe_sf, alpha=0.25) + # dessine les limites des communes
  layer_spatial(border, alpha=0.25) + # contour de la Guadeloupe
  layer_spatial(gpe_sf_communes) +    # ajoute la localisation des bourgs
  #geom_spatial_text_repel(data=gpe_sf_communes_df, aes(x, y, label = name.fr), crs=4326, box.padding = 1) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", which_north = "true", style=north_arrow_fancy_orienteering(line_width = 0.5)) +
  #geom_scatterpie(data=preval_carto, aes(x=Longitude, y=Latitude, group=site), pie_scale = 2, cols=c("YMV","YMMV","YaV1","CoV1","DMaV","Macluravirus","Potexvirus","Betaflexiviridae"), color="black", alpha=.8, legend_name = "Virus") +
  theme_bw()


## tableau long
preval_carto_long <- preval_carto %>% pivot_longer(cols=6:13, names_to="virus", values_to="prevalence")

my_plot_fun <- function(data){
  ggplot(data, aes(1, prevalence, fill = virus)) + 
    geom_col(position = position_dodge(width = 1), 
             alpha = 1) + # , colour = "white"
    geom_text(aes(label = round(prevalence, 0), group = virus), 
              position = position_dodge(width = 1),
              size = 3) +
    scale_fill_hue(l=50) +
    ylim(0,100) +
    theme_void() + guides(fill = F) +
    theme(panel.background = element_rect("white", size=1))
}

annotation_fun <- function(data, Latitude, Longitude, plot_fun) {
  subplot = plot_fun(data)
  sub_grob <- annotation_custom(ggplotGrob(subplot), 
                                x = Longitude-0.03, y = Latitude-0.03, 
                                xmax = Longitude+0.03, ymax = Latitude+0.03)
}

subgrobs <- preval_carto_long %>%
  select(Latitude, Longitude, virus, prevalence) %>%
  nest(-Longitude,-Latitude)  %>%
  pmap(annotation_fun, plot_fun = my_plot_fun)

legend <- get_legend(ggplot(preval_carto_long, aes(1, prevalence, fill = virus)) + geom_col(position = position_dodge(width = 1), alpha = 0.75, colour = "white") + scale_fill_hue(l=50))


ps <- p + subgrobs

plot_grid(ps, legend, ncol = 2, rel_widths = c(1, .2))

############################################""
## Grande Terre

bbox_GT <- getbb("grande-terre, guadeloupe")

## création de la requête overpass sur le serveur de cartes OpenStreetMap
GT_query <- opq(bbox_GT) %>% add_osm_feature(key="boundary", value="administrative") %>%
  add_osm_feature(key="admin_level", value="8") # admin_level=8 donne le découpage communal français

## envoi de la requête, et enregistrement des données au format "osmdata" (liste d'objets "sf")
GT_osmdata <- osmdata_sf(GT_query) # (peut nécessiter plusieurs essais si le serveur ne répond pas)
GT_osmdata # l'objet "osm_multipolygons" contient les polygones (territoires) des 32 communes
# l'objet "osm_points" contient entre autres les localisations des bourgs des communes

## sélection des variables d'intérêt
str(GT_osmdata$osm_multipolygons)
## et extraction des territoires communeaux au format "sf"
GT_sf <- dplyr::select(GT_osmdata$osm_multipolygons, name) %>% arrange(name)

## vérification de la validité de l'objet "gpe_sf"
st_is_valid(GT_sf, reason = T)
## rendre toutes les géométries valides
GT_sf <- st_make_valid(GT_sf)

## extraction des coordonnées des bourgs en se basant sur la variable "name.fr" 
## (la variable "name" contient d'autres points d'intérêt)
GT_sf_communes <- dplyr::filter(GT_osmdata$osm_points, !is.na(name.fr)) %>% dplyr::select(name.fr, addr.postcode) %>% arrange(name.fr)

## requête pour le contour du département
GT_query2 <- opq(bbox_GT) %>% add_osm_feature(key="boundary", value="administrative") %>%
  add_osm_feature(key="admin_level", value="6") # admin_level=8 donne le découpage du département
GT_osmdata2 <- osmdata_sf(GT_query2) # (peut nécessiter plusieurs essais si le serveur ne répond pas)
GT_osmdata2 # l'objet "osm_multipolygons" contient le multipolygone du département (Gpe + dépendances)

## extraction et validation du contour
GT_border <- dplyr::select(GT_osmdata2$osm_multipolygons, name)
GT_border <- st_make_valid(GT_border)

## transformer l'objet gpe_sf_communes en "spatial dataframe" pour ggplot2
GT_sf_communes_df <- df_spatial(GT_sf_communes)

## préparer les données de prévalence
preval_carto <- read.delim("Donnees_pour_carto_prevalence_2019.csv")

GT_p <- ggplot(GT_sf) +
  layer_spatial(GT_sf, alpha=0.25) + # dessine les limites des communes
  #layer_spatial(GT_border, alpha=0.25) + # contour de la Guadeloupe
  layer_spatial(GT_sf_communes) +    # ajoute la localisation des bourgs
  #geom_spatial_text_repel(data=gpe_sf_communes_df, aes(x, y, label = name.fr), crs=4326, box.padding = 1) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", which_north = "true", style=north_arrow_fancy_orienteering(line_width = 0.5)) +
  #geom_scatterpie(data=preval_carto, aes(x=Longitude, y=Latitude, group=site), pie_scale = 2, cols=c("YMV","YMMV","YaV1","CoV1","DMaV","Macluravirus","Potexvirus","Betaflexiviridae"), color="black", alpha=.8, legend_name = "Virus") +
  theme_bw()


## tableau long
preval_carto_long <- preval_carto %>% pivot_longer(cols=6:13, names_to="virus", values_to="prevalence")

my_plot_fun <- function(data){
  ggplot(data, aes(1, prevalence, fill = virus)) + 
    geom_col(position = position_dodge(width = 1), 
             alpha = 1) + # , colour = "white"
    geom_text(aes(label = round(prevalence, 0), group = virus), 
              position = position_dodge(width = 1),
              size = 3) +
    scale_fill_hue(l=50) +
    ylim(0,100) +
    theme_void() + guides(fill = F) +
    theme(panel.background = element_rect("white", size=1))
}

annotation_fun <- function(data, Latitude, Longitude, plot_fun) {
  subplot = plot_fun(data)
  sub_grob <- annotation_custom(ggplotGrob(subplot), 
                                x = Longitude-0.03, y = Latitude-0.03, 
                                xmax = Longitude+0.03, ymax = Latitude+0.03)
}

subgrobs <- preval_carto_long %>%
  select(Latitude, Longitude, virus, prevalence) %>%
  nest(-Longitude,-Latitude)  %>%
  pmap(annotation_fun, plot_fun = my_plot_fun)

legend <- get_legend(ggplot(preval_carto_long, aes(1, prevalence, fill = virus)) + geom_col(position = position_dodge(width = 1), alpha = 0.75, colour = "white") + scale_fill_hue(l=50))


ps <- GT_p + subgrobs

plot_grid(ps, legend, ncol = 2, rel_widths = c(1, .2))
