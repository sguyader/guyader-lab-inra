library(ggspatial)
library(patchwork)
library(ggrepel)

theme_set(theme_bw())

base_map <- read_osm(bbox, type="http://tile.stamen.com/terrain-background/{z}/{x}/{y}.jpg", zoom=11)

gpe_sf_communes_df <- df_spatial(gpe_sf_communes)

ggplot(gpe_sf) +
  layer_spatial(base_map) +
  #layer_spatial(gpe_hillshade_raster) + scale_fill_distiller(palette="Greys") +
  layer_spatial(gpe_sf, alpha=0.25) +
  layer_spatial(border, alpha=0.25) +
  layer_spatial(gpe_sf_communes) +
  geom_spatial_text_repel(data=gpe_sf_communes_df, aes(x, y, label = name.fr), crs=4326, box.padding = 1) +
  annotation_scale(location = "tl") +
  annotation_north_arrow(location = "tr", which_north = "true")
