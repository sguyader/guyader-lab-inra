library(ggspatial)
library(patchwork)
library(ggrepel)
library(OpenStreetMap)
library(scatterpie)

theme_set(theme_bw())

base_map <- read_osm(bbox, type="http://tile.stamen.com/terrain-background/{z}/{x}/{y}.jpg", zoom=11)

gpe_sf_communes_df <- df_spatial(gpe_sf_communes)

d <- data.frame(long=gpe_sf_communes_df[1:4,1], lat=gpe_sf_communes_df[1:4,2])
n <- nrow(d)
d$virus <- factor(1:n)
d$A <- abs(rnorm(n, sd=1))
d$B <- abs(rnorm(n, sd=2))
d$C <- abs(rnorm(n, sd=3))
d$D <- abs(rnorm(n, sd=4))

ggplot(gpe_sf) +
  #layer_spatial(base_map) +
  #layer_spatial(gpe_hillshade_raster) + scale_fill_distiller(palette="Greys") +
  layer_spatial(gpe_sf, alpha=0.25) +
  layer_spatial(border, alpha=0.25) +
  layer_spatial(gpe_sf_communes) +
  geom_spatial_text_repel(data=gpe_sf_communes_df, aes(x, y, label = name.fr), crs=4326, box.padding = 1) +
  annotation_scale(location = "tl") +
  annotation_north_arrow(location = "tr", which_north = "true", style=north_arrow_fancy_orienteering(line_width = 0.5)) +
  geom_scatterpie(data=d, aes(x=x, y=y, group=virus), pie_scale = 7, cols=LETTERS[1:4], color="black", alpha=.8, legend_name = "Virus") +
  theme_bw()
