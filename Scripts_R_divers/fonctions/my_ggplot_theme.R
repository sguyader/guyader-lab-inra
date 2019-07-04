my_ggplot_theme <- function (font_size = 12, font_family = "", line_size = 0.5) {
    half_line <- font_size/2
    small_rel <- 0.857
    small_size <- small_rel * font_size
    theme_grey(base_size = font_size, base_family = font_family) %+replace% 
    theme(rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
		line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
		text = element_text(family = font_family, face = "plain", colour = "black", size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, margin = ggplot2::margin(), debug = FALSE),
		axis.text = element_text(size = rel(0.8)),
		axis.text.x = element_text(margin = ggplot2::margin(t = small_size/4), vjust = 1),
		axis.text.y = element_text(margin = ggplot2::margin(r = small_size/4), hjust = 1),
		axis.title.x = element_text(face="bold", size=12, vjust=-0.2, margin = ggplot2::margin(t = small_size/2, b = small_size/4)),
		axis.title.y = element_text(face="bold", size=12, vjust=1.2, angle = 90, margin = ggplot2::margin(r = small_size/2, l = small_size/4),),
		axis.ticks = element_line(colour = "black", size = line_size), 
    axis.line = element_blank(), 
    legend.key = element_blank(),
		legend.spacing.x = grid::unit(0.2, "cm"),
		legend.spacing.y = grid::unit(0.2, "cm"),
		legend.key.size = grid::unit(1, "lines"),
		legend.key.width = grid::unit(2, "lines"), 
    legend.text = element_text(size = rel(small_rel)), 
    panel.background = element_blank(),
    panel.grid.major = element_blank(), # element_line(colour = "grey90", size = 0.2)
		panel.grid.minor = element_blank(), # element_line(colour = "grey98", size = 0.5)
    panel.border = element_rect(fill = NA, colour = "black"), 
    strip.background = element_rect(fill = NA, colour = "black", size = 0.5),
		strip.text = element_text(size = rel(small_rel)),
		#strip.text.x = element_text(),
		#strip.text.y = element_text(angle = -90),
		plot.background = element_blank(), 
    plot.title = element_text(face = "bold", size = font_size, 
    margin = ggplot2::margin(b = half_line)),
		plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
		complete = TRUE
		)
}