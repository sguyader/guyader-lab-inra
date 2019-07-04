library(cowplot)
library(tidyverse)
library(tibble)

data.2009 <- read.table("./Essais/2009/Notations2009.txt", strip.white=T, header=T) # lit les données
JAP.2009 <- c("112","120","126","133","141","147","154","161","171","176","183","190","198") # dates en JAP

data.2011 <- read.table("./Essais/2011/Notations2011.txt", strip.white=T, header=T) # lit les données, les convertit sous format vecteur long
JAP.2011 <- c("119","127","134","141","149","156","164","178")
 
data.2014 <- read.table("./Essais/2014/Notations2014.txt", strip.white=T, header=T) # lit les données, les convertit sous format vecteur long
JAP.2014 <- c("169","180","187","195","203","210","217","232")

colnames(data.2009)[4:16] <- JAP.2009
data.2009.long <- gather(data.2009, date, note, 4:ncol(data.2009))
data.2009.long <- cbind(year="2009", data.2009.long)

colnames(data.2011)[4:11] <- JAP.2011
data.2011.long <- gather(data.2011, date, note, 4:ncol(data.2011))
data.2011.long <- cbind(year="2011", data.2011.long)

colnames(data.2014)[4:11] <- JAP.2014
data.2014.long <- gather(data.2014, date, note, 4:ncol(data.2014))
data.2014.long <- cbind(year="2014", data.2014.long)

data.long <- as_tibble(rbind(data.2009.long, data.2011.long, data.2014.long))

# Transformer les notes en % de surface nécrosée selon l'échelle :
#1 -> 1.5 
#2 -> 4.5 
#3 -> 9.0 
#4 -> 18.5 
#5 -> 37.5 
#6 -> 62.5 
#7 -> 81.5 
#8 -> 93.5 
#9 -> 100

data.long$severity <- plyr::revalue(factor(data.long$note), c("1"="1.5", "2"="4.5", "3"="9.0", "4"="18.5", "5"="37.5", "6"="62.5", "7"="81.5", "8"="93.5", "9"="100.0"))
data.long$severity <- as.numeric(as.numeric(levels(data.long$severity))[data.long$severity])

ScoreArea.mean <- data.long %>% group_by(year, treatment, date) %>% na.omit() %>% summarise(
  N=n(),
  ScoreArea=mean(severity),
  sd=sd(severity),
  se=sd/sqrt(N),
  ci.student=qt(0.975,df=N-1)*se
)




### Graph 2D


## 2009

data.2009.long <- data.long %>% dplyr::filter(year=="2009")
data.2009.long$date <- factor(data.2009.long$date, levels=(JAP.2009))

gg_2009_plat <- data.2009.long %>% dplyr::filter(treatment=="plat") %>% ggplot(aes(x=x, y=y, fill=severity)) +
  geom_raster()+ #(color="white") +
  scale_fill_gradient2(low="white", mid="yellow", high="red", midpoint=50, limits=c(0, 100), na.value="grey80") +
  scale_x_discrete(limit=seq(1, max(data.2009.long$x[data.2009.long$treatment=="plat"]), by=2), expand = c(0, 0)) +
  scale_y_discrete(limit=seq(1, max(data.2009.long$y[data.2009.long$treatment=="plat"]), by=2), expand = c(0, 0)) +
  coord_equal() +
  facet_wrap( ~ date, ncol=4) +
  ggtitle("Plat") +
  #my_ggplot_theme() +
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(size=8), axis.text.y = element_text(size=8))


gg_2009_tut <- data.2009.long %>% dplyr::filter(treatment=="tuteur") %>% ggplot(aes(x=x, y=y, fill=severity)) +
  geom_tile() +
  scale_fill_gradient2(low="white", mid="yellow", high="red", midpoint=50, limits=c(0, 100), na.value="grey80") +
  scale_x_discrete(limit=seq(1, max(data.2009.long$x[data.2009.long$treatment=="tuteur"]), by=2), expand = c(0, 0)) +
  scale_y_discrete(limit=seq(1, max(data.2009.long$y[data.2009.long$treatment=="tuteur"]), by=2), expand = c(0, 0)) +
  coord_equal() +
  facet_wrap( ~ date, ncol=4) +
  ggtitle("Tuteuré") +
  #my_ggplot_theme() +
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(size=8), axis.text.y = element_text(size=8))

p2009 <- plot_grid(gg_2009_plat, gg_2009_tut)
#ggsave("2d_2009.pdf")

## 2011

data.2011.long <- data.long %>% dplyr::filter(year==2011)
data.2011.long$date <- factor(data.2011.long$date, levels=(JAP.2011))

gg_2011_plat <- data.2011.long %>% dplyr::filter(treatment=="plat") %>% ggplot(aes(x=x, y=y, fill=severity)) +
  geom_tile() +
  scale_fill_gradient2(low="white", mid="yellow", high="red", midpoint=50, limits=c(0, 100), na.value="grey80") +
  scale_x_discrete(limit=seq(1, max(data.2011.long$x[data.2011.long$treatment=="plat"]), by=2), expand = c(0, 0)) +
  scale_y_discrete(limit=seq(1, max(data.2011.long$y[data.2011.long$treatment=="plat"]), by=2), expand = c(0, 0)) +
  coord_equal() +
  facet_wrap( ~ date, ncol=3) +
  ggtitle("plat") +
  #my_ggplot_theme() +
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(size=8), axis.text.y = element_text(size=8))

gg_2011_tut <- data.2011.long %>% dplyr::filter(treatment=="tuteur") %>% ggplot(aes(x=x, y=y, fill=severity)) +
  geom_tile() +
  scale_fill_gradient2(low="white", mid="yellow", high="red", midpoint=50, limits=c(0, 100), na.value="grey80") +
  scale_x_discrete(limit=seq(1, max(data.2011.long$x[data.2011.long$treatment=="tuteur"]), by=2), expand = c(0, 0)) +
  scale_y_discrete(limit=seq(1, max(data.2011.long$y[data.2011.long$treatment=="tuteur"]), by=2), expand = c(0, 0)) +
  coord_equal() +
  ggtitle("tuteuré") +
  facet_wrap( ~ date, ncol=3) +
  #my_ggplot_theme() +
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(size=8), axis.text.y = element_text(size=8))

p2011 <- plot_grid(gg_2011_plat, gg_2011_tut)

## 2014

data.2014.long <- data.long %>% dplyr::filter(year==2014)
data.2014.long$date <- factor(data.2014.long$date, levels=(JAP.2014))

gg_2014_plat <- data.2014.long %>% dplyr::filter(treatment=="plat") %>% ggplot(aes(x=x, y=y, fill=severity)) +
  geom_tile() +
  scale_fill_gradient2(low="white", mid="yellow", high="red", midpoint=50, limits=c(0, 100), na.value="grey80") +
  scale_x_discrete(limit=seq(1, max(data.2014.long$x[data.2014.long$treatment=="plat"]), by=2), expand = c(0, 0)) +
  scale_y_discrete(limit=seq(1, max(data.2014.long$y[data.2014.long$treatment=="plat"]), by=2), expand = c(0, 0)) +
  coord_equal() +
  ggtitle("plat") +
  facet_wrap( ~ date, ncol=3) +
  #my_ggplot_theme() +
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(size=8), axis.text.y = element_text(size=8))

gg_2014_tut <- data.2014.long %>% dplyr::filter(treatment=="tuteur") %>% ggplot(aes(x=x, y=y, fill=severity)) +
  geom_tile() +
  scale_fill_gradient2(low="white", mid="yellow", high="red", midpoint=50, limits=c(0, 100), na.value="grey80") +
  scale_x_discrete(limit=seq(1, max(data.2014.long$x[data.2014.long$treatment=="tuteur"]), by=2), expand = c(0, 0)) +
  scale_y_discrete(limit=seq(1, max(data.2014.long$y[data.2014.long$treatment=="tuteur"]), by=2), expand = c(0, 0)) +
  coord_equal() +
  ggtitle("tuteuré") +
  facet_wrap( ~ date, ncol=3) +
  #my_ggplot_theme() +
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(size=8), axis.text.y = element_text(size=8))

p2014 <- plot_grid(gg_2014_plat, gg_2014_tut)

p2009
p2011
p2014

plot_grid(p2009,p2011,p2014)
