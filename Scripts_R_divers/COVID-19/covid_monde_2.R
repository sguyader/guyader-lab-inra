library(tidyverse)
library(rnaturalearth)
library(gganimate)
library(rlang)

#library(rnaturalearth)
#library(rnaturalearthdata)

theme_set(theme_bw())
setwd("~/TRAVAIL/github/guyader-lab-inra/Scripts_R_divers/COVID-19")

download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", destfile = "covid19_confirmed.csv")
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", destfile = "covid19_deaths.csv")
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", destfile = "covid19_recovered.csv")

covid_confirmed <- read_csv("covid19_confirmed.csv")
covid_deaths <- read_csv("covid19_deaths.csv")
covid_recovered <- read_csv("covid19_recovered.csv")

covid_confirmed_long <- gather(covid_confirmed, 5:ncol(covid_confirmed), key=date, value=confirmed)
covid_deaths_long <- gather(covid_deaths, 5:ncol(covid_deaths), key=date, value=deaths)
covid_recovered_long <- gather(covid_recovered, 5:ncol(covid_recovered), key=date, value=recovered)

covid_join <- full_join(covid_confirmed_long, covid_deaths_long)
covid_join2 <- left_join(covid_join, covid_recovered_long) %>% filter(Lat != 0)

####
WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify()
jhu.covid <- covid_join2
jhu.covid$date <- as.Date(jhu.covid$date, format="%m/%d/%y")

p <- ggplot(WorldData) +
  geom_map(map=WorldData, aes(map_id=region, fill=region), color="white", size=0.2, alpha=0.3) +
  scale_fill_discrete(guide=NULL) +
  geom_point(data=jhu.covid, aes(x=Long, y=Lat, size=confirmed, colour=deaths, stroke=0), alpha=0.6) +
  scale_colour_gradient(low="#3333CC", high="red") +
  scale_size(range=c(0,35)) +
  coord_quickmap(xlim=c(-160,180), ylim=c(-55,80)) +
  labs(title = 'Date: {frame_time}', x = 'Longitude', y = 'Latitude') +
  theme_bw() +
  guides(colour = guide_colourbar(label.vjust=0.5, label.theme = element_text(angle = 90)),
         size = guide_legend(nrow=1)) +
  theme(legend.position="bottom") +
  transition_time(date)

options(gganimate.dev_args = list(width = 1400, height = 800, units = 'px'))

anim <- animate(p, fps=6, renderer = gifski_renderer())
anim_save("test.gif")

anim <- animate(p, fps=8, renderer = ffmpeg_renderer(format="mp4"))
anim_save("test.mp4")


#####
dyna_fr_all <- jhu.covid %>% filter_at(vars(starts_with("Country")), all_vars(. == "France"))
dyna_fr <- dyna_fr_all %>% filter_at(vars(starts_with("Province")), all_vars(is.na(.)))
dyna_fr_gua <- jhu.covid %>% filter_at(vars(starts_with("Province")), all_vars(. == "Guadeloupe"))


data1 <- filter_at(jhu.covid, vars(starts_with("Country")), all_vars(. == "Italy"))

gomp.fit <- nls(confirmed ~ Asym*exp(-exp(-b*(as.numeric(date)-mid))), data=dyna_fr, start=list(b=0.1, Asym=140000, mid=18350))
#nls.fit2 <- nls(confirmed ~ Asym/(1+exp(-b*(as.numeric(date)-mid))), data=dyna_fr_gua, start=list(b=0.1, Asym=140000, mid=18350))

plot(dyna_fr$date, dyna_fr$confirmed)
lines(dyna_fr$date, predict(gomp.fit), lty=1)
lines(dyna_fr$date, predict(gomp.fit), lty=2)

newdata = expand.grid(date=seq(as.Date('2020-01-22'),as.Date('2020-04-29'), by = 1))
gomp.pred <- predict(gomp.fit, newdata=newdata, interval="prediction")
pred <- data.frame(newdata, gomp.pred)

ggplot(filter(dyna_fr, date >= "2020-01-22")) +
  geom_col(aes(x=as.Date(date), y=confirmed), fill="#756bb1") +
  geom_line(data=pred, aes(x=as.Date(date), y=gomp.pred), color="blue", size=1, lty=2) +
  geom_line(aes(x=as.Date(date), y=gomp.pred[1:nrow(dyna_fr)]), color="red", size=1.2) +
  scale_x_date(date_breaks="1 week", labels=scales::date_format("%d-%m-%Y"), limits = as.Date(c("2020-02-20","2020-04-29"))) +
  theme(axis.text.x = element_text(angle = 90)) +
  #geom_text(data = data.frame(), aes(x=as.Date("2020-03-06"), y=120, label = "Modèle Gompertz :\nAsym*exp(-exp(-b*(as.numeric(date)-mid)))"), col="red") +
  #geom_segment(x=as.Date("2020-03-16"), y=100, xend=as.Date("2020-03-23"), yend=75, color="red", size=0.5, arrow=arrow()) +
  labs(title = "Evolution des cas officiels de COVID-19 en France (source ARS)", x="date", y="nombre de cas confirmés")
  

           