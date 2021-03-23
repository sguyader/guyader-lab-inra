# load the package
library(COVID19)
library(TSrepr) # ts representations, use dev version devtools::install_github("PetoLau/TSrepr")
library(dtwclust) # clustering using DTW distance
#library(dygraphs) # interactive visualizations
library(ggrepel) # nice labels in ggplot
library(dendextend) # dendrograms

# additional packages to replicate the examples
library(tidyverse)
library(cowplot)
library(directlabels)

data_covid_ts <- covid19(raw=F) %>%
  filter(date < "2020-12-01") %>%
  filter(!is.na(iso_alpha_2)) %>%
  rename(country = administrative_area_level_1) %>%
  relocate(country, .after = id)

data_covid_ts <- data_covid_ts %>%
  mutate(active_cases=confirmed-deaths-recovered, .after=deaths) %>%
  
  mutate(confirmed_per_million = confirmed/population*1e6, .after=confirmed) %>%
  mutate(auc_confirmed_per_million = DescTools::AUC(as.numeric(date),
         confirmed_per_million, method="linear"), .after=confirmed_per_million) %>%
  mutate(deaths_per_million = deaths/population*1e6, .after=deaths) %>%
  mutate(auc_deaths_per_million = DescTools::AUC(as.numeric(date),
         deaths_per_million, method="linear"), .after=deaths_per_million) %>%
  mutate(recovered_per_million = recovered/population*1e6, .after=recovered) %>%
  mutate(auc_recovered_per_million = DescTools::AUC(as.numeric(date),
         recovered_per_million, method="linear"), .after=recovered_per_million) %>%
  mutate(active_cases_per_million = recovered/population*1e6, .after=active_cases) %>%
  mutate(auc_active_cases_per_million = DescTools::AUC(as.numeric(date),
         active_cases_per_million, method="linear"), .after=active_cases_per_million) %>%
  
  mutate(new_cases = c(confirmed[1], diff(confirmed)), .after=active_cases_per_million) %>%
  mutate(new_cases = replace(new_cases, new_cases < 0, 0)) %>%
  mutate(new_cases_per_million = new_cases/population*1e6, .after=new_cases) %>%
  
  mutate(new_deaths = c(deaths[1], diff(deaths)), .after=new_cases_per_million) %>%
  mutate(new_deaths = replace(new_deaths, new_deaths < 0, 0)) %>%
  mutate(new_deaths_per_million = new_deaths/population*1e6, .after=new_deaths) %>%
  
  mutate(new_recovered = c(recovered[1], diff(recovered)), .after=new_deaths_per_million) %>%
  mutate(new_recovered = replace(new_recovered, new_recovered < 0, 0)) %>%
  mutate(new_recovered_per_million = new_recovered/population*1e6, .after=new_recovered) %>%
  
  mutate(hosp_per_million = hosp/population*1e6, .after=hosp) %>%
  mutate(auc_hosp_per_million = DescTools::AUC(as.numeric(date),
         hosp_per_million, method="linear"), .after=hosp_per_million) %>%
  mutate(vent_per_million = vent/population*1e6, .after=vent) %>%
  mutate(auc_vent_per_million = DescTools::AUC(as.numeric(date),
         vent_per_million, method="linear"), .after=vent_per_million) %>%
  mutate(icu_per_million = icu/population*1e6, .after=icu) %>%
  mutate(auc_icu_per_million = DescTools::AUC(as.numeric(date),
         icu_per_million, method="linear"), .after=icu_per_million)

data_covid_end_date <- filter(data_covid_ts, date == max(date) & confirmed > 50)
write_excel_csv(data_covid_end_date, "data_covid_end_date_30nov2020.csv")

# définir le seuil minimum du nombre de cas pour le décompte
nth_cases <- 50 # you can vary

# Cases greater than threshold
data_covid_50_cases <- data_covid_ts %>% filter(confirmed >= nth_cases & country %in% unique(data_covid_end_date$country))

# Create 'days since' column
data_covid_50_cases <- data_covid_50_cases %>% group_by(country) %>%
  mutate("days_since_first_50_cases" = 1:n(), .after = date)

# choix de la variable à étudier
statistic <- "confirmed_per_million" # you can also choose other stat

# sort by highest "statistic" numbers
data_cases_order <- data_covid_50_cases %>% group_by(country) %>%
  filter(country %in% unique(data_covid_end_date$country)) %>%
  filter(date == max(date)) %>%
  select(country, !!sym(statistic)) %>%
  arrange(desc(!!sym(statistic)))

# subset data based on selected parameter -  top N countries and analyzed columns
N = nrow(data_cases_order)
topN <- ungroup(data_cases_order) %>% filter(between(row_number(), 1, N)) 

data_covid_cases_sub <- data_covid_50_cases %>%
  filter(country %in% topN$country) %>%
  select(country, days_since_first_50_cases, !!sym(statistic))

# Make same length time series from countries data
data_covid_trajectories <- pivot_wider(data_covid_cases_sub, names_from=country, values_from = !!sym(statistic))
data_covid_trajectories <- bind_cols(
  select(data_covid_trajectories, 1),
  select(data_covid_trajectories, sort(colnames(data_covid_trajectories)[2:(N+1)]))
)

# use SMA for preprocessing time series
q_sma <- 3 # order of moving average
cols <- colnames(data_covid_trajectories)

myfun <- function(i) c(rep(NA, q_sma - 1), ceiling(repr_sma(i, q_sma)))

data_covid_trajectories <- data.frame(data_covid_trajectories[1], lapply(data_covid_trajectories[2:(N+1)], myfun))

colnames(data_covid_trajectories) <- cols

# transpose
data_trajectories_trans <- as.data.frame(t(data_covid_trajectories[,2:length(data_covid_trajectories)]), row.names=colnames(data_covid_trajectories)[2:(N+1)])

data_trajectories_trans_list <- lapply(split(data_trajectories_trans, seq(nrow(data_trajectories_trans))),
                                       function(x) x[!is.na(x)])
names(data_trajectories_trans_list) <- colnames(data_covid_trajectories)[-1]

data_trajectories_trans_list_norm <- lapply(names(data_trajectories_trans_list),
                                            function(i) norm_z(data_trajectories_trans_list[[i]]))
names(data_trajectories_trans_list_norm) <- colnames(data_covid_trajectories)[-1]

k <- 2:8
clust_res <- tsclust(data_trajectories_trans_list,
                     k = k,
                     trace = T,
                     #seed = 12345,
                     
                     type = "h",
                     #centroid = "dba",
                     preproc = zscore,
                     control = hierarchical_control(method = "ward.D2"),
                     args = tsclust_args(dist = list(norm = "L2")),
                     distance = "sbd"
)

# vis_df <- sapply(clust_res, cvi, b=clust_res[[1]]@cluster, type="VI")
# names(vis_df) <- k
# vis_df
# plot(vis_df)

vis_df <- sapply(clust_res, cvi, b=clust_res[[1]]@cluster, type="internal")
colnames(vis_df) <- k
vis_df <- as.data.frame(vis_df)
plot(as.numeric(vis_df[4,])~as.numeric(colnames(vis_df)))

index <- 6

plot(clust_res[[index-(min(k)-1)]], type="series")
plot(clust_res[[index-(min(k)-1)]], type="series", labels = list(
  nudge_x = -5, nudge_y = 0.2, size=3, alpha=0.8, label.padding=0.1))
plot(clust_res[[index-(min(k)-1)]], type="centroid")
plot(clust_res[[index-(min(k)-1)]], type="dendrogram", labels = list(nudge_x = -5, nudge_y = 0.2))


data_clust_id <- as_tibble(data.frame(cluster = clust_res@cluster, country = names(data_trajectories_trans_list)))

# 
# data_clust_id <- as_tibble(data.frame(cluster = clust_res[[index]]@cluster, country = names(clust_res[[index-(min(k)-1)]]@cluster)))

data_clust_id_long <- data_clust_id %>% slice(rep(1:n(), each=nrow(data_covid_trajectories)))

data_trajectories_long <- pivot_longer(data_covid_trajectories, !days_since_first_50_cases, names_to="country", values_to=paste0(statistic)) %>% arrange(country)

data_plot <- bind_cols(data_trajectories_long, data_clust_id_long[1])

ggplot(data_plot, aes_string(x="days_since_first_50_cases", y=paste(statistic), group = "country")) +
  facet_wrap(~cluster, ncol = ceiling(sqrt(length(unique(data_plot$cluster)))), scales = "free_y") +
  geom_line(aes(color = country), alpha = 0.75, size = 0.8) +
  #scale_y_continuous(trans = 'log10') +
  guides(color="none") +
  labs(x = colnames(data_plot)[1], y = statistic) +
  theme_bw()

dend <- as.dendrogram(clust_res[[index-(min(k)-1)]])

dend <- dend %>%
  color_branches(k = index) %>%
  color_labels(k = index) %>%
  set("branches_lwd", 0.8) %>%
  set("labels_cex", 0.6) %>%
  plot(horiz=T)
dend

ggd1 <- as.ggdend(dend, type="rectangle")

ggplot(ggd1, horiz = T) +
  ylim(max(get_branches_heights(dend)), -1)



#################################
#      graphes
#################################
ggplot(data = data_covid_ts, aes(x = date, y = deaths/population * 100)) +
  geom_line(aes(color = id)) +
  geom_dl(aes(label = Country), method = list("last.points", cex = .75, hjust = 1, vjust = 0)) +
  #scale_y_continuous(# trans="log10") + 
  theme(legend.position = "none") +
  ggtitle("Cas confirmés en % de la population (échelle log)")

p1 <- ggplot(data = data_covid_ts, aes(x = date, y = icu/confirmed * 100)) +
  #geom_point(aes(color = id)) +
  geom_line(aes(color = id)) +
  geom_dl(aes(label = administrative_area_level_1), method = list("last.points", cex = .75, hjust = 1, vjust = 0)) +
  #geom_line(aes(x = date, y = confirmed/population * 100), col="green") +
  scale_y_continuous(limits=c(0,15)) +
  #geom_smooth(method = "loess") +
  theme(legend.position = "none") +
  ggtitle("Personnes en soins intensifs en % des cas confirmés")

  
p2 <- ggplot(data = data_covid_ts, aes(x = date, y = deaths/confirmed * 100)) +
  geom_point(aes(color = id)) +
  #geom_dl(aes(label = administrative_area_level_1), method = list("last.points", cex = .75, hjust = 1, vjust = 0)) +
  #scale_y_continuous(trans="log10") +
  #scale_y_continuous(limits=c(5,20)) +
  geom_smooth(method = "loess") +
  theme(legend.position = "none") +
  ggtitle("Personnes décédées en % des cas confirmés")

plot_grid(p2, p1)
