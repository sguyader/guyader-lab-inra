data <- data_covid_trajectories2

cluster_trajectories2 <- function(data, k, normalize = FALSE) {
  
  # transpose data for clustering
  data_trajectories_trans2 <- t(data[, .SD,
                                    .SDcols = colnames(data)[-1]])
  
  # create list of time series
  data_trajectories_trans_list2 <- lapply(1:nrow(data_trajectories_trans2), function(i)
    na.omit(data_trajectories_trans2[i,]))
  names(data_trajectories_trans_list2) <- colnames(data)[-1]
  
  # remove data with length <= 1
  n_list <- sapply(1:length(data_trajectories_trans_list2), function(i)
    length(data_trajectories_trans_list2[[i]]))
  names(n_list) <- names(data_trajectories_trans_list2)
  
  if (length(which(n_list %in% 0:1)) != 0) {
    
    data_trajectories_trans_list2 <- data_trajectories_trans_list2[-which(n_list %in% 0:1)]
    
  }
  
  list_names2 <- names(data_trajectories_trans_list2)
  
  # normalization
  if (normalize) {
    
    data_trajectories_trans_list2 <- lapply(names(data_trajectories_trans_list2),
                                           function(i)
                                             norm_z(data_trajectories_trans_list2[[i]])
    )
    names(data_trajectories_trans_list2) <- list_names2
    
  }
  
  
  # clustering with dtwclust package function
  hc_res <- tsclust(data_trajectories_trans_list2,
                    type = "hierarchical",
                    k = 14,
                    distance = "dtw_basic",
                    centroid = dba,
                    trace = FALSE,
                    seed = 54321,
                    control = hierarchical_control(method = "ward.D2"),
                    args = tsclust_args(dist = list(norm = "L2"))
  )
  
  return(hc_res)
  
}

########
clust_res2 <- cluster_trajectories2(data = data_covid_trajectories2,
                                  k = 14,
                                  normalize = TRUE)

# results
clust_res2

# prepare time series
data_clust_id2 <- data.table(Cluster = clust_res2@cluster,
                            Country = names(clust_res2@cluster))

data_plot2 <- melt(data_covid_trajectories2,
                  id.vars = colnames(data_covid_trajectories2)[1],
                  variable.name = "Country",
                  variable.factor = FALSE,
                  value.name = statistic,
                  value.factor = FALSE)

data_plot2 <- copy(data_plot2[.(data_clust_id2$Country), on = .(Country)])

data_plot2[data_clust_id2,
          on = .(Country),
          Cluster := i.Cluster]

# plot
ggplot(data_plot2, aes(get(colnames(data_plot2)[1]), get(statistic), group = Country)) +
  facet_wrap(~Cluster, ncol = ceiling(data_plot2[, sqrt(uniqueN(Cluster))]), scales = "free_y") +
  geom_line(color = "grey10", alpha = 0.75, size = 0.8) +
  scale_y_continuous(trans = 'log10') +
  labs(x = colnames(data_plot2)[1], y = statistic) +
  theme_bw()
