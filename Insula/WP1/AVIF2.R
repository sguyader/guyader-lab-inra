

AVIF_table_df <- as.data.frame(table(AVIF$Id_site, AVIF$Nom_sc))

colnames(AVIF_table_df) <- c("Id_site", "Nom_sc", "freq")

AVIF_matrix <- AVIF_table_df %>% pivot_wider(names_from = Nom_sc, values_from = freq)

AVIF_dist_matrix <- AVIF_matrix %>% select(-Id_site) %>% vegdist()

AVIF_dist_matrix

tr <- (spantree(AVIF_dist_matrix))

cl <- as.hclust(tr)
plot(cl)

plot(tr, col = cutree(cl, 5), pch=16)



test <- AVIF %>% group_by(Id_site) %>% summarize(n_distinct(Nom_sc))
test
