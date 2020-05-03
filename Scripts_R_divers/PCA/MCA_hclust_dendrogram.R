library(FactoMineR)
library(factoextra)

data(tea)
res.mca = MCA(tea, ncp=20, quanti.sup=19, quali.sup=c(20:36), graph=FALSE)
res.hcpc = HCPC(res.mca, graph=FALSE, nb.clust=6)

# 
# plot(res.hcpc, choice="tree", tree.barplot=F)
# 
# fviz_dend(res.hcpc)
# fviz_cluster(res.hcpc)



dend <- as.dendrogram(res.hcpc$call$t$tree)

dend %>% set("labels_col", "transparent") %>% plot(main="Dendrogramme")
rect.dendrogram(dend, k=6, border = rep("black",6), lty = 2, lwd = 1, lower_rect=-0.002)
  
dend <- as.dendrogram(res.hcpc$call$t$tree)
dend <- set(dend, "labels_col", "transparent")
plot(dend, main="Dendrogramme")
rect.dendrogram(dend, k=6, border = rep("black",6), lty = 2, lwd = 1)
