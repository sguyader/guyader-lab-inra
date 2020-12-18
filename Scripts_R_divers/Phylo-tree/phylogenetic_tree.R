library(treeio)
library(ggtree)
library(tidytree)

setwd("~")

tree <- read.tree(file = "tree2.nwk")

d <- data.frame(species=rep("X",59))
d$label <- tree$tip.label

tree2 <- full_join(tree,d,by="label")

RColorBrewer::brewer.pal(n = 3, name = "Dark2")

ggtree(tree2, ladderize = T) +
  geom_treescale(x=0) +
  geom_tiplab(aes(colour=species), size=3, align=F) +
  scale_color_manual(values=c(RColorBrewer::brewer.pal(n = 3, name = "Set2"), "grey20")) +
  theme(legend.position="none")
