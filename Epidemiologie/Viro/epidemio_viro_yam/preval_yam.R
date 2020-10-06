library(tidyverse)

library(FactoMineR)
library(factoextra)
library(Factoshiny)

library(randomForest)
library(partykit)
library(rpart)

data_preval <- read_csv("résultats_prospections_ignames_2019_GO_et_KA_3.csv")
data_preval <- mutate_if(data_preval, is.character, as.factor)
data_preval <- data_preval %>% mutate(duree_culture = factor(duree_culture), nb_cultivars=factor(nb_cultivars), altitude=factor(altitude))
data_preval <- column_to_rownames(data_preval, var = "parcelle")

### partykit
party.fit <- ctree(YMMV ~ region+pluviometrie+altitude, data=data_preval)
plot(party.fit)

### randomforest
rf.fit <- randomForest(YMMV ~ ., data = select(data_preval, c(1:22,24)), ntree=500, importance=T)
plot(rf.fit)
varImpPlot(rf.fit)

### factoshiny
MFAshiny(data_preval)

newDF <- data_preval[,c("region","precedent","nb_cultivars","desherbage","paillage","tuteurage","apport_amendement","apport_fertl","traitement_phyto","freq_surveillance","propre_semence","achat_autres_prod","propre_semence_et_achat_marche","achat_etranger","propre_semence_et_achat_autres_prod","pluviometrie","altitude","Cultures_mitoyennes_1","Cultures_mitoyennes_2","Cultures_mitoyennes_3","YMV","YMMV","Potex","Betaflexi","Ampelo","Maclura","DaMV")]

res.MFA <- MFA(newDF,group=c(1,9,5,5,7), type=c("n","n","n","n","s"),name.group=c("region","itineraire","origine_semence","environmt","prevalence"),num.group.sup=c(1),graph=FALSE)

plot.MFA(res.MFA, choix="ind",partial='all',lab.par=FALSE,invisible= c('ind','quali'),habillage='group',title="Individual factor map",cex=0.55,cex.main=0.55,cex.axis=0.55)
plot.MFA(res.MFA, choix="ind",partial='all',lab.par=FALSE,invisible= c('ind','quali.sup'),habillage='group',title="Individual factor map",cex=0.55,cex.main=0.55,cex.axis=0.55)
plot.MFA(res.MFA, choix="ind",partial='all',lab.par=FALSE,invisible= c('quali','quali.sup'),habillage='group',title="Individual factor map",cex=0.55,cex.main=0.55,cex.axis=0.55)
plot.MFA(res.MFA, choix="ind",lab.par=FALSE,invisible= c('quali','quali.sup'),habillage="propre_semence",title="Individual factor map",cex=0.55,cex.main=0.55,cex.axis=0.55)

plot.MFA(res.MFA, choix="var",habillage='group',title="Correlation circle")
plot.MFA(res.MFA, choix="group")
plot.MFA(res.MFA, choix="axes",habillage='group')

res.HCPC<-HCPC(res.MFA,nb.clust=6,consol=FALSE,graph=FALSE)

plot.HCPC(res.HCPC,choice='tree',title='Hierarchical tree')
plot.HCPC(res.HCPC,choice='map',draw.tree=TRUE,title='Factor map')
plot.HCPC(res.HCPC,choice='3D.map',ind.names=TRUE,centers.plot=TRUE,angle=60,title='Hierarchical tree on the factor map')

### correspondance analysis
data_preval_preval <- select(data_preval, 23:29)

library(gplots)
dt <- as.table(as.matrix(data_preval_preval))
balloonplot(t(dt), main ="prevalence", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

# test de d'association colonnes/lignes
chisq <- chisq.test(data_preval_preval)
chisq

# CA
res.ca <- CA(data_preval_preval, ncp = 5, graph = TRUE)
summary(res.ca)

# chi2 statistics
chi2 <- chisq[["statistic"]][["X-squared"]]
chi2
# Degree of freedom
df <- chisq[["parameter"]][["df"]]
# P-value
pval <- pchisq(chi2, df = df, lower.tail = FALSE)
pval
# eigen values
eig.val <- get_eigenvalue(res.ca)
eig.val
# keep the dimensions with explained variance larger than the largest of:
rows <- 1/(nrow(data_preval_preval)-1)*100
# or
cols <- 1/(ncol(data_preval_preval)-1)*100
# screeplot
fviz_screeplot(res.ca) + geom_hline(yintercept=max(c(rows,cols)), linetype=2, color="red")

# biplot
# repel= TRUE to avoid text overlapping (slow if many point)
fviz_ca_biplot(res.ca, repel = TRUE)

row <- get_ca_row(res.ca)
row

# Coordinates
head(row$coord)
# Cos2: quality on the factore map
head(row$cos2)
# Contributions to the principal components
head(row$contrib)
fviz_ca_row(res.ca, repel = TRUE, col.row="steelblue", shape.row = 15)
head(row$cos2)
fviz_ca_row(res.ca, col.row = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
library("corrplot")
corrplot(row$cos2, is.corr=FALSE)
head(row$contrib)
corrplot(row$contrib, is.corr=FALSE)
# Contributions of rows to dimension 1
fviz_contrib(res.ca, choice = "row", axes = 1, top = 10)
# Contributions of rows to dimension 2
fviz_contrib(res.ca, choice = "row", axes = 2, top = 10)
fviz_contrib(res.ca, choice = "row", axes = 1:2, top = 10)
fviz_ca_row(res.ca, col.row = "contrib",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)

col <- get_ca_col(res.ca)
col
# Coordinates of column points
head(col$coord)
# Quality of representation
head(col$cos2)
# Contributions
head(col$contrib)
fviz_ca_col(res.ca, col.col = "cos2", 
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE)
corrplot(col$cos2, is.corr=FALSE)
fviz_cos2(res.ca, choice = "col", axes = 1:2)
fviz_contrib(res.ca, choice = "col", axes = 1:2)
fviz_ca_biplot(res.ca, repel = TRUE)
fviz_ca_biplot(res.ca, 
               map ="rowprincipal", arrow = c(TRUE, TRUE),
               repel = TRUE)
fviz_ca_biplot(res.ca, map ="colgreen", arrow = c(TRUE, FALSE),
               repel = TRUE)

# Dimension description
res.desc <- dimdesc(res.ca, axes = c(1,2))
# Description of dimension 1 by row points
head(res.desc[[1]]$row, 4)
head(res.desc[[1]])
# Description of dimension 2 by row points
res.desc[[2]]$row
# Description of dimension 1 by column points
res.desc[[2]]$col

### CCA
library(vegan)
data_preval_parcelles <- select(data_preval, 5:26)
data_preval_virus <- select(data_preval, 27:33)
res.cca <- cca(data_preval_virus ~ region+altitude+pluviometrie, data=data_preval_parcelles)
res.cca <- cca(data_preval_virus, data_preval_parcelles)
res.cca
plot(res.cca)
anova.cca(res.cca, by="term", permutations=199)

