library(tidyverse)

library(FactoMineR)
library(factoextra)
library(Factoshiny)
library(missMDA)
library(visdat)
library(corrplot)

library(skimr)
library(caret)
library(randomForest)
library(reprtree)


covid_multivar <- read_csv("Docs_Astro/Data_Covid_ensemble_toutes_var_26-05-2020_Seb.csv")

covid_multivar_1 <- covid_multivar %>%
  # filter(!ISO_A3 %in% c("ABW","AIA","AND","BLR","BMU","CHI","CUW","CYM","DMA","ERI","ESH","FLK","FRO","GIB","GLP","GRL","GUF","IMN","KNA","LIE","MAF","MCO","MSR","MTQ","MYT","NCL","PYF","REU","SMR","SPM","SSD","SXM","TCA","TWN","VAT","VGB","XKX")) %>%
  select(-ISO_A3)

covid_multivar_1 <- column_to_rownames(covid_multivar_1, var = "Country_Name")

# covid_multivar_1 <- covid_multivar_1 %>% select(-"population_WDI", -"pauvrete_seuil_nat_pcentpop", -"pauvrete_5.50usd_pcentpop", -"GINI_index", -"depense_recherche_pcentPIB", -"ventes_PUT_kg_percapita",  -"dette_totale", -"pop_urb_1M_pcent")
# covid_multivar_1 <- covid_multivar_1 %>% select(-"cumul_tests", -"type_test", -"temp_dec19", -"temp_jan20", -"temp_fev20", -"temp_mar20")
covid_multivar_1 <- covid_multivar_1 %>% select(-"confirmed_30avr", -"deaths_30avr", -"recovered_30avr", -"date_first_cases")
covid_multivar_1 <- covid_multivar_1 %>% select(-c(40:44))
#covid_multivar_1 <- covid_multivar_1 %>% select(-groupe_economie_pays, -groupe_PIB_pays, -continent, -region_ONU, -region_WB, -sous_region)

vis_miss(covid_multivar_1, cluster=F)

naniar::gg_miss_var(covid_multivar_1)
naniar::gg_miss_case(covid_multivar_1)

nb <- estim_ncpPCA(covid_multivar_1,ncp.max=5)
res.comp <- imputePCA(covid_multivar_1,ncp=nb$ncp)
res.pca <- PCA(res.comp$completeObs, scale.unit = T, graph = F)

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 30))

# Variables
var <- get_pca_var(res.pca)

corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(res.pca, choice = "var", axes = 1:2)

fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             alpha.var = "cos2",
             repel = TRUE)

corrplot(var$contrib, is.corr=FALSE)

fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)

fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)

fviz_pca_var(res.pca, axes = c(1,2), labelsize=3,
             col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             alpha.var="contrib", repel = TRUE)

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
res.desc$Dim.1
res.desc$Dim.2

res.desc <- dimdesc(res.pca, axes = c(2,3), proba = 0.05)
res.desc$Dim.3

# Individus
ind <- get_pca_ind(res.pca)

fviz_pca_ind(res.pca, axes = c(1,2), labelsize=3,
             col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             alpha.var="contrib", repel = TRUE, addEllipses = T)

fviz_contrib(res.pca, choice = "ind", axes = 1:2)

# # tab.disj <- imputeFAMD(covid_multivar_1[,c(1:ncol(covid_multivar_1))])$tab.disj
# # 
# # res.famd <- FAMD(covid_multivar_1[,c(1:ncol(covid_multivar_1))], ncp = 5, graph = F, tab.disj = tab.disj)
# 
# fviz_screeplot(res.famd)
# vars <- get_famd_var(res.famd)
# vars$cos2
# 
# # Variables
# fviz_famd_var(res.famd, repel = TRUE)
# # Contribution to the first dimension
# fviz_contrib(res.famd, "var", axes = 1)
# # Contribution to the second dimension
# fviz_contrib(res.famd, "var", axes = 2)
# 
# quanti.var <- get_famd_var(res_famd, "quanti.var")
# fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
#               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#               repel = TRUE)
# 
# quali.var <- get_famd_var(res.famd, "quali.var")
# fviz_famd_var(res.famd, "quali.var", col.var = "contrib", 
#               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
# 
# # Individuals
# ind <- get_famd_ind(res.famd)
# 
# fviz_famd_ind(res.famd, col.ind = "cos2", 
#               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#               repel = TRUE)

### Random forest
rf.data <- select(covid_multivar_1, 1:72)

rf.skim <- skim(rf.data)

rf.data.indep <- select(rf.data, c(1:49))
rf.data.dep <- select(rf.data, 53)


rf.data.indep.num <- select(rf.data, -c(38,40))
rf.data.indep.qual <- select(rf.data, c(38,40))

rf.imputed <- rfImpute(x=rf.data.indep.num, y=rf.data[,53])

rf.imputed.tot <- bind_cols(rf.data.indep.qual, select(rf.imputed, -1))

rf.fit <- randomForest(AUC_confirmed_pmillion ~ ., data = select(rf.imputed.tot, c(1:49,53)), ntree=800, importance=T, mtry=31, nodesize=1)
plot(rf.fit)
varImpPlot(rf.fit)

rf.pred <- predict(rf.fit, newdata=rf.imputed.tot)

plot(rf.imputed.tot$AUC_confirmed_pmillion ~ rf.pred)
plot(rf.imputed.tot$AUC_confirmed_pmillion ~ rf.imputed.tot$PIB_pcapita)

library(partykit)
rf.data.indep <- select(rf.imputed.tot, c(2:49)) %>%
  #mutate_at(c("region_WB"),funs(factor(.)))
  mutate_if(is.character,as.factor)

rf.data.dep <- select(rf.imputed.tot, 53:54)
rf.data.combined <- cbind(rf.data.indep, rf.data.dep)
rf.data.combined$region_WB <- as.factor(rf.data.combined$region_WB)

party.fit <- ctree(AUC_confirmed_pmillion + AUC_deaths_pmillion ~ ., data=rf.data.combined)
plot(party.fit)

# ###
# oob.err=double(7)
# for(nodesize in 1:7)
# {
#   rf=randomForest(confirmed_p_million ~ ., data = rf.imputed.tot, ntree=800, nPerm=1, mtry=31, nodesize=nodesize)
#   oob.err[nodesize] = rf$mse[1000] #Error of all Trees fitted
#   cat(nodesize," ") #printing the output to the console
# 
# }
# matplot(1:7 , oob.err[1:7], pch=19 , col=c("red"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
# legend("topright",legend=c("Out of Bag Error"),pch=19, col=c("red"))

### caret
correlationMatrix <- cor(rf.data.num[,1:59], use="pairwise.complete.obs")
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)