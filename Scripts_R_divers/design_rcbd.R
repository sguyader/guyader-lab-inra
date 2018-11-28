# Méthode perso

trt <- c("A","B","C","D")  # modalités de traitement
rep <- 3               # nombre de répétitions/blocs

plan <- matrix(nrow=rep, ncol=length(trt))
rownames(plan) <- paste("Rep.", 1:rep, ":")

for(i in 1:rep) {
  draw <- sample(trt, length(trt))
  plan[i,] <- draw
}

print(plan)

# Méthode "agricolae"

library(agricolae)

plan2 <- design.rcbd(c("A","B","C","D"), r=3, serie=1, seed=123, kinds="Super-Duper", first=TRUE,
                     continue=FALSE,randomization=TRUE)
print(plan2$sketch)
