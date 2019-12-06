source("../R_files/utilities_volcan_simple.R")

# Q1. Ecrire une fonction qui implémente un plan d’expériences uniforme
# (à l’aide des fonctions Rrunif, matrix). Cette fonction doit prendre en paramètres le
# nombre de points n et le nombre de variables d, et retourner une matrice n × d de
# valeurs entre 0 et 1. Générer un plan à 100 points en dimension 5. Visualiser.

unif_doe <- function(n, d) {
  return(matrix(runif(n*d), nrow=n, ncol=d))
}
X_rand <- unif_doe(n=100,d=5)

pairs(X_rand, diag.panel=panel.hist)

# Q2. Ecrire une fonction qui implémente un hypercube latin (aléatoire) : on pourra
# utiliser la fonction sample. Cette fonction utilisera les mêmes entrées / sorties
# que la précédente. Attention à la normalisation. Cette question peut être sautée
# par les stagiaires souhaitant directement explorer le cas-test. Générer un plan à
# 100 points en dimension 5. Visualiser.

lhs_doe <- function(n, d) {
  X <- matrix(0, nrow=n, ncol=d)
  x <- (1:n)/n-1/(2*n)
  for(i in 1:d){
    X[,i] <- sample(x)
  }
  return(X)
}

X_lhs <- lhs_doe(n=100, d=5)

pairs(X_lhs, diag.panel=panel.hist)

# Q3. A l’aide du paquet DiceDesign, générer un hypercube latin optimisé à
# 100 points et 5 variables, par exemple avec la fonction maximinESE_LHS, ou
# bien avec le paquet lhs et la fonction improvedLHS.

library(DiceDesign)
X <- lhsDesign(100, 5)$design
colnames(X) <- 1:5
Xopt <- maximinESE_LHS(design=X, inner_it=10, it=1)
plot(Xopt$critValues,type="l")
X_lhsopt <- Xopt$design

# Q4.

X_faure <- runif.faure(100, 5)$design
colnames(X) <- 1:5
Xopt <- maximinESE_LHS(design=X, inner_it=10, it=1)
plot(Xopt$critValues,type="l")
X_lhsopt <- Xopt$design

# Q5.

# Un par un
mindist(X_rand)

# Tous ensemble
Xlist <- list(X_rand, X_lhs, X_lhsopt, X_faure)

unlist(lapply(Xlist, mindist))

unlist(lapply(Xlist, meshRatio))

# Q6 et Q7.

# Génération des données
X <- X_lhsopt
Y <- compute_wls(X)

# Visualisation
par(mfrow=c(1,5))
for(i in 1:5){
  plot(X[,i], Y)
}


