## A faire si necessaire. Attention, la première ligne élimine tous les objets.
## Si vous voulez conserver vos anciens resultats, mettez en commentaire la premiere ligne.
source('../R_files/utilities_volcan_simple.R')
source('Cours1_TP_util.R')

library('sensitivity')
library('lhs')


##################################################################
# Visualisations des plans d'experience de Morris (sans modele)
##################################################################

## dimension 2 
K=2

# fonction d'affichage des trajectoires en dim 2
plot_morris_Keq2 = function(nb_r = 50,nb_l = 10,delta = 5)
{
	set.seed(123)
	x <- morris(model = NULL, factors = K, r = nb_r,design = list(type = "oat", levels = nb_l, grid.jump = delta)) 
	#nb traj effectives
	nb_r=length(x$X[,1])/(K+1)
	
	# dimension du plan : r * (k+1) 
	print(dim(x$X))

	# trajectoires
	x11()
	plot(c(0,1),c(0,1),typ='n')
	for (r in 1:nb_r)
	{
		X_r = x$X[(1+(r-1)*(K+1)):(r*(K+1)),]
		points(X_r,typ='b',col=r%%10,xlim=c(0,1),ylim=c(0,1))
	}
}


#######################################
# Tests
# => comprehension nature trajectoires
# => effets nb traj
# => visu nb niveaux, jump
plot_morris_Keq2(5,10,5)
plot_morris_Keq2(5,10,3)
plot_morris_Keq2(50,10,3)


#########################################
## dimension >2

# fonction de visualisation partielle du plan
panel.hist <- function(x, ...)
     {
         usr <- par("usr"); on.exit(par(usr))
         par(usr = c(usr[1:2], 0, 1.5) )
         h <- hist(x, plot = FALSE)
         breaks <- h$breaks; nB <- length(breaks)
         y <- h$counts; y <- y/max(y)
         rect(breaks[-nB], 0, breaks[-1], y, col = "grey", ...)
     }


plot_morris_K = function(K=5,nb_r = 50,nb_l = 10,delta = 5)
{
	set.seed(123)
	x <- morris(model = NULL, factors = K, r = nb_r,design = list(type = "oat", levels = nb_l, grid.jump = delta)) 
	# dimension du plan : r * (k+1) 
	print(dim(x$X))
	x11()
	pairs(x$X,diag.panel=panel.hist)
}

##########################
# Tests
# => effets nb parametres
# => effet taille jump
plot_morris_K(5,50,10,4)
plot_morris_K(5,50,10,7)
plot_morris_K(8,50,10,4)
plot_morris_K(8,50,10,7)




##################################################################
## Methode de Morris sur le modele volcan
##################################################################


## On va s'interesser au comportement du modele aux points d'observation

Loc.sites = data.frame( Glb_xi, Glb_yi, Glb_zi)
Loc.sites = Loc.sites[order(Glb_zi),]

loc.sites = Loc.sites[round(seq(1,length(Glb_zi), length.out=5)),]
loc.sites = as.matrix(loc.sites)

##########################
# Tests
# => Visualisation du plan
# => effet de jump

set.seed(2018)
res.morris.loc1 = morris( NULL, factors = names(n2i), r = 30, binf=xmin, bsup=xmax,
  design = list(type = "oat", levels = 5, grid.jump = 3))
pairs(res.morris.loc1$X,diag.panel=panel.hist)
set.seed(2018)
res.morris.loc1 = morris( NULL, factors = names(n2i), r = 30, binf=xmin, bsup=xmax,
  design = list(type = "oat", levels = 5, grid.jump = 2))
pairs(res.morris.loc1$X,diag.panel=panel.hist)

##########################
# Analyse du modele volcan en certains points par la methode de Morris
# Attention, la fonction compute_normUcalc_xyz fait d'elle même le passage de (0-1) à (xmin-xmax)

# Tests
# => appel avec la fonctions modele avec entrées normalisées (bornes 0,1)
# => variabilité des resultats selon des points d'observations
# => interpretation des sorties mu*,sigma

##
set.seed(2018)
res.morris.loc1 = morris( compute_normUcalc_xyz, factors = names(n2i), r = 30, binf=0, bsup=1, 
  design = list(type = "oat", levels = 5, grid.jump = 2), px = loc.sites[1,] )
plot(res.morris.loc1)

##
set.seed(2018)
res.morris.loc5 = morris( compute_normUcalc_xyz, factors = names(n2i), r = 30, binf=0, bsup=1,
  design = list(type = "oat", levels = 5, grid.jump = 2), px = loc.sites[5,] )
plot(res.morris.loc5)


## => appel de l'AS en 2 temps (instruction 'tell')
res.morris.loc = res.morris.loc1
res.morris.loc$y = NULL

y = apply(res.morris.loc$X, 1, function(v) { compute_normUcalc_xyz(v,px=loc.sites[2,]) })
res.morris.loc2= tell(res.morris.loc,y)

y = apply(res.morris.loc$X, 1, function(v) { compute_normUcalc_xyz(v,px=loc.sites[3,]) })
res.morris.loc3 = tell(res.morris.loc,y)

y = apply(res.morris.loc$X, 1, function(v) { compute_normUcalc_xyz(v,px=loc.sites[4,]) })
res.morris.loc4 = tell(res.morris.loc,y)


res.morris.loc1 ; res.morris.loc2 ; res.morris.loc3 ; res.morris.loc4 ; res.morris.loc5


####################################################################
#############       Passage a Sobol'sur modele analytique ##########
####################################################################
## On va faire quelques tests avec une fonction simple y= x1*x2 + x3
## 

monmodele = function(x) { x[,1]*x[,2] + x[,3] }

set.seed(2034)
# resultat fast : intervalles [0,1]x[0,1]x[0,1]
res.fast99 = fast99(model = monmodele, factors = LETTERS[1:3], n = 1000, M=4, omega=NULL, q = rep("qunif",3),
                    q.arg = list(min=0,max=1))
plot(res.fast99)

# resultat fast intervalles [-1,1]x[0,1]x[0,1]
res.fast99 = fast99(model = monmodele, factors = LETTERS[1:3], n = 1000, M=4, omega=NULL, q = rep("qunif",3),
                    q.arg = list(list(min=-1,max=1),list(min=0,max=1),list(min=0,max=1)))
plot(res.fast99)

# resultat fast intervalles [0,1]x[0,10]x[0,1]
res.fast99 = fast99(model = monmodele, factors = LETTERS[1:3], n = 1000, M=4, omega=NULL, q = rep("qunif",3),
                    q.arg = list(list(min=0,max=1),list(min=0,max=10),list(min=0,max=1)))
plot(res.fast99)

# resultat fast intervalles [0,1]x[0,1]x[0,10]
res.fast99 = fast99(model = monmodele, factors = LETTERS[1:3], n = 1000, M=4, omega=NULL, q = rep("qunif",3),
                    q.arg = list(list(min=0,max=1),list(min=0,max=1),list(min=0,max=10)))
plot(res.fast99)


#######

## Vérifier sur les calculs theoriques ...



####################################################################
#############       Sobol' sur volcan ##############################
####################################################################
## On va utiliser soboljansen
## On utilise toujours la normalisation à l interieur de compute_normUcalc_xyz

set.seed(2020)
X1 = improvedLHS(1000,5,dup=3)
X2 = improvedLHS(1000,5,dup=3)

dimnames(X1) = dimnames(X2) = list(NULL,names(n2i))

# Test
# => methode de Sobol sur plusieurs points observes
res.soboljansen.1 = soboljansen(model = compute_normUcalc_xyz, X1, X2,  nboot=30, conf = 0.95, px = loc.sites[1, ])
res.soboljansen.1
plot(res.soboljansen.1)
res.soboljansen.5 = soboljansen(compute_normUcalc_xyz, X1, X2, nboot=30, conf = 0.95, px = loc.sites[5, ])
res.soboljansen.5
plot(res.soboljansen.5)


##################
# Test
# => methode de Sobol incluant la localisation des points observes comme facteur

dim(Loc.sites)
Z1 = cbind.data.frame(X1,px = sample(1:220, 1000, replace=TRUE))
Z2 = cbind.data.frame(X2,px = sample(1:220, 1000, replace=TRUE))

summary(c(table(factor(Z1$px))))

res.sj.tous = soboljansen(model=NULL, Z1, Z2, nboot=30, conf = 0.95)

toto = function(v) {
 num = round(v[6])
 pxs = unlist(Loc.sites[ num, ])
 compute_normUcalc_xyz( v[1:5], px = pxs) 
}

y = apply(res.sj.tous$X, 1, toto )

res.sj.tous = tell(res.sj.tous,y)

plot(res.sj.tous)

plot(cbind.data.frame(res.sj.tous$X[,6],res.sj.tous$y))
plot(cbind.data.frame(res.sj.tous$X[,5],res.sj.tous$y))
plot(cbind.data.frame(res.sj.tous$X[,4],res.sj.tous$y))


# Si vous voulez le faire avec fast99
# ####################################################################
# #############       Passage a Fast99  ##############################
# ####################################################################
# 
# 
# res_zmax=fast99(model = compute_normUcalc_xyz, factors = names(n2i), n = 1000, M=4, omega=NULL, q = rep("qunif",5), q.arg = list(min=0,max=1), px=loc.sites[5, ])
# 
# plot(res_zmax)
# 
# 
# res_ztous = fast99(model = NULL, factors = c(names(n2i),"px"), n = 1000, M=4, omega=NULL, 
#  q = rep("qunif",6),  q.arg = list(min=0,max=1))
# 
# 
# ####################################################################
# #############         Fin de Fast99   ##############################
# ####################################################################
# 
