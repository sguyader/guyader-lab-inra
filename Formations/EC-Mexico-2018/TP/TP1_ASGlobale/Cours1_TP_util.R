
#################################################
#################################################

# calcul de la distance du vecteur deplacement en un point px=(x,y,z) pour les parametres incertains xx

norm_Ucalc_xyz <- function(xx,px){
  #
  # Les entrees xx et px doivent etre des vecteurs de taille 5 pour xx et 3 pour p
  #
  G = 2000 # Shear modulus in MPa
  nu = 0.25 # Poisson's ratio
  nlos = c(-0.664,-0.168,0.728) # vector of direction of line of sight (satellite)
  #
  n2i <- Glb_var$n2i
  
  # Compute surface displacements at xi,yi,zi
  U <- mogi_3D(G,nu,xx[n2i$xs],xx[n2i$ys],xx[n2i$zs],xx[n2i$a],xx[n2i$p],
               px[1],px[2],px[3])
  # norm of displacement
  #  u_norm <- U$x*U$x + U$y*U$y + U$z*U$z
  u_norm <- sqrt(sum(unlist(U)**2))
  names(u_norm)="distance"
  return(u_norm)  
}


# encapsulation vectorisation des appels
compute_normUcalc_xyz <- function(x,px) {
  # nbvar = 5
  if (is.null(dim(x))) {
    x <- matrix(data = x, nrow=1)
  }  else if (ncol(x)!=nbvar) {
    x <- t(x)
  }
  # passage d'un intervalle (0,1) a un intervalle (min,max)
  xx <- unnorm_var(Xnorm=x)
  y <- apply(xx, 1, norm_Ucalc_xyz,px)
  return(y)
}

#################################################
################################################

