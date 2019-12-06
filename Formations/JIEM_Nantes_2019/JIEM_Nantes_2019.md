# JIEM 2019 - Ifremer - Nantes - 22 et 23 octobre 2019

## 1. Introduction : Réseaux

  - GDR EcoStat (S. Dray)
   - objectifs : faire reconnaitre écologie statistique comme
    discipline, évaluer, développer et appliquer méthodes
    nouvelles
   - projet intial déposé en 2013 au CNRS ; renouvelé en 2017
   - plus de 700 inscrits sur la liste
   - convention CNRS + Ifremer
   - site web, liste de diffusion, bourse d'échanges,
    réunions annuelles, gratifications M2
   - 9 et 10 mars 2020 : 5èmes journées annuelles (Rennes)
  - GDR MascotNum (A. Nouy)
   - méthodes stochastiques pour l'analyse de codes
    numériques
   - objectis : expériences numériques, quantification de
    l'incertitude,
   - créé en 2006, dirigé par H. Monod
   - partenaires académiques et non (10 companies)
   - plus de 400 inscrits sur la liste
   - fonds du CNRS + partenaires industriels (\"club des
    partenaires)
   - plusieurs événements annuels selon thématiques
  - Mexico (R. Faivre)
   - créé en 2006
   - objectifs : animation scientifique autour des méthodes,
    inciter scientifiques à analyse statitique de leurs
    simulations
   - écoles chercheurs, journées thématiques, Rencontres
    Mexico
   - ouvrage édité en 2013 \"Analyse de sensibilité et
    explration de modèles\" (Quae)
  - Réseau RESSTE (T. Opitz)
   - fondé en 2014
   - environ 70 membres
   - financé par INRA dpt MIA
   - modèles, méthodes et algorithmes pour les données
    spatiotemporelles
   - article de revue \"Analyszing spatio-temporal data...\"
    journal de la Société Française de Staistiques

## 2. Session 1 : Analyse des structures spatiales multivariées...

####  Analyse des structures spatiales multivariées en écologie des communautés avec le package \"adespatial\" (Stéphane Dray, Laboratoire de Biométrie et Biologie Evolutive)

  - étude des communeautés écologiques (assemblages d'individus
   de différentes espèces), comment cette composition varie
   dans le temps et l'espace, identifier les facteurs ou
   processus à \'origine de ces variations
  - principe : abondance d'espèces dans différents sites,
   mesure de différentes variables -\> différents tableaux de
   données
  - initialmeent, on pratiquait des analyses type PCA, COA...
  - on ajoute les paramètres écologiques décrivant un filtrage
   environnemental -\> niches écologiques : tableau espèces +
   tableau environnement -\> RDA, CCA...
  - problème : il peut manquer des données ou variables,
   d'autres processus non mesurés peuvent agir -\> utiliser
   les structures spatiales comme proxy (variables latentes)
  - on ajoute la structure spatiale de manière explicite
  - package \"adespatial\" sur R : lien packages
   \"multivariate\" et \"spatial\"
  - l'espace est traité comme un graphe -\> autocorrélation
   spatiale (indice de Moran), décomposition positive/négative,
   décomposition multiéchelle
  - méthodes multivariées spatiales

####  Méthode pour des suivis environnementaux plus efficaces (Claire Kermorvant, CNRS / UNIV PAU)

  - exemples : inventaires naturalistes, études d'impacts
   environnementaux, évaluer périodiquement des mesures de
   gestion
  - théorie : basé sur modèle, ou sur protocole probabiliste
  - résultats pour la palourde en Arcachon : il faut toujour une
   taille d'échantillonnage supérieure pour échantillonnage
   simple aléatoire, que si on utilise une méthode
   d'optimisation spatiale (par exemple par tessellation)

## 3. Session 2 : statistiques spatiotemporelles

####  Multiple Factor Analysis applied to the spatio-temporal analysis of fish habitats (Mathieu DORAY, IFREMER)

  - analyses multivariée (MFA) pour description des habitats
  - analyse de séries temporelles de cartes
  - individu = cellule d'une grille spatiale ; groupe = année ;
   variables multiples dans chaque cellule
  - MFA poissons, MFA environnement, puis inférence entre les
   deux
  - l'autocorreléation n'est pas modélisée explicitement, mais
   on voit l'autocorrélation quand on replace les cellules sur
   une carte

####  Guiding decision-making to mitigate lynx-vehicle collisions using spatially-explicit individual-based models (Sarah Bauduin, Centre d'Ecologie Fonctionnelle et Evolutive Montpellier)

  - modèles individu-centrés spatiallement explicites
  - les collisions sont le principal facteur de mortalité des
   lynx en France
  - dévelopé avec NetLogoR
  - interface utilisateur simplifiée pour que les acteurs
   puissent tester eux-mêmes les odifications de
   l'environnement et faire tourner le modèle

####  Application de modèles dynamiques bayésiens aux séries temporelles de surveillance de l'environnement marin (Dominique Soudant, VIGIES)

  - généralisation dynamique des modèles linéaires : les
   paramètres peuvent varier dans le temps
  - permet de ne plus avoir à adapter les données aux méthodes
  - permet d'identifier des ruptures dans les variables
   mesurées

####  Spatial and temporal variability of Sabellaria alveolata bioconstructions: from multi-scale monitoring to statistical modelling to disentangle drivers of changes (Aurélien Boyé, IFREMER)

####  Modélisation hiérarchique spatiotemporelle et inférence avec INLA (Thomas Opitz, Biostatistique et Processus Spatiaux)

  - INLA : integrated nested laplace approximation ; pour
   modèles généralisés additifs
  - SPDE : stochastic partial derivative equation : pour estimer
   les effets aléatoires spatiotemporels (erreur)
  - INLA est dans un cadre bayésien : le prédicteur linéaire
   (covariables prédictives + erreur) est une gaussienne
   multivariée
  - package R-INLA : r-inla.org

####  Approche INLA/SPDE de la distribution des juvéniles de poissons plats dans la zone de nourricerie de Baie de Seine (Thibault CARIOU, Ifremer)

####  Impacts de dynamiques territoriales sur la diversité aviaire en méditerranée - Une analyse spatiale à grande échelle (Julien Papaïx / Emily Walker, INRA BioSP)

  - première partie de l'étude : regarder les trajectoires
   d'évolution agricole dans le bassin méditerranéen à
   résolution de 2km par typologie : intensification,
   extensification, sous pression urbaine, abandon de surface
  - deuxième partie : effet de ces changements sur diversité des
   oiseaux
  - 4 indices de diversité : rihesse spécifique, richesse
   fonctionnell, dispersion fonctionnelle, functionnal evenness
  - sur 355 espèces d'oiseaux, à la résolution de 110 km
  - données additionnelles : élévation, paramètres
   bioclimatiques
  - en passant par des ACP
  - développement de modèles GLMM spatiaux avec INLA-SPDE
  - modèle 1 : sans land system, avec juste fonction de
   correlation pour le spatial
  - modèle 2 : le land system est explicitement ajouté

## 4. Session 3 : Incertitude

####  High-dimensional approximation for uncertainty and parametric analyses (Anthony Nouy, École Centrale de Nantes)

  - approximation du modèle *X* : surrogate model plus faciles à
   opérer (évaluation, intégration, dérivation)
  - approximation en hautes dimensions :
  - approximations de faible rang et formats de tenseurs

####  Approche probabiliste pour la prédiction d'ondes de tempête (Pierre Sochala)

  - modèle de propagation acoustique à 9 paramètres
  - AS avec méthode de Morris (faire varier les paramètres avec
   des valeurs discrètes : min, val1, val2, max)

####  Modélisation des incertitudes de la prévision du bruit dans l'environnement - Application au bruit des éoliennes (Bill Kayser, Unité Mixte de Recherche en Acoustique Environnementale)

## 5. Session 4 : Analyse de Sensibilité - Calibration

####  Guide pour la pratique de l'optimisation de modèles complexes (Stéphanie Mahévas, Ifremer)

  - calibration : c'est quoi ?
   - en stats, méthode d'estimation (méthode inverse) :
    chercher valeurs de X permettant de rerpoduire au mieux
    Y
   - calbbration d emodèle : ajustement de paramètres en
    intégrant l'incertitude des paramètres et/ou du modèle
    pour obtenir une représntantion du modèle
   - le cas le plus simple : ajustement linéaire Y=aX
   - objectif : estimer des paramètres qu\'on n'arrive pas à
    estimer réellement, ou trouver un meilleur modèle, ou
    donner plus de crédibilité au modèle en soutien à la
    prise de décision
  - calibration : comment ?
   - comparer par un critère Ysim et Yobs : on cherche les
    valeurs de X (paramètres) qui minimisent la différence
    par un processus itératif
   - 3 grandes étapes :
    - pré-processing : construire une fonction objectif
    - choix de l'algorithme
    - post-processing : évaluer la qualité de
     l'optimisation

####  OpenMOLE pour l'étude des modèles systèmes complexes (Mathieu Leclaire, Institut des sytèmes complexes de Paris Île de France / Centre de recherche en èpistémologie appliquée)

  - exemple : modèle \"zombieland\"
  - plateforme pour calibrer touts types de modèles

####  Elucidation et conséquences de la dynamique spatiale de la sole de Manche-est : Calibration, validation et analyse de sensibilité de modèles alternatifs de pêcherie (Sigrid Lehuta, IFREMER)

  - 3 versions d'un modèle avec différentes assumptions de
   structure (1 population, 1 métapopulation, ou 3 populations
   différentes ?)
  - calibration du modèle ISIS Fish avec le paquet R
   \"calibrar\"
  - puis validation

####  Améliorer la confiance dans les modèles d'écosystèmes complexes : l'analyse de sensibilité d'un modèle d'écosystème Atlantis (Chloe Bracis, IFREMER)

  - modèle end-to-end : hydrographie + ecologie + facteurs
   humains
  - AS par méthode de Morris (globale, one-at-a-time)

  Assessing the stability of marine systems when species feed on
  fisheries catches: a qualitative modelling approach (Lyndsay
  Clavareau, Ifremer)

  - impact de la déprédation dan les modèles écologiques

## 6. Session 5 : Dynamique de population

####  Utilisation de la modélisation pour l'analyse de séries temporelles hétérogènes: cas du lièvre d'Europe en France (Ilona Grentzmann, Direction de la Recherche et Expertise, ONCFS)

  - etudier l'effet de la structure d'âge sur la dynamique de
   reproduction du lièvre
  - state-space modèle : processus d'observation est lié à un
   processus biologique caché lié au temps (chaîne de Markov
   cachée)

####  Résultats préliminaires sur les relations entre un indice d'abondance issu des données SACROIS et de variables environnementales (Claire Kerorvant, Ifremer)

  - effet du changement climatique sur le rouget en golfe de
   Gascogne
  - données issues de navires de pêche sur la présence de
   rougets dans les filets, dans l'espace et le temps
  - covariables environnementales (nutriments et production
   primaire)
  - modèles GAMM avec effet aléatoire sur chaque maille de la
   carte (effet spatial), décomposition de Fourier pour la
   saisonalité

####  Relative contribution of processes involved in the fish community response to climate change (Morgane Travers-Trolet, Ifremer)

  - contribution relative de différents processus écologiques
  - lesquels sont plus impactés par le changement climatique ?
  - modèle individu centré spatialisé OSMOSE
  - calibré avec \"calibrar\"
  - 2 projections climatiques testées : RCP4.5 et 8.5

####  Accounting for non-stationarity in epidemiology using stochastic models with time-varying parameters (Bernard Cazelles, CNRS/IRD)

  - prendre en compte la non stationarité
  - non stat. due aux changements et évolution dans les
   caractéristiques des pathogènes, et dépendance à
   l'environnement et aux actions humaines
  - approche statistique : utiliser décomposition en ondelettes
   (domaine fréquences et temporel)
  - reconstruction de l'évolution dans le temps de certains
   paramètres avec space time model
  - processus système : SIR avec beta(t) et gamma(t)
  - processus d'observation
  - inférence avec filtre de Kalman étendu
  - particle diffusion (SMC) et MCMC
  - Cf. Cazelles (2018) PLoS Computational Biology

####  Méthode statistique génétique pour inférer la dynamique spatio-temporelle d'une population (Verena Trenkel, Ifremer)

  - modèle de biomasse Bayésien
  - estimation de l'abondance par la proportion de juvéniles et
   adultes apparentés génétiquement
  - package R \"kinference\"
