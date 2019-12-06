---
title: "Journal du MOOC RR"
author: "Sébastien Guyader"
date: "22-24 octobre 2018"
output: html_document
---
------------

## Module 1

### Ce que je retiens

- La partie historique était intéressante. J'ai surtout apprécié la partie concernant l'**indexation**, et les méthodes et outils s'y rapportant.
- Je connaissais déjà le langage `Markdown`, pour l'avoir utilisé sur d'autres plateformes collaboratives (github, pixls.us) et sous RStudio avec `Rmarkdown` pour les rapports de travaux.

### Remarques

J'ai démarré une [discussion](https://www.fun-mooc.fr/courses/course-v1:inria+41016+session01bis/discussion/forum/c309fd5170d67bde98ab305f18f063d9a55554f8/threads/5bce23551c89dc02ae005933) concernant la première question du Quizz 1 : je pense que la réponse "outils numériques" devrait être considérée comme valide, car cet outil est clairement cité dans [l'une des vidéos](https://d381hmu4snvm3e.cloudfront.net/videos/ZXRQTwenN9LW/SD.mp4) par l'historienne interviewée.

## Module 2

### Parties 1 à 3
- Sur les parties 1 à 3, rien à signaler, mais OrgMode a piqué mon intérêt.
- Je suis habitué à RStudio, mais pour la première fois je l'ai interfacé avec gitlab.
- *Note* : malgré la création de la paire de clés `SSH`, RStudio ne m'a pas permis de cloner mon dépôt *via* `SSH` (message d'erreur lié à l'authentification). Cela fonctionnait en `https`. Pour pouvoir fonctionner en `SSH`, j'ai dû d'abord cloner en ligne de commande (`git clone`) puis une fois le dépôt local créé j'ai pu travailler et réaliser les `commit` et `push` *via* `SSH` depuis `RStudio.
- J'ai installé emacs et `OrgMode`. Je vais tenter de suivre ce
  module en plus du module RStudio.

### Partie 4
- Je me suis exercé avec `OrgMode` et `magit`, et j'ai pu faire des
  modications et commiter les modifications directement à partir
  d'`OrgMode` (après quelques tatonnements, notamment parceque j'avais
  oublié de faire un `git pull` après avoir fait des modifications sur
  gitlab.
- Je suis en train de taper ces lignes du journal grâce au mode
  `markdown` sur emacs !

### Exercices
- J'ai fait le premier exercice en `rmarkdown` et aussi en `org-mode`
- Pour les exercices suivants, j'ai tout fait sous RStudio (je préfère l'interface git sous RStudio que sous org-mode, pour débuter)

### Exo 5
- J'ai soumis mas solution au prblème de la navette Challenger (exo 5)
- Note : je trouve l'interface `git` très pratique et assez intuitive sous RStudio

### Hôtes de service git
Je me pose une question quand aux différents services `git` existant (Gitlab, Github, Bitbucket...). Lequel est le mieux adapté à un labo de recherche publique ? D'un côté, il devient nécessaire d'ouvrir les résultats de nos recherches et nos analyses quand elles sont financées par des fonds publics, ce qui est le cas avec un compte git "public", d'un autre côté pour certains cas on pourrait vouloir ne pas laisser les données entièrment publiques, et souhaiter pouvoir contrôler les permissions y compris en lecture sur ces données. Dans ce cas, sous Gitlab, peut-on gérer les permissions assez finement, sans avoir à payer pour un compte, dans le cas où notre institution ne serait pas cliente de Gitlab ?

## Module 3
