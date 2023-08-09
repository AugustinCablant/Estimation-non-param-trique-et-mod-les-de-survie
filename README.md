# Viager

Stage effectué en première année à l'ENSAE.

L'objectif est de déterminer si les vendeurs de biens en viager vivent plus longtemps que le reste de la population. 
Pour faire cela, nous avons effectuer de l'analyse de survie et des statistiques non paramétriques. En l'occurrence, j'ai été amené à utiliser l'estimateur de Kaplan Meier et de Cox. 

## Trois étapes dans le stage : 
- Constitution de la base de données et statistiques descriptives 
- Estimation de la fonction de survie 
- Écriture de la fonction de vraisemblance 

## Logiciels utilisés : 
- Python 
- R 
- Stata
- SAS 

## Structure du répertoire
Ce répertoire est composé d'un fichier 'estimation fonction de sruvie' qui effectue dans un premier temps une liste de statistiques descriptives portant sur les vendeurs et les clones du fichier principal et qui ensuite estime la fonction de survie des vendeurs grâce aux estimateurs de Kaplan Meier et de Cox. 

D'un deuxième fichier "Fct_vraisemblance" qui a pour but d'écrire et maximiser la fonction de vraisemblance. 

## Modèle développé 
On commence par des notatios : 
$T_d \equiv$ la durée de vie (en jours) d'une personne 

$T_s \equiv$ l'âge (en jours) au moment où la personne vend son bien en viager

$X \equiv$ un vecteur contenant les déterminants observables de $T_s$ et $T_d$ (genre, jour de naissance, département de naissance, etc.)

$\tau_{birth} \equiv$ la date de naissance de l'individu

$\tau_{contract} \equiv$ la date de la vente en viager

$\tau_{end} \equiv$ la date de la fin d'observation des fichiers INSEE (31-12-2022)

$V_d \equiv$ une variable captant l'effet des déterminants inobservables de $T_d$ (état de santé, richesse, niveau d'éducation)

$V_s \equiv$ une variable captant l'effet des déterminants inobservables de $T_s$ (potentiellement les mêmes variables que celles captées par $V_d$, une variable indiquant si la personne a des enfants, une autre variable indiquant si elle a perdu son conjoint, etc.)

## Compétences développées :  
- Nettoyage d'une base de donnée
- Apprentissage des bonnes pratiques avec python
- Apprentissage des bonnes pratiques avec R
- Apprentissage des bonnes pratiques avec STATA
- Manipulation de bases de données
- Statistiques descriptives
- Apprendre à présenter ses résultats
- Initiation au Machine Learning
- Utilisation de Overleaf pour gérer un projet
- Statistiques non paramétriques 



