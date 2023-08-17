# Stage au Centre de Recherche pour l'Économie et la Statistique (CREST)

Stage effectué en première année à l'ENSAE.

L'objectif est de déterminer si les vendeurs de biens en viager vivent plus longtemps que le reste de la population. 
Pour faire cela, nous avons effectuer de l'analyse de survie et des statistiques non paramétriques. En l'occurrence, j'ai été amené à utiliser l'estimateur de Kaplan Meier et de Cox. 

## Trois étapes dans le stage : 
- Constitution de la base de données et statistiques descriptives 
- Estimation de la fonction de survie 
- Écriture de la fonction de vraisemblance 

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

## Logiciels utilisés : 
- Python 
- R 
- Stata
- SAS 

## Structure du répertoire
Ce répertoire est composé de : 

- Un dossier construction de base peu pertinent. Il s'agit avant tout de clean la base de données. 

- Un dossier Data avec le fichier csv sur lequel nous avons travaillé pour calculer la fonction de vraissemblance. L'autre fichier csv est trop gros. 

- Un dossier 'Fonction de survie' où l'on trouve un fichier 'estimation fonction de sruvie' qui effectue dans un premier temps une liste de statistiques descriptives portant sur les vendeurs et les clones du fichier principal et qui ensuite estime la fonction de survie des vendeurs grâce aux estimateurs de Kaplan Meier et de Cox. 

- D'un dossier "vraissemblance" qui a pour but d'écrire et maximiser la fonction de vraisemblance.

## Principaux résultats : 
i) Estimation des **fonctions de survie** : 
<picture>
 <source media="(prefers-color-scheme: dark)" srcset="https://github.com/AugustinCablant/Estimation-non-param-trique-et-mod-les-de-survie/blob/main/Fonction%20de%20survie/R%C3%A9sultats/survival_function.png">
 <source media="(prefers-color-scheme: light)" srcset="https://github.com/AugustinCablant/Estimation-non-param-trique-et-mod-les-de-survie/blob/main/Fonction%20de%20survie/R%C3%A9sultats/survival_function.png">
 <img alt="Un rapide aperçu" src="https://github.com/AugustinCablant/Estimation-non-param-trique-et-mod-les-de-survie/blob/main/Fonction%20de%20survie/R%C3%A9sultats/survival_function.png">
</picture>

ii) Pour l'**estimation des paramètres** dans le cadre le plus simple (on suppose que $var(V_d)=var(V_s)=0$ et $\delta(t,t_s,x)=\delta$). <br>
Voici le lien vers le code : [estimation](https://github.com/AugustinCablant/Estimation-non-param-trique-et-mod-les-de-survie/blob/main/Vraissemblance/vraissemblance_code.py) 
<picture>
 <source media="(prefers-color-scheme: dark)" srcset="https://github.com/AugustinCablant/Estimation-non-param-trique-et-mod-les-de-survie/blob/main/Vraissemblance/result.png">
 <source media="(prefers-color-scheme: light)" srcset="https://github.com/AugustinCablant/Estimation-non-param-trique-et-mod-les-de-survie/blob/main/Vraissemblance/result.png">
 <img alt="Un rapide aperçu" src="https://github.com/AugustinCablant/Estimation-non-param-trique-et-mod-les-de-survie/blob/main/Vraissemblance/result.png">
</picture>
