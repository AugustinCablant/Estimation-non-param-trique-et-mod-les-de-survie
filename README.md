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
i) Estimation des **fonctions de survie** : ![Cover]([Fonction de survie/Résultats/survival_function.png](https://github.com/AugustinCablant/Estimation-non-param-trique-et-mod-les-de-survie/blob/main/Fonction%20de%20survie/R%C3%A9sultats/survival_function.png))


ii) Pour l'**estimation des paramètres** dans le cadre le plus simple (on suppose que $var(V_d)=var(V_s)=0$ et $\delta(t,t_s,x)=\delta$). <br>
Voici le lien vers le code : [estimation](https://github.com/AugustinCablant/Estimation-non-param-trique-et-mod-les-de-survie/blob/main/Vraissemblance/vraissemblance_code.py) <br>
<br>
| Parameter |                      Value                      |  <br>
+-----------+-------------------------------------------------+  <br>
|  lambda_d |               -30.659550125563594               |  <br>
|  lambda_s |                25.039506999029843               |  <br>
|  beta_d0  |               -30.522912675901814               |  <br>
|  beta_d1  |                34.810007017242235               |  <br>
|  beta_d2  |                -49.33468615756241               |  <br>
|  beta_d3  |                -49.62839881472315               |  <br>
|  beta_d4  |                49.848439687168714               |  <br>
|  beta_d5  |               -23.887138433918633               |  <br>
|  beta_d6  |                 33.7017337661741                |  <br>
|  beta_d7  |                26.843703414465196               |  <br>
|  beta_d8  |                -42.71868655709029               |  <br>
|  beta_s0  |                -38.58476194868536               |  <br>
|  beta_s1  |                30.242940723686417               |  <br>
|  beta_s2  |                -13.86566972508601               |  <br>
|  beta_s3  |                -1.734455518165402               |  <br>
|  beta_s4  |                32.45084254283722                |  <br>
|  beta_s5  |                16.67260447570751                |  <br>
|  beta_s6  |                -35.28304594855922               |  <br>
|  beta_s7  |                25.40490181568113                |  <br>
|  beta_s8  |                29.087471212992604               |  <br>
|   delta   |                20.357750621094294               |  <br>
|  Success  |                       True                      |  <br>
|  Message  | CONVERGENCE: REL_REDUCTION_OF_F_<=_FACTR*EPSMCH |  <br>
+-----------+-------------------------------------------------+  <br>
