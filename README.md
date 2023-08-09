# Stage

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
Ce répertoire est composé d'un fichier 'estimation fonction de sruvie' qui effectue dans un premier temps une liste de statistiques descriptives portant sur les vendeurs et les clones du fichier principal et qui ensuite estime la fonction de survie des vendeurs grâce aux estimateurs de Kaplan Meier et de Cox. 

D'un deuxième fichier "Fct_vraisemblance" qui a pour but d'écrire et maximiser la fonction de vraisemblance. 





# Modèle développé 
## On commence par des notations : 

$T_d \equiv$ la durée de vie (en jours) d'une personne 

$T_s \equiv$ l'âge (en jours) au moment où la personne vend son bien en viager

$X \equiv$ un vecteur contenant les déterminants observables de $T_s$ et $T_d$ (genre, jour de naissance, département de naissance, etc.)

$\tau_{birth} \equiv$ la date de naissance de l'individu

$\tau_{contract} \equiv$ la date de la vente en viager

$\tau_{end} \equiv$ la date de la fin d'observation des fichiers INSEE (01-07-2023)

$V_d \equiv$ une variable captant l'effet des déterminants inobservables de $T_d$ (état de santé, richesse, niveau d'éducation)

$V_s \equiv$ une variable captant l'effet des déterminants inobservables de $T_s$ (potentiellement les mêmes variables que celles captées par $V_d$, une variable indiquant si la personne a des enfants, une autre variable indiquant si elle a perdu son conjoint, etc.)

$V \equiv (V_d,V_s)$

Notons que la variable $T_s$ est seulement observée pour un vendeur en viager, elle ne l'est pas pour son clone (dans ce cas on sait seulement que $T_s>T_d$). Le but est maintenant de modéliser la distribution jointe de $(T_d,T_s)$ conditionnellement à $X$ et $V$, ou bien, d'une manière équivalente, la distribution de $T_d$ conditionnellement à $T_s$, $X$ et $V$, et la distribution de $T_s$ conditionnellement à $X$ et $V$. 

## Quelques définitions : 

Une des caractéristiques des données de survie est l'existence d'observations incomplètes.
En effet, les données sont souvent recueillies partiellement, notamment, à cause des processus de **censure** et de **troncature**. Les données censurées ou tronquées proviennent du fait qu'on n'a pas accès à toute l'information : au lieu d'observer des réalisations indépendantes et identiquement distribuées (i.i.d.) de durées $T$, on observe la réalisation de la variable $T$ soumise à diverses perturbations, indépendantes ou non du phénomène étudié. 

En probabilité et en statistique, une loi **tronquée** est une loi conditionnelle, dérivée d'une autre loi de probabilité, où l'on ne garde que les tirages sur un intervalle défini. 
Soit $X$ une variable aléatoire réelle de fonction de répartition $F$, admettant une densité par rapport à la mesure de lebesque. On note 
$ f := \frac{ dP_{x} }{d \lambda _{LEB}} $. 
X est tronquée par un sous ensemble éventuellement aléatoire $A$ de $ \mathbb{R}^{+} $ si on observe uniquement $ X | X \in A $. Les points de l'échantillon tronqué appartiennent tous à $A$,et suivent donc la loi de $X$ conditionnée par $X \in A$. 
\bigskip 

\noindent \textbf{Pour notre modèle} : 
Dans notre cas, en notant $T_{i}$ la durée de vie du vendeur (en jours) $i$, $T_{s,i}$ l'âge auquel la personne vend son bien en viager (en jours) et $T_{birth,i}$ la date à laquelle le vendeur en viager naît. 
L'individu $i$ est observable si et seulement s'il est décédé avant la fin de l'observation (01-07-2023) $\tau_{end}$ et qu'il a vendu son bien avant de décéder. On l'observe uniquement à partir du moment où le contrat a été signé. 
Autrement dit : $T_{birth,i} + T_i \leq \tau _{end}$ et $T_{s,i} \leq T_{birth,i} + T_i $. 
L'ensemble de troncature noté $T$ est : $T := \{ i : T_{s,i} - T_{birth,i} \leq T_{i} \leq \tau _{end} - \tau_{birth , i} \}$. 
Ainsi, $T_{i}$ est tronquée à gauche et à droite.




