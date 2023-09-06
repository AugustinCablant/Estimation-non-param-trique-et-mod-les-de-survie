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

$\theta_d(t|t_s,x)= \lambda_d \phi_d(x)$ si  $t \le t_s$ 
$\theta_d(t|t_s,x)= \lambda_d \phi_d(x) \delta$ si $t >t_s$

et

$\theta_s(t|x)=\lambda_s \phi_s(x).$

On a donc fait l'hypothèse que l'hétérogénéité inobservée est absente et que les durées $T_d|T_s,X$ et $T_s|X$ suivent des lois exponentielles (les taux de hasard de base $\lambda_d(t)$ et $\lambda_s(t)$ ne dépendent pas de $t$). La contribution $L_i^{seller}$ s'écrit alors comme
$L_{id}^{seller}&=\frac{f_d(t_{id}^{seller}|t_{is}^{seller},x_i)f_s(t_{is}^{seller}|x_i)}{Pr(\tau_{begin}<\tau_{i,birth}+T_{id}^{seller}<\tau_{end}|X_i=x_i)}\\
&=\frac{f_d(t_{id}^{seller}|t_{is}^{seller},x_i)f_s(t_{is}^{seller}|x_i)}{\int \left[F_d(\tau_{end}-\tau_{i,birth}|t_s,x_i)-F_d(\tau_{begin}-\tau_{i,birth}|t_s,x_i) \right] f_s(t_s|x_i)dt_s} \\
&=\frac{\lambda_d \phi_d(x_i) \delta e^{-\phi_d(x_i)I_d(t_{id}^{seller},t_{is}^{seller})} \lambda_s \phi_s(x_i) e^{-\phi_s(x_i)I_s(t_{is}^{seller})}}{\int \left[e^{-\phi_d(x_i)I_d(\tau_{begin}-\tau_{i,birth},t_s)}-e^{-\phi_d(x_i)I_d(\tau_{end}-\tau_{i,birth},t_s)}\right] \lambda_s \phi_s(x_i) e^{-\phi_s(x_i)I_s(t_s)} dt_s}$ et la contribution du clone s'écrit comme $L_i^{clone}&= \int_{t_{id}^{clone}} ^{\infty} \frac{f_d(t_{id}^{clone}|u,x_i)f_s(u|x_i)}{Pr(\tau_{contract}<\tau_{i,birth}+T_{id}^{seller}<\tau_{end}|X_i=x_i)}du \\
&=\frac{\lambda_d \phi_d(x_i)e^{-\phi_d(x_i)I_d(t_{id}^{clone})}e^{-\phi_s(x_i)I_s(t_{id}^{clone})}}{\int \left[e^{-\phi_d(x_i)I_d(\tau_{contract}-\tau_{i,birth},t_s)}-e^{-\phi_d(x_i)I_d(\tau_{end}-\tau_{i,birth},t_s)}\right] \lambda_s \phi_s(x_i) e^{-\phi_s(x_i)I_s(t_s)} dt_s}.$

Comme ci-dessus on spécifie $\phi_j(x_i)=exp(x_i'\beta_j)$, $j=d,s$, où $x_i'\beta_j=x_{i1}\beta_{j1}+x_{i2}\beta_{j2}+ ...+x_{iK}\beta_{jK}$, avec $K$ le nombre de variables explicatives.

Voici le lien vers le code : [estimation](https://github.com/AugustinCablant/Estimation-non-param-trique-et-mod-les-de-survie/blob/main/Vraisemblance/vraisemblance_mod%C3%A8le_simple.py) 
<picture>
 <source media="(prefers-color-scheme: dark)" srcset="https://github.com/AugustinCablant/Estimation-non-param-trique-et-mod-les-de-survie/blob/main/Vraisemblance/resultats.png">
 <source media="(prefers-color-scheme: light)" srcset="https://github.com/AugustinCablant/Estimation-non-param-trique-et-mod-les-de-survie/blob/main/Vraisemblance/resultats.png">
 <img alt="Un rapide aperçu" src="https://github.com/AugustinCablant/Estimation-non-param-trique-et-mod-les-de-survie/blob/main/Vraisemblance/resultats.png">
</picture>
Avec : 

$\beta _{d0}$ (resp. $\beta _{s0}$) est le paramètre devant $1 _{\text{sexe femme}}$  

$\beta _{d1}$ (resp. $\beta _{s1}$) est le paramètre devant $1 _{\text{idf}}$ 

$\beta _{d2}$ (resp. $\beta _{s2}$) est le paramètre devant $1 _{\text{étranger}}$ 

$\beta _{d3}$ (resp. $\beta _{s3}$) est le paramètre devant $1 _{\text{dec1}}$ 

$\beta _{d4}$ (resp. $\beta _{s4}$) est le paramètre devant $1 _{\text{dec2}}$ 

$\beta _{d5}$ (resp. $\beta _{s5}$) est le paramètre devant $1 _{\text{dec3}}$ 

