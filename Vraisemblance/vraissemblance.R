#install.packages("bbmle")
#install.packages("Rmpfr")
#library(Rmpfr)
library(stats4)
library(bbmle)



### Chargement des données ###
donnees <- read.csv('Data/df_vraissemblance1.csv')
colnames(donnees)
### 


### Nomenclature ###
pred = c('type_libre','sexe_homme','idf','etranger','une_tete','dec1','dec2','dec3')
columns = c('type_libre','sexe_homme','idf','etranger','une_tete','dec1','dec2','dec3','tau_birth','tau_contract','Td','Ts','Td_clone','Ts_clone','tau_begin','tau_end')
###

### Les valeurs dans données sont trop grandes ### 
donnees $ tau_birth <- donnees $ tau_birth * 10^-3
donnees $ tau_contract <- donnees $ tau_contract * 10^-3
donnees $ Td <- donnees $ Td * 10^-3
donnees $ Ts <- donnees $ Ts * 10^-3
donnees $ Td_clone <- donnees $ Td_clone * 10^-3
donnees $ Ts_clone <- donnees $ Ts_clone * 10^-3
donnees $ tau_begin <- donnees $ tau_begin * 10^-3
donnees $ tau_end <- donnees $ tau_end * 10^-3
###


### Définition de quelques fonctions utiles pour calculer L_clone et L_seller ###

# beta_d et beta_s sont des vecteurs de taille 8 
phiD <- function(beta_d, donnees) {
phi <- c()
for (i in donnees$X) {
x_i <- donnees[i+1, pred]
phi_i <- exp(sum(x_i * beta_d))
phi <- c(phi,phi_i)
}
return(phi)
}

phiS <- function(beta_s, donnees) {
phi <- c()
for (i in donnees$X) {
x_i <- donnees[i+1, pred]
phi_i <- exp(sum(x_i * beta_s))
phi <- c(phi,phi_i)
}
return(phi)
}

IDD <- function(delta, lambda_d, t_1, t_2) {
if (t_1 <= t_2) {
I <- lambda_d * t_1} 
else {
I <- lambda_d * (t_2 + delta * (t_1 - t_2))}
return(I)}

ID <- function(lambda_d, t) {
I <- lambda_d * t  
return(I)}

IS <- function(lambda_s, t) {
I <- lambda_s * t
return(I)}
###



### L_clone et L_seller ### 

LSeller_i <- function(lambda_d, lambda_s, phi_d, phi_s, delta, donnees, i) {
  # numérateur
numerateur_d_exp <- exp(-(phi_d[i] * IDD(delta, lambda_d, donnees$Td[i], donnees$Ts[i])))
numerateur_d <- lambda_d * phi_d[i] * delta * numerateur_d_exp
  
numerateur_s_exp <- exp((phi_s[i] * IS(lambda_s, donnees$Ts[i])))
numerateur_s <- lambda_s * phi_s[i] * numerateur_s_exp
  
numerateur <- numerateur_d * numerateur_s


  # dénominateur 
s <- lambda_s * phi_s[i]
d <- lambda_d * phi_d[i]
t_end <- donnees$tau_end[i] - donnees$tau_birth[i]
t_begin <- donnees$tau_begin[i] - donnees$tau_birth[i]
deno <- d * (1 - delta) + s

membre_1 <- - (s * (exp(-d * (delta * t_end - (1 - delta) * t_begin)) - exp(-t_end * (d - s)))) / deno

membre_2 <- exp(-t_end * (d + s)) - exp(-d * t_begin - s * t_end)

membre_3 <- exp(-d * t_begin - s * t_end) - exp(-t_end * (d + s)) - s * (exp(-d * (delta * t_end - (1 - delta) * t_begin) - s * t_begin) - exp(-t_end * (s + d))) / deno

denominateur <- membre_1 + membre_2 + membre_3

resultat <- numerateur / denominateur
return(resultat)}

LClone_i <- function(lambda_d, lambda_s, phi_d, phi_s, delta, donnees, i) {
  # numérateur
numerateur_d_exp <- exp(-(phi_d[i] * IDD(delta, lambda_d, donnees$Td_clone[i], donnees$Ts_clone[i])))
numerateur_d <- lambda_d * phi_d[i] * delta * numerateur_d_exp
  
numerateur_s_exp <- exp(-(phi_s[i] * IS(lambda_s, donnees$Ts_clone[i])))
numerateur_s <- lambda_s * phi_s[i] * numerateur_s_exp
  
numerateur <- numerateur_d * numerateur_s

  # dénominateur 
s <- lambda_s * phi_s[i]
d <- lambda_d * phi_d[i]
t_end <- donnees$tau_end[i] - donnees$tau_birth[i]
t_begin <- donnees$tau_begin[i] - donnees$tau_birth[i]
deno <- d * (1 - delta) + s
  
membre_1 <- - (s * (exp(-d * (delta * t_end - (1 - delta) * t_begin)) - exp(-t_end * (d - s)))) / deno
  
membre_2 <- exp(-t_end * (d + s)) - exp(-d * t_begin - s * t_end)
  
membre_3 <- exp(-d * t_begin - s * t_end) - exp(-t_end * (d + s)) - s * (exp(-d * (delta * t_end - (1 - delta) * t_begin) - s * t_begin) - exp(-t_end * (s + d))) / deno

denominateur <- membre_1 + membre_2 + membre_3
resultat <- numerateur / denominateur
return(resultat)}
### 



### Fonction de vraisemblance ###

  # Paramètres à trouver
  # lambda_d, lambda_s et delta des réels 
  # beta_d et beta_s des vecteurs de taille 954

log_likelihood <- function(parameters) {
  lambda_d = parameters[1]
  lambda_s = parameters[2]
  beta_d = parameters[3:10]
  beta_s = parameters[11:18]
  delta = parameters[19]

  phi_d <- phiD(beta_d, donnees)
  phi_s <- phiS(beta_s, donnees)
  L_seller_log_sum <- 0
  L_clone_log_sum <- 0
  
  for (i in donnees$X) {
    #cat("Calculating for i =", i, "\n")
    L_seller_i <- LSeller_i(lambda_d, lambda_s, phi_d, phi_s, delta, donnees, i+1)
    L_clone_i <- LClone_i(lambda_d, lambda_s, phi_d, phi_s, delta, donnees, i+1)
    L_seller_log_sum <- L_seller_log_sum + log(L_seller_i)
    L_clone_log_sum <- L_clone_log_sum + log(L_clone_i)
  }
  
  loglikelihood <- L_seller_log_sum + L_clone_log_sum
  return(-loglikelihood)
}

# Estimation
#fit <- mle(log_likelihood) 

# Spécifier les valeurs de départ
parametres_depart <- list(runif(19, min = 0, max = 1))

# Effectuer l'estimation du maximum de vraisemblance
fit <- mle2(log_likelihood, start = parametres_depart, data = list(donnees = donnees))
