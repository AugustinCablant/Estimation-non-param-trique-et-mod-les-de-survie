#install.packages("bbmle")
#install.packages("Rmpfr")
library(stats4)
library(bbmle)
library(Rmpfr)

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
numerateur_d_exp <- exp(mpfr(-(phi_d[i] * IDD(delta, lambda_d, donnees$Td[i], donnees$Ts[i])), precBits = 64))
numerateur_d <- lambda_d * phi_d[i] * delta * numerateur_d_exp
  
numerateur_s_exp <- exp(mpfr(-(phi_s[i] * IS(lambda_s, donnees$Ts[i])), precBits = 64))
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
numerateur_d_exp <- exp(mpfr(-(phi_d[i] * IDD(delta, lambda_d, donnees$Td_clone[i], donnees$Ts_clone[i])), precBits = 64))
numerateur_d <- lambda_d * phi_d[i] * delta * numerateur_d_exp
  
numerateur_s_exp <- exp(mpfr(-(phi_s[i] * IS(lambda_s, donnees$Ts_clone[i])), precBits = 64))
numerateur_s <- lambda_s * phi_s[i] * numerateur_s_exp
  
numerateur <- numerateur_d * numerateur_s
  
  # dénominateur 
s <- lambda_s * phi_s[i]
d <- lambda_d * phi_d[i]
t_end <- donnees$tau_end[i] - donnees$tau_birth[i]
t_begin <- donnees$tau_begin[i] - donnees$tau_birth[i]
deno <- d * (1 - delta) + s
  
membre_1 <- - (s * (exp(mpfr(-d * (delta * t_end - (1 - delta) * t_begin), precBits = 64)) - exp(mpfr(-t_end * (d - s), precBits = 64)))) / deno
  
membre_2 <- exp(mpfr(-t_end * (d + s), precBits = 64)) - exp(mpfr(-d * t_begin - s * t_end, precBits = 64))
  
membre_3 <- exp(mpfr(-d * t_begin - s * t_end, precBits = 64)) - exp(mpfr(-t_end * (d + s), precBits = 64)) - s * (exp(mpfr(-d * (delta * t_end - (1 - delta) * t_begin) - s * t_begin, precBits = 64)) - exp(mpfr(-t_end * (s + d), precBits = 64))) / deno
  
denominateur <- membre_1 + membre_2 + membre_3
  
resultat <- numerateur / denominateur
return(resultat)}
### 



### Fonction de vraisemblance ###

  # Paramètres à trouver
  # lambda_d, lambda_s et delta des réels 
  # beta_d et beta_s des vecteurs de taille 954

log_likelihood <- function(parametres) {
lambda_d <- parametres[1]
lambda_s <- parametres[2]
beta_d <- parametres[3:10]  # 8 éléments pour beta_d
beta_s <- parametres[11:18]  # 8 éléments pour beta_s
delta <- parametres[19]
phi_d <- phiD(beta_d,donnees)
phi_s <- phiS(beta_s,donnees)
L_seller_log_sum <- 0
L_clone_log_sum <- 0
  
for (i in donnees$X) {
    L_seller_i <- LSeller_i(lambda_d, lambda_s, phi_d, phi_s, delta, donnees, i+1)
    L_clone_i <- LClone_i(lambda_d, lambda_s, phi_d, phi_s, delta, donnees, i+1)
    L_seller_log_sum <- L_seller_log_sum + log(mpfr(L_seller_i, precBits = 64))
    print(L_seller_log_sum)
    L_clone_log_sum <- L_clone_log_sum + log(mpfr(L_clone_i, precBits = 64))
    }

loglikelihood <- L_seller_log_sum + L_clone_log_sum
return(-loglikelihood)
}
###

### Estimation ###
lambda_d0 <- runif(1, min = 0, max = 1)
lambda_s0 <- runif(1, min = 0, max = 1)
delta0 <- runif(1, min = 0, max = 1)
beta_d0 <- runif(8, min = 0, max = 1)
beta_s0 <- runif(8, min = 0, max = 1)
parametres_depart <- c(lambda_d0, lambda_s0, beta_d0, beta_s0, delta0)
fit <- mle(log_likelihood, start = parametres_depart, data = donnees) 
print(fit)
### 