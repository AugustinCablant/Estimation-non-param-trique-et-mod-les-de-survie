#install.packages("pracma")
library(readr)
library(stats4)
library(Matrix)
library(bbmle)
library(pracma)

# Charger les données
seller <- read.csv('Data/dataset_vraissemblance.csv')
facteur_de_normalisation <- 10 ^ (-5)

# Colonnes que l'on utilise
X <- c('sexe_femme', 'idf', 'etranger', 'dec1', 'dec2', 'dec3')
columns <- c('sexe_femme', 'idf', 'etranger', 'dec1', 'dec2', 'dec3', 'tau_birth', 'tau_contract',
              'Td', 'Ts', 'Td_clone', 'Ts_clone', 'tau_begin', 'tau_end')

# Réduire l'ordre de grandeur des variables
td_mean <- (mean(seller$Td) + mean(seller$Td_clone)) / 2
ts_mean <- mean(seller$Ts)
seller$Ts <- seller$Ts * facteur_de_normalisation
seller$Td <- seller$Td * facteur_de_normalisation
seller$Td_clone <- seller$Td_clone * facteur_de_normalisation
seller$Ts_clone <- seller$Ts_clone * facteur_de_normalisation

# Fonctions pour la contribution
phiD <- function(beta_d) {
  x_i <- as.matrix(seller[X])
  phi <- exp(x_i %*% beta_d)
  return(phi)
}

phiS <- function(beta_s) {
  x_i <- as.matrix(seller[X])
  phi <- exp(x_i %*% beta_s)
  return(phi)
}

IDD <- function(alpha_d, alpha_s, delta, t_1, t_2) {
  n <- length(t_2)  # Nombre d'éléments dans le vecteur t_1
  I <- numeric(n)   # Initialiser un vecteur vide pour stocker les résultats
  for (i in 1:n) {
    if (t_1<= t_2[i]) {
      I[i] <- (exp(alpha_d * t_1) - 1) / alpha_d
    } else {
      I[i] <- ((1 - delta) * exp(alpha_d * t_2[i] ) + delta * exp(alpha_d * t_1) - 1) / alpha_d
    }
  }
  return(I)
}

# Définir la fonction lambda_d 
lambda_d <- function(alpha_d, t) {
  return(exp(alpha_d * t))
}

# Définir la fonction lambda_s 
lambda_s <- function(alpha_s, t) {
  return(t ^ (alpha_s - 1))
}
ID <- function(lambda_d, t) {
  I <- lambda_d * t
  return(I)
}

IS <- function(lambda_s, t) {
  I <- lambda_s * t
  return(I)
}


# Définir la fonction LSeller_i
LSeller_i <- function(alpha_d, alpha_s, sigma_d2, sigma_s2, phi_d, phi_s, delta, i) {
  # Quelques variables pour faciliter la lecture
  Td <- seller$Td[i]
  Ts <- seller$Ts[i]
  t_end <- (seller$tau_end[i] - seller$tau_birth[i]) * facteur_de_normalisation
  # Numérateur
  numerateur1 <- (1 + sigma_d2 * phi_d[i] * IDD(alpha_d, alpha_s, delta, Td, Ts))**(-round(sigma_d2, digits = 0) - 1)
  numerateur2 <- delta * phi_d[i,] * phi_s[i,] * lambda_s(alpha_s, Ts) * lambda_d(alpha_d, Td) 
  numerateur3 <- (1 + sigma_s2 * phi_s[i,] * IS(alpha_s, Ts))**(-round(sigma_s2, digits = 0) - 1)
  numerateur <- numerateur1 * numerateur2 * numerateur3

  # Dénominateur
  integrande_denominateur <- function(t) {
    gauche <- 1 - (1 + sigma_d2 * phi_d[i,] * IDD(alpha_d, alpha_s, delta, t_end, t))**(-round(sigma_d2, digits = 0) - 1)
    droite <- phi_s[i,] * lambda_s(alpha_s, t) * (1 + (1 + sigma_s2 * phi_s[i,] * IS(alpha_s, t))**(-round(sigma_s2, digits=0) - 1))
    return (gauche * droite) }
  
  denominateur <- integrate(integrande_denominateur, lower = 0.001, upper = 10)$value
  # Résultat
  return(numerateur / denominateur)
}

# Définir la fonction LClone_i
LClone_i <- function(alpha_d, alpha_s, sigma_d2, sigma_s2, phi_d, phi_s, delta, i) {
  # Quelques variables pour faciliter la lecture
  Td_seller <- seller$Td[i]
  Td_clone <- seller$Td_clone[i]
  Ts <- seller$Ts[i]
  t_end <- (seller$tau_end[i] - seller$tau_birth[i]) * facteur_de_normalisation
  # Numérateur
  numerateur1 <- (1 + sigma_d2 * phi_d[i])**(-round(sigma_d2) - 1)
  numerateur2 <- phi_d[i] * lambda_d(alpha_d, Td_seller) * (1 + sigma_s2 * phi_s[i] * IS(alpha_s, Td_clone))**(-round(sigma_s2) - 1)
  numerateur <- numerateur1 * numerateur2
  
  # Dénominateur
  integrande_denominateur <- function(t) {
    gauche <- 1 - (1 + sigma_d2 * phi_d[i,] * IDD(alpha_d, alpha_s, delta, t_end, t))**(-round(sigma_d2, digits = 0) - 1)
    droite <- phi_s[i,] * lambda_s(alpha_s, t) * (1 + (1 + sigma_s2 * phi_s[i,] * IS(alpha_s, t))**(-round(sigma_s2, digits=0) - 1))
    return (gauche * droite) }
  denominateur <- integrate(integrande_denominateur1, lower = 0.001, upper = 10)$value
  # Résultat
  return(numerateur / denominateur)
}

# Fonction de vraissemblance
log_likelihood <- function(alpha_d, alpha_s, sigma_d2, sigma_s2, delta, beta_d_1, beta_d_2, beta_d_3, beta_d_4, beta_d_5, beta_d_6,
                                                      beta_s_1, beta_s_2, beta_s_3, beta_s_4, beta_s_5, beta_s_6) {
  # Paramètres à trouver
  # lambda_d, lambda_s et delta des réels
  # beta_d et beta_s des vecteurs de taille 6
  beta_d <- c(beta_d_1, beta_d_2, beta_d_3, beta_d_4, beta_d_5, beta_d_6)
  beta_s <- c(beta_s_1, beta_s_2, beta_s_3, beta_s_4, beta_s_5, beta_s_6)
  phi_d <- phiD(beta_d)
  phi_s <- phiS(beta_s)
  L_seller_sum <- 0
  L_clone_sum <- 0
  for (i in 1:nrow(seller)) {
    #print(LSeller_i(alpha_d, alpha_s, sigma_d2, sigma_s2, phi_d, phi_s, delta, i))
    Log_seller_i <- log(LSeller_i(alpha_d, alpha_s, sigma_d2, sigma_s2, phi_d, phi_s, delta, i))
    Log_clone_i <- log(LClone_i(alpha_d, alpha_s, sigma_d2, sigma_s2, phi_d, phi_s, delta, i))
    L_seller_sum <- L_seller_sum + Log_seller_i
    L_clone_sum <- L_clone_sum + Log_clone_i
  }
  Likelihood <- (L_seller_sum + L_clone_sum) / nrow(seller)
  return(-Likelihood)}

phi_d = phiD(c(-1.83292187e-01, -4.64990779e-03, -7.75338530e-02, 3.41439558e-01, 1.74069565e-01, 2.85908646e-02))
phi_s = phiS(c(2.97050800e-02, -9.56602490e-02, -3.41291509e-02, 9.31972376e-02, 6.91866371e-02, 9.63147673e-02))


alpha_d = -1.59337252e-01
alpha_s = -8.31844675e-01
sigma_d2 = 1.37925407e-03
sigma_s2 = 1.77170059e+00
delta = 7.17338893e-01
beta_d_1 = -1.83292187e-01
beta_d_2 = -4.64990779e-03
beta_d_3 = -7.75338530e-02
beta_d_4 = 3.41439558e-01
beta_d_5 = 1.74069565e-01
beta_d_6 = 2.85908646e-02 
beta_s_1 = 2.97050800e-02
beta_s_2 = -9.56602490e-02
beta_s_3 = -3.41291509e-02
beta_s_4 = 9.31972376e-02
beta_s_5 = 6.91866371e-02
beta_s_6 = 9.63147673e-02
i = 12
t_end <- (seller$tau_end[i] - seller$tau_birth[i]) * facteur_de_normalisation
integrande_denominateur <- function(t) {
  gauche <- 1 - (1 + sigma_d2 * phi_d[i,] * IDD(alpha_d, alpha_s, delta, t_end, t))**(-round(sigma_d2, digits = 0) - 1)
  droite <- phi_s[i,] * lambda_s(alpha_s, t) * (1 + (1 + sigma_s2 * phi_s[i,] * IS(alpha_s, t))**(-round(sigma_s2, digits=0) - 1))
  return (gauche * droite) }

x <- seq(0.001, 1000, 0.01)
y <- integrande_denominateur(x)
plot(x,y)