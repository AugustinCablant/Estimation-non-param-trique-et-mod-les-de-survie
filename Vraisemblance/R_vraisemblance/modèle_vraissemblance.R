library(tidyverse)
library(minpack.lm)
# Chargement des bibliothèques
library(data.table)
library(Matrix)
library(stats)

# Charger les données
seller <- read.csv("Data/dataset_vraissemblance.csv")
facteur_de_normalisation <- 10^-4

# Colonnes à utiliser
X <- c("sexe_femme", "idf", "etranger", "dec1", "dec2", "dec3")
columns <- c("sexe_femme", "idf", "etranger", "dec1", "dec2", "dec3", "tau_birth", "tau_contract",
             "Td", "Ts", "Td_clone", "Ts_clone", "tau_begin", "tau_end")

# Fonctions pour la contribution
phiD <- function(beta_d, i) {
  x_i <- seller[i, X]
  phi <- exp(sum(x_i * beta_d))
  return(phi)
}

phiS <- function(beta_s, i) {
  x_i <- seller[i, X]
  phi <- exp(sum(x_i * beta_s))
  return(phi)
}

IDD <- function(delta, lambda_d, t_1, t_2) {
  if (t_1 <= t_2) {
    I <- lambda_d * t_1
  } else {
    I <- lambda_d * (t_2 + delta * (t_1 - t_2))
  }
  return(I)
}

ID <- function(lambda_d, t) {
  I <- lambda_d * t
  return(I)
}

IS <- function(lambda_s, t) {
  I <- lambda_s * t
  return(I)
}

# Contribution du vendeur
LSeller_i <- function(lambda_d, lambda_s, phi_d, phi_s, delta, i) {
  d <- lambda_d * phi_d[i]
  s <- lambda_s * phi_s[i]
  deno <- (1 - delta) * d + s
  t_end <- (seller[i, tau_end] - seller[i, tau_birth]) * facteur_de_normalisation
  t_begin <- (seller[i, tau_begin] - seller[i, tau_birth]) * facteur_de_normalisation
  numerateur <- delta * d * exp(-phi_d[i] * IDD(delta, lambda_d, seller[i, Td], seller[i, Ts])) * s * exp(-phi_s[i] * IS(lambda_s, seller[i, Ts]))
  return(numerateur)
}

# Contribution des clones
LClone_i <- function(lambda_d, lambda_s, phi_d, phi_s, delta, i) {
  d <- lambda_d * phi_d[i]
  s <- lambda_s * phi_s[i]
  deno <- (1 - delta) * d + s
  t_end <- (seller[i, tau_end] - seller[i, tau_birth]) * facteur_de_normalisation
  t_begin <- (seller[i, tau_contract] - seller[i, tau_birth]) * facteur_de_normalisation
  numerateur <- d * exp(-phi_d[i] * ID(lambda_d, seller[i, Td_clone])) * exp(-phi_s[i] * IS(lambda_s, seller[i, Td_clone]))
  return(numerateur)
}

# Fonction de vraisemblance
likelihood <- function(parameters) {
  lambda_d <- parameters[1]
  lambda_s <- parameters[2]
  delta <- parameters[3]
  beta_d <- parameters[4:9]
  beta_s <- parameters[10:15]
  L_seller_sum <- 0
  L_clone_sum <- 0
  for (i in 1:nrow(seller)) {
    phi_d <- phiD(beta_d) 
    phi_s <- phiS(beta_s) 
    Log_seller_i <- -log_negatif(LSeller_i(lambda_d, lambda_s, phi_d, phi_s, delta, i))
    Log_clone_i <- -log_negatif(LClone_i(lambda_d, lambda_s, phi_d, phi_s, delta, i))
    L_seller_sum <- L_seller_sum + Log_seller_i
    L_clone_sum <- L_clone_sum + Log_clone_i
  }
  L_1 <- sum(L_seller_sum)
  L_2 <- sum(L_clone_sum)
  Likelihood <- L_1 + L_2
  cat(-Likelihood, "\n")
  return(-Likelihood)
}
