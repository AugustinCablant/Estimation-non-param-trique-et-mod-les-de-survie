#install.packages("bbmle")
#install.packages("Rmpfr")
# Charger les bibliothèques
library(readr)
library(stats)
library(Matrix)
library(numDeriv)
library(bbmle)

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
  d <- lambda_d * phi_d[i,]
  s <- lambda_s * phi_s[i,]
  deno <- (1 - delta) * d + s
  t_end <- (seller$tau_end[i] - seller$tau_birth[i]) * facteur_de_normalisation
  t_begin <- (seller$tau_begin[i] - seller$tau_birth[i]) * facteur_de_normalisation
  numerateur <- delta * d * exp(-phi_d[i,] * IDD(delta, lambda_d, seller$Td[i], seller$Ts[i])) * s * exp(
    -phi_s[i,] * IS(lambda_s, seller$Ts[i]))
  denominateur1 <- s * (exp(-delta * d * t_begin) - exp(-delta * d * t_end) + exp(
    -delta * d * t_end - t_begin * ((1 - delta) * d + s) - exp(-t_begin * (d + s)))) / deno
  denominateur2 <- exp(-t_begin * (d + s)) - exp(-t_begin * d - t_end * s) - s * (
    exp(-d * (delta * t_end + (1 - delta) * t_begin) - s * t_begin) - exp(-t_end * (d + s))) / deno
  denominateur3 <- exp(-d * t_begin - s * t_end) - exp(-t_end * (d + s))
  denominateur <- denominateur1 + denominateur2 + denominateur3
  return(numerateur / denominateur)
}

# Contribution des clones
LClone_i <- function(lambda_d, lambda_s, phi_d, phi_s, delta, i) {
  d <- lambda_d * phi_d[i,]
  s <- lambda_s * phi_s[i,]
  deno <- (1 - delta) * d + s
  t_end <- (seller$tau_end[i] - seller$tau_birth[i]) * facteur_de_normalisation
  t_begin <- (seller$tau_contract[i] - seller$tau_birth[i]) * facteur_de_normalisation
  numerateur <- d * exp(-phi_d[i,] * ID(lambda_d, seller$Td_clone[i])) * exp(
    -phi_s[i,] * IS(lambda_s, seller$Td_clone[i]))
  denominateur1 <- s * (exp(-delta * d * t_begin) - exp(-delta * d * t_end) + exp(
    -delta * d * t_end - t_begin * ((1 - delta) * d + s) - exp(-t_begin * (d + s)))) / deno
  denominateur2 <- exp(-t_begin * (d + s)) - exp(-t_begin * d - t_end * s) - s * (
    exp(-d * (delta * t_end + (1 - delta) * t_begin) - s * t_begin) - exp(-t_end * (d + s))) / deno
  denominateur3 <- exp(-d * t_begin - s * t_end) - exp(-t_end * (d + s))
  denominateur <- denominateur1 + denominateur2 + denominateur3
  return(numerateur / denominateur)
}

# Fonction de vraissemblance
log_likelihood <- function(lambda_d, lambda_s, delta, beta_d_1, beta_d_2, beta_d_3, beta_d_4, beta_d_5, beta_d_6,
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
    Log_seller_i <- log(LSeller_i(lambda_d, lambda_s, phi_d, phi_s, delta, i))
    Log_clone_i <- log(LClone_i(lambda_d, lambda_s, phi_d, phi_s, delta, i))
    L_seller_sum <- L_seller_sum + Log_seller_i
    L_clone_sum <- L_clone_sum + Log_clone_i
  }
  Likelihood <- (L_seller_sum + L_clone_sum) / nrow(seller)
  return(-Likelihood)}



# Minimisation de l'opposé de la log-vraisemblance
estim <- mle2(log_likelihood, start = list(lambda_d = 8.84172963, lambda_s = 1.23516850, delta = 7.17338893e-01, 
                                      beta_d_1 = -1.83292187e-01, beta_d_2 = -4.64990779e-03, beta_d_3 = -7.75338530e-02, 
                                      beta_d_4 = 3.41439558e-01, beta_d_5 = 1.74069565e-01, beta_d_6 = 2.85908646e-02, 
                                      beta_s_1 = 2.97050800e-02, beta_s_2 = -9.56602490e-02, beta_s_3 = -3.41291509e-02, 
                                      beta_s_4 = 9.31972376e-02, beta_s_5 = 6.91866371e-02, beta_s_6 = 9.63147673e-02),
                                      method="L-BFGS-B", optimizer = "nlminb", hessian = TRUE)
print(summary(estim))
