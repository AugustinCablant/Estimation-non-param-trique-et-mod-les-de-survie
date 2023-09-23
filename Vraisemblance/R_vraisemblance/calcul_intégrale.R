
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
  if (t_1 <= t_2) {
    I <- (exp(alpha_d * t_1) - 1) / alpha_d} 
  else {I <- ((1 - delta) * exp(alpha_d * t_2) + exp(alpha_d * t_1) - 1) / alpha_d
  }
  return(I)
}

#variante de IDD car je rencontre des problèmes dans le calcul des intégrales
#ici c'est le cas où t_1 <= t_2 (ça sera notre intégrale avec intervalle 1)
IDD1 <- function(alpha_d, alpha_s, delta, t_1, t_2) {
  I <- (exp(alpha_d * t_1) - 1) / alpha_d
  return(I)}

IDD2 <- function(alpha_d, alpha_s, delta, t_1, t_2) {
  I <- ((1 - delta) * exp(alpha_d * t_2) + exp(alpha_d * t_1) - 1) / alpha_d
  return(I)}

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

LSeller_i <- function(alpha_d, alpha_s, sigma_d2, sigma_s2, phi_d, phi_s, delta, i) {
  # Quelques variables pour faciliter la lecture
  Td <- seller$Td[i]
  Ts <- seller$Ts[i]
  t_end <- (seller$tau_end[i] - seller$tau_birth[i]) * facteur_de_normalisation
  # Numérateur
  numerateur1 <- (1 + sigma_d2 * phi_d[i] * IDD(alpha_d, alpha_s, delta, Td, Ts))^{-round(sigma_d2, digits = 0) - 1}
  numerateur2 <- delta * phi_d[i,] * phi_s[i,] * lambda_s(alpha_s, Ts) * lambda_d(alpha_d, Td) *
    (1 + sigma_s2 * phi_s[i,] * IS(alpha_s, Ts)^{(-round(sigma_s2, digits = 0) - 1)})
  numerateur <- numerateur1 * numerateur2
  
  # Dénominateur
  integrande_denominateur1 <- function(t) {
    gauche <- 1 - (1 + sigma_d2 * phi_d[i,] * IDD1(alpha_d, alpha_s, delta, t_end, t))^{-round(sigma_d2, digits = 0) - 1}
    droite <- phi_s[i,] * lambda_s(alpha_s, t) * (1 + (1 + sigma_s2 * phi_s[i,] * IS(alpha_s, t))^{-round(sigma_s2, digits=0) - 1})
    return (gauche * droite) }
  integrande_denominateur2 <- function(t) {
    gauche <- 1 - (1 + sigma_d2 * phi_d[i,] * IDD2(alpha_d, alpha_s, delta, t_end, t))^{-round(sigma_d2, digits = 0) - 1}
    droite <- phi_s[i,] * lambda_s(alpha_s, t) * (1 + (1 + sigma_s2 * phi_s[i,] * IS(alpha_s, t))^{-round(sigma_s2, digits=0) - 1})
    return (gauche * droite) }
  intervalle1 <- integrate(integrande_denominateur1, lower = 0.001, upper = t_end)$value
  intervalle2 <- integrate(integrande_denominateur2, lower = t_end, upper = t_end+1)$value
  print(intervalle2)
  denominateur <- intervalle1 + intervalle2
  # Résultat
  return(numerateur / denominateur)
}

phidd = phiD(c(-1.83292187e-01, -4.64990779e-03, -7.75338530e-02, 3.41439558e-01, 1.74069565e-01, 2.85908646e-02))
phiss = phiS(c(2.97050800e-02, -9.56602490e-02, -3.41291509e-02, 9.31972376e-02, 6.91866371e-02, 9.63147673e-02))
print(LSeller_i(-1.59337252e-01, -8.31844675e-01, 1.37925407e-03, 1.77170059e+00, phidd, phiss, 7.17338893e-01,14))