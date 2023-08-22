library(tidyverse)
library(minpack.lm)

# Télécharger le dataset
seller <- read.csv('Data/dataset_vraissemblance.csv')

# Colonnes que nous allons utiliser plus tard
X <- c('type_libre', 'sexe_homme', 'sexe_femme', 'idf', 'etranger', 'une_tete', 'dec1', 'dec2', 'dec3')
columns <- c('type_libre', 'sexe_homme', 'sexe_femme', 'idf', 'etranger', 'une_tete', 'dec1', 'dec2', 
             'dec3', 'tau_birth', 'tau_contract', 'Td', 'Ts', 'Td_clone', 'Ts_clone', 'tau_begin', 'tau_end')

# Création de quelques fonctions utiles 
phiD <- function(beta_d) {
  x_i <- seller[, X]
  phi <- exp(sum(x_i * beta_d))
  return(phi)
}

phiS <- function(beta_s) {
  x_i <- seller[, X]
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

LSeller_i <- function(lambda_d, lambda_s, phi_d, phi_s, delta, i) {
  # Numerateur
  numerateur_d_exp <- exp(-(phi_d * IDD(delta, lambda_d, seller[i, 'Td'], seller[i, 'Ts'])))
  numerateur_d <- lambda_d * phi_d * delta * numerateur_d_exp
  numerateur_s_exp <- exp(-(phi_s * IS(lambda_s, seller[i, 'Ts'])))
  numerateur_s <- lambda_s * phi_s * numerateur_s_exp
  numerateur <- numerateur_d * numerateur_s
  
  # Denominateur
  s <- lambda_s * phi_s
  d <- lambda_d * phi_d
  t_end <- seller[i, 'tau_end'] - seller[i, 'tau_birth']
  t_begin <- seller[i, 'tau_begin'] - seller[i, 'tau_birth']
  deno <- d * (1 - delta) + s
  membre_1 <- - (s * (exp(-d * (delta * t_end - (1 - delta) * t_begin)) - exp(-t_end * (d - s)))) / deno
  membre_2 <- exp(-t_end * (d + s)) - exp(-d * t_begin - s * t_end)
  membre_3 <- exp(-d * t_begin - s * t_end) - exp(-t_end * (d + s)) - s * (exp(-d * (delta * t_end - (1 - delta) * t_begin) - s * t_begin) - exp(-t_end * (s + d))) / deno
  denominateur <- membre_1 + membre_2 + membre_3
  # Resultat
  resultat <- numerateur / denominateur
  return(resultat)
}

LClone_i <- function(lambda_d, lambda_s, phi_d, phi_s, delta, i) {
  # Numerateur
  numerateur_d_exp <- exp(-(phi_d * IDD(delta, lambda_d, seller[i, 'Td_clone'], seller[i, 'Ts_clone'])))
  numerateur_d <- lambda_d * phi_d * delta * numerateur_d_exp
  numerateur_s_exp <- exp(-(phi_s * IS(lambda_s, seller[i, 'Ts_clone'])))
  numerateur_s <- lambda_s * phi_s * numerateur_s_exp
  numerateur <- numerateur_d * numerateur_s
  
  # Denominateur
  s <- lambda_s * phi_s
  d <- lambda_d * phi_d
  t_end <- seller[i, 'tau_end'] - seller[i, 'tau_birth']
  t_begin <- seller[i, 'tau_begin'] - seller[i, 'tau_birth']
  deno <- d * (1 - delta) + s
  membre_1 <- - (s * (exp(-d * (delta * t_end - (1 - delta) * t_begin)) - exp(-t_end * (d - s)))) / deno
  membre_2 <- exp(-t_end * (d + s)) - exp(-d * t_begin - s * t_end)
  membre_3 <- exp(-d * t_begin - s * t_end) - exp(-t_end * (d + s)) - s * (exp(-d * (delta * t_end - (1 - delta) * t_begin) - s * t_begin) - exp(-t_end * (s + d))) / deno
  denominateur <- membre_1 + membre_2 + membre_3
  # Resultat
  resultat <- numerateur / denominateur
  return(resultat)
}

log_negatif <- function(x) {
  if (!is.na(x)) {
    if (x > 0) return(log(x))
    else if (x < 0) return(-log(-x))
    else return(0)  # Pour x égal à 0
  } else {
    return(0)  # Remplacer par la valeur par défaut appropriée
  }
}
vlog_negatif <- Vectorize(log_negatif)


# Fonction de vraisemblance
likelihood <- function(parameters) {
    lambda_d <- parameters[1]
    lambda_s <- parameters[2]
    beta_d <- parameters[3:11]
    beta_s <- parameters[12:20]
    delta <- parameters[21]

    phi_d <- phiD(beta_d)
    phi_s <- phiS(beta_s)
    L_seller_sum <- 0
    L_clone_sum <- 0

    for (i in seq_len(nrow(seller))) {
        Log_seller <- vlog_negatif(LSeller_i(lambda_d, lambda_s, 
                                            phi_d, phi_s, delta, i))
        Log_clone <- vlog_negatif(LClone_i(lambda_d, lambda_s, phi_d, 
                                            phi_s, delta, i))
    if (!is.infinite(Log_seller) && 
        !is.infinite(Log_clone) && 
        !is.na(Log_seller) && 
        !is.na(Log_clone)) {
            L_seller_sum <- L_seller_sum + Log_seller
            L_clone_sum <- L_clone_sum + Log_clone
    }
  }


  L_1 <- sum(L_seller_sum)
  L_2 <- sum(L_clone_sum)

  Likelihood <- L_1 + L_2
  return(-Likelihood)
}

# Réduire l'ordre de grandeur des variables
seller$tau_birth <- seller$tau_birth / mean(seller$tau_birth)
seller$tau_contract <- seller$tau_contract / mean(seller$tau_contract)
seller$Ts <- seller$Ts / mean(seller$Ts)
seller$Td <- seller$Td / mean(seller$Td)
seller$Td_clone <- seller$Td_clone / mean(seller$Td_clone)
seller$Ts_clone <- seller$Ts_clone / mean(seller$Ts_clone)

num_repeats <- 2
parameters_list <- c("lambda_d", "lambda_s", "delta", 
                    paste0("beta_d", 0:8), paste0("beta_s", 0:8))

data <- data.frame(parameters = parameters_list, 
                   valeurs = rep(0, length(parameters_list)))
all_estimations <- matrix(0, ncol = length(parameters_list), nrow = num_repeats)

for (i in 1:num_repeats) {
  initial_params <- runif(21, -50, 50)
  result <- nlminb(start = initial_params, objective = likelihood, 
                   control = list(iter.max = 1000))

  estimated_params <- result$par
  all_estimations[i, ] <- estimated_params

  for (j in seq_len(length(estimated_params))) 
  { 
    if (j <= 21) {
      data$valeurs[j] <- data$valeurs[j] + estimated_params[j]
    }
  }
}

param_means <- colMeans(all_estimations)
param_stds <- apply(all_estimations, 2, sd)

result <- data.frame(parameters = parameters_list, 
                     valeurs = param_means, 
                     std = param_stds) 
print(result)
