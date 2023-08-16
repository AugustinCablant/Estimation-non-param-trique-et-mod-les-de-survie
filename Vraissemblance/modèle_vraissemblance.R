# Apprendre à maximiser une fonction de vraissemblance : 

galap <- read.csv("Vraissemblance/galapagos.csv")
str(galap)

# Tout d’abord, nous devons écrire une fonction qui calcule l’opposé de la log-vraisemblance 
#(negative loglikelihood) pour notre problème.
#Par convention, les algorithmes d’optimisation demandent une fonction à
#minimiser, donc au lieu de maximiser la log-vraisemblance, on minimise son opposé.

nll_galap <- function(b_0, b_area, b_near, theta) {
mu_sp <- exp(b_0 + b_area * log(galap$Area) + b_near * log(galap$Nearest))
-sum(dnbinom(galap$Species, mu = mu_sp, size = theta, log = TRUE))
}

library(bbmle)
mle_galap <- mle2(nll_galap, start = list(b_0 = 0, b_area = 0, b_near = 0, theta = 1))
mle_galap