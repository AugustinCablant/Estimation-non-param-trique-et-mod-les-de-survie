###### Install packages
#install.packages('survival')
#install.packages('SurvTrunc')
#install.packages('ggplot2')
#install.packages("progress")
#install.packages('frailtyEM')
#install.packages("survminer")
#install.packages("glm2")

library(survival)
library(SurvTrunc)
library(ggplot2)
library(progress)
library(frailtyEM)
library(glm2)
library(gridExtra)
#library(survminer)





# Chargement des donnees
donnees <- read.csv(file = "/Users/augustincablant/Desktop/Viagers/merge_clone_seller.csv", header = TRUE)
print(colnames(donnees))
table(donnees$groupe)


##############################################################################################################
# Créer les variables que nous allons utiliser (à faire une fois)
# Ce code créer le data frame de travail que nous utiliserons ensuite 
# Il suppose que les variables Td, rtrunc, ltrunc, Tr, nb_tete (nombre de tetes sur la vente)
# head (vaut 1 (resp.2) si tête 1 (resp. 2) en présence) aient été crées chez les vendeurs


donnees$Tr_dif <- 0

liste_jd <- unique(donnees[donnees$groupe == 'seller', 'jd'])
pb <- progress_bar$new(total = length(liste_jd))
# Parcourir chaque valeur unique de 'jd'
for (jd in liste_jd) {
  # Filtrer le sous-ensemble des données pour la valeur de 'jd' en cours
  sub_df <- donnees[donnees$jd == jd, ]
  sub_liste <- which(donnees$jd == jd & donnees$groupe == "clone")
  # Récupérer les valeurs spécifiques pour chaque variable pour le sous-groupe 'seller'
  rtrunc <- sub_df[sub_df$groupe == 'seller', 'rtrunc']
  ltrunc <- sub_df[sub_df$groupe == 'seller', 'ltrunc']
  Tr <- sub_df[sub_df$groupe == 'seller', 'Tr']
  nb_tete <- sub_df[sub_df$groupe == 'seller', 'nb_tete']
  head <- sub_df[sub_df$groupe == 'seller', 'head']
  
  # Parcourir les indices du sous-ensemble de données
  for (k in sub_liste) {
    # Mettre à jour les valeurs dans le dataframe donnees
    donnees$Tr_dif[k] <- Tr - donnees$Tr_clone[k]
    donnees$rtrunc[k] <- rtrunc
    donnees$ltrunc[k] <- ltrunc
    donnees$head[k] <- head
    donnees$nb_tete[k] <- nb_tete
  }
  pb$tick()
}
pb$close()

indices_aleatoires <- sample(nrow(donnees), 10)  
sample(donnees[indices_aleatoires, c("Tr_dif", "rtrunc", "ltrunc","head","nb_tete","jd","Td_clone","Ts_clone")])

write.csv(donnees, file = "/Users/augustincablant/Desktop/Viagers/merge_clone_seller.csv", row.names = FALSE)
##############################################################################################################




# Autres data frames que nous allons utiliser
femmes <- subset(donnees, b_sexe == 2)
hommes <- subset(donnees, b_sexe == 1)
vendeurs <- subset(donnees, groupe == "seller")
clones <- subset(donnees,groupe == "clone")


#################################### I - Statistiques descriptives #################################### 
# Nous commençons par une section statistiques descriptives, elles ne sont pas exhaustives

# Vendeurs vs. Clones
table(donnees$groupe)


# Sexes
barplot(prop.table(table(vendeurs$b_sexe)), width = 0.3, 
        col = "yellow", ylim = c(0,0.6), main = "vendeurs")

barplot(prop.table(table(clones$b_sexe)), width = 0.3, 
        col = "green", ylim = c(0,0.6), main = "clones")

# Age au moment de la signature
layout(mat = matrix(c(1,1,2,3), nrow = 2))

hist(round(vendeurs$age_acte/365,0), main = "Hommes et Femmes", freq = FALSE, xlab = "",
     col = "purple4")

hist(round(vendeurs$age_acte[vendeurs$b_sexe == 1]/365, 0), 
     main = "Homme", freq = FALSE, xlab = "",
     col = "aliceblue")

hist(round(vendeurs$age_acte[vendeurs$b_sexe == 2]/365, 0), 
     main = "Femme", freq = FALSE, xlab = "",
     col = "pink")

# Département de naissance  
hist(round(vendeurs$b_dep), col = c("burlywood", "bisque2"), main = "Département de naiss.")


# La variable de durée
layout(mat = matrix(c(1,1,2,3), nrow = 2))

boxplot(Td_seller ~ b_sexe, data = vendeurs, horizontal = TRUE)
boxplot(Td_seller ~ b_dep, data = vendeurs, horizontal = TRUE)


# Representation de la troncature
layout(mat = matrix(1))

x <- c(-1.5, 1.2)
y <- c(1, -1)

plot(x, y, xlab="", ylab="",
     xlim=c(-2, 1.5), ylim=c(-2, 1.5), pch=22, col="black",
     bg="black", bty="l", main = "", las = 1, cex = 3, xaxt = "n", yaxt = "n")
points(0, 0, pch=22, col="black", bg="white", bty="l", tcl=0.4, las=1, cex=3)
points(1.2, 1, pch=22, col="black", bg="black", bty="l", tcl=0.4, las=1, cex=1.5)
points(1.2, 0.85, pch=22, col="black", bg="white", bty="l", tcl=0.4, las=1, cex=1.5)
text(x = rep(1.35,2), y = c(1,0.85), 
     labels = c("tronqué", "observé"), cex = 0.9)
abline(v = c(-1, 1), lty = 2, lwd = 2)
segments(x0 = c(-2.5, -2.5, -2.5), y0 = c(1, 0, -1), 
         x1 = c(-1.5, -0.03, 1.2) , y1 = c(1, 0, -1), lwd = 3.5)
text(x = rep(-1.8,3), y = c(1,0,-1) + 0.1, 
     labels = c("Sujet 1", "Sujet 2", "Sujet 3"), cex = 1.2)
axis(side = 1, c(-1, 1), tcl=-0.2, 
     labels = c("entrée dans l'étude","fin de l'étude"), cex = 3.5)
axis(side = 1, c(-2.2, 1.7), lwd = 3.5, labels = c("",""), lwd.ticks = -1)
axis(side = 2, c(-2.2, 1.7), lwd = 3.5, labels = c("",""), lwd.ticks = -1)

#### 


######## Quelques stat des demandées par Mr Visser (numérotées comme ds le overleaf)

# 1) Stat des sur Tr 
summary(vendeurs $ Tr_seller)
summary(clones $ Tr_clone)
hist(vendeurs $ Tr_seller, main = "Histogramme de la durée de vie résiduelle des vendeurs", freq = FALSE, xlab = "",
     col = "brown")
hist(clones $ Tr_clone, main = "Histogramme de la durée de vie résiduelle des clones", freq = FALSE, xlab = "",
     col = "cyan")

densite_seller <- density(vendeurs $ Tr_seller)
densite_clone <- density(clones $ Tr_clone)

plot(densite_seller, col = "brown", lwd = 3, main = "Densités de Tr_seller et Tr_clone")
lines(densite_clone, col = "cyan", lwd = 3)
legend("topright", legend = c("Tr_seller", "Tr_clone"), col = c("cyan", "brown"), lwd = 3, bty = "n")


##########


# 2) Stat def sur Tr_dif
summary(donnees $ Tr_dif)
resultat_test <- t.test(donnees$ Tr_dif, mu = 0)
print(resultat_test)
donnees_sans_na <- na.omit(donnees$Tr_dif)
densite <- density(donnees_sans_na)
plot(densite, col = "lightblue", lwd = 3, main = "Densités de Tr_dif")
legend("topright", legend = c("Tr_dif"), col = c("lightblue"), lwd = 3, bty = "n")


######## Distinction sur les têtes 
unetete <- donnees[donnees$nb_tete == 1, ]
unetete_femme <- unetete[unetete$b_sexe == 2, ]
unetete_homme <- unetete[unetete$b_sexe == 1, ]

deuxtete <- donnees[donnees$nb_tete == 2, ]
deuxtete_tete1 <- deuxtete[deuxtete$head == 1, ]
deuxtete_tete2 <- deuxtete[deuxtete$head == 2, ]


##########


# 3) Stat des sur Tr_dif sur les ventes à une tête 
summary(unetete $ Tr_dif)
resultat_test <- t.test(unetete$ Tr_dif, mu = 0)
print(resultat_test)
donnees_sans_na <- na.omit(unetete$Tr_dif)
densite <- density(donnees_sans_na)
plot(densite, col = "orange", lwd = 3, main = "Densités de Tr_dif pour les ventes à une tête")
legend("topright", legend = c("Tr_dif"), col = c("orange"), lwd = 3, bty = "n")


##########


# 4) Stat des sur Tr_dif sur les ventes à deux têtes, focus sur la tête 1 
summary(deuxtete_tete1 $ Tr_dif)
resultat_test <- t.test(deuxtete_tete1$ Tr_dif, mu = 0)
print(resultat_test)
donnees_sans_na <- na.omit(deuxtete_tete1$Tr_dif)
densite <- density(donnees_sans_na)
plot(densite, col = "magenta", lwd = 3, main = "Densités de Tr_dif pour la tete 1 des ventes à deux têtes ")
legend("topright", legend = c("Tr_dif"), col = c("magenta"), lwd = 3, bty = "n")


##########


# 5) Stat des sur Tr_dif sur les ventes à deux têtes, focus sur la tête 2 
summary(deuxtete_tete2 $ Tr_dif)
resultat_test <- t.test(deuxtete_tete2$ Tr_dif, mu = 0)
print(resultat_test)
donnees_sans_na <- na.omit(deuxtete_tete2$Tr_dif)
densite <- density(donnees_sans_na)
plot(densite, col = "cyan", lwd = 3, main = "Densités de Tr_dif pour la tete 2 des ventes à deux têtes ")
legend("topright", legend = c("Tr_dif"), col = c("cyan"), lwd = 3, bty = "n")


##########



# 6) Stat des sur Tr_dif sur les ventes à une tête femme
summary(unetete_femme $ Tr_dif)
resultat_test <- t.test(unetete_femme$ Tr_dif, mu = 0)
print(resultat_test)
donnees_sans_na <- na.omit(unetete_femme$Tr_dif)
densite <- density(donnees_sans_na)
plot(densite, col = "red", lwd = 3, main = "Densités de Tr_dif pour les ventes à une tête (femmes")
legend("topright", legend = c("Tr_dif"), col = c("red"), lwd = 3, bty = "n")


##########


# 7) Stat des sur Tr_dif sur les ventes à une tête homme
summary(unetete_homme $ Tr_dif)
resultat_test <- t.test(unetete_homme$ Tr_dif, mu = 0)
print(resultat_test)
donnees_sans_na <- na.omit(unetete_homme$Tr_dif)
densite <- density(donnees_sans_na)
plot(densite, col = "blue", lwd = 3, main = "Densités de Tr_dif pour les ventes à une tête (hommes)")
legend("topright", legend = c("Tr_dif"), col = c("blue"), lwd = 3, bty = "n")


##########



# 8) Par genre 
table(donnees $ b_sexe)

densite_femme_sans_na <- na.omit(femmes $ Td_seller)
densite_femme_seller <- density(densite_femme_sans_na)
densite_femme_sans_na2 <- na.omit(femmes $ Td_clone)
densite_femme_clone <- density(densite_femme_sans_na2)

plot(densite_femme_seller, col = "orange", lwd = 3, main = "Densités Td_clone et Td_seller chez les femmes")
lines(densite_femme_clone, col = "cyan", lwd = 3)
legend("topright", legend = c("Td_seller", "Td_clone"), col = c("orange", "cyan"), lwd = 3, bty = "n")

densite_homme_sans_na <- na.omit(hommes $ Td_seller)
densite_homme_seller <- density(densite_homme_sans_na)
densite_homme_sans_na2 <- na.omit(hommes $ Td_clone)
densite_homme_clone <- density(densite_homme_sans_na2)

plot(densite_homme_seller, col = "green", lwd = 3, main = "Densités Td_clone et Td_seller chez les femmes")
lines(densite_homme_clone, col = "purple", lwd = 3)
legend("topright", legend = c("Td_seller", "Td_clone"), col = c("green", "purple"), lwd = 3, bty = "n")


# Stat des sur Tr_dif par genre 
## femmes 
summary(femmes $ Tr_dif)
resultat_test <- t.test(femmes$ Tr_dif, mu = 0)
print(resultat_test)
## hommes 
summary(hommes $ Tr_dif)
resultat_test <- t.test(hommes$ Tr_dif, mu = 0)
print(resultat_test)

donnees_sans_na1 <- na.omit(femmes$Tr_dif)
densite1 <- density(donnees_sans_na1)
donnees_sans_na2 <- na.omit(hommes$Tr_dif)
densite2 <- density(donnees_sans_na2)
plot(densite1, col = "orange", lwd = 3, main = "Densités de Tr_dif pour les femmes et les hommes")
lines(densite2, col = "cyan", lwd = 3)
legend("topright", legend = c("Tr_dif femme","Tr_dif homme") , col = c("orange","cyan"), lwd = 3, bty = "n")


##########



# 9) Stat des sur Tr_dif par période de naissance
summary(donnees $ b_annee)
periode1 <- subset(donnees, b_annee <= 1910)
periode2 <- subset(donnees, b_annee <= 1919 & b_annee > 1910)
periode3 <- subset(donnees, b_annee <= 1923 & b_annee > 1919)
periode4 <- subset(donnees, b_annee > 1923)

test1 <- t.test(periode1 $ Tr_dif, mu = 0)
test2 <- t.test(periode2 $ Tr_dif, mu = 0)
test3 <- t.test(periode3 $ Tr_dif, mu = 0)
test4 <- t.test(periode4 $ Tr_dif, mu = 0)

print(test1) 
print(test2) 
print(test3) 
print(test4) 

donnees_sans_na1 <- na.omit(periode1$Tr_dif)
densite1 <- density(donnees_sans_na1)
donnees_sans_na2 <- na.omit(periode2$Tr_dif)
densite2 <- density(donnees_sans_na2)
donnees_sans_na3 <- na.omit(periode3$Tr_dif)
densite3 <- density(donnees_sans_na3)
donnees_sans_na4 <- na.omit(periode4$Tr_dif)
densite4 <- density(donnees_sans_na4)

plot(densite1, col = "orange", lwd = 3, main = "Densités de Tr_dif selon les périodes")
lines(densite2, col = "cyan", lwd = 3)
lines(densite3, col = "green", lwd = 3)
lines(densite4, col = "purple", lwd = 3)
legend("topright", legend = c("Tr_dif<1910","1910<Tr_dif<=1919","1919<Tr_dif<=1923", "Tr_dif>1923") , col = c("orange","cyan", "green", "purple"), lwd = 3, bty = "n")


##########



# 11) Par tranche d'âge à la signature du contrat 
donnees $ Ts <- ifelse(donnees$groupe == "seller", donnees $ Ts_seller, donnees $ Ts_clone)
donnees $ Ts_age <- round((donnees $ Ts)/365)
summary(donnees $ Ts)
summary(donnees $ Ts_age)

ts1 <- subset(donnees, Ts<= 25401)
ts2 <- subset(donnees, Ts <= 26907 & Ts > 25401)
ts3 <- subset(donnees, Ts <= 28455 & Ts > 26907)
ts4 <- subset(donnees, Ts> 28455)

test1 <- t.test(ts1 $ Tr_dif, mu = 0)
test2 <- t.test(ts2 $ Tr_dif, mu = 0)
test3 <- t.test(ts3 $ Tr_dif, mu = 0)
test4 <- t.test(ts4 $ Tr_dif, mu = 0)

print(test1) 
print(test2) 
print(test3) 
print(test4) 

donnees_sans_na1 <- na.omit(ts1$Tr_dif)
densite1 <- density(donnees_sans_na1)
donnees_sans_na2 <- na.omit(ts2$Tr_dif)
densite2 <- density(donnees_sans_na2)
donnees_sans_na3 <- na.omit(ts3$Tr_dif)
densite3 <- density(donnees_sans_na3)
donnees_sans_na4 <- na.omit(ts4$Tr_dif)
densite4 <- density(donnees_sans_na4)

par(ask = FALSE, mar = c(5, 5, 4, 2) + 0.2)
plot(densite1, col = "red", lwd = 3, main = "Densités de Tr_dif selon l'âge du vendeur au moment de la signature du contrat", ylim = c(0, 0.00015))
lines(densite2, col = "blue", lwd = 3)
lines(densite3, col = "green", lwd = 3)
lines(densite4, col = "magenta", lwd = 3)
legend("topright", legend = c("Ts<= 25401","Ts <= 26907 & Ts > 25401","Ts <= 26907 & Ts > 25401", "Ts> 28455") , col = c("red","blue", "green", "magenta"), lwd = 3, bty = "n")


##########



# 12) Par région en France métropolitaine 
ile_france <- subset(donnees, old_b_dep %in% c(75, 78, 91, 92, 93, 94, 95))
reste_france_metropolitaine <- subset(donnees, !(old_b_dep %in% c(75, 78, 91, 92, 93, 94, 95)))
etranger <- subset(donnees, old_b_dep==99)

test1 <- t.test(ile_france $ Tr_dif, mu = 0)
test2 <- t.test(reste_france_metropolitaine $ Tr_dif, mu = 0)
test3 <- t.test(etranger $ Tr_dif, mu = 0)


print(test1) 
print(test2) 
print(test3) 


donnees_sans_na1 <- na.omit(ile_france$Tr_dif)
densite1 <- density(donnees_sans_na1)
donnees_sans_na2 <- na.omit(reste_france_metropolitaine$Tr_dif)
densite2 <- density(donnees_sans_na2)
donnees_sans_na3 <- na.omit(etranger$Tr_dif)
densite3 <- density(donnees_sans_na3)


plot(densite1, col = "black", lwd = 3, main = "Densités de Tr_dif selon les régions de naissance", ylim = c(0,0.00014))
lines(densite2, col = "red", lwd = 3)
lines(densite3, col = "grey", lwd = 3)
legend("topright", legend = c("Île-de-France","Reste de la France métropolitaine","Étranger") , col = c("black","red", "grey"), lwd = 3, bty = "n")


##########



# 13) Par tranche de bouquet 
summary(vendeurs $ downp)


##########



# 14) Par tranche de rente 
summary(vendeurs $ annuity)


##########



# 15) Régression par MCO de Td^{seller} sur sexe, region, bouquet, rente, vente à une têtes 

## Je commence par créer les variables nécessaires  

vendeurs_MCO = vendeurs
vendeurs_MCO$sexe_homme = as.numeric(vendeurs_MCO$b_sexe==1)
vendeurs_MCO$idf = as.numeric(vendeurs_MCO$b_dep %in% c(75,78,91,92,93,94,95))
vendeurs_MCO$etranger = as.numeric(vendeurs_MCO$b_dep==99)
vendeurs_MCO$une_tete = as.numeric(vendeurs_MCO$nb_tete==1)
vendeurs_MCO$rente = vendeurs_MCO$annuity
vendeurs_MCO$bouquet = vendeurs_MCO$downp


modele_regression <- lm(Td ~ sexe_homme + idf + etranger + une_tete + rente + bouquet , data = vendeurs_MCO)
summary(modele_regression)

p1 <- ggplot(data = vendeurs_MCO, aes(x = sexe_homme, y = Td)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("sexe_homme") +
  ylab("Td") +
  ggtitle("Régression linéaire : Td en fonction de sexe_homme")

p2 <- ggplot(data = vendeurs_MCO, aes(x = idf, y = Td)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("idf") +
  ylab("Td") +
  ggtitle("Régression linéaire : Td en fonction de idf")

p3 <- ggplot(data = vendeurs_MCO, aes(x = etranger, y = Td)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("etranger") +
  ylab("Td") +
  ggtitle("Régression linéaire : Td en fonction de etranger")

p4 <- ggplot(data = vendeurs_MCO, aes(x = une_tete, y = Td)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("une_tete") +
  ylab("Td") +
  ggtitle("Régression linéaire : Td en fonction de une_tete")

p5 <- ggplot(data = vendeurs_MCO, aes(x = rente, y = Td)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("rente") +
  ylab("Td") +
  ggtitle("Régression linéaire : Td en fonction de rente")

p6 <- ggplot(data = vendeurs_MCO, aes(x = bouquet, y = Td)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("bouquet") +
  ylab("Td") +
  ggtitle("Régression linéaire : Td en fonction de bouquet")


grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)



# Créer un data frame avec les valeurs prédites et les valeurs observées
predictions <- data.frame(Y_obs = vendeurs_MCO$Td, Y_pred = fitted(modele_regression) )

# Créer le graphique de dispersion avec une ligne de régression ajustée
ggplot(data = predictions, aes(x = Y_obs, y = Y_pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue", linetype = "dashed") +
  xlab("Valeurs observées de Td") +
  ylab("Valeurs prédites de Td") +
  ggtitle("Régression linéaire par mCO de Td sur sexe, region, bouquet, rente, vente à une têtes")


########


# 16) Régression par MCO de Tr^{seller} sur sexe, region, bouquet, rente, vente à une têtes 

modele_regression <- lm(Tr ~ sexe_homme + idf + etranger + une_tete + rente + bouquet , data = vendeurs_MCO)
summary(modele_regression)

p1 <- ggplot(data = vendeurs_MCO, aes(x = sexe_homme, y = Tr)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("sexe_homme") +
  ylab("Tr") +
  ggtitle("Régression linéaire : Tr en fonction de sexe_homme")

p2 <- ggplot(data = vendeurs_MCO, aes(x = idf, y = Tr)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("idf") +
  ylab("Tr") +
  ggtitle("Régression linéaire : Tr en fonction de idf")

p3 <- ggplot(data = vendeurs_MCO, aes(x = etranger, y = Tr)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("etranger") +
  ylab("Tr") +
  ggtitle("Régression linéaire : Tr en fonction de etranger")

p4 <- ggplot(data = vendeurs_MCO, aes(x = une_tete, y = Tr)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("une_tete") +
  ylab("Tr") +
  ggtitle("Régression linéaire : Tr en fonction de une_tete")

p5 <- ggplot(data = vendeurs_MCO, aes(x = rente, y = Tr)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("rente") +
  ylab("Tr") +
  ggtitle("Régression linéaire : Tr en fonction de rente")

p6 <- ggplot(data = vendeurs_MCO, aes(x = bouquet, y = Tr)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("bouquet") +
  ylab("Tr") +
  ggtitle("Régression linéaire : Tr en fonction de bouquet")


grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)

# Créer un data frame avec les valeurs prédites et les valeurs observées
predictions <- data.frame(Y_obs = vendeurs_MCO$Tr, Y_pred = fitted(modele_regression) )

# Créer le graphique de dispersion avec une ligne de régression ajustée
ggplot(data = predictions, aes(x = Y_obs, y = Y_pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue", linetype = "dashed") +
  xlab("Valeurs observées de Tr") +
  ylab("Valeurs prédites de Tr") +
  ggtitle("Régression linéaire par MCO de Tr sur sexe, region, bouquet, rente, vente à une têtes")


##########


# 17) Régression par MCO de T_{dif} sur sexe, region, bouquet, rente, vente à une têtes
clones_MCO = clones
clones_MCO$sexe_homme = as.numeric(clones_MCO$b_sexe==1)
clones_MCO$idf = as.numeric(clones_MCO$old_b_dep %in% c(75,78,91,92,93,94,95))
clones_MCO$etranger = as.numeric(clones_MCO$old_b_dep==99)
clones_MCO$une_tete = as.numeric(clones_MCO$nb_tete==1)


modele_regression <- lm(Tr_dif ~ sexe_homme + idf + etranger + une_tete , data = clones_MCO)
summary(modele_regression)

p1 <- ggplot(data = clones_MCO, aes(x = sexe_homme, y = Tr_dif)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("sexe_homme") +
  ylab("Tr_dif") +
  ggtitle("Régression linéaire : Tr_dif en fonction de sexe_homme")

p2 <- ggplot(data = clones_MCO, aes(x = idf, y = Tr_dif)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("idf") +
  ylab("Tr_dif") +
  ggtitle("Régression linéaire : Tr_dif en fonction de idf")

p3 <- ggplot(data = clones_MCO, aes(x = etranger, y = Tr_dif)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("etranger") +
  ylab("Tr_dif") +
  ggtitle("Régression linéaire : Tr_dif en fonction de etranger")

p4 <- ggplot(data = clones_MCO, aes(x = une_tete, y = Tr_dif)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("une_tete") +
  ylab("Tr_dif") +
  ggtitle("Régression linéaire : Tr_dif en fonction de une_tete")



grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)

# Créer un data frame avec les valeurs prédites et les valeurs observées
predictions <- data.frame(Y_obs = clones_MCO$Tr_dif, Y_pred = fitted(modele_regression) )

# Créer le graphique de dispersion avec une ligne de régression ajustée
ggplot(data = predictions, aes(x = Y_obs, y = Y_pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue", linetype = "dashed") +
  xlab("Valeurs observées de Tr_dif") +
  ylab("Valeurs prédites de Tr_dif") +
  ggtitle("Régression linéaire par MCO de Tr_dif sur sexe, region, vente à une têtes")

#########

# 18) Régression logistique de indicatrice(Tr_dif > 0) sur sexe, region, bouquet, rente, vente à une têtes 


################################################################################################################################################ 




######################### II - Estimation non paramétrique, cdfDT #########################
clones <- clones[order(clones$jd, clones$index), ]

# Test d'indépendance entre les vars de troncature de duree 
## Vérification que nous n'avons pas de NA

## Test
test.indep <- indeptestDT(y = vendeurs$Td, 
                          l = vendeurs$ltrunc, r = vendeurs$rtrunc)
test.indep

# Estimation sur toute la population 
vend.Td <- vendeurs$Td
vend.ltrunc <- vendeurs$ltrunc
vend.rtrunc <- vendeurs$rtrunc


clones.list <- split.data.frame(clones, clones$jd)



tirage <- function(data, nb.obs) {
  echantillon <-  sample(1:nrow(data), size = nb.obs, replace = FALSE)
  data <- data[echantillon,]
  return(data)
}


tirage2 <- function(data, nb.obs) {
  echantillon <-  sample(1:nrow(data), size = nb.obs, replace = TRUE)
  data <- data[echantillon,]
  return(data)
}


clones.list <- lapply(clones.list, tirage2, nb.obs = 10)
clones <- do.call(rbind, clones.list)

rm(clones.list, tirage)

clone.Td <- clones$Td_clone
clone.ltrunc <- clones$ltrunc
clone.rtrunc <- clones$rtrunc

table(c(vendeurs$groupe, clones$groupe))


results.vend <- cdfDT(y = vend.Td, l = vend.ltrunc, r = vend.rtrunc, 
                      error= 1e-6, display = FALSE, boot = TRUE)


results.clone <- cdfDT(y = clone.Td, l = clone.ltrunc, r = clone.rtrunc, 
                       error= 1e-6, display = FALSE, boot = TRUE)


vend.estim <- data.frame(groupe = "Vendeur", time = results.vend$time, 
                          survival = results.vend$Survival, 
                          CI.lower = 1 - results.vend$CI.lower.F,
                          CI.upper = 1 - results.vend$CI.upper.F)

clone.estim <- data.frame(groupe = "Clone", time = results.clone$time, 
                           survival = results.clone$Survival, 
                           CI.lower = 1 - results.clone$CI.lower.F,
                           CI.upper = 1 - results.clone$CI.upper.F)

estimations <- rbind(vend.estim, clone.estim)



ggplot(data = estimations, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = groupe, colour = groupe)) +
  geom_ribbon(aes(group = groupe, fill = groupe, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survie (avec 95% IC)") +
  xlab("Annees") + ylab("Fonction de survie") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = quantile(vend.Td),
                     labels = function(x) round(x/365.25, digits = 0))

ggplot(data = estimations, aes(x = time, y = survival)) +
  geom_line(aes(group = groupe, colour = groupe)) +
  geom_ribbon(aes(group = groupe, fill = groupe, ymin = CI.lower, ymax = CI.upper), 
              alpha = 0.3) +  # alpha = 0.3 pour rendre l'intervalle de confiance plus transparent
  labs(title = "Estimation des survie (avec 95% IC)",
       x = "Années",
       y = "Fonction de survie") +
  theme_minimal() +  # Utiliser un thème minimal
  theme(panel.grid.major = element_blank(),  # Enlever la grille
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),  # Faire pivoter les étiquettes de l'axe x pour une meilleure lisibilité
        legend.position = "right",  # Placer la légende à droite
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.y = element_text(vjust = 1)) +  # Ajuster la position du titre de l'axe y
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1), 
                     expand = c(0, 0),  # Déplacer les options limits et expand dans scale_y_continuous
                     breaks = seq(0, 1, 0.1)) +  # Ajouter des étiquettes d'axe y avec un intervalle de 0.1
  scale_x_continuous(breaks = seq(0, max(estimations$time), by = 365.25*2),  # Définir les étiquettes de l'axe x avec un intervalle de deux ans
                     labels = function(x) ifelse(x %% 365.25 == 0, round(x/365.25, digits = 0), "")) +
  guides(colour = guide_legend(title = "Groupes"),  # Ajouter un titre à la légende des couleurs
         fill = guide_legend(title = "Groupes")) 


##################
# Estimations selon le sexe
######## Femmes
resultsfemme.vend <- cdfDT(y = vendeurs$Td[(vendeurs$b_sexe == 2)], l = vendeurs$ltrunc[(vendeurs$b_sexe == 2)], r = vendeurs$rtrunc[(vendeurs$b_sexe == 2)], 
                           plot.cdf = FALSE, display = FALSE, boot = TRUE)


resultsfemme.clone <- cdfDT(y = clones$Td_clone[clones$b_sexe == 2], l = clones$ltrunc[clones$b_sexe == 2], r = clones$rtrunc[clones$b_sexe == 2], 
                            plot.cdf = FALSE, display = FALSE, boot = TRUE)

vendfemme.estim <- data.frame(group = "Vendeuses femme", time = resultsfemme.vend$time, 
                              survival = resultsfemme.vend$Survival, 
                              CI.lower = 1 - resultsfemme.vend$CI.lower.F,
                              CI.upper = 1 - resultsfemme.vend$CI.upper.F)


clonefemme.estim <- data.frame(group = "Clone femme", time = resultsfemme.clone$time, 
                               survival = resultsfemme.clone$Survival, 
                               CI.lower = 1 - resultsfemme.clone$CI.lower.F,
                               CI.upper = 1 - resultsfemme.clone$CI.upper.F)

estimationsfemme <- rbind(vendfemme.estim, clonefemme.estim)

ggplot(data = estimationsfemme, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeuses et clones femmes (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

####### Hommes 
resultshomme.vend <- cdfDT(y = vendeurs$Td[(vendeurs$b_sexe == 1)], l = vendeurs$ltrunc[(vendeurs$b_sexe == 1)], r = vendeurs$rtrunc[(vendeurs$b_sexe == 1)], 
                           plot.cdf = FALSE, display = FALSE, boot = TRUE)

resultshomme.clone <- cdfDT(y = clones$Td_clone[clones$b_sexe == 1], l = clones$ltrunc[clones$b_sexe == 1], r = clones$rtrunc[clones$b_sexe == 1], 
                            plot.cdf = FALSE, display = FALSE, boot = TRUE)

vendhomme.estim <- data.frame(group = "Vendeur homme", time = resultshomme.vend$time, 
                              survival = resultshomme.vend$Survival, 
                              CI.lower = 1 - resultshomme.vend$CI.lower.F,
                              CI.upper = 1 - resultshomme.vend$CI.upper.F)


clonehomme.estim <- data.frame(group = "Clone homme", time = resultshomme.clone$time, 
                               survival = resultshomme.clone$Survival, 
                               CI.lower = 1 - resultshomme.clone$CI.lower.F,
                               CI.upper = 1 - resultshomme.clone$CI.upper.F)

estimationshomme <- rbind(vendhomme.estim, clonehomme.estim)
ggplot(data = estimationshomme, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones hommes (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")


estimsexe = rbind(estimationsfemme, estimationshomme)


ggplot(data = estimsexe, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones selon le sexe (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

################################################################################################### 




################################# Modélisation de la survie ################################# 

clones.list <- split.data.frame(clones, clones$jd)

tirage <- function(data, nb.obs) {
  echantillon <-  sample(1:nrow(data), size = nb.obs, replace = FALSE)
  data <- data[echantillon,]
  return(data)
}

clones.list <- lapply(clones.list, tirage, nb.obs = 3)
clones <- do.call(rbind, clones.list)

rm(clones.list, tirage)


vendeurs$statuts <- 1
clones$statuts <- 1

# 1) Estimation sans bouquet et rente
vendeurs2 <- data.frame(numbase = 1:nrow(vendeurs),  
                        time = vendeurs$Td, 
                        ltrunc = vendeurs$ltrunc,
                        rtrunc = vendeurs$rtrunc, 
                        sexe_homme = as.numeric(vendeurs$b_sexe == 1),
                        ile2france = as.numeric(vendeurs$b_dep%in% c(75,78,91,92,93,94,95)),
                        b_min_Q1 = as.numeric(vendeurs$b_annee %in% c(1891:1910)), #Min-Q1
                        b_Q1_median = as.numeric(vendeurs$b_annee %in% c(1911:1916)), #Q1-Median
                        b_median_Q3 = as.numeric(vendeurs$b_annee %in% c(1916:1922)), #Median-Q3
                        b_Q3_max = as.numeric(vendeurs$b_annee %in% c(1923:1940)),#Q3-Max
                        oneheadcontr = as.numeric(vendeurs$nb_tete==1),
                        #age_sign_vieux = as.numeric(vendeurs$cat_age_sign == "Vieux"),
                        bouquet = vendeurs$downp,
                        rente = vendeurs$annuity,
                        status = vendeurs$statuts)

clones2 <- data.frame(numbase = 1:nrow(clones),
                      time = clones$Td_clone, 
                      ltrunc = clones$ltrunc,
                      rtrunc = clones$rtrunc, 
                      sexe_homme = as.numeric(clones$b_sexe == 1),
                      ile2france = as.numeric(clones$old_b_dep%in% c(75,78,91,92,93,94,95)),
                      b_min_Q1 = as.numeric(clones$b_annee %in% c(1891:1910)), #Min-Q1
                      b_Q1_median = as.numeric(clones$b_annee %in% c(1911:1916)), #Q1-Median
                      b_median_Q3 = as.numeric(clones$b_annee %in% c(1916:1922)), #Median-Q3
                      b_Q3_max = as.numeric(clones$b_annee %in% c(1923:1940)),#Q3-Max
                      #age_sign_vieux = as.numeric(clones$cat_age_sign == "Vieux"),
                      status = clones$statuts)

my.formula <- Surv(time, status) ~ sexe_homme + ile2france + b_min_Q1 + 
  b_Q1_median + b_median_Q3 + b_Q3_max #+ age_sign_vieux


# Utilisation de coxDT()
model.vend <- coxDT(my.formula, L = ltrunc, R = rtrunc, data = vendeurs2)
model.clone <- coxDT(my.formula, L = ltrunc, R = rtrunc, data = clones2)

haz.ratio.vend <- round(exp(as.numeric(model.vend$results.beta[,1])), digits = 3)
haz.ratio.clone <- round(exp(as.numeric(model.clone$results.beta[,1])), digits = 3)

varnames <- rownames(model.vend$results.beta)
estim.names <- c("Haz.ratio", colnames(model.vend$results.beta))
estim.names[c(2,3,6)] <- c("beta", "se.beta", "Wald stat") 

resultats.vend <- matrix(c(haz.ratio.vend, as.numeric(model.vend$results.beta)), 
                         nrow = length(varnames), dimnames = list(varnames, estim.names))
resultats.vend[is.na(resultats.vend)] <- 0.000

resultats.clone <- matrix(c(haz.ratio.clone, as.numeric(model.clone$results.beta)), 
                          nrow = length(varnames), dimnames = list(varnames, estim.names))
resultats.clone[is.na(resultats.clone)] <- 0.000

resultats.vend
resultats.clone

## Afficher les résultats 
hazard_ratios1 <- resultats.clone[, "Haz.ratio"]

barplot(hazard_ratios1, horiz = FALSE, col = "orange", main = "Hazard Ratios",
        ylab = "Hazard Ratio", ylim = c(0, max(hazard_ratios1) * 1.2))
text(x = hazard_ratios1*1.1, labels = names(hazard_ratios1))

hazard_ratios2 <- resultats.vend[, "Haz.ratio"]

barplot(hazard_ratios2, horiz = FALSE, col = "lightblue", main = "Hazard Ratios",
        ylab = "Hazard Ratio", ylim = c(0, max(hazard_ratios2) * 1.2))
text(x = hazard_ratios2*1.1, labels = names(hazard_ratios2))

#########

# 2) Estimations avec bouquet et rente
sum(is.na(vendeurs2$bouquet))
sum(is.na(vendeurs2$rente))

# Comme nous allons utiliser le log, il nous faut retirer les 0 du dataset 

new_vendeurs <- subset(vendeurs2, (rente != 0)&(bouquet != 0))

my.formula2 <- Surv(time, status) ~ sexe_homme + ile2france + b_min_Q1 + 
  b_Q1_median + b_median_Q3 + b_Q3_max + oneheadcontr + 
  log(bouquet) + log(rente)

model.vend2 <- coxDT(my.formula2, L = ltrunc, R = rtrunc, 
                     data = new_vendeurs)

haz.ratio.vend2 <- round(exp(as.numeric(model.vend2$results.beta[,1])), digits = 3)

varnames2 <- rownames(model.vend2$results.beta)

resultats.vend2 <- matrix(c(haz.ratio.vend2, as.numeric(model.vend2$results.beta)), 
                          nrow = length(varnames2), dimnames = list(varnames2, estim.names))
resultats.vend2[is.na(resultats.vend2)] <- 0.000

resultats.vend2

## Afficher les résultats 
hazard_ratios <- resultats.vend2[, "Haz.ratio"]

barplot(hazard_ratios, horiz = FALSE, col = "lightblue", main = "Hazard Ratios",
        ylab = "Hazard Ratio", ylim = c(0, max(hazard_ratios) * 1.2))
text(x = hazard_ratios*1.1, labels = names(hazard_ratios))



##### Modèle de fragilté 
vendeurs2 <- na.omit(vendeurs2)

vendeurs2 <- vendeurs2[order(vendeurs2$time), ]

mph.vend <- emfrail(formula = Surv(time, status) ~ sexe_homme + 
                      ile2france + b_min_Q1 + 
                      b_Q1_median + b_median_Q3 + b_Q3_max + 
                      cluster(numbase), 
                    data = subset(vendeurs2), 
                    distribution = emfrail_dist(dist = "gamma",
                                                 left_truncation = TRUE, 
                                                 basehaz = "exponential")) 

# plot this 
fr_var <- seq(from = 0.01, to = 1.4, length.out = 20)
# For gamma the variance is 1/theta (see parametrizations)
pll_gamma <- emfrail_pll(formula = Surv(time, status) ~ sexe_homme + 
                           ile2france + b_min_Q1 + 
                           b_Q1_median + b_median_Q3 + b_Q3_max + 
                           cluster(numbase),
                         data = vendeurs2,
                         values = 1/fr_var )
plot(fr_var, pll_gamma,
     type = "l",
     xlab = "Frailty variance",
     ylab = "Profile log-likelihood")


#########
clones2 <- na.omit(clones2)

clones2 <- clones2[order(clones2$time), ]

mph.clone <- emfrail(formula = Surv(time, status==1) ~ sexe_homme + 
                       ile2france + b_min_Q1 + 
                       b_Q1_median + b_median_Q3 + b_Q3_max + cluster (numbase), 
                     data = subset(clones2, time > ltrunc), 
                     distribution = emfrail_dist (dist = "gamma",
                                                  left_truncation = TRUE, 
                                                  basehaz = "exponential")) 
mph.clone


# Avec bouquet et rente
vendeurs3 <- subset(vendeurs2, (rente != 0)&(bouquet != 0))
any(is.na(vendeurs3$bouquet))
any(vendeurs3$bouquet == 0)

any(is.na(vendeurs3$rente))
any(vendeurs3$rente == 0)


vendeurs3 <- na.omit(vendeurs3)
vendeurs3 <- vendeurs3[order(vendeurs3$time), ]

mph.vend2 <- emfrail(formula = Surv(time, status) ~ sexe_homme + 
                       ile2france + b_min_Q1 + 
                       b_Q1_median + b_median_Q3 + b_Q3_max + log(bouquet) + log(rente) + cluster(numbase), 
                     data = vendeurs3,  
                     distribution = emfrail_dist (dist = "gamma",
                                                  left_truncation = TRUE, 
                                                  basehaz = "exponential")) 
mph.vend2



rm(list = ls())
################################################################################################### 