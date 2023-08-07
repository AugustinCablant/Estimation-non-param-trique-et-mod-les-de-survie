###### Install packages
#install.packages('survival')
#install.packages('SurvTrunc')
#install.packages('ggplot2')
#install.packages("progress")
#install.packages('frailtyEM')
#install.packages("survminer")
#install.packages("glm2")
#install.packages("gridExtra")

library(survival)
library(dplyr)
library(SurvTrunc)
library(ggplot2)
library(stats)
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
### Faire les stats pour un clone tiré aléatoirement 
tirage <- function(data, nb.obs) {
  echantillon <-  sample(1:nrow(data), size = nb.obs, replace = FALSE)
  data <- data[echantillon,]
  return(data)
}
clones.list <- split.data.frame(clones, clones$jd)
clones.list <- lapply(clones.list, tirage, nb.obs = 1)
sample_clone = do.call(rbind,clones.list)

clones = sample_clone

# 1) Stat des sur Tr 
vendeurs $ Tr_seller = vendeurs $ Tr
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

##########


# 2) Stat def sur Tr_dif

summary(clones $ Tr_dif)
resultat_test <- t.test(clones$ Tr_dif, mu = 0)
print(resultat_test)
donnees_sans_na <- na.omit(clones$Tr_dif)
densite <- density(donnees_sans_na)
plot(densite, col = "lightblue", lwd = 3, main = "Densités de Tr_dif chez les clones")
legend("topright", legend = c("Tr_dif"), col = c("lightblue"), lwd = 3, bty = "n")

######## Distinction sur les têtes 
unetete <- clones[clones$nb_tete == 1, ]
unetete_femme <- unetete[unetete$b_sexe == 2, ]
unetete_homme <- unetete[unetete$b_sexe == 1, ]

deuxtete <- clones[clones$nb_tete == 2, ]
deuxtete_tete1 <- deuxtete[deuxtete$head == 1, ]
deuxtete_tete2 <- deuxtete[deuxtete$head == 2, ]

table(deuxtete_tete2$b_sexe)

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

## 4)bis) Stat des sur Tr_dif sur les ventes à deux têtes, focus sur la tête 1, femme / homme
deuxtete_tete1_femme = deuxtete_tete1[deuxtete_tete1$b_sexe == 2,]
deuxtete_tete1_homme = deuxtete_tete1[deuxtete_tete1$b_sexe == 1,]
summary(deuxtete_tete1_femme $ Tr_dif)
summary(deuxtete_tete1_homme $ Tr_dif)

resultat_test <- t.test(deuxtete_tete1_femme$ Tr_dif, mu = 0)
print(resultat_test)
resultat_test2 <- t.test(deuxtete_tete1_homme$ Tr_dif, mu = 0)
print(resultat_test2)

donnees_sans_na <- na.omit(deuxtete_tete1_femme$Tr_dif)
densite <- density(donnees_sans_na)

donnees_sans_na2 <- na.omit(deuxtete_tete1_homme$Tr_dif)
densite2 <- density(donnees_sans_na2)

plot(densite, col = "red", lwd = 3, main = "Densités de Tr_dif pour la tete 1 (homme vs femme) des ventes à deux têtes ")
lines(densite2, col = "blue", lwd = 3)
legend("topright", legend = c("Tr_dif femme", 'Tr_dif homme'), col = c("red","blue"), lwd = 3, bty = "n")


##########


# 5) Stat des sur Tr_dif sur les ventes à deux têtes, focus sur la tête 2 
summary(deuxtete_tete2 $ Tr_dif)
resultat_test <- t.test(deuxtete_tete2$ Tr_dif, mu = 0)
print(resultat_test)
donnees_sans_na <- na.omit(deuxtete_tete2$Tr_dif)
densite <- density(donnees_sans_na)
plot(densite, col = "cyan", lwd = 3, main = "Densités de Tr_dif pour la tete 2 des ventes à deux têtes ")
legend("topright", legend = c("Tr_dif"), col = c("cyan"), lwd = 3, bty = "n")

## 5)bis) Stat des sur Tr_dif sur les ventes à deux têtes, focus sur la tête 2, femme / homme
deuxtete_tete2_femme = deuxtete_tete2[deuxtete_tete2$b_sexe == 2,]
deuxtete_tete2_homme = deuxtete_tete2[deuxtete_tete2$b_sexe == 1,]
summary(deuxtete_tete2_femme $ Tr_dif)
summary(deuxtete_tete2_homme $ Tr_dif)

resultat_test <- t.test(deuxtete_tete2_femme$ Tr_dif, mu = 0)
print(resultat_test)
resultat_test2 <- t.test(deuxtete_tete2_homme$ Tr_dif, mu = 0)
print(resultat_test2)

donnees_sans_na <- na.omit(deuxtete_tete2_femme$Tr_dif)
densite <- density(donnees_sans_na)

donnees_sans_na2 <- na.omit(deuxtete_tete2_homme$Tr_dif)
densite2 <- density(donnees_sans_na2)

max_density <- max(densite$y, densite2$y)
plot(densite, col = "orange", lwd = 3, main = "Densités de Tr_dif pour la tete 2 (homme vs femme) des ventes à deux têtes ", ylim = c(0, max_density))
lines(densite2, col = "cyan", lwd = 3)
legend("topright", legend = c("Tr_dif femme", 'Tr_dif homme'), col = c("orange","cyan"), lwd = 3, bty = "n")


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
clones_femme = clones[clones$b_sexe == 2,]
clones_homme = clones[clones$b_sexe == 1,]
vendeurs_homme = vendeurs[vendeurs$b_sexe==1,]
vendeurs_femme = vendeurs[vendeurs$b_sexe==1,]


densite_femme_sans_na <- na.omit(vendeurs_femme $ Td)
densite_femme_seller <- density(densite_femme_sans_na)
densite_femme_sans_na2 <- na.omit(clones_femme $ Td_clone)
densite_femme_clone <- density(densite_femme_sans_na2)

plot(densite_femme_seller, col = "orange", lwd = 3, main = "Densités Td_clone et Td_seller chez les femmes")
lines(densite_femme_clone, col = "cyan", lwd = 3)
legend("topright", legend = c("Td_seller", "Td_clone"), col = c("orange", "cyan"), lwd = 3, bty = "n")

densite_homme_sans_na <- na.omit(vendeurs_homme $ Td)
densite_homme_seller <- density(densite_homme_sans_na)
densite_homme_sans_na2 <- na.omit(clones_homme $ Td_clone)
densite_homme_clone <- density(densite_homme_sans_na2)

plot(densite_homme_seller, col = "green", lwd = 3, main = "Densités Td_clone et Td_seller chez les hommes")
lines(densite_homme_clone, col = "purple", lwd = 3)
legend("topright", legend = c("Td_seller", "Td_clone"), col = c("green", "purple"), lwd = 3, bty = "n")


# Stat des sur Tr_dif par genre 
## femmes 
summary(clones_femme $ Tr_dif)
resultat_test <- t.test(femmes$ Tr_dif, mu = 0)
print(resultat_test)
## hommes 
summary(clones_homme $ Tr_dif)
resultat_test <- t.test(hommes$ Tr_dif, mu = 0)
print(resultat_test)

donnees_sans_na1 <- na.omit(clones_femme$Tr_dif)
densite1 <- density(donnees_sans_na1)
donnees_sans_na2 <- na.omit(clones_homme$Tr_dif)
densite2 <- density(donnees_sans_na2)
max_density <- max(densite1$y, densite2$y)
plot(densite1, col = "orange", lwd = 3, main = "Densités de Tr_dif pour les femmes et les hommes", ylim = c(0, max_density))
lines(densite2, col = "cyan", lwd = 3)
legend("topright", legend = c("Tr_dif femme","Tr_dif homme") , col = c("orange","cyan"), lwd = 3, bty = "n")


##########



# 9) Stat des sur Tr_dif par période de naissance
donnees <- bind_rows(vendeurs,clones)

summary(vendeurs $ b_annee)
periode1 <- subset(donnees, b_annee <= 1910)
periode2 <- subset(donnees, b_annee <= 1916 & b_annee > 1910)
periode3 <- subset(donnees, b_annee <= 1922 & b_annee > 1916)
periode4 <- subset(donnees, b_annee > 1922)

periode1_filtered <- subset(periode1, Tr_dif != 0)
periode2_filtered <- subset(periode2, Tr_dif != 0)
periode3_filtered <- subset(periode3, Tr_dif != 0)
periode4_filtered <- subset(periode4, Tr_dif != 0)

test1 <- t.test(periode1 $ Tr_dif, mu = 0)
test2 <- t.test(periode2 $ Tr_dif, mu = 0)
test3 <- t.test(periode3 $ Tr_dif, mu = 0)
test4 <- t.test(periode4 $ Tr_dif, mu = 0)

test11 <- t.test(periode1_filtered $ Tr_dif, mu = 0)
test22 <- t.test(periode2_filtered $ Tr_dif, mu = 0)
test33 <- t.test(periode3_filtered $ Tr_dif, mu = 0)
test44 <- t.test(periode4_filtered $ Tr_dif, mu = 0)

print(test1) 
print(test2) 
print(test3) 
print(test4) 
print(test11) 
print(test22) 
print(test33) 
print(test44)

donnees_sans_na1 <- na.omit(periode1_filtered$Tr_dif)
densite1 <- density(donnees_sans_na1)
donnees_sans_na2 <- na.omit(periode2_filtered$Tr_dif)
densite2 <- density(donnees_sans_na2)
donnees_sans_na3 <- na.omit(periode3_filtered$Tr_dif)
densite3 <- density(donnees_sans_na3)
donnees_sans_na4 <- na.omit(periode4_filtered$Tr_dif)
densite4 <- density(donnees_sans_na4)

plot(densite1, col = "orange", lwd = 3, main = "Densités de Tr_dif selon les périodes")
lines(densite2, col = "cyan", lwd = 3)
lines(densite3, col = "green", lwd = 3)
lines(densite4, col = "purple", lwd = 3)
legend("topright", legend = c("année naissance<1910","1910<année naissance<=1919",
                              "1919<année naissance<=1923", "année naissance>1923") , col = c("orange","cyan", "green", "purple"), lwd = 3, bty = "n")


##########



# 11) Par tranche d'âge à la signature du contrat 
donnees $ Ts <- ifelse(donnees$groupe == "seller", donnees $ Ts, donnees $ Ts_clone)
donnees $ Ts_age <- round((donnees $ Ts)/365)
summary(donnees $ Ts)
summary(donnees $ Ts_age)

ts1 <- subset(donnees, Ts<= 25401)
ts2 <- subset(donnees, Ts <= 26907 & Ts > 25401)
ts3 <- subset(donnees, Ts <= 28455 & Ts > 26907)
ts4 <- subset(donnees, Ts> 28455)

ts1_filtered <- subset(ts1, Tr_dif != 0)
ts2_filtered <- subset(ts2, Tr_dif != 0)
ts3_filtered <- subset(ts3, Tr_dif != 0)
ts4_filtered <- subset(ts4, Tr_dif != 0)

test1 <- t.test(ts1 $ Tr_dif, mu = 0)
test2 <- t.test(ts2 $ Tr_dif, mu = 0)
test3 <- t.test(ts3 $ Tr_dif, mu = 0)
test4 <- t.test(ts4 $ Tr_dif, mu = 0)
test11 <- t.test(ts1_filtered $ Tr_dif, mu = 0)
test22 <- t.test(ts2_filtered $ Tr_dif, mu = 0)
test33 <- t.test(ts3_filtered $ Tr_dif, mu = 0)
test44 <- t.test(ts4_filtered $ Tr_dif, mu = 0)

print(test1) 
print(test2) 
print(test3) 
print(test4)
print(test11) 
print(test22) 
print(test33) 
print(test44) 

donnees_sans_na1 <- na.omit(ts1_filtered$Tr_dif)
densite1 <- density(donnees_sans_na1)
donnees_sans_na2 <- na.omit(ts2_filtered$Tr_dif)
densite2 <- density(donnees_sans_na2)
donnees_sans_na3 <- na.omit(ts3_filtered$Tr_dif)
densite3 <- density(donnees_sans_na3)
donnees_sans_na4 <- na.omit(ts4_filtered$Tr_dif)
densite4 <- density(donnees_sans_na4)

max_density <- max(densite1$y, densite2$y, densite3$y, densite4$y)

par(ask = FALSE, mar = c(5, 5, 4, 2) + 0.2)
plot(densite1, col = "red", lwd = 3, main = "Densités de Tr_dif selon l'âge du vendeur au moment de la signature du contrat", ylim = c(0, max_density))
lines(densite2, col = "blue", lwd = 3)
lines(densite3, col = "green", lwd = 3)
lines(densite4, col = "magenta", lwd = 3)
legend("topright", legend = c("Ts<= 25401","Ts <= 26907 & Ts > 25401","Ts <= 26907 & Ts > 25401", "Ts> 28455") , col = c("red","blue", "green", "magenta"), lwd = 3, bty = "n")


##########



# 12) Par région en France métropolitaine 
ile_france <- subset(donnees, old_b_dep %in% c(75, 78, 91, 92, 93, 94, 95))
reste_france_metropolitaine <- subset(donnees, !(old_b_dep %in% c(75, 78, 91, 92, 93, 94, 95)))
etranger <- subset(donnees, old_b_dep==99)

ile_france_filtered <- subset(ile_france, Tr_dif != 0)
reste_france_metropolitaine_filtered <- subset(reste_france_metropolitaine, Tr_dif != 0)
etranger_filtered <- subset(etranger, Tr_dif != 0)

test1 <- t.test(ile_france $ Tr_dif, mu = 0)
test2 <- t.test(reste_france_metropolitaine $ Tr_dif, mu = 0)
test3 <- t.test(etranger $ Tr_dif, mu = 0)
test11 <- t.test(ile_france_filtered $ Tr_dif, mu = 0)
test22 <- t.test(reste_france_metropolitaine_filtered $ Tr_dif, mu = 0)
test33 <- t.test(etranger_filtered $ Tr_dif, mu = 0)


print(test1) 
print(test2) 
print(test3) 
print(test11) 
print(test22) 
print(test33)


donnees_sans_na1 <- na.omit(ile_france_filtered$Tr_dif)
densite1 <- density(donnees_sans_na1)
donnees_sans_na2 <- na.omit(reste_france_metropolitaine_filtered$Tr_dif)
densite2 <- density(donnees_sans_na2)
donnees_sans_na3 <- na.omit(etranger_filtered$Tr_dif)
densite3 <- density(donnees_sans_na3)


plot(densite1, col = "black", lwd = 3, main = "Densités de Tr_dif selon les régions de naissance", ylim = c(0,0.00014))
lines(densite2, col = "red", lwd = 3)
lines(densite3, col = "grey", lwd = 3)
legend("topright", legend = c("Île-de-France","Reste de la France métropolitaine","Étranger") , col = c("black","red", "grey"), lwd = 3, bty = "n")


##########



# 13) Par tranche de bouquet 
summary(vendeurs $ downp)

liste_jd <- unique(donnees[donnees$groupe == 'seller', 'jd'])

for (j in liste_jd) {
  sub_df = vendeurs[vendeurs$jd==j,]
  bouquet = sub_df$downp
  rente = sub_df$annuity
  clones[clones$jd==j, "downp"] = bouquet
  clones[clones$jd==j, "annuity"] = rente
}

bouquet1 = subset(clones, downp <= 10671)
bouquet2 = subset(clones, downp <= 27441 & downp > 10671)
bouquet3 = subset(clones, downp <= 59455 & downp > 27441)
bouquet4 = subset(clones, downp > 59455)

test1 <- t.test(bouquet1 $ Tr_dif, mu = 0)
test2 <- t.test(bouquet2 $ Tr_dif, mu = 0)
test3 <- t.test(bouquet3 $ Tr_dif, mu = 0)
test3 <- t.test(bouquet4 $ Tr_dif, mu = 0)

print(test1)
print(test2)
print(test3)
print(test4)


donnees_sans_na1 <- na.omit(bouquet1$Tr_dif)
densite1 <- density(donnees_sans_na1)
donnees_sans_na2 <- na.omit(bouquet2$Tr_dif)
densite2 <- density(donnees_sans_na2)
donnees_sans_na3 <- na.omit(bouquet3$Tr_dif)
densite3 <- density(donnees_sans_na3)
donnees_sans_na4 <- na.omit(bouquet4$Tr_dif)
densite4 <- density(donnees_sans_na4)

max_density <- max(densite1$y, densite2$y, densite3$y, densite4$y)

par(ask = FALSE, mar = c(5, 5, 4, 2) + 0.2)
plot(densite1, col = "cyan", lwd = 3, main = "Densités de Tr_dif selon le bouquet", ylim = c(0, max_density))
lines(densite2, col = "green", lwd = 3)
lines(densite3, col = "grey", lwd = 3)
lines(densite4, col = "black", lwd = 3)
legend("topright", legend = c("bouquet<= 10671€"," 10671€<bouquet<=27441€","27441€<bouquet<=59455€", "bouquet> 59455€") , col = c("cyan","green", "grey", "black"), lwd = 3, bty = "n")


##########



# 14) Par tranche de rente 
summary(vendeurs $ annuity)

rente1 = subset(clones, annuity <= 4574)
rente2 = subset(clones, downp <= 7318 & downp > 4574)
rente3 = subset(clones, downp <= 12193 & downp > 7318)
rente4 = subset(clones, downp > 12193)

test1 <- t.test(rente1 $ Tr_dif, mu = 0)
test2 <- t.test(rente2 $ Tr_dif, mu = 0)
test3 <- t.test(rente3 $ Tr_dif, mu = 0)
test3 <- t.test(rente4 $ Tr_dif, mu = 0)

print(test1)
print(test2)
print(test3)
print(test4)


donnees_sans_na1 <- na.omit(rente1$Tr_dif)
densite1 <- density(donnees_sans_na1)
donnees_sans_na2 <- na.omit(rente2$Tr_dif)
densite2 <- density(donnees_sans_na2)
donnees_sans_na3 <- na.omit(rente3$Tr_dif)
densite3 <- density(donnees_sans_na3)
donnees_sans_na4 <- na.omit(rente4$Tr_dif)
densite4 <- density(donnees_sans_na4)

max_density <- max(densite1$y, densite2$y, densite3$y, densite4$y)

par(ask = FALSE, mar = c(5, 5, 4, 2) + 0.2)
plot(densite1, col = "yellow", lwd = 3, main = "Densités de Tr_dif selon la rente", ylim = c(0, max_density))
lines(densite2, col = "orange", lwd = 3)
lines(densite3, col = "red", lwd = 3)
lines(densite4, col = "purple", lwd = 3)
legend("topright", legend = c("rente<= 4574€"," 4574€<rente<=7318€","7318€<rente<=12193€", "rente> 12193€") , col = c("yellow","orange", "red", "purple"), lwd = 3, bty = "n")


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


##########


# 17) Régression par MCO de T_{dif} sur sexe, region, bouquet, rente, vente à une têtes
clones_MCO = clones
clones_MCO$sexe_homme = as.numeric(clones_MCO$b_sexe==1)
clones_MCO$idf = as.numeric(clones_MCO$old_b_dep %in% c(75,78,91,92,93,94,95))
clones_MCO$etranger = as.numeric(clones_MCO$old_b_dep==99)
clones_MCO$une_tete = as.numeric(clones_MCO$nb_tete==1)


modele_regression <- lm(Tr_dif ~ sexe_homme + idf + etranger + une_tete + downp + annuity , data = clones_MCO)
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

p5 <- ggplot(data = clones_MCO, aes(x = downp, y = Tr_dif)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("une_tete") +
  ylab("Tr_dif") +
  ggtitle("Régression linéaire : Tr_dif en fonction de bouquet")

p6 <- ggplot(data = clones_MCO, aes(x = annuity, y = Tr_dif)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("une_tete") +
  ylab("Tr_dif") +
  ggtitle("Régression linéaire : Tr_dif en fonction de rente")



grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)

#########

# 18) Régression logistique de indicatrice(Tr_dif > 0) sur sexe, region, bouquet, rente, vente à une têtes 

clones_MCO $ indicatrice_Tr = ifelse(clones_MCO$Tr_dif <= 0, 0, 1)


modele_regression_logistique <- glm(indicatrice_Tr ~ sexe_homme + idf + etranger + une_tete , data = clones_MCO, family=binomial)
summary(modele_regression_logistique)

p1 <- ggplot(data = clones_MCO, aes(x = sexe_homme, y = indicatrice_Tr)) +
  geom_point() +
  geom_smooth(method = "glm",method.args = list(family = binomial), se = FALSE, color = "blue") +
  xlab("sexe_homme") +
  ylab("Indicatrice(Tr_dif>0)") +
  ggtitle("Régression logistique : Indicatrice(Tr_dif>0) en fonction de sexe_homme")

p2 <- ggplot(data = clones_MCO, aes(x = idf, y = indicatrice_Tr)) +
  geom_point() +
  geom_smooth(method = "glm",method.args = list(family = binomial), se = FALSE, color = "blue") +
  xlab("idf") +
  ylab("Indicatrice(Tr_dif>0)") +
  ggtitle("Régression logistique : Indicatrice(Tr_dif>0) en fonction de idf")

p3 <- ggplot(data = clones_MCO, aes(x = etranger, y = indicatrice_Tr)) +
  geom_point() +
  geom_smooth(method = "glm",method.args = list(family = binomial), se = FALSE, color = "blue") +
  xlab("etranger") +
  ylab("Indicatrice(Tr_dif>0)") +
  ggtitle("Régression logistique : Indicatrice(Tr_dif>0) en fonction de etranger")

p4 <- ggplot(data = clones_MCO, aes(x = une_tete, y = indicatrice_Tr)) +
  geom_point() +
  geom_smooth(method = "glm",method.args = list(family = binomial), se = FALSE, color = "blue") +
  xlab("une_tete") +
  ylab("Indicatrice(Tr_dif>0)") +
  ggtitle("Régression logistique : Indicatrice(Tr_dif>0) en fonction de une_tete")


grid.arrange(p1, p2, p3, p4, ncol = 2)
################################################################################################################################################ 
################################################################################################################################################ 
################################################################################################################################################ 








################################################################################################################################################ 
################################################################################################################################################ 
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


clones.list <- lapply(clones.list, tirage, nb.obs = 1)
clones <- do.call(rbind, clones.list)

rm(clones.list, tirage)

clone.Td <- clones$Td_clone
clone.ltrunc <- clones$ltrunc
clone.rtrunc <- clones$rtrunc

liste_jd = unique(vendeurs $ jd)

clones <- clones[clones $ jd %in% liste_jd, ]

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

summary(estimations)

ggplot(data = estimations, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = groupe, colour = groupe)) +
  geom_ribbon(aes(group = groupe, fill = groupe, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survie (avec 95% IC)") +
  xlab("Annees") + ylab("Fonction de survie") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = quantile(vend.Td),
                     labels = function(x) round(x, digits = 0))

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

summary(estimationsfemme)

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

summary(estimationshomme)

ggplot(data = estimationshomme, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones hommes (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")


estimsexe = rbind(estimationsfemme, estimationshomme)

# Graphique en commun
ggplot(data = estimsexe, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones selon le sexe (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")


########## Estimation par âge au moment de l'acte de vente 
# Créer la variable age_acte qui correspond à l'âge du vendeur au moment de la vente en viager
vendeurs$age_acte = vendeurs$annee - vendeurs$b_annee
summary(vendeurs$age_acte)

# création de la variable tranche_age
##### tranche_age == 1 si age_acte in [55,71]
##### tranche_age == 2 si age_acte in [72,75]
##### tranche_age == 3 si age_acte in [76,80]
##### tranche_age == 4 si age_acte in [80,90]
vendeurs$tranche_age = 0
clones$tranche_age = 0
liste_jd <- unique(vendeurs$jd)
for (k in liste_jd) {
  # Filtrer les données pour le jd en cours
  subset_data <- vendeurs[vendeurs$jd == k, ]
  
  # Mettre à jour la colonne tranche_age en fonction de la valeur de age_acte
  vendeurs[vendeurs$jd == k & vendeurs$age_acte %in% 55:71, "tranche_age"] <- 1
  vendeurs[vendeurs$jd == k & vendeurs$age_acte %in% 72:75, "tranche_age"] <- 2
  vendeurs[vendeurs$jd == k & vendeurs$age_acte %in% 76:80, "tranche_age"] <- 3
  vendeurs[vendeurs$jd == k & vendeurs$age_acte %in% 81:90, "tranche_age"] <- 4
}

for (k in liste_jd) {
  # Filtrer les données pour le jd en cours
  subset_data <- clones[clones$jd == k, ]
  tranche = vendeurs$tranche_age[k]
  # Mettre à jour la colonne tranche_age en fonction de la valeur de age_acte
  clones[clones$jd == k, "tranche_age"] <- tranche
}

# tranche 1 
resultstranche1.vend <- cdfDT(y = vendeurs$Td[(vendeurs$tranche_age == 1)], l = vendeurs$ltrunc[(vendeurs$tranche_age == 1)], r = vendeurs$rtrunc[(vendeurs$tranche_age == 1)], 
                           plot.cdf = FALSE, display = FALSE, boot = TRUE)

resultstranche1.clone <- cdfDT(y = clones$Td_clone[clones$tranche_age == 1], l = clones$ltrunc[clones$tranche_age == 1], r = clones$rtrunc[clones$tranche_age == 1], 
                            plot.cdf = FALSE, display = FALSE, boot = TRUE)

vendtranche1.estim <- data.frame(group = "Tranche 1 Vendeurs", time = resultstranche1.vend$time, 
                              survival = resultstranche1.vend$Survival, 
                              CI.lower = 1 - resultstranche1.vend$CI.lower.F,
                              CI.upper = 1 - resultstranche1.vend$CI.upper.F)


clonetranche1.estim <- data.frame(group = "Tranche 1 Clone", time = resultstranche1.clone$time, 
                               survival = resultstranche1.clone$Survival, 
                               CI.lower = 1 - resultstranche1.clone$CI.lower.F,
                               CI.upper = 1 - resultstranche1.clone$CI.upper.F)

estimationstranche1 <- rbind(vendtranche1.estim, clonetranche1.estim)

# tranche 2
resultstranche2.vend <- cdfDT(y = vendeurs$Td[(vendeurs$tranche_age == 2)], l = vendeurs$ltrunc[(vendeurs$tranche_age == 2)], r = vendeurs$rtrunc[(vendeurs$tranche_age == 2)], 
                              plot.cdf = FALSE, display = FALSE, boot = TRUE)

resultstranche2.clone <- cdfDT(y = clones$Td_clone[clones$tranche_age == 2], l = clones$ltrunc[clones$tranche_age == 2], r = clones$rtrunc[clones$tranche_age == 2], 
                               plot.cdf = FALSE, display = FALSE, boot = TRUE)

vendtranche2.estim <- data.frame(group = "Tranche 2 Vendeurs", time = resultstranche2.vend$time, 
                                 survival = resultstranche2.vend$Survival, 
                                 CI.lower = 1 - resultstranche2.vend$CI.lower.F,
                                 CI.upper = 1 - resultstranche2.vend$CI.upper.F)


clonetranche2.estim <- data.frame(group = "Tranche 2 Clone", time = resultstranche2.clone$time, 
                                  survival = resultstranche2.clone$Survival, 
                                  CI.lower = 1 - resultstranche2.clone$CI.lower.F,
                                  CI.upper = 1 - resultstranche2.clone$CI.upper.F)

estimationstranche2 <- rbind(vendtranche2.estim, clonetranche2.estim)

# tranche 3
resultstranche3.vend <- cdfDT(y = vendeurs$Td[(vendeurs$tranche_age == 3)], l = vendeurs$ltrunc[(vendeurs$tranche_age == 3)], r = vendeurs$rtrunc[(vendeurs$tranche_age == 3)], 
                              plot.cdf = FALSE, display = FALSE, boot = TRUE)

resultstranche3.clone <- cdfDT(y = clones$Td_clone[clones$tranche_age == 3], l = clones$ltrunc[clones$tranche_age == 3], r = clones$rtrunc[clones$tranche_age == 3], 
                               plot.cdf = FALSE, display = FALSE, boot = TRUE)

vendtranche3.estim <- data.frame(group = "Tranche 3 Vendeurs", time = resultstranche3.vend$time, 
                                 survival = resultstranche3.vend$Survival, 
                                 CI.lower = 1 - resultstranche3.vend$CI.lower.F,
                                 CI.upper = 1 - resultstranche3.vend$CI.upper.F)


clonetranche3.estim <- data.frame(group = "Tranche 3 Clone", time = resultstranche3.clone$time, 
                                  survival = resultstranche3.clone$Survival, 
                                  CI.lower = 1 - resultstranche3.clone$CI.lower.F,
                                  CI.upper = 1 - resultstranche3.clone$CI.upper.F)

estimationstranche3 <- rbind(vendtranche3.estim, clonetranche3.estim)

# tranche 4
resultstranche4.vend <- cdfDT(y = vendeurs$Td[(vendeurs$tranche_age == 4)], l = vendeurs$ltrunc[(vendeurs$tranche_age == 4)], r = vendeurs$rtrunc[(vendeurs$tranche_age == 4)], 
                              plot.cdf = FALSE, display = FALSE, boot = TRUE)

resultstranche4.clone <- cdfDT(y = clones$Td_clone[clones$tranche_age == 4], l = clones$ltrunc[clones$tranche_age == 4], r = clones$rtrunc[clones$tranche_age == 4], 
                               plot.cdf = FALSE, display = FALSE, boot = TRUE)

vendtranche4.estim <- data.frame(group = "Tranche 4 Vendeurs", time = resultstranche4.vend$time, 
                                 survival = resultstranche4.vend$Survival, 
                                 CI.lower = 1 - resultstranche4.vend$CI.lower.F,
                                 CI.upper = 1 - resultstranche4.vend$CI.upper.F)


clonetranche4.estim <- data.frame(group = "Tranche 4 Clone", time = resultstranche4.clone$time, 
                                  survival = resultstranche4.clone$Survival, 
                                  CI.lower = 1 - resultstranche4.clone$CI.lower.F,
                                  CI.upper = 1 - resultstranche4.clone$CI.upper.F)

estimationstranche4 <- rbind(vendtranche4.estim, clonetranche4.estim)

summary(estimationstranche1)
summary(estimationstranche2)
summary(estimationstranche3)
summary(estimationstranche4)

## Affichage graphique 
p1 <-ggplot(data = estimationstranche1, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones de 55-71 ans (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

p2 <- ggplot(data = estimationstranche2, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones de 72-75 ans (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

p3 <-ggplot(data = estimationstranche3, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones de 76-80 ans (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

p4 <-ggplot(data = estimationstranche4, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones de 80-90 ans (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

grid.arrange(p1, p2, p3, p4, ncol = 2)



########## Estimation par décennie de naissance 
summary(vendeurs$b_annee)
# création de la variable dec
###dec==1 si b_annee dans [1891,1910]
###dec==2 si b_annee dans [1911,1916]
###dec==3 si b_annee dans [1917,1922]
###dec==4 si b_annee dans [1923,1936]

vendeurs$dec = 0
clones$dec = 0
liste_jd <- unique(vendeurs$jd)
for (k in liste_jd) {
  # Filtrer les données pour le jd en cours
  subset_data <- vendeurs[vendeurs$jd == k, ]
  
  # Mettre à jour la colonne tranche_age en fonction de la valeur de age_acte
  vendeurs[vendeurs$jd == k & vendeurs$b_annee %in% 1891:1910, "dec"] <- 1
  vendeurs[vendeurs$jd == k & vendeurs$b_annee %in% 1911:1916, "dec"] <- 2
  vendeurs[vendeurs$jd == k & vendeurs$b_annee %in% 1917:1922, "dec"] <- 3
  vendeurs[vendeurs$jd == k & vendeurs$b_annee %in% 1923:1936, "dec"] <- 4
}

for (k in liste_jd) {
  # Filtrer les données pour le jd en cours
  subset_data <- clones[clones$jd == k, ]
  decc = vendeurs$dec[k]
  # Mettre à jour la colonne tranche_age en fonction de la valeur de age_acte
  clones[clones$jd == k, "dec"] <- decc
}


# dec 1 
resultsdec1.vend <- cdfDT(y = vendeurs$Td[(vendeurs$dec == 1)], l = vendeurs$ltrunc[(vendeurs$dec == 1)], r = vendeurs$rtrunc[(vendeurs$dec == 1)], 
                              plot.cdf = FALSE, display = FALSE, boot = TRUE)

resultsdec1.clone <- cdfDT(y = clones$Td_clone[clones$dec == 1], l = clones$ltrunc[clones$dec == 1], r = clones$rtrunc[clones$dec == 1], 
                               plot.cdf = FALSE, display = FALSE, boot = TRUE)

venddec1.estim <- data.frame(group = "Dec 1 Vendeurs", time = resultsdec1.vend$time, 
                                 survival = resultsdec1.vend$Survival, 
                                 CI.lower = 1 - resultsdec1.vend$CI.lower.F,
                                 CI.upper = 1 - resultsdec1.vend$CI.upper.F)


clonedec1.estim <- data.frame(group = "Dec 1 Clone", time = resultsdec1.clone$time, 
                                  survival = resultsdec1.clone$Survival, 
                                  CI.lower = 1 - resultsdec1.clone$CI.lower.F,
                                  CI.upper = 1 - resultsdec1.clone$CI.upper.F)

estimationsdec1 <- rbind(venddec1.estim, clonedec1.estim)

# dec 2
resultsdec2.vend <- cdfDT(y = vendeurs$Td[(vendeurs$dec == 2)], l = vendeurs$ltrunc[(vendeurs$dec == 2)], r = vendeurs$rtrunc[(vendeurs$dec == 2)], 
                              plot.cdf = FALSE, display = FALSE, boot = TRUE)

resultsdec2.clone <- cdfDT(y = clones$Td_clone[clones$dec == 2], l = clones$ltrunc[clones$dec == 2], r = clones$rtrunc[clones$dec == 2], 
                               plot.cdf = FALSE, display = FALSE, boot = TRUE)

venddec2.estim <- data.frame(group = "Dec 2 Vendeurs", time = resultsdec2.vend$time, 
                                 survival = resultsdec2.vend$Survival, 
                                 CI.lower = 1 - resultsdec2.vend$CI.lower.F,
                                 CI.upper = 1 - resultsdec2.vend$CI.upper.F)


clonedec2.estim <- data.frame(group = "Dec 2 Clone", time = resultsdec2.clone$time, 
                                  survival = resultsdec2.clone$Survival, 
                                  CI.lower = 1 - resultsdec2.clone$CI.lower.F,
                                  CI.upper = 1 - resultsdec2.clone$CI.upper.F)

estimationsdec2 <- rbind(venddec2.estim, clonedec2.estim)

# tranche 3
resultsdec3.vend <- cdfDT(y = vendeurs$Td[(vendeurs$dec == 3)], l = vendeurs$ltrunc[(vendeurs$dec == 3)], r = vendeurs$rtrunc[(vendeurs$dec == 3)], 
                              plot.cdf = FALSE, display = FALSE, boot = TRUE)

resultsdec3.clone <- cdfDT(y = clones$Td_clone[clones$dec == 3], l = clones$ltrunc[clones$dec == 3], r = clones$rtrunc[clones$dec == 3], 
                               plot.cdf = FALSE, display = FALSE, boot = TRUE)

venddec3.estim <- data.frame(group = "Dec 3 Vendeurs", time = resultsdec3.vend$time, 
                                 survival = resultsdec3.vend$Survival, 
                                 CI.lower = 1 - resultsdec3.vend$CI.lower.F,
                                 CI.upper = 1 - resultsdec3.vend$CI.upper.F)


clonedec3.estim <- data.frame(group = "Dec 3 Clone", time = resultsdec3.clone$time, 
                                  survival = resultsdec3.clone$Survival, 
                                  CI.lower = 1 - resultsdec3.clone$CI.lower.F,
                                  CI.upper = 1 - resultsdec3.clone$CI.upper.F)

estimationsdec3 <- rbind(venddec3.estim, clonedec3.estim)

# tranche 4
resultsdec4.vend <- cdfDT(y = vendeurs$Td[(vendeurs$dec == 4)], l = vendeurs$ltrunc[(vendeurs$dec == 4)], r = vendeurs$rtrunc[(vendeurs$dec == 4)], 
                              plot.cdf = FALSE, display = FALSE, boot = TRUE)

resultsdec4.clone <- cdfDT(y = clones$Td_clone[clones$dec == 4], l = clones$ltrunc[clones$dec == 4], r = clones$rtrunc[clones$dec == 4], 
                               plot.cdf = FALSE, display = FALSE, boot = TRUE)

venddec4.estim <- data.frame(group = "Dec 4 Vendeurs", time = resultsdec4.vend$time, 
                                 survival = resultsdec4.vend$Survival, 
                                 CI.lower = 1 - resultsdec4.vend$CI.lower.F,
                                 CI.upper = 1 - resultsdec4.vend$CI.upper.F)


clonedec4.estim <- data.frame(group = "Dec 4 Clone", time = resultsdec4.clone$time, 
                                  survival = resultsdec4.clone$Survival, 
                                  CI.lower = 1 - resultsdec4.clone$CI.lower.F,
                                  CI.upper = 1 - resultsdec4.clone$CI.upper.F)

estimationsdec4 <- rbind(venddec4.estim, clonedec4.estim)

summary(estimationsdec1)
summary(estimationsdec2)
summary(estimationsdec3)
summary(estimationsdec4)
## Affichage graphique 
p1 <-ggplot(data = estimationsdec1, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones nés entre 1891 et 1910 (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

p2 <- ggplot(data = estimationsdec2, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones nés entre 1911 et 1916 (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

p3 <-ggplot(data = estimationstranche3, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones nés entre 1917 et 1922 (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

p4 <-ggplot(data = estimationstranche4, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones nés entre 1923 et 1936 (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

grid.arrange(p1, p2, p3, p4, ncol = 2)



# Croiser les informations : 
liste_graphiques <- list()
for (i in c(1, 2, 3, 4)) {
  for (j in c(1, 2, 3, 4)) {
    e.vend <- cdfDT(y = vendeurs$Td[(vendeurs$dec == i)&(vendeurs$tranche_age == j)], 
                    l = vendeurs$ltrunc[(vendeurs$dec == i)&(vendeurs$tranche_age == j)], 
                    r = vendeurs$rtrunc[(vendeurs$dec == i)&(vendeurs$tranche_age == j)], 
                              plot.cdf = FALSE, display = FALSE, boot = TRUE)
    
    r.clone <- cdfDT(y = clones$Td_clone[(clones$dec == i)&(clones$tranche_age == j)], 
                     l = clones$ltrunc[(clones$dec == i)&(clones$tranche_age == j)], 
                     r = clones$rtrunc[(clones$dec == i)&(clones$tranche_age == j)], 
                               plot.cdf = FALSE, display = FALSE, boot = TRUE)
    
    vende.estim <- data.frame(group = paste("Vendeurs, dec, tranche_age :", i, j) , 
                                time = e.vend$time, 
                                 survival = e.vend$Survival, 
                                 CI.lower = 1 - e.vend$CI.lower.F,
                                 CI.upper = 1 - e.vend$CI.upper.F)
    
    
    cloner.estim <- data.frame(group = paste("Clones, dec, tranche_age :", i, j), 
                                  time = r.clone$time, 
                                  survival = r.clone$Survival, 
                                  CI.lower = 1 - r.clone$CI.lower.F,
                                  CI.upper = 1 - r.clone$CI.upper.F)
    
    
    assign(paste("estim",i,j, sep = "-"), rbind(vende.estim, cloner.estim))
    
    nom_graphique <- paste("p", i, j, sep = "-")
    graphique <- ggplot(data =  get(paste("estim",i,j, sep = "-")), aes(x = time, y = survival)) +
      theme_classic() +
      geom_line(aes(group = group, colour = group)) +
      geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
                  alpha = 1/3) +
      ggtitle(paste("Estimation des survies des vendeurs et clones dec, tranche  (avec 95% IC) :",i,j)) +
      xlab("Jours") + ylab("Fonction de survie")
    liste_graphiques[[nom_graphique]] <- graphique}}


grid.arrange(
  liste_graphiques$`p-1-1`, liste_graphiques$`p-1-2`, liste_graphiques$`p-1-3`, liste_graphiques$`p-1-4`,
  liste_graphiques$`p-2-1`, liste_graphiques$`p-2-2`, liste_graphiques$`p-2-3`, liste_graphiques$`p-2-4`,
  ncol = 2
)

grid.arrange(
  liste_graphiques$`p-3-1`, liste_graphiques$`p-3-2`, liste_graphiques$`p-3-3`, liste_graphiques$`p-3-4`,
  liste_graphiques$`p-4-1`, liste_graphiques$`p-4-2`, liste_graphiques$`p-4-3`, liste_graphiques$`p-4-4`,
  ncol = 2
)

# corrélation b_année et age_acte 
vendeurs$age_acte_jour = vendeurs$dateA - vendeurs$dateN
cor(vendeurs$dateN, vendeurs$age_acte)



paste("estim", i,j, sep = "-")

####### Estimation par nombre de clones 
vendeurs <- subset(donnees, groupe == "seller")
clones <- subset(donnees,groupe == "clone")
vendeurs$nb_clone = 0
clones$nb_clone = 0
liste_jd <- unique(vendeurs$jd)
for (j in liste_jd) {
  clones[clones$jd == j, 'nb_clone'] = nrow(clones[clones$jd == j,])
  vendeurs[vendeurs$jd == j, 'nb_clone'] = nrow(clones[clones$jd == j,])
  }
quantile(vendeurs$nb_clone, probs = seq(0.01, 0.99, 0.05))
summary(vendeurs$nb_clone)

####################################### Peut-être plus tard mais pas pour le moment ####################################### 
## création de la variable nbclone 
#### nbclone==1 si nb_clone dans [4,116]
#### nbclone==2 si nb_clone dans [117,222]
#### nbclone==3 si nb_clone dans [223,685]
#### nbclone==4 si nb_clone dans [686,2329]

liste_jd <- unique(vendeurs$jd)
vendeurs$nbclone <- 0
clones$nbclone <- 0

for (k in liste_jd) {
  # Filtrer les données pour le jd en cours
  subset_data <- vendeurs[vendeurs$jd == k, ]
  
  # Mettre à jour la colonne tranche_age en fonction de la valeur de age_acte
  vendeurs[vendeurs$jd == k & vendeurs$nb_clone %in% 4:116, "nbclone"] <- 1
  vendeurs[vendeurs$jd == k & vendeurs$nb_clone %in% 117:222, "nbclone"] <- 2
  vendeurs[vendeurs$jd == k & vendeurs$nb_clone %in% 223:685, "nbclone"] <- 3
  vendeurs[vendeurs$jd == k & vendeurs$nb_clone %in% 686:2329, "nbclone"] <- 4
  
  #clones
  nb = vendeurs[vendeurs$jd == k, "nbclone"]
  clones[clones$jd == k, "nbclone"] <- nb
}

## Estimations
vendeurs1 <- vendeurs[vendeurs$nbclone==1,]
vendeurs2 <- vendeurs[vendeurs$nbclone==2,]
vendeurs3 <- vendeurs[vendeurs$nbclone==3,]
vendeurs4 <- vendeurs[vendeurs$nbclone==4,]
clones1 <- clones[clones$nbclone==1,]
clones2 <- clones[clones$nbclone==2,]
clones3 <- clones[clones$nbclone==3,]
clones4 <- clones[clones$nbclone==4,]

clones1 <- clones1[order(clones1$jd, clones1$index), ]
clones2 <- clones2[order(clones2$jd, clones2$index), ]
clones3 <- clones3[order(clones3$jd, clones3$index), ]
clones4 <- clones4[order(clones4$jd, clones4$index), ]


vend1.Td <- vendeurs1$Td
vend1.ltrunc <- vendeurs1$ltrunc
vend1.rtrunc <- vendeurs1$rtrunc

vend2.Td <- vendeurs2$Td
vend2.ltrunc <- vendeurs2$ltrunc
vend2.rtrunc <- vendeurs2$rtrunc

vend3.Td <- vendeurs3$Td
vend3.ltrunc <- vendeurs3$ltrunc
vend3.rtrunc <- vendeurs3$rtrunc

vend4.Td <- vendeurs4$Td
vend4.ltrunc <- vendeurs4$ltrunc
vend4.rtrunc <- vendeurs4$rtrunc



clones1.list <- split.data.frame(clones1, clones1$jd)
clones2.list <- split.data.frame(clones2, clones2$jd)
clones3.list <- split.data.frame(clones3, clones3$jd)
clones4.list <- split.data.frame(clones4, clones4$jd)



tirage <- function(data, nb.obs) {
  echantillon <-  sample(1:nrow(data), size = nb.obs, replace = FALSE)
  data <- data[echantillon,]
  return(data)
}


clones1.list <- lapply(clones1.list, tirage, nb.obs = 4)
clones2.list <- lapply(clones2.list, tirage, nb.obs = 30)
clones3.list <- lapply(clones3.list, tirage, nb.obs = 40)
clones4.list <- lapply(clones4.list, tirage, nb.obs = 50)


clones1 <- do.call(rbind, clones1.list)
clones2 <- do.call(rbind, clones2.list)
clones3 <- do.call(rbind, clones3.list)
clones4 <- do.call(rbind, clones4.list)


rm(clones1.list, tirage)
rm(clones2.list, tirage)
rm(clones3.list, tirage)
rm(clones4.list, tirage)

clone1.Td <- clones1$Td_clone
clone1.ltrunc <- clones1$ltrunc
clone1.rtrunc <- clones1$rtrunc

clone2.Td <- clones2$Td_clone
clone2.ltrunc <- clones2$ltrunc
clone2.rtrunc <- clones2$rtrunc

clone3.Td <- clones3$Td_clone
clone3.ltrunc <- clones3$ltrunc
clone3.rtrunc <- clones3$rtrunc

clone4.Td <- clones4$Td_clone
clone4.ltrunc <- clones4$ltrunc
clone4.rtrunc <- clones4$rtrunc

#######

results1.vend <- cdfDT(y = vend1.Td, l = vend1.ltrunc, r = vend1.rtrunc, 
                      error= 1e-6, display = FALSE, boot = TRUE)


results1.clone <- cdfDT(y = clone1.Td, l = clone1.ltrunc, r = clone1.rtrunc, 
                       error= 1e-6, display = FALSE, boot = TRUE)


vend1.estim <- data.frame(groupe = "Vendeur1", time = results1.vend$time, 
                         survival = results1.vend$Survival, 
                         CI.lower = 1 - results1.vend$CI.lower.F,
                         CI.upper = 1 - results1.vend$CI.upper.F)

clone1.estim <- data.frame(groupe = "Clone1", time = results1.clone$time, 
                          survival = results1.clone$Survival, 
                          CI.lower = 1 - results1.clone$CI.lower.F,
                          CI.upper = 1 - results1.clone$CI.upper.F)

estimations1 <- rbind(vend1.estim, clone1.estim)

###########
results2.vend <- cdfDT(y = vend2.Td, l = vend2.ltrunc, r = vend2.rtrunc, 
                       error= 1e-6, display = FALSE, boot = TRUE)


results2.clone <- cdfDT(y = clone2.Td, l = clone2.ltrunc, r = clone2.rtrunc, 
                        error= 1e-6, display = FALSE, boot = TRUE)


vend2.estim <- data.frame(groupe = "Vendeur2", time = results2.vend$time, 
                          survival = results2.vend$Survival, 
                          CI.lower = 1 - results2.vend$CI.lower.F,
                          CI.upper = 1 - results2.vend$CI.upper.F)

clone2.estim <- data.frame(groupe = "Clone2", time = results2.clone$time, 
                           survival = results2.clone$Survival, 
                           CI.lower = 1 - results2.clone$CI.lower.F,
                           CI.upper = 1 - results2.clone$CI.upper.F)

estimations2 <- rbind(vend2.estim, clone2.estim)

###########
results3.vend <- cdfDT(y = vend3.Td, l = vend3.ltrunc, r = vend3.rtrunc, 
                       error= 1e-6, display = FALSE, boot = TRUE)


results3.clone <- cdfDT(y = clone3.Td, l = clone3.ltrunc, r = clone3.rtrunc, 
                        error= 1e-6, display = FALSE, boot = TRUE)


vend3.estim <- data.frame(groupe = "Vendeur3", time = results3.vend$time, 
                          survival = results3.vend$Survival, 
                          CI.lower = 1 - results3.vend$CI.lower.F,
                          CI.upper = 1 - results3.vend$CI.upper.F)

clone3.estim <- data.frame(groupe = "Clone3", time = results3.clone$time, 
                           survival = results3.clone$Survival, 
                           CI.lower = 1 - results3.clone$CI.lower.F,
                           CI.upper = 1 - results3.clone$CI.upper.F)

estimations3 <- rbind(vend3.estim, clone3.estim)

###########
results4.vend <- cdfDT(y = vend4.Td, l = vend4.ltrunc, r = vend4.rtrunc, 
                       error= 1e-6, display = FALSE, boot = TRUE)


results4.clone <- cdfDT(y = clone4.Td, l = clone4.ltrunc, r = clone4.rtrunc, 
                        error= 1e-6, display = FALSE, boot = TRUE)


vend4.estim <- data.frame(groupe = "Vendeur3", time = results4.vend$time, 
                          survival = results4.vend$Survival, 
                          CI.lower = 1 - results4.vend$CI.lower.F,
                          CI.upper = 1 - results4.vend$CI.upper.F)

clone4.estim <- data.frame(groupe = "Clone3", time = results4.clone$time, 
                           survival = results4.clone$Survival, 
                           CI.lower = 1 - results4.clone$CI.lower.F,
                           CI.upper = 1 - results4.clone$CI.upper.F)

estimations4 <- rbind(vend4.estim, clone4.estim)

## Affichage 

## Affichage graphique 
p1 <-ggplot(data = estimations1, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones avec 4 clones (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

p2 <- ggplot(data = estimations2, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones avec 30 clones (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

grid.arrange(p1, p2, ncol = 1)

p3 <-ggplot(data = estimations3, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones avec 40 clones (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

p4 <-ggplot(data = estimations4, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones avec 50 clones (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

#grid.arrange(p1, p2, p3, p4, ncol = 1)
##################################################################################################################### 
##################################################################################################################### 

######################################### 15 clones ###########################################

# On conserve les individus avec plus de 15 clones 
sub_vendeurs = vendeurs[vendeurs$nb_clone >= 15 , ]
nrow(sub_vendeurs)
liste_jd_sub_vendeurs = unique(sub_vendeurs$jd)
# 936 vendeurs

sub_clones = clones[clones$jd %in% liste_jd_sub_vendeurs, ]
nrow(sub_clones)
# 406725 clones 


sub_vend.Td <- sub_vendeurs$Td
sub_vend.ltrunc <- sub_vendeurs$ltrunc
sub_vend.rtrunc <- sub_vendeurs$rtrunc

sub_clones.list <- split.data.frame(sub_clones, sub_clones$jd)

tirage <- function(data, nb.obs) {
  echantillon <-  sample(1:nrow(data), size = nb.obs, replace = FALSE)
  data <- data[echantillon,]
  return(data)
}


sub_clones.list <- lapply(sub_clones.list, tirage, nb.obs = 15)
sub_clones <- do.call(rbind, sub_clones.list)
rm(sub_clones.list, tirage)
sub_clones.Td <- sub_clones$Td_clone
sub_clones.ltrunc <- sub_clones$ltrunc
sub_clones.rtrunc <- sub_clones$rtrunc


results.vend <- cdfDT(y = sub_vend.Td, l = sub_vend.ltrunc, r = sub_vend.rtrunc, 
                       error= 1e-6, display = FALSE, boot = TRUE)


results.clone <- cdfDT(y = sub_clones.Td, l = sub_clones.ltrunc, r = sub_clones.rtrunc, 
                        error= 1e-6, display = FALSE, boot = TRUE)


sub_vend.estim <- data.frame(groupe = "sub_vendeur", time = results.vend$time, 
                          survival = results.vend$Survival, 
                          CI.lower = 1 - results.vend$CI.lower.F,
                          CI.upper = 1 - results.vend$CI.upper.F)

sub_clone.estim <- data.frame(groupe = "sub_clone", time = results.clone$time, 
                           survival = results.clone$Survival, 
                           CI.lower = 1 - results.clone$CI.lower.F,
                           CI.upper = 1 - results.clone$CI.upper.F)

sub_estimations <- rbind(sub_vend.estim, sub_clone.estim)



ggplot(data = sub_estimations, aes(x = time, y = survival)) +
  geom_line(aes(group = groupe, colour = groupe)) +
  geom_ribbon(aes(group = groupe, fill = groupe, ymin = CI.lower, ymax = CI.upper), 
              alpha = 0.3) +  # alpha = 0.3 pour rendre l'intervalle de confiance plus transparent
  labs(title = "Estimation des survies des vendeurs et clones avec plus de 15 clones (avec 95% IC)",
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
  #scale_x_continuous(breaks = seq(0, max(sub_estimations$time), by = 365.25*2),  # Définir les étiquettes de l'axe x avec un intervalle de deux ans
                     #labels = function(x) ifelse(x %% 365.25 == 0, round(x, digits = 0), "")) +
  guides(colour = guide_legend(title = "Groupes"),  # Ajouter un titre à la légende des couleurs
         fill = guide_legend(title = "Groupes")) 


vendeurs = sub_vendeurs
clones = sub_clones



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


estimsexe = rbind(estimationsfemme, estimationshomme)

# Graphique en commun
ggplot(data = estimsexe, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones selon le sexe (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")


########## Estimation par âge au moment de l'acte de vente 
# Créer la variable age_acte qui correspond à l'âge du vendeur au moment de la vente en viager
vendeurs$age_acte = vendeurs$annee - vendeurs$b_annee
summary(vendeurs$age_acte)

# création de la variable tranche_age
##### tranche_age == 1 si age_acte in [55,71]
##### tranche_age == 2 si age_acte in [72,75]
##### tranche_age == 3 si age_acte in [76,80]
##### tranche_age == 4 si age_acte in [80,90]
vendeurs$tranche_age = 0
clones$tranche_age = 0
liste_jd <- unique(vendeurs$jd)
for (k in liste_jd) {
  # Filtrer les données pour le jd en cours
  subset_data <- vendeurs[vendeurs$jd == k, ]
  
  # Mettre à jour la colonne tranche_age en fonction de la valeur de age_acte
  vendeurs[vendeurs$jd == k & vendeurs$age_acte %in% 55:71, "tranche_age"] <- 1
  vendeurs[vendeurs$jd == k & vendeurs$age_acte %in% 72:75, "tranche_age"] <- 2
  vendeurs[vendeurs$jd == k & vendeurs$age_acte %in% 76:80, "tranche_age"] <- 3
  vendeurs[vendeurs$jd == k & vendeurs$age_acte %in% 81:90, "tranche_age"] <- 4
}

for (k in liste_jd) {
  # Filtrer les données pour le jd en cours
  subset_data <- clones[clones$jd == k, ]
  tranche = vendeurs$tranche_age[k]
  # Mettre à jour la colonne tranche_age en fonction de la valeur de age_acte
  clones[clones$jd == k, "tranche_age"] <- tranche
}

# tranche 1 
resultstranche1.vend <- cdfDT(y = vendeurs$Td[(vendeurs$tranche_age == 1)], l = vendeurs$ltrunc[(vendeurs$tranche_age == 1)], r = vendeurs$rtrunc[(vendeurs$tranche_age == 1)], 
                              plot.cdf = FALSE, display = FALSE, boot = TRUE)

resultstranche1.clone <- cdfDT(y = clones$Td_clone[clones$tranche_age == 1], l = clones$ltrunc[clones$tranche_age == 1], r = clones$rtrunc[clones$tranche_age == 1], 
                               plot.cdf = FALSE, display = FALSE, boot = TRUE)

vendtranche1.estim <- data.frame(group = "Tranche 1 Vendeurs", time = resultstranche1.vend$time, 
                                 survival = resultstranche1.vend$Survival, 
                                 CI.lower = 1 - resultstranche1.vend$CI.lower.F,
                                 CI.upper = 1 - resultstranche1.vend$CI.upper.F)


clonetranche1.estim <- data.frame(group = "Tranche 1 Clone", time = resultstranche1.clone$time, 
                                  survival = resultstranche1.clone$Survival, 
                                  CI.lower = 1 - resultstranche1.clone$CI.lower.F,
                                  CI.upper = 1 - resultstranche1.clone$CI.upper.F)

estimationstranche1 <- rbind(vendtranche1.estim, clonetranche1.estim)

# tranche 2
resultstranche2.vend <- cdfDT(y = vendeurs$Td[(vendeurs$tranche_age == 2)], l = vendeurs$ltrunc[(vendeurs$tranche_age == 2)], r = vendeurs$rtrunc[(vendeurs$tranche_age == 2)], 
                              plot.cdf = FALSE, display = FALSE, boot = TRUE)

resultstranche2.clone <- cdfDT(y = clones$Td_clone[clones$tranche_age == 2], l = clones$ltrunc[clones$tranche_age == 2], r = clones$rtrunc[clones$tranche_age == 2], 
                               plot.cdf = FALSE, display = FALSE, boot = TRUE)

vendtranche2.estim <- data.frame(group = "Tranche 2 Vendeurs", time = resultstranche2.vend$time, 
                                 survival = resultstranche2.vend$Survival, 
                                 CI.lower = 1 - resultstranche2.vend$CI.lower.F,
                                 CI.upper = 1 - resultstranche2.vend$CI.upper.F)


clonetranche2.estim <- data.frame(group = "Tranche 2 Clone", time = resultstranche2.clone$time, 
                                  survival = resultstranche2.clone$Survival, 
                                  CI.lower = 1 - resultstranche2.clone$CI.lower.F,
                                  CI.upper = 1 - resultstranche2.clone$CI.upper.F)

estimationstranche2 <- rbind(vendtranche2.estim, clonetranche2.estim)

# tranche 3
resultstranche3.vend <- cdfDT(y = vendeurs$Td[(vendeurs$tranche_age == 3)], l = vendeurs$ltrunc[(vendeurs$tranche_age == 3)], r = vendeurs$rtrunc[(vendeurs$tranche_age == 3)], 
                              plot.cdf = FALSE, display = FALSE, boot = TRUE)

resultstranche3.clone <- cdfDT(y = clones$Td_clone[clones$tranche_age == 3], l = clones$ltrunc[clones$tranche_age == 3], r = clones$rtrunc[clones$tranche_age == 3], 
                               plot.cdf = FALSE, display = FALSE, boot = TRUE)

vendtranche3.estim <- data.frame(group = "Tranche 3 Vendeurs", time = resultstranche3.vend$time, 
                                 survival = resultstranche3.vend$Survival, 
                                 CI.lower = 1 - resultstranche3.vend$CI.lower.F,
                                 CI.upper = 1 - resultstranche3.vend$CI.upper.F)


clonetranche3.estim <- data.frame(group = "Tranche 3 Clone", time = resultstranche3.clone$time, 
                                  survival = resultstranche3.clone$Survival, 
                                  CI.lower = 1 - resultstranche3.clone$CI.lower.F,
                                  CI.upper = 1 - resultstranche3.clone$CI.upper.F)

estimationstranche3 <- rbind(vendtranche3.estim, clonetranche3.estim)

# tranche 4
resultstranche4.vend <- cdfDT(y = vendeurs$Td[(vendeurs$tranche_age == 4)], l = vendeurs$ltrunc[(vendeurs$tranche_age == 4)], r = vendeurs$rtrunc[(vendeurs$tranche_age == 4)], 
                              plot.cdf = FALSE, display = FALSE, boot = TRUE)

resultstranche4.clone <- cdfDT(y = clones$Td_clone[clones$tranche_age == 4], l = clones$ltrunc[clones$tranche_age == 4], r = clones$rtrunc[clones$tranche_age == 4], 
                               plot.cdf = FALSE, display = FALSE, boot = TRUE)

vendtranche4.estim <- data.frame(group = "Tranche 4 Vendeurs", time = resultstranche4.vend$time, 
                                 survival = resultstranche4.vend$Survival, 
                                 CI.lower = 1 - resultstranche4.vend$CI.lower.F,
                                 CI.upper = 1 - resultstranche4.vend$CI.upper.F)


clonetranche4.estim <- data.frame(group = "Tranche 4 Clone", time = resultstranche4.clone$time, 
                                  survival = resultstranche4.clone$Survival, 
                                  CI.lower = 1 - resultstranche4.clone$CI.lower.F,
                                  CI.upper = 1 - resultstranche4.clone$CI.upper.F)

estimationstranche4 <- rbind(vendtranche4.estim, clonetranche4.estim)


## Affichage graphique 
p1 <-ggplot(data = estimationstranche1, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones de 55-71 ans (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

p2 <- ggplot(data = estimationstranche2, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones de 72-75 ans (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

p3 <-ggplot(data = estimationstranche3, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones de 76-80 ans (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

p4 <-ggplot(data = estimationstranche4, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones de 80-90 ans (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

grid.arrange(p1, p2, p3, p4, ncol = 2)



########## Estimation par décennie de naissance 
summary(vendeurs$b_annee)
# création de la variable dec
###dec==1 si b_annee dans [1891,1910]
###dec==2 si b_annee dans [1911,1916]
###dec==3 si b_annee dans [1917,1922]
###dec==4 si b_annee dans [1923,1936]

vendeurs$dec = 0
clones$dec = 0
liste_jd <- unique(vendeurs$jd)
for (k in liste_jd) {
  # Filtrer les données pour le jd en cours
  subset_data <- vendeurs[vendeurs$jd == k, ]
  
  # Mettre à jour la colonne tranche_age en fonction de la valeur de age_acte
  vendeurs[vendeurs$jd == k & vendeurs$b_annee %in% 1891:1910, "dec"] <- 1
  vendeurs[vendeurs$jd == k & vendeurs$b_annee %in% 1911:1916, "dec"] <- 2
  vendeurs[vendeurs$jd == k & vendeurs$b_annee %in% 1917:1922, "dec"] <- 3
  vendeurs[vendeurs$jd == k & vendeurs$b_annee %in% 1923:1936, "dec"] <- 4
}

for (k in liste_jd) {
  # Filtrer les données pour le jd en cours
  subset_data <- clones[clones$jd == k, ]
  decc = vendeurs$dec[k]
  # Mettre à jour la colonne tranche_age en fonction de la valeur de age_acte
  clones[clones$jd == k, "dec"] <- decc
}


# dec 1 
resultsdec1.vend <- cdfDT(y = vendeurs$Td[(vendeurs$dec == 1)], l = vendeurs$ltrunc[(vendeurs$dec == 1)], r = vendeurs$rtrunc[(vendeurs$dec == 1)], 
                          plot.cdf = FALSE, display = FALSE, boot = TRUE)

resultsdec1.clone <- cdfDT(y = clones$Td_clone[clones$dec == 1], l = clones$ltrunc[clones$dec == 1], r = clones$rtrunc[clones$dec == 1], 
                           plot.cdf = FALSE, display = FALSE, boot = TRUE)

venddec1.estim <- data.frame(group = "Dec 1 Vendeurs", time = resultsdec1.vend$time, 
                             survival = resultsdec1.vend$Survival, 
                             CI.lower = 1 - resultsdec1.vend$CI.lower.F,
                             CI.upper = 1 - resultsdec1.vend$CI.upper.F)


clonedec1.estim <- data.frame(group = "Dec 1 Clone", time = resultsdec1.clone$time, 
                              survival = resultsdec1.clone$Survival, 
                              CI.lower = 1 - resultsdec1.clone$CI.lower.F,
                              CI.upper = 1 - resultsdec1.clone$CI.upper.F)

estimationsdec1 <- rbind(venddec1.estim, clonedec1.estim)

# dec 2
resultsdec2.vend <- cdfDT(y = vendeurs$Td[(vendeurs$dec == 2)], l = vendeurs$ltrunc[(vendeurs$dec == 2)], r = vendeurs$rtrunc[(vendeurs$dec == 2)], 
                          plot.cdf = FALSE, display = FALSE, boot = TRUE)

resultsdec2.clone <- cdfDT(y = clones$Td_clone[clones$dec == 2], l = clones$ltrunc[clones$dec == 2], r = clones$rtrunc[clones$dec == 2], 
                           plot.cdf = FALSE, display = FALSE, boot = TRUE)

venddec2.estim <- data.frame(group = "Dec 2 Vendeurs", time = resultsdec2.vend$time, 
                             survival = resultsdec2.vend$Survival, 
                             CI.lower = 1 - resultsdec2.vend$CI.lower.F,
                             CI.upper = 1 - resultsdec2.vend$CI.upper.F)


clonedec2.estim <- data.frame(group = "Dec 2 Clone", time = resultsdec2.clone$time, 
                              survival = resultsdec2.clone$Survival, 
                              CI.lower = 1 - resultsdec2.clone$CI.lower.F,
                              CI.upper = 1 - resultsdec2.clone$CI.upper.F)

estimationsdec2 <- rbind(venddec2.estim, clonedec2.estim)

# tranche 3
resultsdec3.vend <- cdfDT(y = vendeurs$Td[(vendeurs$dec == 3)], l = vendeurs$ltrunc[(vendeurs$dec == 3)], r = vendeurs$rtrunc[(vendeurs$dec == 3)], 
                          plot.cdf = FALSE, display = FALSE, boot = TRUE)

resultsdec3.clone <- cdfDT(y = clones$Td_clone[clones$dec == 3], l = clones$ltrunc[clones$dec == 3], r = clones$rtrunc[clones$dec == 3], 
                           plot.cdf = FALSE, display = FALSE, boot = TRUE)

venddec3.estim <- data.frame(group = "Dec 3 Vendeurs", time = resultsdec3.vend$time, 
                             survival = resultsdec3.vend$Survival, 
                             CI.lower = 1 - resultsdec3.vend$CI.lower.F,
                             CI.upper = 1 - resultsdec3.vend$CI.upper.F)


clonedec3.estim <- data.frame(group = "Dec 3 Clone", time = resultsdec3.clone$time, 
                              survival = resultsdec3.clone$Survival, 
                              CI.lower = 1 - resultsdec3.clone$CI.lower.F,
                              CI.upper = 1 - resultsdec3.clone$CI.upper.F)

estimationsdec3 <- rbind(venddec3.estim, clonedec3.estim)

# tranche 4
resultsdec4.vend <- cdfDT(y = vendeurs$Td[(vendeurs$dec == 4)], l = vendeurs$ltrunc[(vendeurs$dec == 4)], r = vendeurs$rtrunc[(vendeurs$dec == 4)], 
                          plot.cdf = FALSE, display = FALSE, boot = TRUE)

resultsdec4.clone <- cdfDT(y = clones$Td_clone[clones$dec == 4], l = clones$ltrunc[clones$dec == 4], r = clones$rtrunc[clones$dec == 4], 
                           plot.cdf = FALSE, display = FALSE, boot = TRUE)

venddec4.estim <- data.frame(group = "Dec 4 Vendeurs", time = resultsdec4.vend$time, 
                             survival = resultsdec4.vend$Survival, 
                             CI.lower = 1 - resultsdec4.vend$CI.lower.F,
                             CI.upper = 1 - resultsdec4.vend$CI.upper.F)


clonedec4.estim <- data.frame(group = "Dec 4 Clone", time = resultsdec4.clone$time, 
                              survival = resultsdec4.clone$Survival, 
                              CI.lower = 1 - resultsdec4.clone$CI.lower.F,
                              CI.upper = 1 - resultsdec4.clone$CI.upper.F)

estimationsdec4 <- rbind(venddec4.estim, clonedec4.estim)


## Affichage graphique 
p1 <-ggplot(data = estimationsdec1, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones nés entre 1891 et 1910 (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

p2 <- ggplot(data = estimationsdec2, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones nés entre 1911 et 1916 (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

p3 <-ggplot(data = estimationstranche3, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones nés entre 1917 et 1922 (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

p4 <-ggplot(data = estimationstranche4, aes(x = time, y = survival)) +
  theme_classic() +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group, ymin = CI.lower, ymax = CI.upper), 
              alpha = 1/3) +
  ggtitle("Estimation des survies des vendeurs et clones nés entre 1923 et 1936 (avec 95% IC)") +
  xlab("Jours") + ylab("Fonction de survie")

grid.arrange(p1, p2, p3, p4, ncol = 2)

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