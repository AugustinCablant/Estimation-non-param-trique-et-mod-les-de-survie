# Packages
install.packages('SurvTrunc')
library(SurvTrunc)
#install.packages('survival')
library(survival)
install.packages('ggplot2')
library(ggplot2)
install.packages('frailtyEM')
library(frailtyEM)

# Chargement des donnees
donnees <- read.csv(file = "/Users/augustincablant/Desktop/Viagers/merge_clone_seller.csv", header = TRUE)

print(colnames(donnees))

femmes <- subset(donnees, b_sexe == 2)
hommes <- subset(donnees, b_sexe == 1)
vendeurs <- subset(donnees, groupe == "seller")
clones <- subset(donnees,groupe == "clone")


#################################### Statistiques descriptives#################################### 

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

# 2) Stat def sur Tr_dif
summary(donnees $ Tr_dif)
resultat_test <- t.test(donnees$ Tr_dif, mu = 0)
print(resultat_test)
donnees_sans_na <- na.omit(donnees$Tr_dif)
densite <- density(donnees_sans_na)
plot(densite, col = "lightblue", lwd = 3, main = "Densités de Tr_dif")
legend("topright", legend = c("Tr_dif"), col = c("lightblue"), lwd = 3, bty = "n")

# 3) Stat des sur Tr_dif sur les ventes à une tête 

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

# 12) Par région en France métropolitaine 

# 13) Par tranche de bouquet 

# 14) Par tranche de rente 
######################### Estimation non paramétrique, cdfDT #########################
