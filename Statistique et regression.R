
summary(pauvrete1)
attach(pauvrete1)

# 1. Introduction

# 2.2. Variables
#Les différentes types de variables 
str(pauvrete1)

# 4.1. Statistiques descriptives
# Statistiques descriptives et analyses univariées 
hist(pauvrete1$Urbanisation,main = "Histogramme d'Urbanisation",xlab ="Taux d'urbanisation",freq = TRUE)
hist(pauvrete1$DepensesPuEduc,main ="Histogramme de DepensesPuEduc",xlab ="Taux de dépense publique en éducation",freq =TRUE)
hist(pauvrete1$PIB.hab,main ="Histogramme du PIB.hab en milliards de dollars",xlab ="PIB.hab en milliards de dollars",freq =TRUE )
hist(pauvrete1$AccElectricite,main ="Histogramme de AccElectricite",xlab ="Taux d'accès à l'éléctricité par pays",freq = TRUE)

# 4.1.2 Analyse bivariée
cor.test(pauvrete1$Pauvrete,pauvrete1$Urbanisation)
cor.test(pauvrete1$Pauvrete,pauvrete1$PIB.hab)
cor.test(pauvrete1$Pauvrete,pauvrete1$DepensesPuEduc)
cor.test(pauvrete1$Pauvrete,pauvrete1$AccElectricite)

# 4.1.3. Normalisation des variables à distribution asymétrique
hist(log(PIB.hab))
hist(log(AccElectricite))

# 4.2.1. Représentation graphiques
# Nuage de points
plot(Pauvrete~Urbanisation,main="Nuage de points Urbanisation",data=pauvrete1)
plot(Pauvrete~DepensesPuEduc,main="Nuage de points DepensesPuEduc",data=pauvrete1)
plot(Pauvrete~log(PIB.hab),main= "Nuage de points PIB.hab",data=pauvrete1)
plot(Pauvrete~AccElectricite,main="Nuage de points AccElectricite", data=pauvrete1)

# Régression linéaire simple : Méthode MCO
scatter.smooth(Pauvrete~Urbanisation,main="Nuage de points Urbanisation",data=pauvrete1)
scatter.smooth(Pauvrete~DepensesPuEduc,main="Nuage de points DepensesPuEduc",data=pauvrete1)
scatter.smooth(Pauvrete~log(PIB.hab),main= "Nuage de points log(PIB.hab)",data=pauvrete1)
scatter.smooth(Pauvrete~AccElectricite,main="Nuage de points AccElectricite", data=pauvrete1)

# Résulats des éstimations MCO
reg=lm(Pauvrete~Urbanisation,data=pauvrete1)
summary(reg)
plot(reg)
reg2=lm(Pauvrete~DepensesPuEduc,data=pauvrete1)
summary(reg2)
plot(reg2)
reg3=lm(Pauvrete~log(PIB.hab),data=pauvrete1)
summary(reg3)
plot(reg3)
reg4=lm(Pauvrete~AccElectricite,data=pauvrete1)
summary(reg4)
reg5=lm(Pauvrete~Urbanisation+DepensesPuEduc+log(PIB.hab)+AccElectricite,data=pauvrete1)
summary(reg5)
plot(reg5)

# 4.2.4. Test de colinéarité : Test de VIF
library(car)
vif(reg5)
vif_values=vif(reg5)
summary(vif_values)
hist(vif(reg5),main="VIF Values")


# 4.2.7. Diagnistics graphiques des résidus et respect des hypothèses
# Hypothèses d'un modèle de regression par méthode des MCO
#Linéarité des paramètres
library("lmtest")
raintest(reg5)
plot(reg5) #"Entrée" à plusieurs reprises.
#Residuals vs Fitted

#Indépendance des résidus 
acf(residuals(reg5),main="") 
durbinWatsonTest (reg5)

#Normalité des résidus
resid=residuals(reg5)
qqnorm(resid)
qqline(resid)
resid<- residuals(reg5)
shapiro.test(resid)
plot(reg5) #"Entrée" à plusieurs reprises
#Normal QQ

#Homoscédasticité
lmtest::bptest(reg5)
install.packages("skedastic")
library("skedastic")
white(reg5)
glejser(reg5)
plot(reg5)#"Entrée" à plusieurs reprises
#Scale-Location

#Points abberants
reg5bis=lm(Pauvrete~Urbanisation+DepensesPuEduc+log(PIB.hab)+AccElectricite,data=pauvrete1[-c(1,5,27),])
outlierTest(reg5bis)
#plot(reg5)
#Residuals vs Leverage

# 5. Correction du modèle
reg8=lm(Pauvrete~Urbanisation+log(PIB.hab),data=pauvrete1)
summary(reg8)


`

