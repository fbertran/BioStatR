#' ---
#' title: "Initiation \u00e0 la statistique avec R, code et compl\u00e9ments chapitre 9"
#' author: "Fr\u00e9d\u00e9ric Bertrand et Myriam Maumy-Bertrand"
#' date: "20 mars 2023"
#' ---

#Chapitre 9
require(BioStatR)

#page 378
#Exercice 9.1
#1)
lauriers<-subset(Mesures,subset=(Mesures$espece=="laurier rose"))
plot(taille~masse,data=lauriers,pch=19)

#page 379
#3)
droite_lauriers<-lm(taille~masse,data=lauriers)
coef(droite_lauriers)
#4)
fitted(droite_lauriers)

#page 380
#5)
abline(coef(droite_lauriers),col="red",lwd=2)
#6)
predict(droite_lauriers,data.frame(masse=4.8))
#fonctionne comme predict(droite_lauriers,list(masse=4.8))
#7)
residuals(droite_lauriers)[lauriers$masse==4.8]

#page 381
#8)
mean(lauriers$taille)
6.413523+1.700114*mean(lauriers$masse)
coef(droite_lauriers)[1]+coef(droite_lauriers)[2]*mean(lauriers$masse)
#9)
summary(droite_lauriers)

#page 382
#10)
anova(droite_lauriers)
#11)
summary(droite_lauriers)

#page 383
#12)
residus<-residuals(droite_lauriers)
shapiro.test(residus)

#page 384
plot(lauriers$masse,residus)
pdf("residusmasse.pdf")
plot(lauriers$masse,residus)
dev.off()

#Les r\'esidus ont l'air corrects => homosc\'edasticit\'e des erreurs ok et 
#absence d'effet syst\'ematique
#Approche par permutation valide

#13)
if(!("lmPerm" %in% rownames(installed.packages()))){install.packages("lmPerm")}
library(lmPerm)
lmp(taille~masse,lauriers)

#page 385
perm_laurier<-lmp(taille~masse,lauriers,center=FALSE)
summary(perm_laurier)

#page 386
#14)
confint(droite_lauriers)
predict(droite_lauriers,list(masse=c(4.8)),interval="confidence")
predict(droite_lauriers,list(masse=c(4.8)),interval="prediction")

#page 387
#Exercice 9.2
#1)
bignones<-subset(Mesures5,subset=(Mesures5$espece=="bignone"))[,c(1,4)]
plot(masse~masse_sec,data=bignones,pch=19)
pdf("figure94.pdf")
plot(masse~masse_sec,data=bignones,pch=19)
dev.off()
#3)a)
droite_bignones<-lm(masse~masse_sec,data=bignones)
coef(droite_bignones)

#page 388
residus<-residuals(droite_bignones)
plot(bignones$masse_sec,residus)
pdf("figure95.pdf")
plot(bignones$masse_sec,residus)
dev.off()
#Les r\'esidus n'ont l'air corrects car ils pr\'esentent une forme en trompette, 
#ce qui remet en question de l'homosc\'edasticit\'e des erreurs. Nous proc\'ederons
#dans la suite \`a un test pour nous assurer que ce d\'efaut est significatif au 
#seuil de \alpha=5%. Par contre les r\'esidus ont l'air r\'epartis al\'eatoirement 
#au-dessus ou en-dessous de l'axe des abscisses. Vous notez \'egalement l'absence 
#d'un effet syst\'ematique qui se traduirait par exemple par une forme de banane. 
#L'hypoth\`ese d'ind\'ependance n'est pas remise en question.
#Malgr\'e l'inhomog\'en\'eit\'e des variances l'estimation de la pente et de l'ordonn\'ee 
#\`a l'origine reste sans biais. Il sera, par contre, n\'ecessaire tenir compte de 
#l'h\'et\'erosc\'edastict\'e des erreurs pour la mise en oeuvre des proc\'edures de test et 
#la construction des intervalles de confiance.

#page 389
#4)
fitted(droite_bignones)
#5)
plot(masse~masse_sec,data=bignones,pch=19)
abline(coef(droite_bignones),col="red",lwd=2)
pdf("figure96.pdf")
plot(masse~masse_sec,data=bignones,pch=19)
abline(coef(droite_bignones),col="red",lwd=2)
dev.off()
#6)
predict(droite_bignones,data.frame(masse_sec=2.5))
plot(masse~masse_sec,data=bignones,pch=19)
abline(coef(droite_bignones),col="red",lwd=2)
points(2.5,predict(droite_bignones,data.frame(masse_sec=2.5)),pch=17,col="blue")
segments(2.5, bignones$masse[bignones$masse_sec==2.5],2.5,
  predict(droite_bignones,data.frame(masse_sec=2.5)),lty=2,lwd=2)
pdf("figure96residusmasselinepoint.pdf")
plot(masse~masse_sec,data=bignones,pch=19)
abline(coef(droite_bignones),col="red",lwd=2)
points(2.5,predict(droite_bignones,data.frame(masse_sec=2.5)),pch=17,col="blue")
segments(2.5, bignones$masse[bignones$masse_sec==2.5],2.5,
  predict(droite_bignones,data.frame(masse_sec=2.5)),lty=2,lwd=2)
dev.off()

#page 390
#7)
residuals(droite_bignones)[bignones$masse_sec==2.5]
#8)
mean(bignones$masse)
-0.5391407+4.8851935*mean(bignones$masse_sec)
coef(droite_bignones)[1]+coef(droite_bignones)[2]*mean(bignones$masse_sec)

#page 391
#9)
summary(droite_bignones)
#10)
anova(droite_bignones)

#page 392
#12) et 13)
residus<-residuals(droite_bignones)
shapiro.test(residus)
length(residus)
#Les r\'esidus sont au nombre de 70 sup\'erieur ou \'egal \`a 30. Le test de normalit\'e 
#est donc fiable. La $p$-valeur du test est strictement sup\'erieure \`a \alpha=5%, 
#le test n'est pas significatif. Nous conservons, par d\'efaut, l'hypoth\`ese H0 
#de normalit\'e des erreurs.

#page 393
#Le test de White est un cas particulier du test de Breusch-Pagan qui est 
#disponible dans le biblioth\`eque lmtest
if(!("lmtest" %in% rownames(installed.packages()))){install.packages("lmtest")}
library(lmtest)
bptest(droite_bignones, ~ masse_sec + I(masse_sec^2), data = bignones)
## White test (Table 5.1, p. 113)
#bptest(cig_lm2, ~ income * price + I(income^2) + I(price^2), data = CigarettesB)

#Le test de White permet de s'int\'eresser aux deux hypoth\`eses : 
#"H0 : les erreurs sont homosc\'edastiques" 
#contre
#"H1 : les erreurs sont h\'et\'erosc\'edastiques". 
#L'hypoth\`ese de normalit\'e des erreurs n'a \'et\'e remise en cause, le test de White 
#est donc fiable. La $p$-valeur du test est inf\'erieure ou \'egale \`a \alpha=5%, 
#le test est significatif. Nous rejetons l'hypoth\`ese H0 d'homosc\'edasticit\'e 
#des erreurs et d\'ecidons que l'hypoth\`ese alternative d'h\'et\'erosc\'edasticit\'e 
#des erreurs est vraie.

#Comme nous l'avions per\c cu graphiquement, les erreurs ne sont pas homosc\'edastiques,
#il faut tenir compte de cette inhomog\'en\'eit\'e des variances lors de l'estimation 
#des param\`etres du mod\`ele puis de la mise en oeuvre des tests de student ou 
#du test global de Fisher pour la r\'egression.
if(!("sandwich" %in% rownames(installed.packages()))){install.packages("sandwich")}
library(sandwich)
vcovHC(droite_bignones)
#Estimation, tenant de l'inhomog\'en\'eit\'e des variances, de la matrice de 
#variance-covariance des estimateurs \hat\beta_0 et \hat\beta_1.
coeftest(droite_bignones, df="inf", vcov=vcovHC)
#Tests de student des coefficient \beta_0 et \beta_1.

#page 394
waldtest(droite_bignones, vcov=vcovHC)
#Tests de Fihser global du mod\`ele de r\'egression lin\'eaire simple.

#Pour construire les intervalles de confiance autour des param\`etres, 
#vous poouvez utiliser la biblioth\`eque hcci.
if(!("hcci" %in% rownames(installed.packages()))){install.packages("hcci")}
library(hcci)
?hcci
#L'aide de la biblioth\`eque HCCI vous apprend qu'il existe plusieurs proc\'edures 
#permettant de tenir compte de l'h\'et\'erosc\'edasticit\'e. La fonction vcovHC utilise 
#la m\'ethode HC3 par d\'efaut  La fonction HC, la m\'ethode HC4 avec le param\`etre k=0.7
#par d\'efaut. Les m\'ethodes HC3, HC4 et HC5 sont recommend\'ees. En comparant leurs 
#r\'esultats, vous constatez qu'elles aboutissent toutes aux m^emes conclusions 
#au seuil de \alpha=5% : conservation, par d\'efaut, de "H0 : \beta_0=0" pour 
#le test de l'ordonn\'ee \`a l'origine et d\'ecision que "H1 : \beta_1<>0" est vraie.
HC(droite_bignones,method=3)
coeftest(droite_bignones, df="inf", vcov=HC(droite_bignones,method=3))

#page 395
vcovHC(droite_bignones,type="HC4")
coeftest(droite_bignones, df="inf", vcov=vcovHC(droite_bignones,type="HC4"))
vcovHC(droite_bignones,type="HC4m")
coeftest(droite_bignones, df="inf", vcov=vcovHC(droite_bignones,type="HC4m"))

#page 396
HC(droite_bignones,method=4,k=0.7)
coeftest(droite_bignones, df="inf", vcov=HC(droite_bignones,method=4,k=0.7))
vcovHC(droite_bignones,type="HC5")
coeftest(droite_bignones, df="inf", vcov=vcovHC(droite_bignones,type="HC5"))
HC(droite_bignones,method=5)

#page 397
coeftest(droite_bignones, df="inf", vcov=HC(droite_bignones,method=5))

#Passons \`a la construction d'intervalles de confiance sur les param\`etres 
#\beta_0 et \beta_1 de la r\'egression lin\'eaire simple. Nous devons passer par 
#cette \'etape de r\'e\'ecriture du mod\`ele pour pouvoir utiliser les fonctions Pboot 
#et Tbootde la biblioth\`eque hcci.
y = bignones$masse 
x = bignones$masse_sec
model = lm(y ~ x)

#Il est possible de "fixer" le point de d\'epart du g\'en\'erateur al\'eatoir
#pour avoir des r\'esultats reproductibles \`a l'aide de la fonction set.seed
set.seed(123456)

#Commencez par utiliser une technique de bootstrap simple.
#Bootstrap percentile simple.
Pboot(model, significance = 0.05, double = FALSE, J=1000, K = 100, 
  distribution = "rademacher")

#page 398
#Bootstrap t simple.
Tboot(model, significance = 0.05, double = FALSE, J=1000, K = 100, 
  distribution = "rademacher")

#Utilisez maintenant une technique de bootstrap double.
#Bootstrap percentile double.
Pboot(model, significance = 0.05, double = TRUE, J=1000, K = 100, 
  distribution = "rademacher")
#Bootstrap t double.
Tboot(model, significance = 0.05, double = TRUE, J=1000, K = 100, 
  distribution = "rademacher")

#Le mod\`ele \'etant h\'et\'erosc\'edastique, la construction d'intervalles de pr\'ediction 
#n'est pas fiable

