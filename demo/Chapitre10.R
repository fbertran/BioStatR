#' ---
#' title: "Initiation \u00e0 la statistique avec R, code et compl\u00e9ments chapitre 10"
#' author: "Fr\u00e9d\u00e9ric Bertrand et Myriam Maumy-Bertrand"
#' date: "20 mars 2023"
#' ---

#Chapitre 10
require(BioStatR)
#page 403
foret<-rep(1:3,c(10,10,10))
hauteur<-c(23.4,24.4,24.6,24.9,25,26.2,26.1,24.8,25.5,25.8,18.9,21.1,21.1,
  22.1,22.5,23.5,22.7,21.3,22.2,21.7,22.5,22.9,23.7,24,24,24.5,24.3,24.2,
  23.4,23.9)
foret<-factor(foret)
arbre<-data.frame(foret,hauteur)
rm(foret)
rm(hauteur)
arbre
moyennes<-tapply(arbre$hauteur,arbre$foret,mean)
moyennes

#page 404
variances<-tapply(arbre$hauteur,arbre$foret,var)
variances

#page 405
moy.g<-mean(arbre$hauteur)
moy.g
mean(moyennes)

#page 406
plot(arbre$foret,arbre$hauteur)
points(1:3,moyennes,pch="@")
abline(h=moy.g)
pdf("ch11fig101.pdf")
plot(arbre$foret,arbre$hauteur)
points(1:3,moyennes,pch="@")
abline(h=moy.g)
dev.off()

#page 409
options(contrasts=c("contr.sum","contr.poly"))
modele1<-lm(hauteur~foret,data=arbre)
anova(modele1)
modele1_aov<-aov(hauteur~foret,data=arbre)
summary(modele1_aov)

#page 410
options(contrasts=c("contr.sum","contr.poly"))

#page 411
residus<-residuals(modele1)
shapiro.test(residus)
length(residus)

#En plus : les r\'esidus des deux mod\`eles sont \'egaux
all(residuals(modele1)==residuals(modele1_aov))

#page 413
bartlett.test(residus~foret,data=arbre)
coef(modele1)

#En plus : les coefficients des deux mod\`eles sont \'egaux
all(coef(modele1)==coef(modele1_aov))

#page 414
-sum(coef(modele1)[2:3])
dummy.coef(modele1)

#En plus : fonctionne aussi avec le mod\`ele aov et introduction de la 
#fonction model.tables
dummy.coef(modele1_aov)
model.tables(modele1_aov)

if(!("granova" %in% rownames(installed.packages()))){
  install.packages("granova")}
library(granova)
granova.1w(arbre$hauteur,arbre$foret)
pdf("chap10fig102.pdf")
print(granova.1w(arbre$hauteur,arbre$foret))
dev.off()

#page 416
if(!("granovaGG" %in% rownames(installed.packages()))){
  install.packages("granovaGG")}
library(granovaGG)
granovagg.1w(arbre$hauteur,arbre$foret)
pdf("chap10fig103.pdf")
print(granovagg.1w(arbre$hauteur,arbre$foret))
dev.off()

#page 419
modele2<-aov(hauteur~foret,data=arbre)
model.tables(modele2)
TukeyHSD(modele2)
plot(TukeyHSD(modele2))
pdf("chap10fig104.pdf")
plot(TukeyHSD(modele2))
dev.off()

#En plus : export des graphiques en niveaux de gris et aux formats .png ou .ps
png("chap10fig102.png")
granova.1w(arbre$hauteur,arbre$foret)
dev.off()
postscript("chap10fig102.ps")
granova.1w(arbre$hauteur,arbre$foret)
dev.off()
pdf("chap10fig102bw.pdf",colormodel="gray")
granova.1w(arbre$hauteur,arbre$foret)
dev.off()
postscript("chap10fig102bw.ps",colormodel="gray")
granova.1w(arbre$hauteur,arbre$foret)
dev.off()
png("chap10fig103.png")
granovagg.1w(arbre$hauteur,arbre$foret)
dev.off()
postscript("chap10fig103.ps")
granovagg.1w(arbre$hauteur,arbre$foret)
dev.off()
pdf("chap10fig103bw.pdf",colormodel="gray")
granovagg.1w(arbre$hauteur,arbre$foret)
dev.off()
postscript("chap10fig103bw.ps",colormodel="gray")
granovagg.1w(arbre$hauteur,arbre$foret)
dev.off()

#page 426
#Exercice 10.1
#1)
options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))

#page 427
#2)
variete<-rep(1:6,c(5,5,5,5,5,5))
vitamine<-c(93.6,95.3,96,93.7,96.2,95.3,96.9,95.8,97.3,97.7,94.5,97,97.8,97,
98.3,98.8,98.2,97.8,97.2,97.9,94.6,97.8,98,95,98.9,93.2,94.4,93.8,95.6,94.8)
variete<-factor(variete)
exo1<-data.frame(variete,vitamine)
modele1<-aov(vitamine~variete,data=exo1)
residus1<-residuals(modele1)
shapiro.test(residus1)
length(residus1)
bartlett.test(residus1~variete,data=exo1)

#page 428
#3)
modele1
summary(modele1)

#page 429
#4)
granovagg.1w(vitamine,group=variete)
pdf("chap10fig105.pdf")
granovagg.1w(vitamine,group=variete)
dev.off()

#page 431
#6)
Tukey1 <- TukeyHSD(modele1, conf.level = 0.95)
Tukey1

#page 432
#4)
if(!("multcomp" %in% rownames(installed.packages()))){
  install.packages("multcomp")}
library(multcomp)
wht = glht(modele1, linfct = mcp(variete = "Tukey"))
cld(wht)
plot(Tukey1)
pdf("chap10fig106.pdf")
plot(Tukey1)
dev.off()

#page 433
CI <- confint(wht)
fortify(CI)
ggplot(CI,aes(lhs,estimate,ymin=lwr,ymax=upr))+geom_pointrange()+
  geom_hline(yintercept = 0)
pdf("chap10fig107.pdf")
print(ggplot(CI,aes(lhs,estimate,ymin=lwr,ymax=upr))+geom_pointrange()+
  geom_hline(yintercept = 0))
dev.off()

ggplot(aes(lhs,estimate),data=fortify(summary(wht))) + 
  geom_linerange(aes(ymin=lwr,ymax=upr),data=CI) + 
  geom_text(aes(y=estimate+1,label=round(p,3)))+geom_hline(yintercept = 0) +
  geom_point(aes(size=p),data=summary(wht)) +scale_size(trans="reverse")
pdf("chap10fig108.pdf")
ggplot(aes(lhs,estimate),data=fortify(summary(wht))) + 
  geom_linerange(aes(ymin=lwr,ymax=upr),data=CI) + 
  geom_text(aes(y=estimate+1,label=round(p,3)))+geom_hline(yintercept = 0) +
  geom_point(aes(size=p),data=summary(wht)) +scale_size(trans="reverse")
dev.off()

#page 434
if(!("multcompView" %in% rownames(installed.packages()))){
  install.packages("multcompView")}
library(multcompView)
if(!("plyr" %in% rownames(installed.packages()))){install.packages("plyr")}
library(plyr)
generate_label_df <- function(HSD,flev){
Tukey.levels <- HSD[[flev]][,4]
Tukey.labels <- multcompLetters(Tukey.levels)['Letters']
plot.labels <- names(Tukey.labels[['Letters']])
boxplot.df <- ddply(exo1, flev, function (x) max(fivenum(x$vitamine)) + 0.2)
plot.levels <- data.frame(plot.labels, labels = Tukey.labels[['Letters']],
  stringsAsFactors = FALSE)
labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev,
  sort = FALSE)
return(labels.df)
}

#page 435
p_base <- ggplot(exo1,aes(x=variete,y=vitamine)) + geom_boxplot() + 
  geom_text(data = generate_label_df(Tukey1, 'variete'), aes(x = plot.labels, 
  y = V1, label = labels))
p_base
pdf("chap10fig109.pdf")
print(p_base)
dev.off()

#page 436
#Exercice 10.1
#2)
traitement<-rep(1:5,c(7,7,7,7,7))
taux<-c(4.5,2.5,6,4.5,3,5.5,3.5,7.5,3,2.5,4,2,4,5.5,8,6.5,6,3.5,5,
  7,5,2,7.5,4,2.5,5,3.5,6.5,6.5,5.5,6,4.5,4,7,5.5)
traitement<-factor(traitement)
exo2<-data.frame(traitement,taux)
modele2<-aov(taux~traitement,data=exo2)
residus2<-residuals(modele2)
shapiro.test(residus2)
length(residus2)
bartlett.test(residus2~traitement,data=exo2)

#page 437
#3)
modele1<-lm(taux~traitement,data=exo2)
anova(modele1)
#4)
power.anova.test(5,7,19.043,76.42857)

#page 438
power.anova.test(groups=5,between.var=19.043,within.var=76.42857,power=.80)
granovagg.1w(taux,group=traitement)
pdf("chap10fig1010.pdf",colormodel="gray")
granovagg.1w(taux,group=traitement)
dev.off()











