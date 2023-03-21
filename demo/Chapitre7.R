#' ---
#' title: "Initiation \u00e0 la statistique avec R, code et compl\u00e9ments chapitre 7"
#' author: "Fr\u00e9d\u00e9ric Bertrand et Myriam Maumy-Bertrand"
#' date: "20 mars 2023"
#' ---

#Chapitre 7
require(BioStatR)
#page 300
gaz<-c(52.0,60.2,68.8,46.8,62.2,53.5,50.9,44.9,73.2,60.4,61.9,
  67.8,30.5,52.5,40.4,29.6,58.3,62.6,53.6,64.6,54.4,53.8,49.8,
  57.4,63.1,53.4,59.4,48.6,40.7,51.9)
shapiro.test(gaz)
length(gaz)

#page 301
(z<-(sqrt(30)*(mean(gaz)-50))/10)
qnorm(0.95)
if(!("TeachingDemos" %in% rownames(installed.packages()))){
  install.packages("TeachingDemos")}

#page 302
library(TeachingDemos)
z.test(gaz,mu=50,sd=10,alternative="greater",conf.level=0.95)

#page 303
glycine<-subset(Mesures,subset=(Mesures$espece=="glycine blanche"))
shapiro.test(glycine$taille)

#page 304
length(glycine$taille)
t.test(glycine$taille,mu=15)
power.t.test(n=54,delta=mean(glycine$taille)-15,
  sd=sd(glycine$taille),type="one.sample",alternative="two.sided")

#page 305
power.t.test(power=.8,delta=mean(glycine$taille)-15,
  sd=sd(glycine$taille),type="one.sample",alternative="two.sided")

#page 307
pesee<-c(2.53,1.51,1.52,1.44,4.32,2.36,2.41,2.06,1.57,1.68,
  3.09,0.54,2.32,0.19,2.66,2.20,1.04,1.02,0.74,1.01,
  0.35,2.42,2.66,1.11,0.56,1.75,1.51,3.80,2.22,2.88)
shapiro.test(pesee)
length(pesee)
((length(pesee)-1)*var(pesee))/4

#page 308
qchisq(0.95,29)
library(TeachingDemos)
sigma.test(pesee,sigma=2,alternative="greater")
if(!("OneTwoSamples" %in% rownames(installed.packages()))){
  install.packages("OneTwoSamples")}
library(OneTwoSamples)
var_test1(pesee,sigma2=4)

#page 310
binom.test(507,988,0.5)

#page 317
pipit<-c(17.0,16.9,16.9,17.3,16.8,16.8,17.0,16.5,16.9,16.5,
  17.0,17.0,16.8,17.0,16.9,17.0,17.0,17.3,16.8,17.1,16.9,16.8,
  17.1,17.0,17.1,17.2,16.7,16.6,17.2,17.0,17.0)
fauvette<-c(16.0,16.1,16.3,16.5,16.2,15.2,15.6,15.6,16.6,16.0,
  16.2,16.8,16.0,17.0,17.9,16.0,16.4,16.3,16.9,17.1,17.0,16.1,
  16.5,16.5,16.1,16.5,17.9,16.5,16.7,16.8)
shapiro.test(pipit)
length(pipit)
shapiro.test(fauvette)
length(fauvette)

#page 318
var.test(pipit,fauvette)
t.test(pipit,fauvette,var.equal=FALSE)
t.test(pipit,fauvette)

#page 325
#Probl\`eme 7.1
#2)
glycines<-subset(Mesures,subset=(Mesures$espece=="glycine violette"
  |Mesures$espece=="glycine blanche"))
glycines$espece<-factor(glycines$espece)
tapply(glycines$taille,glycines$espece,summary)
tapply(glycines$taille,glycines$espece,sd)

#page 326
#4)
layout(matrix(c(1,2,1,3),nrow=2,ncol=2,byrow=F))
boxplot(taille~espece,data=glycines)
glycine_blanche<-glycines[glycines$espece=="glycine blanche",]
qqnorm(glycine_blanche$taille,ylab="Taille des glycines blanches")
qqline(glycine_blanche$taille)
glycine_violette<-glycines[glycines$espece=="glycine violette",]
qqnorm(glycine_violette$taille,ylab="Taille des glycines violettes")
qqline(glycine_violette$taille)

#Page 327
#7)
wilcox.test(taille~espece,data=glycines,conf.int=TRUE)

#Page 330
#Exercice 7.1
#1)
jus_orange=c(8.2,9.4,9.6,9.7,10.0,14.5,15.2,16.1,17.6,21.5,14.0,13.8,
  12.8,15.0,9.5,10.9,12.4,14.7,10.7,11.1,13.8,13.1,8.6,13.9,15.2,13.6,13.4,
  12.3,15.2,11.2,19.6,7.8,14.1,12.5,14.1,17.6,13.5,12.4,12.6,14.6,15.5,11.6,
  11.8,12.9,8.1,11.8,18.7,12.6,16.0,15.8,17.2,16.4,11.2,10.2,13.6,13.2,15.9,
  9.8,8.8,12.0)
acide_ascorbique=c(4.2,5.2,5.8,6.4,7.0,7.3,10.1,11.2,11.3,11.5,7.1,9.8,
  5.3,4.8,11.9,10.1,12.5,14.6,4.9,9.7,7.0,3.8,5.0,9.3,8.7,8.7,8.7,9.5,2.5,
  6.6,13.6,6.6,9.4,12.1,13.1,4.1,12.1,8.8,7.0,7.5)
#2)
shapiro.test(jus_orange)
length(jus_orange)

#Page 331
shapiro.test(acide_ascorbique)
length(acide_ascorbique)
#4)
var.test(jus_orange,acide_ascorbique)

#Page 332
t.test(jus_orange,acide_ascorbique,alternative="greater",var.equal=TRUE)

#Exercice 7.2
#1)
avnt<-c(15,18,17,20,21,18,17,15,19,16,19,17,19,15,14,16,21,20,21,18,17,17,
  17,15,17,18,16,10,17,18,14,15,15,17,17,20,17)
aprs<-c(12,16,17,18,17,15,18,14,16,18,20,16,15,17,18,16,15,14,11,13,13,15,
  14,15,19,14,16,14,14,15,19,19,16,19,15,17,16)
mode(avnt)

#Page 333
mode(aprs)
length(avnt)
length(aprs)

#2)
diff<-aprs-avnt
diff
#4)
shapiro.test(diff)

#Page 334
length(diff)
#5)
t.test(diff)

#page 335
#Probl\`eme 7.1
glycines<-subset(Mesures,subset=(Mesures$espece=="glycine violette"|Mesures$espece=="glycine blanche"))
glycines$espece<-factor(glycines$espece)
#2)
tapply(glycines$taille,glycines$espece,summary)
tapply(glycines$taille,glycines$espece,sd)

#page 336
#4)
layout(matrix(c(1,2,1,3),nrow=2,ncol=2,byrow=F))
boxplot(taille~espece,data=glycines,main="Bo^ites \`a moustaches")
glycine_blanche<-glycines[glycines$espece=="glycine blanche",]
qqnorm(glycine_blanche$taille,ylab="Taille des glycines blanches")
qqline(glycine_blanche$taille)
glycine_violette<-glycines[glycines$espece=="glycine violette",]
qqnorm(glycine_violette$taille,ylab="Taille des glycines violettes")
qqline(glycine_violette$taille)

#page 337
#6)
tapply(glycines$taille,glycines$espece,shapiro.test)
tapply(glycines$taille,glycines$espece,length)

#page 338
#8)
wilcox.test(taille~espece,data=glycines,conf.int=TRUE)

#Probl\`eme 7.2
#1)
lauriers<-subset(Mesures5,subset=(Mesures5$espece=="laurier rose"))
#2)
str(lauriers)

#page 339
#3)
la_masse<-lauriers$masse
la_masse_sec<-lauriers$masse_sec
diff_laurier<-(la_masse-la_masse_sec)
#4)
layout(matrix(c(1,2),nrow=1,ncol=2,byrow=F))
boxplot(diff_laurier,ylab="Diff\'erence entre la masse et la masse s\`eche pour une graine de 
  laurier",main="Bo^ite \`a moustaches")
abline(h=0, lty=2)
qqnorm(diff_laurier,ylab="Diff\'erence entre la masse et la masse s\`eche")
qqline(diff_laurier)

#page 340
#6)
shapiro.test(diff_laurier)
length(diff_laurier)
#7)
t.test(diff_laurier)

#page 341
#9)
wilcox.test(diff_laurier)
t.test(lauriers$masse,lauriers$masse_sec,paired=TRUE)
wilcox.test(lauriers$masse,lauriers$masse_sec,paired=TRUE)
